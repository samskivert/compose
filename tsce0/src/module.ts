import { computed, observable, observe } from 'mobx'
import { Name } from './names'
import * as S from './symbols'
import * as T from './trees'
import * as TP from './types'

/** A globally unique id used to identify projects, components & modules across time and space. */
export type UUID = string

type XRef = {uuid :UUID, mid :number}

/** Maintains the symbol and tree information for a single module. */
export class Module implements S.Index {

  /** An index of module-scoped symbols defined by this module. */
  index :Map<number, S.Symbol> = observable.map()

  /** The symbol that represents this module. */
  readonly sym = new ModuleSym(this)

  /** A scope that encloses all of this module's module-scoped symbols. */
  readonly scope = new ModuleScope(this)

  /** The human readable name of this module. */
  @computed get name () :Name { return this.sym.name }

  private trees :Map<number, T.DefTree> = new Map()
  private jsons :Map<number, any> = new Map()

  constructor (readonly uuid :UUID, name :Name,
               private readonly resolver :Resolver,
               private readonly xrefs :Map<number, XRef>) {
    this.sym.name = name
  }

  tree (sym :DefSym) :T.DefTree {
    const tree = this.trees.get(sym.id)
    if (tree) return tree
    const json = this.jsons.get(sym.id)
    if (json) {
      this.jsons.delete(sym.id)
      return this.inflateDef(sym, json)
    }
    throw new Error(`Missing tree, id: ${sym.id}`)
  }

  mkDefHole () :T.DefTree {
    const sym = new DefSym(this, "term", "none", 0, "")
    const tree = new T.DefHoleTree(sym, this.scope)
    return tree
  }

  mkFunDef (name :Name, id :number = this.nextSymId()) :T.DefTree {
    const sym = this.mkDefSym("fundef", id, name)
    const tree = new T.FunDefTree(sym, this.scope)
    this.trees.set(sym.id, tree)
    return tree
  }

  mkTypeDef (name :Name, id :number = this.nextSymId()) :T.DefTree {
    const sym = this.mkDefSym("typedef", id, name)
    const tree = new T.TypeDefTree(sym, this.scope)
    this.trees.set(sym.id, tree)
    return tree
  }

  indexDef (json :any) :DefSym {
    const sym = this.mkDefSym(json.kind, json.sym.id, json.sym.name)
    this.jsons.set(sym.id, json)
    return sym
  }

  // from S.Index
  nextSymId () :number {
    return 0 // TODO
  }
  insert (sym :S.Symbol) {
    this.index.set(sym.id, sym)
  }
  remove (sym :S.Symbol) {
    this.index.delete(sym.id || 0)
  }

  private mkDefSym (kind :string, id :number, name :string) :DefSym {
    let sym :DefSym
    switch (kind) {
    case  "fundef": sym = new DefSym(this, "term", "func", id, name) ; break
    case "typedef": sym = new DefSym(this, "type", "none", id, name) ; break
    default: throw new Error(`Unknown def kind: '${kind}'`)
    }
    this.index.set(sym.id, sym)
    return sym
  }

  private inflateDef (sym :DefSym, json :any) :T.DefTree {
    let tree :T.DefTree
    switch (json.kind) {
    case  "fundef": tree = new T.FunDefTree(sym, this.scope) ; break
    case "typedef": tree = new T.TypeDefTree(sym, this.scope) ; break
    default: throw new Error(`Unknown deftree kind: '${json.kind}'`)
    }
    this.trees.set(sym.id, tree)

    const locals :Map<number, S.Symbol> = new Map()
    const index :T.SzIndex = {
      add: (sym :S.Symbol) => {
        locals.set(sym.id, sym)
      },
      req: (whence :T.Whence, id :number) => {
        switch (whence) {
        case "l": return locals.get(id) || new S.MissingSym("term", `l${id}`)
        case "m": return this.index.get(id) || new S.MissingSym("term", `m${id}`)
        case "x":
          let xref = this.xrefs.get(id)
          if (!xref) return new S.MissingSym("term", `x${id}`)
          let {uuid, mid} = xref
          let xmod = this.resolver.resolve(uuid)
          if (!xmod) return new S.MissingSym("term", `x${id}:${uuid}`)
          return xmod.index.get(mid) || new S.MissingSym("term", `x${id}:${uuid}:m${mid}`)
        }
      },
    }

    return tree.setBranch("body", T.inflateTree(index, json.body))
  }
}

export class ModuleSym extends S.Symbol {
  // TODO: modules need a unique hash
  constructor (readonly mod :Module) { super("module", "none", 0, name) }
  get scope () :S.Scope { return this.mod.scope }
  get type () :TP.Type { return TP.hole } // TODO: special module type? none type?
  get owner () :S.Symbol { return this }
  get index () :S.Index { return this.mod }
  toString () { return `msym#${this.mod.name}` }
}

export class DefSym extends S.Symbol {
  constructor (readonly mod :Module, kind :S.Kind, flavor :S.Flavor, id :number, name :Name) {
    super(kind, flavor, id, name)
  }
  get type () :TP.Type { return this.mod.tree(this).sig }
  get owner () :S.Symbol { return this.mod.sym }
  get index () :S.Index { return this.mod }
}

type Disposer = () => void

export class ModuleScope extends S.Scope {
  // TODO: we really want a tree map or something we can prefix query
  symbols = new Map<Name,S.Symbol[]>()
  listeners = new Map<S.Symbol,Disposer>()

  constructor (readonly mod :Module) {
    super()
    observe(mod.index, change => {
      const sym :S.Symbol = change.object.get(change.name)
      switch (change.type) {
      case "add": this.onInsert(sym) ; break
      case "update": break // TODO: anything?
      case "remove": this.onRemove(sym) ; break
      }
    })
  }

  // TODO: revamp to return all matching symbols
  lookup (kind :S.Kind, name :Name) :S.Symbol {
    const syms = this.symbols.get(name)
    if (syms) {
      for (let sym of syms) {
        if (sym.kind == kind) return sym
      }
    }
    return new S.MissingSym(kind, name)
  }

  toString () :string {
    return `<module:${this.mod.name}>`
  }

  _addCompletions (pred :(sym :S.Symbol) => Boolean, prefix :string, syms :S.Symbol[]) {
    // TODO: such inefficient, so expense
    for (let symvec of Array.from(this.symbols.values())) {
      for (let sym of symvec) {
        if (pred(sym) && this.isCompletion(prefix, sym)) {
          syms.push(sym)
        }
      }
    }
  }

  private onInsert (sym :S.Symbol) {
    if (sym.name !== "") this.map(sym.name, sym)
    this.listeners.set(sym, observe(sym, "name", change => {
      if (change.oldValue && change.oldValue !== "") this.unmap(change.oldValue, sym)
      if (change.newValue !== "") this.map(change.newValue, sym)
    }))
  }

  private onRemove (sym :S.Symbol) {
    if (sym.name !== "") this.unmap(sym.name, sym)
    const disp = this.listeners.get(sym)
    disp && disp()
  }

  private map (name :Name, sym :S.Symbol) {
    const syms = this.symbols.get(name)
    if (syms) syms.push(sym)
    else this.symbols.set(name, [sym])
  }

  private unmap (name :Name, sym :S.Symbol) {
    const syms = this.symbols.get(name)
    if (syms) {
      const idx = syms.indexOf(sym)
      if (idx >= 0) syms.splice(idx, 1)
    }
  }
}

/** Used to resolve module cross references when inflating a module. */
export interface Resolver {
  /** Resolves and returns the module identified by `uuid`. `undefined` is returned if the module
    * cannot be located. This is indicative of a corrupted module and/or project. */
  resolve (uuid :UUID) :Module|void
}

export function inflateMod (resolver :Resolver, json :any) :Module {
  // convert the xrefs table into a more more useful runtime format (from xid to uuid+mid)
  const xrefs :Map<number,XRef> = new Map()
  const xrefsJson :{[uuid :string] :{[xid :string] :number}} = json.xrefs
  for (let uuid in xrefsJson) {
    const mrefs = xrefsJson[uuid]
    for (let xid in mrefs) {
      const mid = mrefs[xid]
      xrefs.set(parseInt(xid), {uuid, mid})
    }
  }
  const mod = new Module(json.uuid, json.name, resolver, xrefs)
  for (let defJson of json.defs) mod.indexDef(defJson)
  return mod
}
