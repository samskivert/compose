import { computed, observable, observe } from 'mobx'
import { Name } from './names'
import * as S from './symbols'
import * as T from './trees'
import * as TP from './types'

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

  private trees :Map<number, T.Tree> = new Map()

  constructor (name :Name) {
    this.sym.name = name
  }

  tree (id :number) :T.Tree {
    const tree = this.trees.get(id)
    if (tree) return tree
    throw new Error(`Missing tree, id: ${id}`)
  }

  mkDefHole () :T.DefTree {
    const sym = new DefSym(this, "term", "none", 0, "")
    const tree = new T.DefHoleTree(sym, this.scope)
    return tree
  }

  mkFunDef (name :Name, id :number = this.nextSymId()) :T.FunDefTree {
    const sym = new DefSym(this, "term", "func", id, name)
    const tree = new T.FunDefTree(sym, this.scope)
    this.trees.set(sym.id, tree)
    this.index.set(sym.id, sym)
    return tree
  }

  mkTypeDef (name :Name, id :number = this.nextSymId()) :T.FunDefTree {
    const sym = new DefSym(this, "type", "none", id, name)
    const tree = new T.TypeDefTree(sym, this.scope)
    this.trees.set(sym.id, tree)
    this.index.set(sym.id, sym)
    return tree
  }

  inflateDef (json :any) :T.DefTree {
    let tree :T.DefTree
    const kind :string = json.kind, {name, id} = json.sym
    switch (kind) {
    case  "fundef": tree = this.mkFunDef(name, id) ; break
    case "typedef": tree = this.mkTypeDef(name, id) ; break
    default:
      throw new Error(`Unknown deftree kind: '${json.kind}'`)
    }

    const locals :Map<number, S.Symbol> = new Map()
    const index :T.SzIndex = {
      add: (sym :S.Symbol) => {
        locals.set(sym.id, sym)
      },
      req: (whence :T.Whence, id :number) => {
        let sym :S.Symbol|void
        switch (whence) {
        case "l": sym = locals.get(id) ; break
        case "m": sym = this.index.get(id) ; break
        case "x": break // TODO
        }
        return sym || new S.MissingSym("term", `${whence}:${id}`)
      },
      // ids: (sym :S.Symbol) => {
      //   let whence :string
      //   if (sym.lexical) whence = "l"
      //   else if (sym.root === mod.sym) whence = "m"
      //   else whence = "x"
      //   // TODO: assign persistent id?
      //   return {whence, id: sym.reqId}
      // }
    }

    return tree.setBranch("body", T.inflateTree(index, json.body))
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
  get type () :TP.Type { return this.mod.tree(this.id).sig }
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

export function inflateMod (json :any) :Module {
  const mod = new Module(json.name)
  for (let defJson of json.defs) mod.inflateDef(defJson)
  return mod
}
