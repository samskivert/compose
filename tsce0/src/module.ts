import { computed, observable, observe } from "mobx"
import { Name, UUID } from "./names"
import * as S from "./symbols"
import * as T from "./trees"
import * as TP from "./types"

type XRef = {uuid :UUID, mid :number}
type XRefsJson = {[uuid :string] :number[]}

/** Tracks references to symbols outside a module (external refs). A relation is maintained:
  * `{uuid,mid}` <-> `{xid}`. The uuid+mid pair identifies the module in which the external
  * reference is defined and its id in that module, and xid is the unique (within this referencing
  * module) id assigned to that external symbol.
  *
  * As this information is used to maintain the persistent representation of a module, we keep the
  * xrefs around after inflating a module so that we can reuse the xid assignments when it is next
  * deflated.
  */
class XRefs {
  private readonly _rev :Map<number,XRef> = new Map()
  private readonly _fwd :Map<UUID,Map<number,number>> = new Map()

  constructor (json :XRefsJson) {
    for (let uuid in json) {
      const mfwd :Map<number,number> = new Map()
      this._fwd.set(uuid, mfwd)
      const mrefs = json[uuid]
      for (let ii = 0; ii < mrefs.length; ii += 2) {
        const mid = mrefs[ii], xid = mrefs[ii+1]
        mfwd.set(mid, xid)
        this._rev.set(xid, {uuid, mid})
      }
    }
  }

  /** Returns the `xid` for `sym` (which is assumed to be external).
    * If no `xid` has yet been assigned to `sym`, one will be assigned and returned. */
  fwd (sym :S.Symbol) :number {
    let root = sym.root
    if (!(root instanceof ModuleSym)) throw new Error(
      `Cannot create xref for non-module sym: ${sym}`)
    const uuid = root.mod.uuid
    let mfwd = this._fwd.get(uuid)
    if (mfwd === undefined) this._fwd.set(uuid, mfwd = new Map())
    const mid = sym.id
    let xid = mfwd.get(mid)
    if (xid === undefined) {
      xid = Array.from(mfwd.values()).reduce((a, b) => Math.max(a, b), 0)+1
      mfwd.set(mid, xid)
      this._rev.set(xid, {uuid, mid})
    }
    return xid
  }

  /** Returns `{uuid,mid}` for the specified `xid`, if a mapping exists. */
  rev (xid :number) :XRef|void {
    return this._rev.get(xid)
  }

  /** Converts this xrefs table into a serializable format. */
  deflate () :XRefsJson {
    const json :XRefsJson = {}
    for (let uuid of Array.from(this._fwd.keys()).sort()) {
      const mrefs = this._fwd.get(uuid) || new Map()
      json[uuid] = Array.from(mrefs.keys()).reduce( // poor man's flatMap, woo!
        (arr, mid) => arr.concat([mid, mrefs.get(mid) || 0]), [] as number[])
    }
    return json
  }
}

/** Used to resolve module cross references. */
export interface Resolver {
  /** Resolves and returns the module identified by `uuid`. `undefined` is returned if the module
    * cannot be located. This is indicative of a corrupted module and/or project. */
  resolveModule (uuid :UUID) :Module|void
}

/** Used to load serialized module data. */
export interface Loader {
  /** Loads the serialized data for the module identified by `uuid`.
    * `undefined` is returend if the module does not exist. */
  loadModule (uuid :UUID) :ModuleJson|void
}

type DefJson = any
export type ModuleJson = {uuid :UUID, name :string, xrefs :XRefsJson, defs :DefJson[]}

/** Maintains the symbol and tree information for a single module. */
export class Module implements S.Index {

  /** Identifies this module uniquely across space and time. */
  readonly uuid :UUID
  /** The symbol that represents this module. */
  readonly sym :ModuleSym
  /** A scope that encloses all of this module's module-scoped symbols. */
  readonly scope :ModuleScope
  /** The human readable name of this module. */
  @computed get name () :Name { return this.sym.name }
  /** The top-level definitions in this module. */
  @observable defs :DefSym[] = []

  private readonly index :Map<number, S.Symbol> = new Map()
  private readonly trees :Map<number, T.DefTree> = new Map()
  private readonly jsons :Map<number, DefJson> = new Map()
  private readonly xrefs :XRefs

  constructor (json :ModuleJson, cscope :S.Scope, private readonly resolver :Resolver) {
    this.uuid = json.uuid
    this.sym = new ModuleSym(this, json.name)
    this.scope = new ModuleScope(this, cscope)
    this.xrefs = new XRefs(json.xrefs)
  }

  /** Returns the symbol defined by this module with persistent `id`, or `undefined` if no symbol
    * exists with that id. */
  symById (id :number) :S.Symbol|void {
    return this.index.get(id)
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

  addFunDef (name :Name, id :number = this.nextSymId()) :T.DefTree {
    const sym = this.mkDefSym("fundef", id, name)
    const tree = new T.FunDefTree(sym, this.scope)
    tree.setBranch(
      "body", new T.AbsTree(1, "").setBranch(
        "body", new T.AscTree().
          setBranch("type", new T.THoleTree()).
          setBranch("expr", new T.HoleTree())))
    // focus: new T.Path("sym")
    this.trees.set(sym.id, tree)
    this.defs.push(sym)
    return tree
  }

  addTypeDef (name :Name, id :number = this.nextSymId()) :T.DefTree {
    const sym = this.mkDefSym("typedef", id, name)
    const tree = new T.TypeDefTree(sym, this.scope)
    this.trees.set(sym.id, tree)
    this.defs.push(sym)
    return tree
  }

  addProdDef (name :Name, id :number = this.nextSymId()) :T.DefTree {
    const tree = this.addTypeDef(name, id)
    tree.setBranch(
      "body", new T.CtorTree(1, "").setBranch(
        "prod", new T.ProdTree().setBranch(
          "0", new T.FieldTree(2, "").setBranch(
            "type", new T.THoleTree()))))
    //   focus: new T.Path("sym")
    return tree
  }

  addSumDef (name :Name, id :number = this.nextSymId()) :T.DefTree {
    const tree = this.addTypeDef(name, id)
    tree.setBranch(
      "body", new T.SumTree().setBranch(
        "0", new T.CtorTree(1, "").setBranch(
          "prod", new T.ProdTree())))
    //   focus: new T.Path("sym")
    return tree
  }

  indexDef (json :DefJson) :DefSym {
    const sym = this.mkDefSym(json.kind, json.sym.id, json.sym.name)
    // TODO: we need to know the entire "signature" of the definition; for types this is pretty much
    // the entire thing, for terms (funs) it is enough to reconstruct the type of the term; in the
    // case of types we should probably just decode the entire tree, but for terms we may want to
    // just duplicate the signature separately (via a type tree child of the 'termdef' node?)
    this.jsons.set(sym.id, json)
    this.defs.push(sym)
    return sym
  }

  deflate () :ModuleJson {
    const index = {
      enc: (sym :S.Symbol) :string => {
        if (sym.lexical) return `l${sym.id}`
        else if (sym.root == this.sym) return `m${sym.id}`
        else return `x${this.xrefs.fwd(sym)}`
      }
    }
    // def deflation may update xrefs, so deflate defs prior to deflating xrefs table
    let defs = this.defs.map(sym => T.deflateTree(index, this.tree(sym)))
    // TODO: we need to tell the xrefs we're about to deflate all our defs so that it can keep
    // track of which xrefs are used and then prune all unused xrefs prior to deflate()
    let xrefs = this.xrefs.deflate()
    return {uuid: this.uuid, name: this.name, xrefs, defs}
  }

  // from S.Index
  nextSymId () :number {
    return 0 // TODO
  }
  insert (sym :S.Symbol) {
    this.index.set(sym.id, sym)
    this.scope.onInsert(sym)
  }
  remove (sym :S.Symbol) {
    this.index.delete(sym.id || 0)
    this.scope.onRemove(sym)
  }

  private mkDefSym (kind :string, id :number, name :string) :DefSym {
    let sym :DefSym
    switch (kind) {
    case  "fundef": sym = new DefSym(this, "term", "func", id, name) ; break
    case "typedef": sym = new DefSym(this, "type", "none", id, name) ; break
    default: throw new Error(`Unknown def kind: '${kind}'`)
    }
    this.insert(sym)
    return sym
  }

  private inflateDef (sym :DefSym, json :DefJson) :T.DefTree {
    let tree :T.DefTree
    switch (json.kind) {
    case  "fundef": tree = new T.FunDefTree(sym, this.scope) ; break
    case "typedef": tree = new T.TypeDefTree(sym, this.scope) ; break
    default: throw new Error(`Unknown deftree kind: '${json.kind}'`)
    }
    this.trees.set(sym.id, tree)

    const locals :Map<number, S.Symbol> = new Map()
    const index = {
      add: (sym :S.Symbol) => {
        locals.set(sym.id, sym)
      },
      req: (encId :string) => {
        const whence = encId.substring(0, 1)
        const id = parseInt(encId.substring(1))
        switch (whence) {
        case "l": return locals.get(id) || new S.MissingSym("term", `l${id}`)
        case "m": return this.index.get(id) || new S.MissingSym("term", `m${id}`)
        case "x":
          let xref = this.xrefs.rev(id)
          if (!xref) return new S.MissingSym("term", `x${id}`)
          let {uuid, mid} = xref
          let xmod = this.resolver.resolveModule(uuid)
          if (!xmod) return new S.MissingSym("term", `x${id}:${uuid}`)
          return xmod.index.get(mid) || new S.MissingSym("term", `x${id}:${uuid}:m${mid}`)
        default: throw new Error(`Invalid encoded sym id '${encId}'`)
        }
      },
    }

    try {
      return tree.setBranch("body", T.inflateTree(index, json.body))
    } catch (error) {
      console.warn(`Failed to inflate tree for def: ${sym}`)
      console.warn(JSON.stringify(json.body, undefined, " "))
      throw error
    }
  }
}

export class ModuleSym extends S.Symbol {
  constructor (readonly mod :Module, name :Name) { super("module", "none", 0, name) }
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
  get displayName () :string { return this.name || "?" }
}

type Disposer = () => void

export class ModuleScope extends S.Scope {
  // TODO: we really want a tree map or something we can prefix query
  symbols = new Map<Name,S.Symbol[]>()
  listeners = new Map<S.Symbol,Disposer>()

  constructor (readonly mod :Module, readonly cscope :S.Scope) { super() }

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

  addCompletions (pred :(sym :S.Symbol) => Boolean, prefix :string, syms :S.Symbol[]) {
    // TODO: such inefficient, so expense
    this.symbols.forEach((nsyms, name) => {
      for (let sym of nsyms) {
        if (pred(sym) && this.isCompletion(prefix, sym)) {
          syms.push(sym)
        }
      }
    })
  }

  _addCompletions (pred :(sym :S.Symbol) => Boolean, prefix :string, syms :S.Symbol[]) {
    this.addCompletions(pred, prefix, syms)
    this.cscope._addCompletions(pred, prefix, syms)
  }

  onInsert (sym :S.Symbol) {
    if (sym.name !== "") this.map(sym.name, sym)
    this.listeners.set(sym, observe(sym, "name", change => {
      if (change.oldValue && change.oldValue !== "") this.unmap(change.oldValue, sym)
      if (change.newValue !== "") this.map(change.newValue, sym)
    }))
  }

  onRemove (sym :S.Symbol) {
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

export function inflateMod (cscope :S.Scope, resolver :Resolver, json :ModuleJson) :Module {
  const mod = new Module(json, cscope, resolver)
  for (let defJson of json.defs) mod.indexDef(defJson)
  return mod
}
