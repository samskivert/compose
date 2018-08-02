import { Name } from "./names"
import * as C from "./constants"
import * as S from "./symbols"
import * as TP from "./types"

// ------------
// Syntax trees
// ------------

// TODO: create doc AST, add Doc branch to every node that defines a name (Def, Let, Abs, etc.)

type Branch = Tree|S.Symbol|C.Constant

let treeId = 0
function nextTreeId () { treeId += 1 ; return treeId }

export abstract class Tree {
  readonly id = nextTreeId()

  // initialized by link()
  protected _parent! :Tree|void
  protected _parentId! :string
  protected _owner! :S.ModuleSym|TreeSym
  protected _scope! :S.Scope

  get parent () :Tree|void { return this._parent }
  get parentId () :string { return this._parentId }
  get owner () :S.ModuleSym|TreeSym { return this._owner }
  get scope () :S.Scope { return this._scope }
  get kind () :string {
    const name = this.constructor.name
    return name.substring(0, name.length-4).toLowerCase() // trim the tree
  }

  get requireParent () :Tree {
    if (this.parent) return this.parent
    throw new Error(`Missing required parent @ ${this}`)
  }

  get isHole () :boolean { return false }
  abstract get sig () :TP.Type

  get branchIds () :string[] { return [] }
  branch (id :string) :Branch { return this[id] }
  setBranch (id :string, branch :Branch) :this {
    const oldBranch = this.branch(id)
    if (oldBranch instanceof Tree) oldBranch.unlink()
    // if we're linked, then link this new branch, otherwise wait until we're linked
    if (this._owner && branch instanceof Tree) branch.link(this, id)
    this.storeBranch(id, branch)
    return this
  }
  protected storeBranch (id :string, branch :Branch) {
    this[id] = branch
  }

  /** Returns the prototype (expected type) for this tree. */
  get prototype () :TP.Type {
    const proto = this.parent && this.parent.childPrototype(this.parentId)
    return proto || TP.hole
  }
  protected childPrototype (id :string) :TP.Type|void { return undefined }

  // TODO: ugh...
  sigQuants () :TypeVarTreeSym[] {
    const acc :TypeVarTreeSym[] = []
    this._sigQuants(acc)
    return acc
  }
  protected _sigQuants (acc :TypeVarTreeSym[]) {
    if (this.parent) this.parent._sigQuants(acc)
  }

  link (parent :Tree, parentId :string) {
    // if (this._parent === parent && this._parentId === parentId) return // NOOP
    if (this._parent) throw new Error(`Cannot link already linked child ${this} (to ${parent})`)
    this.didLink(parent, parentId)
    for (let id of this.branchIds) {
      const branch = this.branch(id)
      if (branch instanceof Tree) branch.link(this, id)
    }
  }
  protected didLink (parent :Tree, parentId :string) {
    this._parent = parent
    this._parentId = parentId
    this._owner = parent.childOwner(parentId)
    this._scope = parent.childScope(parentId)
  }

  unlink () {
    if (this._parent) {
      this.willUnlink()
      for (let id of this.branchIds) {
        const branch = this.branch(id)
        if (branch instanceof Tree) branch.unlink()
      }
    }
  }
  protected willUnlink () {
    this._parent = undefined
  }

  get firstEditableId () :string|void { return this.branchIds[0] }

  treeAt (id :string) :Tree {
    this.checkBranchId(id, "tree@")
    const branch = this.branch(id)
    if (branch instanceof Tree) return branch
    else throw new Error(`Fetched branch ${id} as tree, but is not tree: ${branch}`)
  }
  symAt (id :string) :TreeSym {
    this.checkBranchId(id, "sym@")
    const branch = this.branch(id)
    if (branch instanceof TreeSym) return branch
    else throw new Error(`Fetched branch ${id} as sym, but is not sym: ${branch}`)
  }

  childOwner (id :string) :S.ModuleSym|TreeSym { return this.owner }
  protected childScope (id :string) :S.Scope { return this.scope }

  debugShow () :string[] {
    const buf :string[] = []
    this._debugShow("", "", buf)
    return buf
  }
  protected _debugShow (indent :string, id :string, buf :string[]) {
    const pre = id ? `${id}=` : "";
    buf.push(`${indent}${pre}${this.kind} ` +
             // `parent=${this.parent ? this.parent.kind : "<none>"} ` +
             `owner=${this.owner} ` +
             `scope=${this.scope}`)
    for (let id of this.branchIds) {
      let branch = this.branch(id)
      if (branch instanceof Tree) {
        branch._debugShow(`${indent}  `, id, buf)
      } else {
        buf.push(`${indent}  ${id}=${branch}`)
      }
    }
  }

  toString () {
    return `${this.kind}#${this.id}`
  }

  protected checkBranchId (id :string, op :string) {
    if (!this.isValidBranch(id)) throw new Error(
      `Invalid branch (${id}) at ${this} for op '${op}'`)
  }
  protected isValidBranch (id :string) :boolean {
    return this.branchIds.includes(id)
  }
}

export abstract class DefTree extends Tree {

  abstract get sym () :TreeSym

  firstEditable () :Path { return new Path().firstEditable(this) }

  protected didLink (parent :Tree, parentId :string) {
    super.didLink(parent, parentId)
    this._scope = new DefTreeScope(this.sym, this.scope)
  }

  childOwner (id :string) :S.ModuleSym|TreeSym { return this.sym }
}

class DefTreeScope extends S.Scope {

  constructor (readonly sym :S.Symbol, readonly parent :S.Scope) { super() }

  lookup (kind :S.Kind, name :Name) :S.Symbol {
    const defsym = this.sym
    if (kind == defsym.kind && name == defsym.name) return defsym
    else return this.parent.lookup(kind, name)
  }

  // TODO: don't add shadowed names to the completion set? and/or provide some way of disambiguating
  // names (defining module for external syms; highlighting sym in current fun for local syms...)
  _addCompletions (pred :(sym :S.Symbol) => Boolean, prefix :string, syms :S.Symbol[]) {
    const defsym = this.sym
    if (pred(defsym) && this.isCompletion(prefix, defsym)) syms.push(defsym)
    return this.parent._addCompletions(pred, prefix, syms)
  }

  toString () :string { return `${this.sym},${this.parent}` }
}

const emptyModSym = new S.ModuleSym("<empty>")

export class EmptyTree extends Tree {
  get owner () :S.ModuleSym|TreeSym { return emptyModSym }
  get scope () :S.Scope { return S.emptyScope }
  get sig () :TP.Type { return TP.hole }
  link (parent :Tree, parentId :string) { /*NOOP!*/ }
}
export const emptyTree = new EmptyTree()

// ----------
// Type trees
// ----------

export class THoleTree extends Tree {
  get isHole () :boolean { return true }
  get sig () :TP.Type { return TP.hole }
}

export class TConstTree extends Tree {
  constructor (public cnst :C.Constant) { super() }
  get branchIds () :string[] { return ["cnst"] }
  get sig () :TP.Type { return new TP.Const(this.cnst) }
}

export class TRefTree extends Tree {
  constructor (readonly sym :S.Symbol) { super() }
  get branchIds () :string[] { return ["sym"] }
  get sig () :TP.Type { return this.sym.type }
}

export class ArrowTree extends Tree {
  from :Tree = emptyTree
  to :Tree = emptyTree
  get branchIds () :string[] { return ["from", "to"] }
  get sig () :TP.Type { return new TP.Arrow(this.from.sig, this.to.sig) }
}

export class TAppTree extends Tree {
  ctor :Tree = emptyTree
  arg :Tree = emptyTree
  get branchIds () :string[] { return ["ctor", "arg"] }
  get sig () :TP.Type { return new TP.App(this.ctor.sig, this.arg.sig) }
}

export class ProdTree extends Tree {
  fields :Tree[] = []
  get branchIds () :string[] { return this.fields.map((f, ii) => `${ii}`) }
  branch (id :string) :Branch { return this.fields[parseInt(id)] }
  protected storeBranch (id :string, branch :Branch) {
    this.fields[parseInt(id)] = branch as Tree }
  protected isValidBranch (id :string) :boolean {
    const idx = parseInt(id)
    return idx >= 0 && idx <= this.fields.length // allow one past last field
  }
  get sig () :TP.Type { return new TP.Prod(this.fields.map(f => (f as Tree).sig as TP.Def)) }
}

export class SumTree extends Tree {
  cases :Tree[] = []
  get branchIds () :string[] { return this.cases.map((c, ii) => `${ii}`) }
  branch (id :string) :Branch { return this.cases[parseInt(id)] }
  protected storeBranch (id :string, branch :Branch) {
    this.cases[parseInt(id)] = branch as Tree }
  protected isValidBranch (id :string) :boolean {
    const idx = parseInt(id)
    return idx >= 0 && idx <= this.cases.length // allow one past last field
  }
  get sig () :TP.Type { return new TP.Sum(this.cases.map(c => (c as Tree).sig as TP.Def)) }
}

export class TAbsTree extends DefTree {
  readonly sym :TypeVarTreeSym
  body :Tree = emptyTree
  constructor (name :Name) { super() ; this.sym = new TypeVarTreeSym(this, name) }
  get branchIds () :string[] { return ["sym", "body"] }
  get sig () :TP.Type { return new TP.Abs(this.sym, this.body.sig) }
  protected _sigQuants (acc :TypeVarTreeSym[]) { acc.unshift(this.sym) }
}

export class FieldTree extends DefTree {
  readonly sym :TreeSym
  type :Tree = emptyTree
  constructor (name :Name) { super() ; this.sym = new TreeSym(this, "term", "none", name) }
  get branchIds () :string[] { return ["sym", "type"] }
  get sig () :TP.Type { return this.type.sig }
}

export class CtorTree extends DefTree {
  readonly sym :TreeSym
  prod :Tree = emptyTree
  constructor (name :Name) { super() ; this.sym = new TreeSym(this, "term", "ctor", name) }
  get branchIds () :string[] { return ["sym", "prod"] }
  get sig () :TP.Type {
    let defTree = this.parent
    while (defTree && !(defTree instanceof TypeDefTree)) {
      defTree = defTree.parent
    }
    if (!defTree) throw new Error(
      `Constructor must be enclosed by 'type' tree (in: ${this})`)
    function mkArrow (types :TP.Type[], pos :number, acc :TP.Type) :TP.Type {
      const arrow = new TP.Arrow(types[pos], acc)
      return (pos == 0) ? arrow : mkArrow(types, pos-1, arrow)
    }
    function mkAbs (quants :TypeVarTreeSym[], pos :number, acc :TP.Type) :TP.Type {
      const abs = new TP.Abs(quants[pos], acc)
      return (pos == 0) ? abs : mkAbs(quants, pos-1, abs)
    }
    function mkApp (quants :TypeVarTreeSym[], pos :number, acc :TP.Type) :TP.Type {
      const app = new TP.App(acc, new TP.Var(quants[pos]))
      return (pos == quants.length-1) ? app : mkApp(quants, pos+1, acc)
    }
    const prodFields = (this.prod.sig as TP.Prod).fields
    const quants = this.sigQuants()
    let ctorType = defTree.sig
    if (quants.length > 0) ctorType = mkApp(quants, 0, ctorType)
    if (prodFields.length > 0) ctorType = mkArrow(prodFields, prodFields.length-1, ctorType)
    if (quants.length > 0) ctorType = mkAbs(quants, quants.length-1, ctorType)
    return ctorType
  }
  link (parent :Tree, parentId :string) {
    super.link(parent, parentId)
    this.owner.module.scope.insert(this.sym)
  }
  willUnlink () {
    this.owner.module.scope.remove(this.sym)
    super.willUnlink()
  }
}

// -------------
// Pattern trees
// -------------

export abstract class PatTree extends Tree {
  addSymbols (syms :S.Symbol[]) {}
}

export class PHoleTree extends PatTree {
  get branchIds () :string[] { return [] }
  get sig () :TP.Type { return TP.hole }
  get isHole () :boolean { return true }
}

export class PLitTree extends PatTree {
  constructor (public cnst :C.Constant) { super() }
  get branchIds () :string[] { return ["cnst"] }
  get sig () :TP.Type { return new TP.Const(this.cnst) }
}

export class PBindTree extends PatTree {
  readonly sym :TreeSym
  constructor (name :Name) { super() ; this.sym = new TreeSym(this, "term", "none", name) }
  get branchIds () :string[] { return ["sym"] }
  get sig () :TP.Type { return this.prototype }
  addSymbols (syms :S.Symbol[]) { syms.push(this.sym) }
}

export class PDtorTree extends PatTree {
  constructor (readonly ctor :S.Symbol) { super() }
  get branchIds () :string[] { return ["ctor"] }
  get sig () :TP.Type { return TP.patUnify(this.ctor.type, this.prototype) }
  lookup (name :Name) :S.Symbol|void { return undefined }
}

export class PAppTree extends PatTree {
  fun :PatTree = emptyPatTree
  arg :PatTree = emptyPatTree
  get branchIds () :string[] { return ["fun", "arg"] }
  get sig () :TP.Type { return TP.patUnapply(this.fun.sig) }
  protected childPrototype (id :string) :TP.Type|void {
    if (id === "fun") return this.prototype
    if (id === "arg") return TP.patLastArg(this.fun.sig)
  }
  addSymbols (syms :S.Symbol[]) {
    if (this.fun instanceof PatTree) this.fun.addSymbols(syms)
    if (this.arg instanceof PatTree) this.arg.addSymbols(syms)
  }
}

const emptyPatTree = new PHoleTree()

// -----------------
// Abstraction trees
// -----------------

export class LetTree extends DefTree {
  readonly sym :TreeSym
  type :Tree = emptyTree
  body :Tree = emptyTree
  expr :Tree = emptyTree
  constructor (name :Name) { super() ; this.sym = new TreeSym(this, "term", "none", name) }
  get branchIds () :string[] { return ["sym", "type", "body", "expr"] }
  get sig () :TP.Type { return TP.hole } // TODO
  setHoles () :this {
    return this.setBranch("body", new HoleTree())
               .setBranch("expr", new HoleTree())
  }
  protected childPrototype (id :string) :TP.Type|void {
    if (id === "expr") return this.prototype
  }
}

export class LetFunTree extends DefTree {
  readonly sym :TreeSym
  body :Tree = emptyTree
  expr :Tree = emptyTree
  constructor (name :Name) { super() ; this.sym = new TreeSym(this, "term", "func", name) }
  get branchIds () :string[] { return ["sym", "body", "expr"] }
  get sig () :TP.Type { return this.body.sig } // TODO
}

export class AllTree extends DefTree {
  readonly sym :TypeVarTreeSym
  body :Tree = emptyTree
  constructor (name :Name) { super() ; this.sym = new TypeVarTreeSym(this, name) }
  get branchIds () :string[] { return ["sym", "body"] }
  get sig () :TP.Type { return new TP.Abs(this.sym, this.body.sig) }
}

export class AbsTree extends DefTree {
  readonly sym :VarTreeSym
  type :Tree = emptyTree
  body :Tree = emptyTree
  constructor (name :Name) { super() ; this.sym = new VarTreeSym(this, name, t => this.type.sig) }
  get branchIds () :string[] { return ["sym", "type", "body"] }
  get sig () :TP.Type { return new TP.Arrow(this.type.sig, this.body.sig) }
}

// ----------------
// Expression trees
// ----------------

export class LitTree extends Tree {
  constructor (readonly cnst :C.Constant) { super() }
  get branchIds () :string[] { return ["cnst"] }
  get sig () :TP.Type { return new TP.Const(this.cnst) }
}

export class RefTree extends Tree {
  constructor (readonly sym :S.Symbol) { super() }
  get branchIds () :string[] { return ["sym"] }
  get sig () :TP.Type { return this.sym.type }
}

export class AscTree extends Tree {
  expr :Tree = emptyTree
  type :Tree = emptyTree
  get branchIds () :string[] { return ["expr", "type"] }
  get sig () :TP.Type { return this.type.sig } // TODO:?
}

export class HoleTree extends Tree {
  get isHole () :boolean { return true }
  get sig () :TP.Type { return this.prototype }
}

class BaseAppTree extends Tree {
  fun :Tree = emptyTree
  arg :Tree = emptyTree
  get sig () :TP.Type {
    return TP.funApply(this.fun.sig, this.arg.sig)
  }
  protected childPrototype (id :string) :TP.Type|void {
    if (id === "arg") {
      const funType = this.fun.sig
      if (funType instanceof TP.Arrow) return funType.arg
    } else if (id === "fun") {
      // TODO: we need a more disciplined approach to tracking whether we're synthesizing or
      // checking (to avoid infinite loops in type computation)
      const argType = this.arg.isHole ? TP.hole : this.arg.sig
      return new TP.Arrow(argType, this.prototype)
    }
  }
}

export class AppTree extends BaseAppTree {
  get branchIds () :string[] { return ["fun", "arg"] }
}

export class InAppTree extends BaseAppTree {
  get branchIds () :string[] { return ["arg", "fun"] }
}

export class IfTree extends Tree {
  test :Tree = emptyTree
  texp :Tree = emptyTree
  fexp :Tree = emptyTree
  get branchIds () :string[] { return ["test", "texp", "fexp"] }
  get sig () :TP.Type { return TP.hole } // TODO
  setHoles () :this {
    return this.setBranch("test", new HoleTree())
               .setBranch("texp", new HoleTree())
               .setBranch("fexp", new HoleTree())
  }
  protected childPrototype (id :string) :TP.Type|void {
    switch (id) {
    case "test": return undefined // TODO: bool
    case "texp":
    case "fexp": return this.prototype
    }
  }
}

export class MatchTree extends Tree {
  scrut :Tree = emptyTree
  cases :Tree[] = []
  get branchIds () :string[] { return ["scrut"].concat(this.cases.map((c, ii) => `${ii}`)) }
  branch (id :string) :Branch {
    if (id === "scrut") return this.scrut
    else return this.cases[parseInt(id)]
  }

  /** Inserts a new case tree (with holes) at `idx`. Trailing cases are shifted down. */
  insertCase (idx :number) {
    const cases = this.cases
    // shift all the cases from idx and above 'up' one element
    for (let ii = cases.length; ii > idx; ii -= 1) {
      cases[ii] = cases[ii-1]
      cases[ii].link(this, `${ii}`) // tell case about new id
    }
    this.setBranch(`${idx}`, new CaseTree().setHoles())
  }
  /** Deletes case tree at `idx`. Trailig cases are shifted back up. */
  deleteCase (idx :number) {
    const cases = this.cases
    cases[idx].unlink()
    // shift all the cases from idx and above 'down' one element
    for (let ii = idx; ii < cases.length-1; ii += 1) {
      cases[ii] = cases[ii+1]
      cases[ii].link(this, `${ii}`) // tell case about new id
    }
    // trim the last empty case slot out of the array
    cases.splice(cases.length-1, 1)
  }

  setHoles () :this {
    return this.setBranch("scrut", new HoleTree())
               .setBranch("0", new CaseTree().setHoles())
  }

  protected storeBranch (id :string, branch :Branch) {
    if (id === "scrut") this.scrut = branch as Tree
    else this.cases[parseInt(id)] = branch as Tree
  }
  protected isValidBranch (id :string) :boolean {
    if (id === "scrut") return true
    const idx = parseInt(id)
    return idx >= 0 && idx <= this.cases.length // allow one past last field
  }
  get sig () :TP.Type { return this.cases.map(c => c.sig).reduce((mt, ct) => mt.join(ct)) }
  protected childPrototype (id :string) :TP.Type|void {
    // prototype for cases is type of scrutinee expression
    if (id !== "scrut") return this.scrut.sig
  }
}

export class CaseTree extends Tree {
  pat :PatTree = emptyPatTree
  body :Tree = emptyTree
  get branchIds () :string[] { return ["pat", "body"] }
  get sig () :TP.Type { return this.body.sig }
  setHoles () :this {
    return this.setBranch("pat", new PHoleTree())
               .setBranch("body", new HoleTree())
  }
  protected childScope (id :string) :S.Scope {
    return (id == "body") ? new CaseBodyScope(this) : this.scope
  }
  protected childPrototype (id :string) :TP.Type|void {
    // the prototype for a case is the pattern prototype, so we propagate that to the pattern tree
    if (id === "pat") return this.prototype
    // the prototype for the body is the prototype for the whole match expression
    else if (id === "body") return this.requireParent.prototype
  }
}

class CaseBodyScope extends S.Scope {

  constructor (readonly tree :CaseTree) { super() }

  get patSyms () :S.Symbol[] {
    const patTree = this.tree.pat as PatTree, patSyms :S.Symbol[] = []
    patTree.addSymbols(patSyms)
    return patSyms
  }

  lookup (kind :S.Kind, name :Name) :S.Symbol {
    const sym = this.patSyms.find(sym => sym.name === name)
    if (sym && sym.kind === kind) return sym
    else return this.tree.scope.lookup(kind, name)
  }

  _addCompletions (pred :(sym :S.Symbol) => Boolean, prefix :string, syms :S.Symbol[]) :void {
    for (let sym of this.patSyms) {
      if (pred(sym) && this.isCompletion(prefix, sym)) syms.push(sym)
    }
    this.tree.scope._addCompletions(pred, prefix, syms)
  }
}

// -------------------
// Top-level def trees
// -------------------

abstract class TopDefTree extends DefTree {
  initMod (mod :S.ModuleSym) {
    this._owner = mod
    this._scope = mod.scope
  }
}

export class FunDefTree extends TopDefTree {
  readonly sym :TreeSym
  body :Tree = emptyTree
  constructor (name :Name) { super() ; this.sym = new TreeSym(this, "term", "func", name) }
  get branchIds () :string[] { return ["sym", "body"] }
  get sig () :TP.Type { return this.body.sig }
}

export class TypeDefTree extends TopDefTree {
  readonly sym :TypeTreeSym
  body :Tree = emptyTree
  constructor (name :Name) { super() ; this.sym = new TypeTreeSym(this, name) }
  get branchIds () :string[] { return ["sym", "body"] }
  get sig () :TP.Type { return new TP.Def(this.sym) }
}

export class DefHoleTree extends TopDefTree {
  readonly sym = new TreeSym(this, "term", "none", "")
  constructor () { super() }
  get branchIds () :string[] { return ["sym"] }
  get sig () :TP.Type { return TP.hole }
}

// ---------------
// Tree path model
// ---------------

// A path identifies a point in an AST. The editing cursor uses paths to track the insertion point.
// The selection is a pair of paths that track the start and end of the selection.

/**
 * Identifies a path from some root node to a field of some target node. Each path component is the
 * index of the field in the current node. If a path has additional components that field must
 * represent another AST node, but the final component of a path may reference a terminal field like
 * a `Name` or `Constant`.
 *
 * Note that a path will generally start in a `Def` node and may traverse into `Expr` nodes en route
 * to its terminal node (which may itself be a `Def` node, an `Expr` node, a `Name`, etc.). Thus we
 * cannot, in general, obtain the "thing" to which a path points as there is no single type that
 * describes it.
 */
export class Path {
  private ids :string[]
  constructor (...ids :string[]) { this.ids = ids }

  /** Returns whether this path is empty. */
  get isEmpty () :boolean { return this.ids.length === 0 }
  /** Returns the length of this path. */
  get length () :number { return this.ids.length }
  /** Returns the id of the final component of this path. */
  get selectedId () :string {
    return this.id(this.length-1)
  }

  /** Returns the branch selected by this path. */
  selected (root :DefTree) :Branch {
    return this.ids.length == 0 ? root : this.selectionParent(root).branch(this.selectedId)
  }
  /** Returns the branch selected by this path as a `Tree`.
    * @throws Error if this path does not select a `Tree`. */
  selectedTree (root :DefTree) :Tree {
    const branch = this.selected(root)
    if (branch instanceof Tree) return branch
    else throw new Error(`Path does not select tree ${this}`)
  }
  /** Returns the `Tree` that contains the branch selected by this path.
    * @throws Error if this path is empty (i.e. selects the root tree). */
  selectionParent (root :DefTree) :Tree {
    if (this.ids.length == 0) throw new Error(`Path selects root, has no parent.`)
    return this.treeAt(root, this.length-2)
  }
  /** Returns the owner of the selected branch. */
  selectionOwner (root :DefTree) :S.ModuleSym|TreeSym {
    return this.selectionParent(root).childOwner(this.selectedId)
  }
  /** Returns true if the branch selected by this path is a "hole" tree. */
  selectsHole (root :DefTree) :boolean {
    const sel = this.selected(root)
    return sel instanceof Tree && sel.isHole
  }

  /** Returns the child id at offset `idx` (must be `0 <= idx < length`). */
  id (idx :number) :string { return this.ids[idx] }
  /** Returns the tree selected by the `idx`th component of this path. */
  treeAt (root :DefTree, idx :number) :Tree {
    let tree :Tree = root
    for (let ii = 0; ii <= idx; ii++) tree = tree.treeAt(this.id(ii))
    return tree
  }
  /** Returns the kind of the tree selected by the `idx`th component of this path. */
  kindAt (root :DefTree, idx :number) :string { return this.treeAt(root, idx).kind }

  /** Returns whether this path ends with the supplied path matching expression. Paths are matched
    * with sequences of alternating tree `kind` and branch `id` tokens. The last token is always a
    * branch `id`. For example: `[abs, sym]` matches a path that ends by selecting the `sym` branch
    * of an `abs` tree node, `[typedef, body, tabs, body]` matches a path that ends by selecting the
    * `body` branch of a `tabs` node which is directly nested under a `typedef` node in the `body`
    * branch. The special token `*` can be used to match any tree `kind` or branch `id`.
    *
    * TODO: support some way to express matches that skip over intermediate tree nodes.
    */
  endsWith (root :DefTree, ...comps :string[]) :boolean {
    let cii = comps.length-1, pii = this.ids.length-1, checkId = true
    while (cii >= 0) {
      if (checkId) {
        if (this.ids[pii] !== comps[cii]) return false
        pii -= 1
      } else {
        if (this.kindAt(root, pii) !== comps[cii]) return false
      }
      checkId = !checkId
      cii -= 1
    }
    return true
  }

  /** Returns a new path which extends this path with `id`. */
  x (id :string) :Path { return new Path(...this.ids.concat(id)) }
  /** Returns a new path which extends this path with `ids`. */
  concat (ids :string[]) :Path { return new Path(...this.ids.concat(ids)) }
  /** Mutates this path in place, extending it with `id`. */
  push (id :string) { this.ids.push(id) }

  /** Returns a new path which omits the final selector from `this` path (popping up one level of
    * the tree). */
  pop () :Path { return new Path(...this.ids.slice(0, -1)) }
  /** If the path contains a node of `kind` then the path from the root to that node is returned,
    * otherwise `undefined` is returned. */
  popTo (root :DefTree, kind :string) :Path|void {
    let length = this.length-1
    let tree = this.selectionParent(root)
    while (tree.kind !== kind) {
      const parent = tree.parent
      if (!parent) return undefined
      tree = parent
      length -= 1
    }
    return new Path(...this.ids.slice(0, length))
  }

  /** Returns a new path which moves to the sibling of `this` path identified by `id`. This could be
    * used, for example, to move from the `sym` branch of an `abs` node to the `type` branch. */
  sib (id :string) :Path {
    const ids = this.ids.slice(0)
    ids[ids.length-1] = id
    return new Path(...ids)
  }

  /** Returns the extension of this path that selects the first editable. */
  firstEditable (root :DefTree) :Path {
    let selected = this.selected(root)
    if (selected instanceof Tree) {
      let tree :Tree = selected
      let path = this
      let firstId = tree.firstEditableId
      while (firstId) {
        let first = tree.branch(firstId)
        if (first instanceof Tree) {
          path.push(firstId)
          tree = first
          firstId = tree.firstEditableId
        } else if (first instanceof S.Symbol) {
          path.push(firstId)
          return path
        }
        else return path
      }
      return path
    }
    else throw new Error(`Can't obtain firstEditable, path does not select tree: ${this}`)
  }

  /** Applies `editfn` to the subtree selected by this path.
    * @return the edited root tree. */
  edit (editfn :EditFn) :TreeEdit {
    return editfn(new TreeEditor(this))
  }

  /** Returns whether `this` path is equal to `that` path. */
  equals (that :Path) :boolean {
    // console.log(`Path.equals ${this} ?== ${that}`)
    if (this.length !== that.length) return false
    for (let ii = 0; ii < this.length; ii++) {
      if (this.id(ii) !== that.id(ii)) return false
    }
    return true
  }

  mkString (root :DefTree) :string {
    return `${root}` + this.ids.map(
      (id, idx) => idx == this.ids.length-1 ? `.${id}` : `.${id}/${this.kindAt(root, idx)}`
    ).join("")
  }

  toString () :string { return this.ids.join("/") }
}

// ------------
// Tree editing
// ------------

/** Captures a change to a tree as well as its inverse. When an edit is applied to a tree, it
  * returns the new tree as well as an edit that can be used to undo the just applied edit. The undo
  * "edit" usually returns the original edit as its own "undo" operation.
  *
  * Note that the current architecture mutates trees in place. The real compiler will not be so
  * cavalier. An edit can replace a tree wholesale if needed; it must retain the original tree for
  * its undo operation in that case. */
export type TreeEdit = (root :DefTree) => {root :DefTree, undo :TreeEdit}

export type EditFn = (te :TreeEditor) => TreeEdit

/** Merges multiple edits into a single edit which applies (and undos) them all. */
export function merge (...edits :TreeEdit[]) :TreeEdit {
  const edit :TreeEdit = root => {
    const undos :TreeEdit[] = []
    for (let edit of edits) {
      const res = edit(root)
      root = res.root
      undos.unshift(res.undo)
    }
    const undo :TreeEdit = root => {
      for (let undo of undos) root = undo(root).root
      return {root, undo: edit}
    }
    return {root, undo}
  }
  return edit
}

export class TreeEditor {
  constructor (readonly path :Path) {}

  setName (name :Name) :TreeEdit {
    const path = this.path
    const edit :TreeEdit = root => {
      const tree = path.selectionParent(root), id = path.selectedId
      const sym = tree.symAt(id), oldName = sym.name
      sym.name = name
      const undo :TreeEdit = root => {
        sym.name = oldName
        return {root, undo: edit}
      }
      return {root, undo}
    }
    return edit
  }

  setBranch (child :Tree) :TreeEdit {
    const path = this.path
    const edit :TreeEdit = root => {
      const tree = path.selectionParent(root), id = path.selectedId
      const oldChild = tree.branch(id)
      tree.setBranch(id, child)
      const undo :TreeEdit = root => {
        tree.setBranch(id, oldChild)
        return {root, undo: edit}
      }
      return {root, undo}
    }
    return edit
  }

  spliceBranch (child :Tree, childId :string) :TreeEdit {
    const path = this.path
    const edit :TreeEdit = root => {
      const tree = path.selectionParent(root), id = path.selectedId
      const oldChild = tree.branch(id)
      tree.setBranch(id, child)
      child.setBranch(childId, oldChild)
      const undo :TreeEdit = root => {
        if (oldChild instanceof Tree) oldChild.unlink()
        tree.setBranch(id, oldChild)
        return {root, undo: edit}
      }
      return {root, undo}
    }
    return edit
  }

  // Type and Data(type) trees
  setTHole () :TreeEdit { return this.setBranch(new THoleTree()) }
  setTConst (cnst :C.Constant) :TreeEdit { return this.setBranch(new TConstTree(cnst)) }
  setTRef (sym :S.Symbol) :TreeEdit { return this.setBranch(new TRefTree(sym)) }
  setArrow () :TreeEdit { return this.setBranch(new ArrowTree()) }
  setTAbs (name :Name) :TreeEdit { return this.setBranch(new TAbsTree(name)) }
  setTApp () :TreeEdit { return this.setBranch(new TAppTree()) }
  setField (name :Name) :TreeEdit { return this.setBranch(new FieldTree(name)) }
  setProd () :TreeEdit { return this.setBranch(new ProdTree()) }
  setSum () :TreeEdit { return this.setBranch(new SumTree()) }
  setCtor (name :Name) :TreeEdit { return this.setBranch(new CtorTree(name)) }
  spliceTAbs (name :Name) :TreeEdit { return this.spliceBranch(new TAbsTree(name), "body") }
  spliceTApp () :TreeEdit { return this.spliceBranch(
    new TAppTree().setBranch("arg", new THoleTree()), "ctor") }

  // TODO
  // Array Type
  // Interface Name Params Methods
  // Method Name Type
  // setType (name :Name) :Tree {
  //   const sym = new TreeSym("type", name, this.owner)
  //   return this.setDefTreeBranch("type", sym, [sym, emptyTree], t => t.treeAt(1).type)
  // }

  // Pattern trees
  setPHole () :TreeEdit { return this.setBranch(new PHoleTree()) }
  setPLit (cnst :C.Constant) :TreeEdit { return this.setBranch(new PLitTree(cnst)) }
  setPBind (name :Name) :TreeEdit { return this.setBranch(new PBindTree(name)) }
  setPDtor (ctor :S.Symbol) :TreeEdit { return this.setBranch(new PDtorTree(ctor)) }
  setPApp () { return this.setBranch(new PAppTree()) }
  splicePApp () :TreeEdit { return this.spliceBranch(
    new PAppTree().setBranch("arg", new PHoleTree()), "fun") }

  // Abstraction terms
  setLet (name :Name) :TreeEdit { return this.setBranch(new LetTree(name).setHoles()) }
  setLetFun (name :Name) :TreeEdit { return this.setBranch(new LetFunTree(name)) }
  setAll (name :Name) :TreeEdit { return this.setBranch(new AllTree(name)) }
  setAbs (name :Name) :TreeEdit { return this.setBranch(new AbsTree(name)) }
  spliceAll (name :Name) :TreeEdit { return this.spliceBranch(new AllTree(name), "body") }
  spliceAbs (name :Name) :TreeEdit { return this.spliceBranch(new AbsTree(name), "body") }

  // Expression terms
  setHole () :TreeEdit { return this.setBranch(new HoleTree()) }
  setLit (cnst :C.Constant) :TreeEdit { return this.setBranch(new LitTree(cnst)) }
  setRef (sym :S.Symbol) :TreeEdit { return this.setBranch(new RefTree(sym)) }
  setAsc () :TreeEdit { return this.setBranch(new AscTree()) }
  setApp () :TreeEdit { return this.setBranch(new AppTree()) }
  setInApp () :TreeEdit { return this.setBranch(new InAppTree()) }
  setIf () :TreeEdit { return this.setBranch(new IfTree().setHoles()) }
  setMatch () :TreeEdit { return this.setBranch(new MatchTree().setHoles()) }
  setCase (): TreeEdit { return this.setBranch(new CaseTree()) }
  spliceApp () :TreeEdit { return this.spliceBranch(
    new AppTree().setBranch("arg", new HoleTree()), "fun") }
  spliceInApp () :TreeEdit { return this.spliceBranch(
    new InAppTree().setBranch("fun", new HoleTree()), "arg") }

  // TODO
  // Cond Array CondCase
  // CondCase Expr Expr

  insertCase (caseIdx :number) :TreeEdit {
    const path = this.path
    const edit :TreeEdit = root => {
      const matchTree = path.selected(root) as MatchTree
      matchTree.insertCase(caseIdx)
      const undo :TreeEdit = root => {
        const matchTree = path.selected(root) as MatchTree
        matchTree.deleteCase(caseIdx)
        return {root, undo: edit}
      }
      return {root, undo}
    }
    return edit
  }
}

export function mkDefHole (mod :S.ModuleSym) :DefTree {
  const tree = new DefHoleTree()
  tree.initMod(mod)
  return tree
}

export function mkFunDef (mod :S.ModuleSym, name :Name) :DefTree {
  const tree = new FunDefTree(name)
  tree.initMod(mod)
  mod.scope.insert(tree.sym) // insert this term symbol into the module scope
  return tree
}

export function mkTypeDef (mod :S.ModuleSym, name :Name) :DefTree {
  const tree = new TypeDefTree(name)
  tree.initMod(mod)
  mod.scope.insert(tree.sym) // insert this type symbol into the module scope
  return tree
}

// ------------
// Tree symbols
// ------------

/** Symbols assigned to definition trees. Obtains ownership and type info from tree. */
class TreeSym extends S.Symbol {
  constructor (readonly tree :Tree, kind :S.Kind, flavor :S.Flavor, name :Name) {
    super(kind, flavor, name) }
  get owner () :TreeSym|S.ModuleSym { return this.tree.owner }
  get type () :TP.Type { return this.tree.sig }
}

class TypeTreeSym extends TreeSym {
  constructor (tree :Tree, name :Name) { super(tree, "type", "none", name) }
  get bodyType () :TP.Type { return (this.tree as TypeDefTree).body.sig }
}

/** Symbols assigned to pattern tree bindings. */
// class PatTreeSym extends TreeSym {
//   constructor (name :Name, owner :TreeSym|S.ModuleSym,
//                readonly typefn :(t :Tree) => TP.Type) { super("term", name, owner) }
//   get type () :TP.Type { return this.typefn(this.tree) }
// }

/** Symbols assigned to var trees. */
class VarTreeSym extends TreeSym {
  constructor (tree :Tree, name :Name, readonly typefn :(t :Tree) => TP.Type) {
    super(tree, "term", "none", name) }
  get type () :TP.Type { return this.typefn(this.tree) }
}

/** Symbols assigned to type var trees. */
class TypeVarTreeSym extends TreeSym {
  constructor (tree :Tree, name :Name) { super(tree, "type", "none", name) }
  get type () :TP.Var { return new TP.Var(this) }
}

// export class ScalarDef extends TypeTree {
//   readonly type = new TP.Scalar(this.tag, this.size)
//   constructor (readonly tag :Tag, readonly size :number) { super () }
//   toString () { return `Scalar:${this.tag}${this.size}`}
// }

// ------------------
// Tree serialization
// ------------------

function setsym (syms :Map<number, TreeSym>, tree :DefTree, id :number) :DefTree {
  syms.set(id, tree.sym)
  return tree
}
function reqsym (syms :Map<number, TreeSym>, id :number) :TreeSym {
  const sym = syms.get(id)
  if (sym) return sym
  throw new Error(`Missing symbol '${id}'`)
}

export function inflateDef (owner :S.ModuleSym, json :any) :DefTree {
  const syms :Map<number, TreeSym> = new Map()
  const kind :string = json.kind

  switch (kind) {
  case "typedef":
    return setsym(syms, mkTypeDef(owner, json.sym.name), json.sym.id).
      setBranch("body", inflateTree(syms, json.body))
  }

  throw new Error(`Unknown deftree kind: '${json.kind}'`)
}

export function inflateTree (syms :Map<number, TreeSym>, json :any) :Tree {
  const kind :string = json.kind

  switch (kind) {
  case "tabs":
    return setsym(syms, new TAbsTree(json.sym.name), json.sym.id).
      setBranch("body", inflateTree(syms, json.body))
  case "tref":
    return new TRefTree(reqsym(syms, json.symId))
  case "ctor":
    return setsym(syms, new CtorTree(json.sym.name), json.sym.id).
      setBranch("prod", inflateTree(syms, json.prod))
  case "prod":
    return json.branches.reduce(
      (tree :Tree, bj :any, ii :number) => tree.setBranch(`${ii}`, inflateTree(syms, bj)),
      new ProdTree())
  case "field":
    return setsym(syms, new FieldTree(json.sym.name), json.sym.id).
      setBranch("type", inflateTree(syms, json.type))
  }

  throw new Error(`Unknown tree kind: '${json.kind}'`)
}

// ----------
// Test trees
// ----------

export const testModSym = new S.ModuleSym("test")

class PrimTypeTreeSym extends S.Symbol {
  constructor (name :Name,
               readonly owner :TreeSym|S.ModuleSym,
               readonly bodyType :TP.Type) { super("type", "none", name) }
  get type () :TP.Type { return new TP.Def(this) }
}

const natSym = new PrimTypeTreeSym("Nat", testModSym, new TP.Scalar(C.Tag.Int, 32))
const intSym = new PrimTypeTreeSym("Int", testModSym, new TP.Scalar(C.Tag.Int, 32))
const stringSym = new PrimTypeTreeSym("String", testModSym, new TP.Scalar(C.Tag.String, 1))
testModSym.scope.insert(natSym)
testModSym.scope.insert(intSym)
testModSym.scope.insert(stringSym)

class PrimFunTreeSym extends S.Symbol {
  constructor (name :Name,
               readonly owner :TreeSym|S.ModuleSym,
               readonly _type :TP.Type) { super("term", "func", name) }
  get type () :TP.Type { return this._type }
}

const addSym = new PrimFunTreeSym(
  "+", testModSym, new TP.Arrow(natSym.type, new TP.Arrow(natSym.type, natSym.type)))
const subSym = new PrimFunTreeSym(
  "-", testModSym, new TP.Arrow(natSym.type, new TP.Arrow(natSym.type, natSym.type)))
testModSym.scope.insert(addSym)
testModSym.scope.insert(subSym)

// type Box A contents:A
// type Box [A] (contents :A)
export const boxExample = inflateDef(testModSym, {
  kind: "typedef",
  sym: {id: 1, name: "Box"},
  body: {
    kind: "tabs",
    sym: {id: 2, name: "A"},
    body: {
      kind: "ctor",
      sym: {id: 3, name: "Box"},
      prod: {
        kind: "prod",
        branches: [{
          kind: "field",
          sym: {id: 4, name: "contents"},
          type: {kind: "tref", symId: 2}
        }]
      }
    }
  }
})

// type Person name:String age:Nat
// type Person (name :String, age :Nat)
// export const recordExample = mkTypeDef(testModSym, "Person").editBranch(
//   "body", type => type.setCtor("Person").editBranch(
//     "prod", prod => prod.setProd().editBranches({
//       "0": name => name.setField("name").editBranch("type", type => type.setTRef(stringSym)),
//       "1": age => age.setField("age").editBranch("type", type => type.setTRef(natSym))
//     })
//   )
// )

// type List A =
//   * Nil
//   * Cons head:A tail:List A

// type List [A] =
//   * Nil
//   * Cons (head :A, tail :List[A])
// export const listExample = mkTypeDef(testModSym, "List").editBranch(
//   "body", body => body.setTAbs("T").editBranch(
//     "body", body => body.setSum().editBranches({
//       "0": nil => nil.setCtor("Nil").editBranch("prod", prod => prod.setProd()),
//       "1": list => list.setCtor("Cons").editBranch(
//         "prod", prod => prod.setProd().editBranches({
//           "0": head => head.setField("head").editBranch(
//             "type", type => type.setTRef(type.tree.scope.lookupType("T"))
//           ),
//           "1": tail => tail.setField("tail").editBranch(
//             "type", type => type.setTApp().editBranches({
//               "ctor": ctor => ctor.setTRef(ctor.tree.scope.lookupType("List")),
//               "arg": arg => arg.setTRef(arg.tree.scope.lookupType("T"))
//             })
//           )
//         })
//       )
//     })
//   )
// )

// function mkListA (tb :TreeEditor) :Tree {
//   return tb.setTApp().editBranches({
//     "ctor": ctor => ctor.setTRef(ctor.tree.scope.lookupType("List")),
//     "arg": arg  => arg.setTRef(tb.tree.scope.lookupType("A"))
//   })
// }

// fun :: [A] (head :A, tail :List[A]) :List[A] = Cons(head, tail)
// export const consFunExample = mkFunDef(testModSym, "::").editBranch(
//   "body", body => body.setAll("A").editBranch(
//     "body", body => body.setAbs("head").editBranches({
//       "type": type => type.setTRef(type.tree.scope.lookupType("A")),
//       "body": body => body.setAbs("tail").editBranches({
//         "type": mkListA,
//         "body": body => body.setAsc().editBranches({
//           "type": mkListA,
//           "expr": expr => expr.setApp().editBranches({
//             "fun": fun => fun.setApp().editBranches({
//               "fun": fun => fun.setRef(fun.tree.scope.lookupTerm("Cons")),
//               "arg": arg => arg.setRef(arg.tree.scope.lookupTerm("head"))
//             }),
//             "arg": arg => arg.setRef(arg.tree.scope.lookupTerm("tail"))
//           })
//         })
//       })
//     })
//   )
// )

// type IntList = List Int
// type IntList = List[Int]
// export const aliasExample = mkTypeDef(testModSym, "IntList").editBranch(
//   "body", body => body.setTApp().editBranches({
//     "ctor": ctor => ctor.setTRef(ctor.tree.scope.lookupType("List")),
//     "arg": arg  => arg.setTRef(intSym)
//   })
// )

// term fib n:Nat → Nat = case n of
//   0 → 0
//   1 → 1
//   n → fib (n-1) + fib (n-2)

// fun fib (n:Nat) :Nat = case n
//   0 = 0
//   1 = 1
//   n = fib(n-1) + fib(n-2)
// export const fibExample = mkFunDef(testModSym, "fib").editBranch(
//   "body", body => body.setAbs("n").editBranches({
//     "type": type => type.setTRef(natSym),
//     "body": expr => expr.setAsc().editBranches({
//       "type": type => type.setTRef(natSym),
//       "expr": expr => expr.setMatch().editBranches({
//         "scrut": scrut => scrut.setRef(scrut.tree.scope.lookupTerm("n")),
//         "0": case0 => case0.setCase().editBranches({
//           "pat": pat => pat.setPLit(C.constInt(0)),
//           "body": body => body.setLit(C.constInt(0))
//         }),
//         "1": case1 => case1.setCase().editBranches({
//           "pat": pat => pat.setPLit(C.constInt(1)),
//           "body": body => body.setLit(C.constInt(1))
//         }),
//         "2": caseN => caseN.setCase().editBranches({
//           "pat": pat => pat.setPBind("n"),
//           "body": body => body.setApp().editBranches({
//             "fun": fun => fun.setInApp().editBranches({
//               "arg": arg => arg.setApp().editBranches({
//                 "fun": fun => fun.setRef(fun.tree.scope.lookupTerm("fib")),
//                 "arg": arg => arg.setApp().editBranches({
//                   "fun": fun => fun.setInApp().editBranches({
//                     "arg": arg => arg.setRef(arg.tree.scope.lookupTerm("n")),
//                     "fun": fun => fun.setRef(subSym)
//                   }),
//                   "arg": arg => arg.setLit(C.constInt(1))
//                 })
//               }),
//               "fun": fun => fun.setRef(addSym)
//             }),
//             "arg": arg => arg.setApp().editBranches({
//               "fun": fun => fun.setRef(fun.tree.scope.lookupTerm("fib")),
//               "arg": arg => arg.setApp().editBranches({
//                 "fun": fun => fun.setInApp().editBranches({
//                   "arg": arg => arg.setRef(arg.tree.scope.lookupTerm("n")),
//                   "fun": fun => fun.setRef(subSym)
//                 }),
//                 "arg": arg => arg.setLit(C.constInt(2))
//               })
//             })
//           })
//         })
//       })
//     })
//   })
// )

// fun reverse [A] (as :List[A]) :List[A] =
//   fun revacc (as :List[A], acc :List[A]) :List[A] = case as
//     Nil        = acc
//     Cons(h, t) = revacc(t, h :: acc)
//   revacc(as, Nil)
// export const revExample = mkFunDef(testModSym, "reverse").editBranch(
//   "body", body => body.setAll("A").editBranch(
//     "body", body => body.setAbs("as").editBranches({
//       "type": mkListA,
//       "body": body => body.setAsc().editBranches({
//         "type": mkListA,
//         "expr": expr => expr.setLetFun("revacc").editBranches({
//           "body": body => body.setAbs("as").editBranches({
//             "type": mkListA,
//             "body": body => body.setAbs("acc").editBranches({
//               "type": mkListA,
//               "body": body => body.setAsc().editBranches({
//                 "type": mkListA,
//                 "expr": expr => expr.setMatch().editBranches({
//                   "scrut": scrut => scrut.setRef(scrut.tree.scope.lookupTerm("as")),
//                   "0": case0 => case0.setCase().editBranches({
//                     "pat": pat => pat.setPDtor(case0.tree.scope.lookupTerm("Nil")),
//                     "body": body => body.setRef(body.tree.scope.lookupTerm("acc"))
//                   }),
//                   "1": case1 => case1.setCase().editBranches({
//                     "pat": pat => pat.setPApp().editBranches({
//                       "fun": fun => fun.setPApp().editBranches({
//                         "fun": fun => fun.setPDtor(case1.tree.scope.lookupTerm("Cons")),
//                         "arg": arg => arg.setPBind("h")
//                       }),
//                       "arg": arg => arg.setPBind("t")
//                     }),
//                     "body": body => body.setApp().editBranches({
//                       "fun": fun => fun.setApp().editBranches({
//                         "fun": fun => fun.setRef(fun.tree.scope.lookupTerm("revacc")),
//                         "arg": arg => arg.setRef(arg.tree.scope.lookupTerm("t"))
//                       }),
//                       "arg": arg => arg.setApp().editBranches({
//                         "fun": fun => fun.setInApp().editBranches({
//                           "arg": arg => arg.setRef(arg.tree.scope.lookupTerm("h")),
//                           "fun": fun => fun.setRef(fun.tree.scope.lookupTerm("::"))
//                         }),
//                         "arg": arg => arg.setRef(arg.tree.scope.lookupTerm("acc"))
//                       })
//                     })
//                   })
//                 })
//               })
//             })
//           }),
//           "expr": expr => expr.setApp().editBranches({
//             "fun": fun => fun.setApp().editBranches({
//               "fun": fun => fun.setRef(fun.tree.scope.lookupTerm("revacc")),
//               "arg": arg => arg.setRef(arg.tree.scope.lookupTerm("as"))
//             }),
//             "arg": arg => arg.setRef(arg.tree.scope.lookupTerm("Nil"))
//           })
//         })
//       })
//     })
//   )
// )

// -----------------------------
// A bunch of syntax spitballing
// -----------------------------

// reverse :: ∀A → List A → List A
// reverse as = revacc as Nil where
//   revacc :: List A → List A → List A
//   revacc as acc = case as of
//     Nil      → acc
//     Cons h t → revacc t (h :: acc)

// fun reverse ∀A → as:List A → List A =
//   let revacc as:List A → acc:List A → List A = case as of
//     Nil      → acc
//     Cons h t → revacc t (h :: acc)
//   in revacc as Nil

// fun reverse [A] (as :List[A]) :List[A] =
//   fun revacc (as :List[A], acc :List[A]) :List[A] = case as
//   fun revacc (as, acc) = case as
//     Nil        = acc
//     Cons(h, t) = revacc(t, h :: acc)
//     Cons(h, t) = let rest = h :: acc in revacc(t, rest)
//   revacc(as, Nil)

// let reverse :(A ⇒ List A → List A) = (as) =>
//   let revacc :(A ⇒ List A → List A → List A) = (as, acc) => case as of
//   let revacc = as → acc → (case as of
//     Nil      → acc
//     Cons h t → revacc t (h :: acc)
//   in revacc as Nil) :List A
// :List A

// fun foo A ⇒ Monoid A ⇒ a:A → b:A → A = mappend a b
// fun foo A → Monoid A → a:A → b:A → A = mappend a b

// fun foo [A:Monoid] (a :A, b :A) :A = mappend(a, b)
// fun foo [A, Monoid[A]] (a :A, b :A) :A = mappend(a, b)
// fun foo [A] {Monoid[A]} (a :A, b :A) :A = mappend(a, b)

// fun containsKey [K,V] {Eq[K], Hashable[K]} (map :HashMap[K,V], key :K) :Bool = ...

// fun containsKey K → V → Eq K → Hashable K → map:HashMap K V → key:K → Bool = ...

// fun containsKey {K} → {V} → {Eq K} → {Hashable K} → map:HashMap K V → key:K → Bool = ...

// Experimental possible GADT syntax (and ⊕ sigil for sum type, ⊗ sigil for product)

// ⊕ List A =
//   ⊗ Nil A → List A
//   ⊗ Cons A head: A tail:List A → List A

// ⊕ Map A B =
//   ⊗ Empty A B → Map A B
//   ⊗ With A B key: A value: B rest:Map A B → Map A B

// ⊕ Expr A where
//   ⊗ Int val:Int → Expr Int
//   ⊗ Bool val:Bool → Expr Bool
//   ⊗ Add a:Expr Int b:Expr Int → Expr Int
//   ⊗ Mul a:Expr Int b:Expr Int → Expr Int
//   ⊗ Eq Eq A a:Expr A b:Expr A → Expr Bool

// def sequence[F[_]: Monoidal, A] (l :List[F[A]]) :F[List[A]] =
//  l.foldRight(Monoidal[F].pure(List.empty[A])) {
//    (fa :F[A], acc :F[List[A]]) =>
//      val prod :F[(A, List[A])] = fa.product(acc)
//      prod.map(_ +: _)
//  }

// fun sequence ∀F → Monoidal F → ∀A → list:List F A → F List A =
//   fun join fa:F A → acc:F List A = product fa acc ▷ ($1 :: $2)
//   foldRight join (pure List.empty) list

// fun sequence ∀F → Monoidal F → ∀A → list:List F A → F List A =
//   fun join fa → acc = product fa acc ▷ ($1 :: $2)
//   foldRight join (pure List.empty) list

// fun sequence ∀F → Monoidal F → ∀A → list:List F A → F List A =
//   let join = λfa acc → product fa acc ▷ (λa b → a :: b)
//   in foldRight join (pure List.empty) list

// fun sequence ∀F → Monoidal F → ∀A → list:List F A → F List A =
//   let join fa acc = product fa acc ▷ (_ :: _)
//   in foldRight join (pure List.empty) list
