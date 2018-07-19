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

type EditFn = (te :TreeEditor) => void
type EditFns = { [key :string]: EditFn }

export abstract class Tree {
  readonly id = nextTreeId()

  // initialized by init()
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

  abstract get sig () :TP.Type

  get branchIds () :string[] { return [] }
  branch (id :string) :Branch { return this[id] }
  setBranch (id :string, branch :Branch) { this[id] = branch }

  get requireParent () :Tree {
    if (this.parent) return this.parent
    throw new Error(`Missing required parent @ ${this}`)
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

  init (parent :Tree|void, parentId :string, owner :S.ModuleSym|TreeSym, scope :S.Scope) {
    this._parent = parent
    this._parentId = parentId
    this._owner = owner
    this._scope = scope
  }

  get firstEditableId () :string|void { return this.branchIds[0] }

  firstEditable () :Path {
    const path :Path = []
    let tree :Tree = this
    let firstId = tree.firstEditableId
    while (firstId) {
      let first = tree[firstId]
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

  /** Applies `edit` to the branch identified by `path`. */
  edit (path :Path, edit :Edit) :this {
    if (path.length == 0) throw new Error(`Invalid (empty) path for edit of ${this}`)
    const id = path[0]
    if (path.length == 1) edit(this.editAt(id))
    else {
      const branch = this.branch(id)
      if (branch instanceof Tree) branch.edit(path.slice(1), edit)
      else throw new Error(`Edit path terminated in non-tree @${id} in ${this} (branch ${branch})`)
    }
    return this
  }

  select (path :Path) :Branch {
    return this._select(path, 0)
  }
  _select (path :Path, pos :number) :Branch {
    const id = path[pos]
    this.checkBranchId(id, "select")
    const branch = this.branch(id)
    if (pos == path.length-1) return branch
    else if (branch instanceof Tree) return branch._select(path, pos+1)
    else {
      throw new Error(`Select path terminated in non-tree @${id} in ${this} (branch ${branch})`)
    }
  }

  /** Adopts `child` by `reparent`ing it with `this` as its parent. */
  adopt (id :string, child :Tree) {
    console.log(`Adopting at ${id}:\n${child.debugShow().join("\n")}`)
    child.reparent(this)
    this.setBranch(id, child)
  }

  /** Changes this tree's parent to `parent`, potentially recreating its scope hierarchy. */
  reparent (parent :Tree) {
    this._parent = parent
    for (let id of this.branchIds) {
      const branch = this.branch(id)
      if (branch instanceof Tree) branch.reparent(this)
    }
  }

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

  editAt (id :string) :TreeEditor {
    this.checkBranchId(id, "edit@")
    return new TreeEditor(this, this.childOwner(id), this.childScope(id), id)
  }
  protected childOwner (id :string) :S.ModuleSym|TreeSym { return this.owner }
  protected childScope (id :string) :S.Scope { return this.scope }

  editBranch (id :string, editfn :(te :TreeEditor) => void) :this {
    editfn(this.editAt(id))
    return this
  }
  editBranches (editfns :EditFns) :this {
    for (let id in editfns) editfns[id](this.editAt(id))
    return this
  }

  debugShow () :string[] {
    const buf :string[] = []
    this._debugShow("", "", buf)
    return buf
  }
  protected _debugShow (indent :string, id :string, buf :string[]) {
    const pre = id ? `${id}=` : "";
    buf.push(`${indent}${pre}${this.kind} ` +
             `parent=${this.parent ? this.parent.kind : "<none>"} ` +
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
  constructor (readonly self :TreeSym) { super() }
  reparent (parent :Tree) {
    // recreate our scope before calling super as we need the new scope to be in place when we
    // recursively reparent our children
    this._scope = parent.scope.extend([this.self])
    super.reparent(parent)
  }
  protected childOwner (id :string) :S.ModuleSym|TreeSym { return this.self }
}

const emptyModSym = new S.ModuleSym("<empty>")

export class EmptyTree extends Tree {
  get owner () :S.ModuleSym|TreeSym { return emptyModSym }
  get scope () :S.Scope { return S.emptyScope }
  get sig () :TP.Type { return TP.hole }
}
export const emptyTree = new EmptyTree()

// ----------
// Type trees
// ----------

export class THoleTree extends Tree {
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
  setBranch (id :string, branch :Branch) { this.fields[parseInt(id)] = branch as Tree }
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
  setBranch (id :string, branch :Branch) { this.cases[parseInt(id)] = branch as Tree }
  protected isValidBranch (id :string) :boolean {
    const idx = parseInt(id)
    return idx >= 0 && idx <= this.cases.length // allow one past last field
  }
  get sig () :TP.Type { return new TP.Sum(this.cases.map(c => (c as Tree).sig as TP.Def)) }
}

export class TAbsTree extends DefTree {
  body :Tree = emptyTree
  constructor (readonly sym :TreeSym) { super(sym) }
  get branchIds () :string[] { return ["sym", "body"] }
  get sig () :TP.Type { return new TP.Abs(this.sym, this.body.sig) }
  protected _sigQuants (acc :TypeVarTreeSym[]) { acc.unshift(this.self as TypeVarTreeSym) }
}

export class FieldTree extends DefTree {
  type :Tree = emptyTree
  constructor (readonly sym :TreeSym) { super(sym) }
  get branchIds () :string[] { return ["sym", "type"] }
  get sig () :TP.Type { return this.type.sig }
}

export class CtorTree extends DefTree {
  prod :Tree = emptyTree
  constructor (readonly sym :TreeSym) { super(sym) }
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
}

// -------------
// Pattern trees
// -------------

export abstract class PatTree extends Tree {
  addSymbols (syms :S.Symbol[]) {}
}

export class PLitTree extends PatTree {
  constructor (public cnst :C.Constant) { super() }
  get branchIds () :string[] { return ["cnst"] }
  get sig () :TP.Type { return new TP.Const(this.cnst) }
}

export class PBindTree extends PatTree {
  // we're not a DefTree so we have to manually initialize sym.tree (ugh)
  constructor (readonly sym :TreeSym) { super() ; sym.tree = this }
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
  fun :Tree = emptyTree
  arg :Tree = emptyTree
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

// -----------------
// Abstraction trees
// -----------------

export class LetTree extends DefTree {
  type :Tree = emptyTree
  body :Tree = emptyTree
  expr :Tree = emptyTree
  constructor (readonly sym :TreeSym) { super(sym) }
  get branchIds () :string[] { return ["sym", "type", "body", "expr"] }
  get sig () :TP.Type { return TP.hole } // TODO
}

export class LetFunTree extends DefTree {
  body :Tree = emptyTree
  expr :Tree = emptyTree
  constructor (readonly sym :TreeSym) { super(sym) }
  get branchIds () :string[] { return ["sym", "body", "expr"] }
  get sig () :TP.Type { return this.body.sig } // TODO
}

export class AllTree extends DefTree {
  body :Tree = emptyTree
  constructor (readonly sym :TreeSym) { super(sym) }
  get branchIds () :string[] { return ["sym", "body"] }
  get sig () :TP.Type { return new TP.Abs(this.sym, this.body.sig) }
}

export class AbsTree extends DefTree {
  type :Tree = emptyTree
  body :Tree = emptyTree
  constructor (readonly sym :TreeSym) { super(sym) }
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
  constructor (readonly xtype :TP.Type) { super() }
  get sig () :TP.Type { return this.xtype }
}

class BaseAppTree extends Tree {
  fun :Tree = emptyTree
  arg :Tree = emptyTree
  get sig () :TP.Type { return TP.funApply(this.fun.sig, this.arg.sig) }
  protected childPrototype (id :string) :TP.Type|void {
    if (id === "arg") {
      const funType = this.fun.sig
      if (funType instanceof TP.Arrow) return funType.from
    } else if (id === "fun") {
      return new TP.Arrow(this.arg.sig, this.prototype)
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
}

export class MatchTree extends Tree {
  scrut :Tree = emptyTree
  cases :Tree[] = []
  get branchIds () :string[] { return ["scrut"].concat(this.cases.map((c, ii) => `${ii}`)) }
  branch (id :string) :Branch {
    if (id === "scrut") return this.scrut
    else return this.cases[parseInt(id)]
  }
  setBranch (id :string, branch :Branch) {
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
  pat :Tree = emptyTree
  body :Tree = emptyTree
  get branchIds () :string[] { return ["pat", "body"] }
  get sig () :TP.Type { return this.body.sig }
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
      if (pred(sym) && this.isCompletion(prefix, sym.name)) syms.push(sym)
    }
    this.tree.scope._addCompletions(pred, prefix, syms)
  }
}

// ----------------
// Definition trees
// ----------------

export class FunDefTree extends DefTree {
  body :Tree = emptyTree
  constructor (readonly sym :TreeSym) { super(sym) }
  get branchIds () :string[] { return ["sym", "body"] }
  get sig () :TP.Type { return this.body.sig }
}

export class TypeDefTree extends DefTree {
  body :Tree = emptyTree
  constructor (readonly sym :TypeTreeSym) { super(sym) }
  get branchIds () :string[] { return ["sym", "body"] }
  get sig () :TP.Type { return new TP.Def(this.sym) }
}

export class DefHoleTree extends DefTree {
  constructor (readonly sym :TreeSym) { super(sym) }
  get branchIds () :string[] { return ["sym"] }
  get sig () :TP.Type { return TP.hole }
}

// --------------------------
// Creating and editing trees
// --------------------------

export class TreeEditor {
  constructor (readonly tree :Tree,
               readonly owner :S.ModuleSym|TreeSym,
               readonly scope :S.Scope,
               readonly id :string) {}

  get currentTree () :Tree { return this.tree.treeAt(this.id) }

  setName (name :Name) { this.tree.symAt(this.id).name = name }
  setBranch (tree :Tree) :Tree {
    tree.init(this.tree, this.id, this.owner, this.scope)
    this.tree.setBranch(this.id, tree)
    return tree
  }
  setDefBranch (tree :DefTree) :Tree {
    const scope = this.scope.extend([tree.self])
    tree.init(this.tree, this.id, this.owner, scope)
    tree.self.tree = tree
    this.tree.setBranch(this.id, tree)
    return tree
  }

  // Type and Data(type) trees
  setTHole () :Tree { return this.setBranch(new THoleTree()) }
  setTConst (cnst :C.Constant) :Tree { return this.setBranch(new TConstTree(cnst)) }
  setTRef (sym :S.Symbol) :Tree { return this.setBranch(new TRefTree(sym)) }
  setArrow () :Tree { return this.setBranch(new ArrowTree()) }
  setTApp () :Tree { return this.setBranch(new TAppTree()) }
  setTAbs (name :Name) :Tree {
    return this.setDefBranch(new TAbsTree(new TypeVarTreeSym(name, this.owner)))
  }
  setField (name :Name) :Tree {
    return this.setDefBranch(new FieldTree(new TreeSym("term", name, this.owner)))
  }
  setProd () :Tree { return this.setBranch(new ProdTree()) }
  setSum () :Tree { return this.setBranch(new SumTree()) }
  setCtor (name :Name) :Tree {
    const sym = new TreeSym("func", name, this.owner)
    this.owner.module.scope.insert(sym)
    return this.setDefBranch(new CtorTree(sym))
  }
  // TODO
  // Array Type
  // Interface Name Params Methods
  // Method Name Type
  // setType (name :Name) :Tree {
  //   const sym = new TreeSym("type", name, this.owner)
  //   return this.setDefTreeBranch("type", sym, [sym, emptyTree], t => t.treeAt(1).type)
  // }

  // Pattern trees
  setPLit (cnst :C.Constant) :Tree { return this.setBranch(new PLitTree(cnst)) }
  setPBind (name :Name) :Tree {
    return this.setBranch(new PBindTree(new TreeSym("term", name, this.owner)))
  }
  setPDtor (ctor :S.Symbol) :Tree { return this.setBranch(new PDtorTree(ctor)) }
  setPApp () { return this.setBranch(new PAppTree()) }

  // Abstraction terms
  setLet (name :Name) :Tree {
    return this.setDefBranch(new LetTree(new TreeSym("term", name, this.owner)))
  }
  setLetFun (name :Name) :Tree {
    return this.setDefBranch(new LetFunTree(new TreeSym("func", name, this.owner)))
  }
  setAll (name :Name) :Tree {
    return this.setDefBranch(new AllTree(new TypeVarTreeSym(name, this.owner)))
  }
  setAbs (name :Name) :Tree {
    const sym = new VarTreeSym(name, this.owner, tree => (tree as AbsTree).type.sig)
    return this.setDefBranch(new AbsTree(sym))
  }

  // Expression terms
  setHole (xtype :TP.Type = TP.hole) :Tree { return this.setBranch(new HoleTree(xtype)) }
  setLit (cnst :C.Constant) :Tree { return this.setBranch(new LitTree(cnst)) }
  setRef (sym :S.Symbol) :Tree { return this.setBranch(new RefTree(sym)) }
  setAsc () :Tree { return this.setBranch(new AscTree()) }
  setApp () :Tree { return this.setBranch(new AppTree()) }
  setInApp () :Tree { return this.setBranch(new InAppTree()) }
  setIf () :Tree { return this.setBranch(new IfTree()) }
  setMatch () :Tree { return this.setBranch(new MatchTree()) }
  setCase (): Tree { return this.setBranch(new CaseTree()) }
  // TODO
  // Cond Array CondCase
  // CondCase Expr Expr

  // Holey setters
  setLetH (xtype :TP.Type = TP.hole) :Tree {
    return this.setLet("").editBranches({
      "value": value => value.setHole(),
      "body": body => body.setHole(xtype)
    })
  }
  setMatchH (xtype :TP.Type = TP.hole) :Tree {
    return this.setMatch().editBranches({
      "scrut": scrut => scrut.setHole(),
      "0": case0 => case0.setCase().editBranches({
        "pat": pat => pat.setHole(),
        "body": body => body.setHole(xtype)
      })
    })
  }
  setIfH (xtype :TP.Type = TP.hole) :Tree {
    return this.setIf().editBranches({
      "test": test => test.setHole(), // TODO: Bool
      "texp": texp => texp.setHole(xtype),
      "fexp": fexp => fexp.setHole(xtype)
    })
  }
}

export function mkDefHole (mod :S.ModuleSym) :DefTree {
  const sym = new TreeSym("term", "", mod)
  const tree = new DefHoleTree(sym)
  tree.init(undefined, "", mod, mod.scope)
  sym.tree = tree
  return tree
}

export function mkFunDef (mod :S.ModuleSym, name :Name) :DefTree {
  const sym = new TreeSym("func", name, mod)
  // insert this term symbol into the module scope
  mod.scope.insert(sym)
  const tree = new FunDefTree(sym)
  tree.init(undefined, "", mod, mod.scope.extend([sym]))
  sym.tree = tree
  return tree
}

export function mkTypeDef (mod :S.ModuleSym, name :Name) :DefTree {
  const sym = new TypeTreeSym(name, mod)
  // insert this type symbol into the module scope
  mod.scope.insert(sym)
  const tree = new TypeDefTree(sym)
  tree.init(undefined, "", mod, mod.scope.extend([sym]))
  sym.tree = tree
  return tree
}

// ------------
// Tree symbols
// ------------

/** Symbols assigned to definition trees. Obtains type information from tree type. */
class TreeSym extends S.Symbol {
  tree! :Tree // initialized by tree (in constructor or when reincarnated)
  constructor (kind :S.Kind, name :Name, readonly owner :TreeSym|S.ModuleSym) { super(kind, name) }
  get type () :TP.Type { return this.tree.sig }
}

class TypeTreeSym extends TreeSym {
  constructor (name :Name, owner :TreeSym|S.ModuleSym) { super("type", name, owner) }
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
  constructor (name :Name, owner :TreeSym|S.ModuleSym,
               readonly typefn :(t :Tree) => TP.Type) { super("term", name, owner) }
  get type () :TP.Type { return this.typefn(this.tree) }
}

/** Symbols assigned to type var trees. */
class TypeVarTreeSym extends TreeSym {
  constructor (name :Name, owner :TreeSym|S.ModuleSym) { super("type", name, owner) }
  get type () :TP.Var { return new TP.Var(this) }
}

// export class ScalarDef extends TypeTree {
//   readonly type = new TP.Scalar(this.tag, this.size)
//   constructor (readonly tag :Tag, readonly size :number) { super () }
//   toString () { return `Scalar:${this.tag}${this.size}`}
// }

// ---------------
// Tree path model
// ---------------

// A path identifies a point in an AST, generally used for editing. The editing cursor uses paths to
// track the insertion point. The selection is a pair of paths that track the start and end of the
// selection.

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
export type Path = string[]

export const emptyPath :Path = []
export const extendPath = (path :Path, id :string) :Path => path.concat([id])
export const pathsEqual = (pa :Path, pb :Path) :boolean => {
  if (pa.length !== pb.length) return false
  for (let ii = 0; ii < pa.length; ii += 1) if (pa[ii] !== pb[ii]) return false
  return true
}

export type Edit = (te :TreeEditor) => void

// ----------
// Test trees
// ----------

export const testModSym = new S.ModuleSym("test")

class PrimTypeTreeSym extends TreeSym {
  constructor (name :Name, readonly owner :TreeSym|S.ModuleSym,
               readonly bodyType :TP.Type) { super("type", name, owner) }
  get type () :TP.Type { return new TP.Def(this) }
}

const natSym = new PrimTypeTreeSym("Nat", testModSym, new TP.Scalar(C.Tag.Int, 32))
const intSym = new PrimTypeTreeSym("Int", testModSym, new TP.Scalar(C.Tag.Int, 32))
const stringSym = new PrimTypeTreeSym("String", testModSym, new TP.Scalar(C.Tag.String, 1))
testModSym.scope.insert(natSym)
testModSym.scope.insert(intSym)
testModSym.scope.insert(stringSym)

class PrimFunTreeSym extends TreeSym {
  constructor (name :Name, readonly owner :TreeSym|S.ModuleSym,
               readonly _type :TP.Type) { super("func", name, owner) }
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
export const boxExample = mkTypeDef(testModSym, "Box").editBranch(
  "body", body => body.setTAbs("A").editBranch(
    "body", body => body.setCtor("Box").editBranch(
      "prod", prod => prod.setProd().editBranch(
        "0", contents => contents.setField("contents").editBranch(
          "type", type => type.setTRef(contents.tree.scope.lookupType("A"))
        )
      )
    )
  )
)

// type Person name:String age:Nat
// type Person (name :String, age :Nat)
export const recordExample = mkTypeDef(testModSym, "Person").editBranch(
  "body", type => type.setCtor("Person").editBranch(
    "prod", prod => prod.setProd().editBranches({
      "0": name => name.setField("name").editBranch("type", type => type.setTRef(stringSym)),
      "1": age => age.setField("age").editBranch("type", type => type.setTRef(natSym))
    })
  )
)

// type List A =
//   * Nil
//   * Cons head:A tail:List A

// type List [A] =
//   * Nil
//   * Cons (head :A, tail :List[A])
export const listExample = mkTypeDef(testModSym, "List").editBranch(
  "body", body => body.setTAbs("T").editBranch(
    "body", body => body.setSum().editBranches({
      "0": nil => nil.setCtor("Nil").editBranch("prod", prod => prod.setProd()),
      "1": list => list.setCtor("Cons").editBranch(
        "prod", prod => prod.setProd().editBranches({
          "0": head => head.setField("head").editBranch(
            "type", type => type.setTRef(type.tree.scope.lookupType("T"))
          ),
          "1": tail => tail.setField("tail").editBranch(
            "type", type => type.setTApp().editBranches({
              "ctor": ctor => ctor.setTRef(ctor.tree.scope.lookupType("List")),
              "arg": arg => arg.setTRef(arg.tree.scope.lookupType("T"))
            })
          )
        })
      )
    })
  )
)

function mkListA (tb :TreeEditor) :Tree {
  return tb.setTApp().editBranches({
    "ctor": ctor => ctor.setTRef(ctor.tree.scope.lookupType("List")),
    "arg": arg  => arg.setTRef(tb.tree.scope.lookupType("A"))
  })
}

// fun :: [A] (head :A, tail :List[A]) :List[A] = Cons(head, tail)
export const consFunExample = mkFunDef(testModSym, "::").editBranch(
  "body", body => body.setAll("A").editBranch(
    "body", body => body.setAbs("head").editBranches({
      "type": type => type.setTRef(type.tree.scope.lookupType("A")),
      "body": body => body.setAbs("tail").editBranches({
        "type": mkListA,
        "body": body => body.setAsc().editBranches({
          "type": mkListA,
          "expr": expr => expr.setApp().editBranches({
            "fun": fun => fun.setApp().editBranches({
              "fun": fun => fun.setRef(fun.tree.scope.lookupFunc("Cons")),
              "arg": arg => arg.setRef(arg.tree.scope.lookupTerm("head"))
            }),
            "arg": arg => arg.setRef(arg.tree.scope.lookupTerm("tail"))
          })
        })
      })
    })
  )
)

// type IntList = List Int
// type IntList = List[Int]
export const aliasExample = mkTypeDef(testModSym, "IntList").editBranch(
  "body", body => body.setTApp().editBranches({
    "ctor": ctor => ctor.setTRef(ctor.tree.scope.lookupType("List")),
    "arg": arg  => arg.setTRef(intSym)
  })
)

// term fib n:Nat → Nat = case n of
//   0 → 0
//   1 → 1
//   n → fib (n-1) + fib (n-2)

// fun fib (n:Nat) :Nat = case n
//   0 = 0
//   1 = 1
//   n = fib(n-1) + fib(n-2)
export const fibExample = mkFunDef(testModSym, "fib").editBranch(
  "body", body => body.setAbs("n").editBranches({
    "type": type => type.setTRef(natSym),
    "body": expr => expr.setAsc().editBranches({
      "type": type => type.setTRef(natSym),
      "expr": expr => expr.setMatch().editBranches({
        "scrut": scrut => scrut.setRef(scrut.tree.scope.lookupTerm("n")),
        "0": case0 => case0.setCase().editBranches({
          "pat": pat => pat.setPLit(C.constInt(0)),
          "body": body => body.setLit(C.constInt(0))
        }),
        "1": case1 => case1.setCase().editBranches({
          "pat": pat => pat.setPLit(C.constInt(1)),
          "body": body => body.setLit(C.constInt(1))
        }),
        "2": caseN => caseN.setCase().editBranches({
          "pat": pat => pat.setPBind("n"),
          "body": body => body.setApp().editBranches({
            "fun": fun => fun.setInApp().editBranches({
              "arg": arg => arg.setApp().editBranches({
                "fun": fun => fun.setRef(fun.tree.scope.lookupFunc("fib")),
                "arg": arg => arg.setApp().editBranches({
                  "fun": fun => fun.setInApp().editBranches({
                    "arg": arg => arg.setRef(arg.tree.scope.lookupTerm("n")),
                    "fun": fun => fun.setRef(subSym)
                  }),
                  "arg": arg => arg.setLit(C.constInt(1))
                })
              }),
              "fun": fun => fun.setRef(addSym)
            }),
            "arg": arg => arg.setApp().editBranches({
              "fun": fun => fun.setRef(fun.tree.scope.lookupFunc("fib")),
              "arg": arg => arg.setApp().editBranches({
                "fun": fun => fun.setInApp().editBranches({
                  "arg": arg => arg.setRef(arg.tree.scope.lookupTerm("n")),
                  "fun": fun => fun.setRef(subSym)
                }),
                "arg": arg => arg.setLit(C.constInt(2))
              })
            })
          })
        })
      })
    })
  })
)

// fun reverse [A] (as :List[A]) :List[A] =
//   fun revacc (as :List[A], acc :List[A]) :List[A] = case as
//     Nil        = acc
//     Cons(h, t) = revacc(t, h :: acc)
//   revacc(as, Nil)
export const revExample = mkFunDef(testModSym, "reverse").editBranch(
  "body", body => body.setAll("A").editBranch(
    "body", body => body.setAbs("as").editBranches({
      "type": mkListA,
      "body": body => body.setAsc().editBranches({
        "type": mkListA,
        "expr": expr => expr.setLetFun("revacc").editBranches({
          "body": body => body.setAbs("as").editBranches({
            "type": mkListA,
            "body": body => body.setAbs("acc").editBranches({
              "type": mkListA,
              "body": body => body.setAsc().editBranches({
                "type": mkListA,
                "expr": expr => expr.setMatch().editBranches({
                  "scrut": scrut => scrut.setRef(scrut.tree.scope.lookupTerm("as")),
                  "0": case0 => case0.setCase().editBranches({
                    "pat": pat => pat.setPDtor(case0.tree.scope.lookupFunc("Nil")),
                    "body": body => body.setRef(body.tree.scope.lookupTerm("acc"))
                  }),
                  "1": case1 => case1.setCase().editBranches({
                    "pat": pat => pat.setPApp().editBranches({
                      "fun": fun => fun.setPApp().editBranches({
                        "fun": fun => fun.setPDtor(case1.tree.scope.lookupFunc("Cons")),
                        "arg": arg => arg.setPBind("h")
                      }),
                      "arg": arg => arg.setPBind("t")
                    }),
                    "body": body => body.setApp().editBranches({
                      "fun": fun => fun.setApp().editBranches({
                        "fun": fun => fun.setRef(fun.tree.scope.lookupFunc("revacc")),
                        "arg": arg => arg.setRef(arg.tree.scope.lookupTerm("t"))
                      }),
                      "arg": arg => arg.setApp().editBranches({
                        "fun": fun => fun.setInApp().editBranches({
                          "arg": arg => arg.setRef(arg.tree.scope.lookupTerm("h")),
                          "fun": fun => fun.setRef(fun.tree.scope.lookupFunc("::"))
                        }),
                        "arg": arg => arg.setRef(arg.tree.scope.lookupTerm("acc"))
                      })
                    })
                  })
                })
              })
            })
          }),
          "expr": expr => expr.setApp().editBranches({
            "fun": fun => fun.setApp().editBranches({
              "fun": fun => fun.setRef(fun.tree.scope.lookupFunc("revacc")),
              "arg": arg => arg.setRef(arg.tree.scope.lookupTerm("as"))
            }),
            "arg": arg => arg.setRef(arg.tree.scope.lookupFunc("Nil"))
          })
        })
      })
    })
  )
)

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
