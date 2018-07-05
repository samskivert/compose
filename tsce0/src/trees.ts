import { Name } from "./names"
import { Constant, constInt } from "./constants"
import * as S from "./symbols"
import * as TP from "./types"

// ------------
// Syntax trees
// ------------

// TODO: create doc AST, add Doc branch to every node that defines a name (Def, Let, Abs, etc.)

type Branch = Tree|S.Symbol|Constant

export class Tree {
  constructor (
    readonly kind :string,
    readonly parent :Tree|void,
    readonly owner :S.Symbol,
    readonly scope :S.Scope,
    readonly branches :Branch[],
    private typefn :(tree :Tree) => TP.Type
  ) {}

  get type () :TP.Type { return this.typefn(this) }

  toString () {
    return `${this.kind}#${this.branches.length}`
  }

  firstEditable (pre :Path = []) :Path {
    const first = this.branches.length > 0 ? this.branches[0] : undefined
    return first instanceof Tree ? first.firstEditable(extendPath(pre, 0)) : pre
  }

  /** Applies `edit` to the branch identified by `path`. */
  edit (path :Path, edit :Edit) :Tree {
    if (path.length == 0) throw new Error(`Invalid (empty) path for edit of ${this}`)
    const idx = path[0]
    this.checkBranchIdx(idx, "edit")
    if (path.length == 1) edit(new TreeEditor(this, idx))
    else {
      const branch = this.branches[idx]
      if (branch instanceof Tree) branch.edit(path.slice(1), edit)
      else throw new Error(`Edit path terminated in non-tree @${idx} in ${this} (branch ${branch})`)
    }
    return this
  }

  treeAt (idx :number) :Tree {
    this.checkBranchIdx(idx, "tree@")
    return this.branches[idx] as Tree
  }
  symAt (idx :number) :TreeSym {
    this.checkBranchIdx(idx, "sym@")
    return this.branches[idx] as TreeSym
  }
  constAt (idx :number) :Constant {
    this.checkBranchIdx(idx, "const@")
    return this.branches[idx] as Constant
  }
  editAt (idx :number) :TreeEditor {
    return new TreeEditor(this, idx)
  }
  editBranches (...editfns :Array<((te :TreeEditor) => void)|void>) {
    for (let ii = 0; ii < editfns.length; ii += 1) {
      const editfn = editfns[ii]
      editfn && editfn(this.editAt(ii))
    }
  }

  setBranch<T extends Branch> (idx :number, branch :T) :T {
    this.checkBranchIdx(idx, "setBranch")
    this.branches[idx] = branch
    return branch
  }

  private checkBranchIdx (idx :number, op :string) {
    if (idx < 0 || idx >= this.branches.length) throw new Error(
      `Invalid branch (${idx}) at ${this} for op '${op}'`)
  }
}

// --------------------------
// Creating and editing trees
// --------------------------

export class TreeEditor {
  constructor (readonly tree :Tree, readonly idx :number) {}

  get currentSym () :TreeSym { return this.tree.symAt(this.idx) }
  get currentTree () :Tree { return this.tree.treeAt(this.idx) }
  get currentConst () :Constant { return this.tree.constAt(this.idx) }

  setName (name :Name) {
    this.tree.symAt(this.idx).name = name
  }

  // TODO: addBranch, deleteBranch (for 'varargs' trees)
  setTreeBranch (kind :string, branches :Branch[], typefn :(tree :Tree) => TP.Type) :Tree {
    const tree = new Tree(kind, this.tree, this.tree.owner, this.tree.scope, branches, typefn)
    return this.tree.setBranch(this.idx, tree)
  }

  setDefTreeBranch (kind :string, branches :Branch[], typefn :(tree :Tree) => TP.Type) {
    // TODO
  }

  // Type and Data(type) trees
  setTHole () :Tree {
    return this.setTreeBranch("thole", [], t => TP.hole)
  }
  setTConst (cnst :Constant) :Tree {
    return this.setTreeBranch("tconst", [cnst], t => new TP.Const(t.constAt(0)))
  }
  setTRef (sym :S.TypeSym) :Tree {
    return this.setTreeBranch("tref", [sym], t => t.symAt(0).type)
  }
  setArrow (from :Tree, to :Tree) :Tree {
    return this.setTreeBranch("arrow", [from, to],
                              t => new TP.Arrow(t.symAt(0).type, t.symAt(1).type))
  }
  setTApp (ctor :Tree, arg :Tree) :Tree {
    return this.setTreeBranch("tapp", [ctor, arg],
                              t => new TP.App(t.symAt(0).type, t.symAt(1).type))
  }
  setTAbs (name :Name, body :Tree) :Tree {
    const sym = new TypeTreeSym(this.tree.owner, name)
    const scope = this.tree.scope.extend([sym])
    const tree = new Tree("tabs", this.tree, this.tree.owner, scope, [sym, body],
                          t => new TP.Abs(t.symAt(0).name, t.symAt(1).type))
    return this.tree.setBranch(this.idx, tree)
  }
  setProd (fields :Tree[]) :Tree {
    return this.setTreeBranch("prod", fields,
                              t => new TP.Prod(t.branches.map(b => (b as Tree).type as TP.Def)))
  }
  setSum (cases :Tree[]) :Tree {
    return this.setTreeBranch("sum", cases,
                              t => new TP.Sum(t.branches.map(b => (b as Tree).type as TP.Def)))
  }
  // TODO
  // Array Type
  // Interface Name Params Methods
  // Method Name Type
  setType (name :Name, type :Tree) :Tree {
    const sym = new TypeTreeSym(this.tree.owner, name)
    const scope = this.tree.scope.extend([sym])
    const tree = new Tree("type", this.tree, this.tree.owner, scope, [sym, type],
                          t => t.treeAt(1).type)
    sym.tree = tree
    return tree
  }

  // Expression trees
  setLit (cnst :Constant) :Tree {
    return this.setTreeBranch("lit", [cnst], t => new TP.Const(t.constAt(0)))
  }
  setRef (sym :S.TermSym) :Tree {
    return this.setTreeBranch("ref", [sym], t => t.symAt(0).type)
  }
  setHole (xtype :TP.Type) :Tree {
    return this.setTreeBranch("hole", [], t => xtype)
  }
  setApp (fun :Tree = emptyTree, arg :Tree = emptyTree) :Tree {
    return this.setTreeBranch("app", [fun, arg],
                              t => TP.hole) // TODO: type
  }
  setInApp (arg :Tree = emptyTree, fun :Tree = emptyTree) :Tree {
    return this.setTreeBranch("inapp", [arg, fun],
                              t => TP.hole) // TODO: type
  }
  setLet (name :Name, type :Tree = emptyTree, value :Tree = emptyTree,
          body :Tree = emptyTree) :Tree {
    const sym = new TermTreeSym(this.tree.owner, name)
    const scope = this.tree.scope.extend([sym])
    const tree = new Tree("let", this.tree, this.tree.owner, scope, [sym, type, value, body],
                          t => TP.hole) // TODO: type
    return this.tree.setBranch(this.idx, tree)
  }
  setAll (name :Name, body :Tree = emptyTree) :Tree {
    const sym = new TypeTreeSym(this.tree.owner, name)
    const scope = this.tree.scope.extend([sym])
    const tree = new Tree("all", this.tree, this.tree.owner, scope, [sym, body],
                          t => TP.hole) // TODO: type
    return this.tree.setBranch(this.idx, tree)
  }
  setAbs (name :Name, type :Tree = emptyTree, body :Tree = emptyTree) :Tree {
    const sym = new TermTreeSym(this.tree.owner, name)
    const scope = this.tree.scope.extend([sym])
    const tree = new Tree("abs", this.tree, this.tree.owner, scope, [sym, type, body],
                          t => TP.hole) // TODO: type
    return this.tree.setBranch(this.idx, tree)
  }
  setIf (test :Tree = emptyTree, texp :Tree = emptyTree, fexp :Tree = emptyTree) :Tree {
    return this.setTreeBranch("if", [test, texp, fexp],
                              t => TP.hole) // TODO: type
  }
  setMatch (scrut :Tree = emptyTree, cases :Tree[] = []) :Tree {
    return this.setTreeBranch("match", [scrut, ...cases],
                              t => TP.hole) // TODO: type
  }
  setMatchN (cases :number) :Tree {
    return this.setMatch(emptyTree, Array(cases).fill(emptyTree))
  }
  // TODO: `pat` should be a pattern tree
  setCase (pat :Tree = emptyTree, body :Tree = emptyTree) :Tree {
    // TODO: introduce symbols for pattern tree, extend scope, etc.
    const tree = new Tree("case", this.tree, this.tree.owner, this.tree.scope, [pat, body],
                          t => TP.hole) // TODO: type
    return this.tree.setBranch(this.idx, tree)
  }
  // TODO
  // Cond Array CondCase
  // CondCase Expr Expr

  // Holey setters
  setLetH (xtype :TP.Type = TP.hole) :Tree {
    const letTree = this.setLet("")
    letTree.editAt(1).setHole(TP.hole)
    letTree.editAt(2).setHole(TP.hole)
    letTree.editAt(3).setHole(xtype)
    return letTree
  }
  setMatchH () :Tree {
    const matchTree = this.setMatch(emptyTree, [emptyTree])
    matchTree.editAt(0).setHole(TP.hole)
    const caseTree = matchTree.editAt(1).setCase(emptyTree, emptyTree)
    caseTree.editAt(0).setHole(TP.hole) // TODO: setPtHole
    caseTree.editAt(1).setHole(TP.hole)
    return matchTree
  }
}

export const emptyTree = new Tree(
  "empty", undefined, S.noTermSym, S.emptyScope, [], tree => TP.hole)

export function mkTermDef (module :S.ModSym, name :Name, init? :(tree :Tree) => void) {
  const sym = new TermTreeSym(module, name)
  const scope = S.emptyScope.extend([sym])
  const tree = new Tree("term", undefined, sym, scope, [sym, emptyTree, emptyTree],
                        t => TP.hole) // TODO: type
  init && init(tree)
  sym.tree = tree
  return tree
}

export function mkTypeDef (module :S.ModSym, name :Name, init? :(tree :Tree) => void) {
  const sym = new TypeTreeSym(module, name)
  const scope = S.emptyScope.extend([sym])
  const tree = new Tree("type", undefined, sym, scope, [sym, emptyTree], t => t.treeAt(1).type)
  init && init(tree)
  sym.tree = tree
  return tree
}

// ------------
// Tree symbols
// ------------

/** Symbols assigned to trees. Obtains type information from tree type. */
abstract class TreeSym implements S.Symbol {
  tree! :Tree // initialized in tree constructor
  constructor (public name :Name) {}
  abstract get kind () :S.Kind
  get type () :TP.Type { return this.tree.type }
}

class TermTreeSym extends TreeSym implements S.TermSym {
  get kind () :"term" { return "term" }
  constructor (readonly owner :TermTreeSym|S.ModSym, name :Name) { super(name) }
}

class TypeTreeSym extends TreeSym implements S.TypeSym {
  get kind () :"type" { return "type" }
  constructor (readonly owner :TypeTreeSym|S.ModSym, name :Name) { super(name) }
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
export type Path = number[]

export const emptyPath :Path = []
export const extendPath = (path :Path, idx :number) :Path => path.concat([idx])
export const pathsEqual = (pa :Path, pb :Path) :boolean => {
  if (pa.length !== pb.length) return false
  for (let ii = 0; ii < pa.length; ii += 1) if (pa[ii] !== pb[ii]) return false
  return true
}

export type Edit = (te :TreeEditor) => void

// ----------
// Test trees
// ----------

const testModSym = new S.ModSym("test")
const natSym = new TypeTreeSym(testModSym, "Nat")
// const intSym = new TypeTreeSym(testModSym, "Int")
// const stringSym = new TypeTreeSym(testModSym, "String")

// let foo:Bool = true
// export const letExample = new TermDef(fooSym, new ScalarDef(Tag.Bool, 1), new Lit(constTrue))

const addSym = new TermTreeSym(testModSym, "+")
const subSym = new TermTreeSym(testModSym, "-")

// fib n:Nat = case n of
//   0 -> 0
//   1 -> 1
//   n -> (fib (n-1)) + (fib (n-2))
export const fibExample = mkTermDef(testModSym, "fib")
fibExample.editBranches(
  undefined,
  type => type.setTRef(natSym),
  expr => expr.setAbs("n").editBranches(
    undefined,
    type => type.setTRef(natSym),
    body => body.setMatchN(3).editBranches(
      scrut => scrut.setRef(scrut.tree.scope.lookupTerm("n")),
      case0 => case0.setCase().editBranches(
        pat => pat.setLit(constInt(0)),
        body => body.setLit(constInt(0))
      ),
      case1 => case1.setCase().editBranches(
        pat => pat.setLit(constInt(1)),
        body => body.setLit(constInt(1))
      ),
      caseN => caseN.setCase().editBranches(
        pat => pat.setRef(pat.tree.scope.lookupTerm("n")),
        body => body.setApp().editBranches(
          fun => fun.setInApp().editBranches(
            arg => arg.setApp().editBranches(
              fun => fun.setRef(fun.tree.scope.lookupTerm("fib")),
              arg => arg.setApp().editBranches(
                fun => fun.setInApp().editBranches(
                  arg => arg.setRef(arg.tree.scope.lookupTerm("n")),
                  fun => fun.setRef(subSym)
                ),
                arg => arg.setLit(constInt(1))
              )
            ),
            fun => fun.setRef(addSym)
          ),
          arg => arg.setApp().editBranches(
              fun => fun.setRef(fun.tree.scope.lookupTerm("fib")),
              arg => arg.setApp().editBranches(
                fun => fun.setInApp().editBranches(
                  arg => arg.setRef(arg.tree.scope.lookupTerm("n")),
                  fun => fun.setRef(subSym)
                ),
                arg => arg.setLit(constInt(2))
              )
            )
        )
      ),
    )
  )
)

// // data Person = Person name:String age:Nat
// const personSym = new TypeTreeSym(testModSym, "Person")
// const nameSym = new TypeTreeSym(personSym, "name")
// const ageSym = new TypeTreeSym(personSym, "age")

// export const recordExample = new TypeDef(personSym, new Prod([
//   new TypeDef(nameSym, new TRef(stringSym)),
//   new TypeDef(ageSym, new TRef(natSym))
// ]))

// // data List A = Nil | Cons head:A tail:List A
// const listSym = new TypeTreeSym(testModSym, "List")
// const listASym = new TypeTreeSym(listSym, "A")
// const nilSym = new TypeTreeSym(listSym, "Nil")
// const consSym = new TypeTreeSym(listSym, "Cons")
// const consHeadSym = new TypeTreeSym(consSym, "head")
// const consTailSym = new TypeTreeSym(consSym, "tail")

// export const listExample = new TypeDef(listSym, new TAbs(listASym, new Sum([
//   new TypeDef(nilSym, new Prod([])),
//   new TypeDef(consSym, new Prod([
//     new TypeDef(consHeadSym, new TRef(listASym)),
//     new TypeDef(consTailSym, new TApp(new TRef(listSym), new TRef(listASym)))
//   ]))
// ])))

// // type IntList = List Int
// const intListSym = new TypeTreeSym(testModSym, "IntList")
// export const aliasExample = new TypeDef(intListSym, new TApp(new TRef(listSym), new TRef(intSym)))

// // reverse as:List A -> List A =
// //   let revacc as acc = case as of
// //     Nil -> acc
// //     Cons h t -> revacc t (h :: acc)
// //   in revacc as Nil
// const revSym = new TermTreeSym(testModSym, "reverse")
// const revASym = new TypeTreeSym(revSym, "A")
// const tpListA = new TApp(new TRef(listSym), new TRef(revASym))
// const revAsSym = new TermTreeSym(revSym, "as")

// const revAccSym = new TermTreeSym(revSym, "revacc")
// const accAsSym = new TermTreeSym(revAccSym, "as")
// const accAccSym = new TermTreeSym(revAccSym, "acc")
// const accHSym = new TermTreeSym(revAccSym, "h")
// const accTSym = new TermTreeSym(revAccSym, "t")
// const revAccDef = new Abs(
//   accAsSym,
//   tpListA,
//   new Abs(
//     accAccSym,
//     tpListA,
//     new Match(
//       new Ref(accAsSym), [
//         new Case(
//           new Ref(nilSym),
//           new ExprHole(notype)
//         ),
//         new Case(
//           // TODO: this should be a PatDef not an App
//           new App(new App(new Ref(consSym), new Ref(accHSym)), new Ref(accTSym)),
//           new App(
//             new App(new Ref(revAccSym), new Ref(accTSym)),
//             new App(new App(new Ref(consSym), new Ref(accHSym)), new Ref(accAccSym))
//           )
//         )
//       ]
//     )
//   )
// )
// export const revExample = new TermDef(
//   revSym,
//   tpListA,
//   new All(
//     revASym,
//     new Abs(
//       revAsSym,
//       tpListA,
//       new Let(
//         revAccSym,
//         tpListA,
//         revAccDef,
//         new App(
//           new Ref(revAccSym),
//           new App(new Ref(revAsSym), new Ref(nilSym))
//         )
//       )
//     )
//   )
// )
