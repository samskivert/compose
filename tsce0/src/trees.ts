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
    readonly owner :S.ModuleSym|TreeSym,
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
    if (path.length == 1) edit(this.editAt(idx))
    else {
      const branch = this.branches[idx]
      if (branch instanceof Tree) branch.edit(path.slice(1), edit)
      else throw new Error(`Edit path terminated in non-tree @${idx} in ${this} (branch ${branch})`)
    }
    return this
  }

  migrateChild (idx :number, child :Tree) {
    // TODO
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
    return new TreeEditor(this, this.owner, idx)
  }
  editBranch (idx :number, editfn :(te :TreeEditor) => void) :Tree {
    editfn(this.editAt(idx))
    return this
  }
  editBranch1 (editfn :(te :TreeEditor) => void) :Tree {
    return this.editBranch(1, editfn)
  }
  editBranches (...editfns :Array<((te :TreeEditor) => void)|void>) :Tree {
    for (let ii = 0; ii < editfns.length; ii += 1) {
      const editfn = editfns[ii]
      editfn && editfn(this.editAt(ii))
    }
    return this
  }

  debugShow () :string[] {
    const buf :string[] = []
    this._debugShow("", buf)
    return buf
  }
  _debugShow (indent :string, buf :string[]) {
    buf.push(`${indent}${this.kind} ` +
             `parent=${this.parent ? this.parent.kind : "<none>"} ` +
             `owner=${this.owner} ` +
             `scope=${this.scope}`)
    for (let branch of this.branches) {
      if (branch instanceof Tree) {
        branch._debugShow(`${indent}  `, buf)
      } else {
        buf.push(`${indent}  ${branch}`)
      }
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

export class DefTree extends Tree {

  constructor (
    kind :string,
    parent :Tree|void,
    owner :S.ModuleSym|TreeSym,
    readonly self :TreeSym,
    scope :S.Scope,
    branches :Branch[],
    typefn :(tree :Tree) => TP.Type
  ) { super(kind, parent, owner, scope, branches, typefn) }

  editAt (idx :number) :TreeEditor {
    return new TreeEditor(this, this.self, idx)
  }
}

// --------------------------
// Creating and editing trees
// --------------------------

export class TreeEditor {
  constructor (readonly tree :Tree, readonly owner :S.ModuleSym|TreeSym, readonly idx :number) {}

  get currentSym () :TreeSym { return this.tree.symAt(this.idx) }
  get currentTree () :Tree { return this.tree.treeAt(this.idx) }
  get currentConst () :Constant { return this.tree.constAt(this.idx) }

  setName (name :Name) {
    this.tree.symAt(this.idx).name = name
  }

  // TODO: addBranch, deleteBranch (for 'varargs' trees)
  setTreeBranch (kind :string, branches :Branch[], typefn :(tree :Tree) => TP.Type) :Tree {
    const tree = new Tree(kind, this.tree, this.owner, this.tree.scope, branches, typefn)
    return this.tree.setBranch(this.idx, tree)
  }
  setDefTreeBranch (kind :string, self :TreeSym, branches :Branch[],
                    typefn :(tree :Tree) => TP.Type) :Tree {
    const scope = this.tree.scope.extend([self])
    const tree = new DefTree(kind, this.tree, this.owner, self, scope, branches, typefn)
    self.tree = tree
    return this.tree.setBranch(this.idx, tree)
  }

  // Type and Data(type) trees
  setTHole () :Tree {
    return this.setTreeBranch("thole", [], t => TP.hole)
  }
  setTConst (cnst :Constant) :Tree {
    return this.setTreeBranch("tconst", [cnst], t => new TP.Const(t.constAt(0)))
  }
  setTRef (sym :S.Symbol) :Tree {
    return this.setTreeBranch("tref", [sym], t => t.symAt(0).type)
  }
  setArrow (from :Tree, to :Tree) :Tree {
    return this.setTreeBranch("arrow", [from, to],
                              t => new TP.Arrow(t.symAt(0).type, t.symAt(1).type))
  }
  setTApp () :Tree {
    return this.setTreeBranch("tapp", [emptyTree, emptyTree],
                              t => new TP.App(t.symAt(0).type, t.symAt(1).type))
  }
  setTAbs (name :Name) :Tree {
    const sym = new TreeSym("type", name, this.owner)
    return this.setDefTreeBranch("tabs", sym, [sym, emptyTree],
                                 t => new TP.Abs(t.symAt(0).name, t.symAt(1).type))
  }
  setField (name :Name) :Tree {
    const sym = new TreeSym("term", name, this.owner)
    return this.setDefTreeBranch("field", sym, [sym, emptyTree], t => t.treeAt(1).type)
  }
  setProd (fields :Tree[]) :Tree {
    return this.setTreeBranch("prod", fields,
                              t => new TP.Prod(t.branches.map(b => (b as Tree).type as TP.Def)))
  }
  setProdN (cases :number) :Tree {
    return this.setProd(Array(cases).fill(emptyTree))
  }
  setSum (cases :Tree[]) :Tree {
    return this.setTreeBranch("sum", cases,
                              t => new TP.Sum(t.branches.map(b => (b as Tree).type as TP.Def)))
  }
  setCtor (name :Name) :Tree {
    const sym = new TreeSym("func", name, this.owner)
    this.owner.module.scope.insert(sym)
    return this.setDefTreeBranch("ctor", sym, [sym, emptyTree], t => t.treeAt(1).type)
  }
  // TODO
  // Array Type
  // Interface Name Params Methods
  // Method Name Type
  setType (name :Name) :Tree {
    const sym = new TreeSym("type", name, this.owner)
    return this.setDefTreeBranch("type", sym, [sym, emptyTree], t => t.treeAt(1).type)
  }

  // Pattern trees
  setPHole () :Tree {
    return this.setTreeBranch("phole", [emptyTree, emptyTree], t => TP.hole)
  }
  setPLit (cnst :Constant) :Tree {
    return this.setTreeBranch("plit", [cnst, emptyTree], t => new TP.Const(t.constAt(0)))
  }
  setPBind (name :Name) :Tree {
    const sym = new TreeSym("term", name, this.owner)
    return this.setDefTreeBranch("pbind", sym, [sym, emptyTree], t => TP.hole) // TODO: type
  }
  setPDtor (ctor :S.Symbol) :Tree {
    return this.setTreeBranch("pdtor", [ctor, emptyTree], t => ctor.type)
  }

  // Abstraction terms
  setLet (name :Name, value :Tree = emptyTree, expr :Tree = emptyTree) :Tree {
    const sym = new TreeSym("term", name, this.owner)
    return this.setDefTreeBranch("let", sym,[sym, value, expr],
                                 t => TP.hole) // TODO: type
  }
  setLetFun (name :Name, value :Tree = emptyTree, expr :Tree = emptyTree) :Tree {
    const sym = new TreeSym("func", name, this.owner)
    return this.setDefTreeBranch("letfun", sym, [sym, value, expr],
                                 t => TP.hole) // TODO: type
  }
  setAll (name :Name, body :Tree = emptyTree) :Tree {
    const sym = new TreeSym("type", name, this.owner)
    return this.setDefTreeBranch("all", sym,[sym, body],
                                 t => TP.hole) // TODO: type
  }
  setAbs (name :Name, type :Tree = emptyTree, body :Tree = emptyTree) :Tree {
    const sym = new TreeSym("term", name, this.owner)
    return this.setDefTreeBranch("abs", sym, [sym, type, body],
                                 t => TP.hole) // TODO: type
  }

  // Expression terms
  setLit (cnst :Constant) :Tree {
    return this.setTreeBranch("lit", [cnst], t => new TP.Const(t.constAt(0)))
  }
  setRef (sym :S.Symbol) :Tree {
    return this.setTreeBranch("ref", [sym], t => t.symAt(0).type)
  }
  setAsc (type :Tree = emptyTree, expr :Tree = emptyTree) :Tree {
    return this.setTreeBranch("asc", [type, expr], t => t.treeAt(0).type)
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
  // TODO
  // Cond Array CondCase
  // CondCase Expr Expr

  // Holey setters
  setLetH (xtype :TP.Type = TP.hole) :Tree {
    const letTree = this.setLet("")
    letTree.editAt(1).setHole(TP.hole)
    letTree.editAt(2).setHole(xtype)
    return letTree
  }
  setMatchH () :Tree {
    const matchTree = this.setMatch(emptyTree, [emptyTree])
    matchTree.editAt(0).setHole(TP.hole)
    matchTree.editAt(1).setPHole()
    return matchTree
  }
}

const emptyModSym = new S.ModuleSym("<empty>")

export const emptyTree = new Tree(
  "empty", undefined, emptyModSym, S.emptyScope, [], tree => TP.hole)

export function mkFunDef (mod :S.ModuleSym, name :Name) :DefTree {
  const sym = new TreeSym("func", name, mod)
  // insert this term symbol into the module scope
  mod.scope.insert(sym)
  const tree = new DefTree("fun", undefined, mod, sym, mod.scope.extend([sym]), [sym, emptyTree],
                           t => TP.hole) // TODO: type
  sym.tree = tree
  return tree
}

export function mkTypeDef (mod :S.ModuleSym, name :Name) :DefTree {
  const sym = new TreeSym("type", name, mod)
  // insert this type symbol into the module scope
  mod.scope.insert(sym)
  const tree = new DefTree("type", undefined, mod, sym, mod.scope.extend([sym]), [sym, emptyTree],
                           t => t.treeAt(1).type)
  sym.tree = tree
  return tree
}

// ------------
// Tree symbols
// ------------

/** Symbols assigned to trees. Obtains type information from tree type. */
class TreeSym extends S.Symbol {
  tree! :Tree // initialized in tree constructor
  constructor (kind :S.Kind, name :Name, readonly owner :TreeSym|S.ModuleSym) { super(kind, name) }
  get type () :TP.Type { return this.tree.type }
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

const testModSym = new S.ModuleSym("test")

const natSym = new TreeSym("type", "Nat", testModSym)
const intSym = new TreeSym("type", "Int", testModSym)
const stringSym = new TreeSym("type", "String", testModSym)
testModSym.scope.insert(natSym)
testModSym.scope.insert(intSym)
testModSym.scope.insert(stringSym)

const addSym = new TreeSym("func", "+", testModSym)
const subSym = new TreeSym("func", "-", testModSym)
testModSym.scope.insert(addSym)
testModSym.scope.insert(subSym)

// type Box A contents:A
// type Box [A] (contents :A)
export const boxExample = mkTypeDef(testModSym, "Box").editBranch1(
  type => type.setTAbs("A").editBranch1(
    type => type.setCtor("Box").editBranch1(
      prod => prod.setProdN(1).editBranches(
        contents => contents.setField("contents").editBranch1(
          type => type.setTRef(contents.tree.scope.lookupType("A"))),
      )
    )
  )
)

// type Person name:String age:Nat
// type Person (name :String, age :Nat)
export const recordExample = mkTypeDef(testModSym, "Person").editBranch1(
  type => type.setCtor("Person").editBranch1(
    prod => prod.setProdN(2).editBranches(
      name => name.setField("name").editBranch1(type => type.setTRef(stringSym)),
      age => age.setField("age").editBranch1(type => type.setTRef(natSym))
    )
  )
)

// type List A =
//   * Nil
//   * Cons head:A tail:List A

// type List [A] =
//   * Nil
//   * Cons (head :A, tail :List[A])
export const listExample = mkTypeDef(testModSym, "List").editBranch1(
  type => type.setTAbs("A").editBranch1(
    type => type.setSum([emptyTree, emptyTree]).editBranches(
      nil => nil.setCtor("Nil").editBranch1(prod => prod.setProdN(0)),
      list => list.setCtor("Cons").editBranch1(
        prod => prod.setProdN(2).editBranches(
          head => head.setField("head").editBranch1(
            type => type.setTRef(type.tree.scope.lookupType("A"))
          ),
          tail => tail.setField("tail").editBranch1(
            type => type.setTApp().editBranches(
              ctor => ctor.setTRef(ctor.tree.scope.lookupType("List")),
              arg => arg.setTRef(arg.tree.scope.lookupType("A"))
            )
          )
        )
      )
    )
  )
)

function mkListA (tb :TreeEditor) :Tree {
  return tb.setTApp().editBranches(
    ctor => ctor.setTRef(ctor.tree.scope.lookupType("List")),
    arg  => arg.setTRef(tb.tree.scope.lookupType("A"))
  )
}

// fun :: [A] (head :A, tail :List[A]) :List[A] = Cons(head, tail)
export const consFunExample = mkFunDef(testModSym, "::").editBranch1(
  body => body.setAll("A").editBranch1(
    body => body.setAbs("head").editBranches(
      undefined,
      type => type.setTRef(type.tree.scope.lookupType("A")),
      body => body.setAbs("tail").editBranches(
        undefined,
        type => mkListA(type),
        body => body.setAsc().editBranches(
          type => mkListA(type),
          body => body.setApp().editBranches(
            fun => fun.setApp().editBranches(
              fun => fun.setRef(fun.tree.scope.lookupFunc("Cons")),
              arg => arg.setRef(arg.tree.scope.lookupTerm("head"))
            ),
            arg => arg.setRef(arg.tree.scope.lookupTerm("tail"))
          )
        )
      )
    )
  )
)

// type IntList = List Int
// type IntList = List[Int]
export const aliasExample = mkTypeDef(testModSym, "IntList").editBranch1(
  type => type.setTApp().editBranches(
    ctor => ctor.setTRef(ctor.tree.scope.lookupType("List")),
    arg  => arg.setTRef(intSym)
  )
)

// term fib n:Nat → Nat = case n of
//   0 → 0
//   1 → 1
//   n → fib (n-1) + fib (n-2)

// fun fib (n:Nat) :Nat = case n
//   0 = 0
//   1 = 1
//   n = fib(n-1) + fib(n-2)
export const fibExample = mkFunDef(testModSym, "fib").editBranch1(
  body => body.setAbs("n").editBranches(
    undefined,
    type => type.setTRef(natSym),
    expr => expr.setAsc().editBranches(
      type => type.setTRef(natSym),
      expr => expr.setMatchN(3).editBranches(
        scrut => scrut.setRef(scrut.tree.scope.lookupTerm("n")),
        case0 => case0.setPLit(constInt(0)).editBranch1(
          body => body.setLit(constInt(0))
        ),
        case1 => case1.setPLit(constInt(1)).editBranch1(
          body => body.setLit(constInt(1))
        ),
        caseN => caseN.setPBind("n").editBranch1(
          body => body.setApp().editBranches(
            fun => fun.setInApp().editBranches(
              arg => arg.setApp().editBranches(
                fun => fun.setRef(fun.tree.scope.lookupFunc("fib")),
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
              fun => fun.setRef(fun.tree.scope.lookupFunc("fib")),
              arg => arg.setApp().editBranches(
                fun => fun.setInApp().editBranches(
                  arg => arg.setRef(arg.tree.scope.lookupTerm("n")),
                  fun => fun.setRef(subSym)
                ),
                arg => arg.setLit(constInt(2))
              )
            )
          )
        )
      )
    )
  )
)

// fun reverse [A] (as :List[A]) :List[A] =
//   fun revacc (as :List[A], acc :List[A]) :List[A] = case as
//     Nil        = acc
//     Cons(h, t) = revacc(t, h :: acc)
//   revacc(as, Nil)
export const revExample = mkFunDef(testModSym, "reverse").editBranch1(
  body => body.setAll("A").editBranch1(
    body => body.setAbs("as").editBranches(
      undefined,
      type => mkListA(type),
      body => body.setAsc().editBranches(
        type => mkListA(type),
        body => body.setLetFun("revacc").editBranches(
          undefined,
          body => body.setAbs("as").editBranches(
            undefined,
            type => mkListA(type),
            body => body.setAbs("acc").editBranches(
              undefined,
              type => mkListA(type),
              body => body.setAsc().editBranches(
                type => mkListA(type),
                body => body.setMatchN(2).editBranches(
                  scrut => scrut.setRef(scrut.tree.scope.lookupTerm("as")),
                  case0 => case0.setPDtor(case0.tree.scope.lookupFunc("Nil")).editBranch1(
                    body => body.setRef(body.tree.scope.lookupTerm("acc"))
                  ),
                  case1 => case1.setPDtor(case1.tree.scope.lookupFunc("Cons")).editBranch1(
                    body => body.setPBind("h").editBranch1(
                      body => body.setPBind("t").editBranch1(
                        body => body.setApp().editBranches(
                          fun => fun.setApp().editBranches(
                            fun => fun.setRef(fun.tree.scope.lookupFunc("revacc")),
                            arg => arg.setRef(arg.tree.scope.lookupTerm("t"))
                          ),
                          arg => arg.setApp().editBranches(
                            fun => fun.setInApp().editBranches(
                              arg => arg.setRef(arg.tree.scope.lookupTerm("h")),
                              fun => fun.setRef(fun.tree.scope.lookupFunc("::"))
                            ),
                            arg => arg.setRef(arg.tree.scope.lookupTerm("acc"))
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          ),
          expr => expr.setApp().editBranches(
            fun => fun.setApp().editBranches(
              fun => fun.setRef(fun.tree.scope.lookupFunc("revacc")),
              arg => arg.setRef(arg.tree.scope.lookupTerm("as"))
            ),
            arg => arg.setRef(arg.tree.scope.lookupFunc("Nil"))
          )
        )
      )
    )
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
