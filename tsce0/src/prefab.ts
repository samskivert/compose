import * as C from "./constants"
import * as T from "./trees"
import * as TP from "./types"
import * as M from "./module"

// ----------
// Test trees
// ----------

export const testMod = new M.Module("test")

const natTree = testMod.mkTypeDef("Nat", 1).
  setBranch("body", new T.PrimTree(new TP.Scalar(C.Tag.Int, 32)))
testMod.mkTypeDef("Int", 2).setBranch("body", new T.PrimTree(new TP.Scalar(C.Tag.Int, 32)))
testMod.mkTypeDef("String", 3).setBranch("body", new T.PrimTree(new TP.Scalar(C.Tag.String, 1)))

const natType = natTree.sym.type
const natNatToNat = new TP.Arrow(natType, new TP.Arrow(natType, natType))
testMod.mkFunDef("+", 4).setBranch("body", new T.PrimTree(natNatToNat))
testMod.mkFunDef("-", 5).setBranch("body", new T.PrimTree(natNatToNat))

function mkSymTree (kind :string, id :number, name :string, branchId :string, branch :any) :any {
  const tree = {kind, sym: {id, name}}
  tree[branchId] = branch
  return tree
}

// type Box ∀A contents:A
export const boxExample = testMod.inflateDef(
  mkSymTree(
    "typedef", 10, "Box", "body", mkSymTree(
      "tabs", 1, "A", "body", mkSymTree(
        "ctor", 20, "Box", "prod", {
          kind: "prod",
          branches: [mkSymTree("field", 21, "contents", "type", {kind: "tref", symId: "l1"})]
        }))))

// type Person name:String age:Nat
export const recordExample = testMod.inflateDef(
  mkSymTree("typedef", 11, "Person", "body", {
    kind: "prod",
    branches: [mkSymTree("field", 22, "name", "type", {kind: "tref", symId: "m3"}),
               mkSymTree("field", 23, "age", "type", {kind: "tref", symId: "m1"})]
  }))

// type List ∀A =
//   * Nil
//   * Cons head:A tail:List A
export const listExample = testMod.inflateDef(
  mkSymTree(
    "typedef", 12, "List", "body", mkSymTree(
      "tabs", 1, "T", "body", {
        kind: "sum",
        cases: [mkSymTree("ctor", 24, "Nil", "prod", {kind: "prod", branches: []}),
                mkSymTree("ctor", 25, "Cons", "prod", {
          kind: "prod",
          branches: [mkSymTree("field", 26, "head", "type", {kind: "tref", symId: "l1"}),
                     mkSymTree("field", 27, "tail", "type", {
            kind: "tapp", ctor: {kind: "tref", symId: "m12"}, arg: {kind: "tref", symId: "l1"}})]
        })]
      })))

// function mkListA (tb :TreeEditor) :Tree {
//   return tb.setTApp().editBranches({
//     "ctor": ctor => ctor.setTRef(ctor.tree.scope.lookupType("List")),
//     "arg": arg  => arg.setTRef(tb.tree.scope.lookupType("A"))
//   })
// }

// fun :: ∀A head:A tail:List A → List A = Cons(head, tail)
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

// fun reverse ∀A as:List A → List A =
//   fun revacc as:List A acc:List A → List A = case as
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
