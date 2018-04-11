//
// Compose Mark 0 compiler
// https://github.com/samskivert/compose/c0/LICENSE

package compose

import org.junit.Assert._
import org.junit._

class TypingTest {
  import Indexer._
  import Names._
  import Parser._
  import TestCode._
  import Trees._

  val PrintTypedTrees = false

  def check (trees :Seq[Tree]) :Seq[Tree] = {
    debugPrint(trees)
    checkTrees(trees)
  }

  def debugPrint (trees :Seq[Tree]) :Seq[Tree] = {
    if (PrintTypedTrees) trees foreach debugTree(out)
    trees
  }

  @Test def testConstants () :Unit = check(typeCode("consts", "0"))

  @Test def testBinOp () :Unit = check(typeCode("binOp", """
    fun add (a :I32, b :I32) :I32 = a
    let foo = 0 + 2
  """))

  @Test def testId () :Unit = check(typeCode("id", """
    fun id[A] (a :A) :A = a
  """))

  @Test def testInnerLoop () :Unit = check(typeCode("innerLoop", """
    fun foo () :I32 = {
      fun bar (a :Bool) :I32 = if (a) bar(false) else 0
      bar(true)
    }
    let baz = {
      fun bar (a :Bool) :I32 = if (a) bar(false) else 0
      bar(true)
    }
  """))

  @Test def testFib () :Unit = check(typeCode("matchFib", MatchFib))

  @Test def testFunParamInfer () :Unit = check(typeCode("funParamInfer", """
    data List[A] = Nil | Cons(head :A, tail :List[A])
    fun id[A] (a :A) :A = a
    fun fst[A] (a :A, b :A) :A = a
    let foo :List[I32] = fst(id(id(Nil)), id(Cons(32, Nil)))
  """))

  @Test def testParamFunDef () :Unit = check(typeCode("paramFunDef", """
    fun less (i0 :I32, i1 :I32) :Bool = false
    fun sub (i0 :I32, i1 :I32) :I32 = i0
    fun id[A] (a :A) :A = a
    fun foo[A] (a1 :A, ii :I32) :I32 = if (ii < 1) ii else foo(id(id(a1)), ii-1)
  """))

  @Test def testLambdaParamInfer () :Unit = check(typeCode("lambdaParamInfer", """
    fun compose[A,B,C] (f :A => B, g :B => C) :A => C = a => g(f(a))
    compose(a :I32 => 1, b :I32 => 2)
  """))

  @Test def testFaceImpl () :Unit = check(typeCode("faceImpl", """
    interface Foo[A] {
      fun pick (a1 :A, a2 :A) :A
    }
    fun first[T] (t1 :T, t2 :T) :T = t1
    impl i32Foo = Foo[I32](pick=first)
  """))

  @Test def testLiterals () :Unit = check(typeFile("tests/literals.cz"))

  @Test def testIndexData () :Unit = {
    val trees = extract(program.parse(OrdData))
    implicit val ctx = testContext("data")
    trees foreach index
    val ordSym = ctx.scope.lookup(typeName("Ordering"))
    assertTrue(ordSym.exists)
    trees foreach { _.typed() }
    checkTrees(trees)
  }

  @Test def testIndexRecursiveData () :Unit = {
    val trees = extract(program.parse(ListData))
    implicit val ctx = testContext("data")
    trees foreach index
    assertTrue(ctx.scope.lookup(typeName("List")).exists)
    assertTrue(ctx.scope.lookup(typeName("Nil")).exists)
    assertTrue(ctx.scope.lookup(typeName("Cons")).exists)
    trees foreach { _.typed() }
    checkTrees(trees)
  }

  @Test def testPrelude () :Unit = check(typeFile("std/prelude.cz"))
  @Test def testLogic () :Unit = check(typeFiles(Seq("std/prelude.cz", "std/logic.cz")))
  @Test def testSemigroup () :Unit = check(typeFiles(Seq("std/prelude.cz", "std/semigroup.cz")))
  @Test def testStdlib () :Unit = check(typeFiles(StdlibFiles))
  @Test def testApply () :Unit = check(typeCode("listApply", ListApply))
  @Test def testApplyImpl () :Unit = check(typeCode("applyImpl", ApplyImpl))
  @Test def testParenBlock () :Unit = check(typeCode("code", ParenBlock))

  @Test def testBlockScope () :Unit = {
    val trees = typeCode("blockScope", """
      let foo = {
        let bar = 3
        bar
      }
      let baz = bar
    """)
    assertFalse(trees.flatMap(Trees.errors).isEmpty)
  }

  @Test def testSimpleMatch () :Unit = check(typeCode("code", SimpleMatch))
  @Test def testTupleMatch () :Unit = check(typeCode("code", TupleMatch))
  @Test def testDestructMatch () :Unit = check(typeCode("code", DestructMatch))
  @Test def testParamDestructMatch () :Unit = check(typeCode("code", ParamDestructMatch))
  @Test def testGuardedMatch () :Unit = check(typeCode("code", GuardedMatch))
}
