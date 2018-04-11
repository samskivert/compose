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

  @Test def testConstants () :Unit = {
    val trees = extract(program.parse("0"))
    implicit val ctx = testContext("constants")
    trees foreach { tree => debugTree(out)(tree.typed()) }
  }

  @Test def testBinOp () :Unit = {
    val trees = extract(program.parse("""
      fun add (a :I32, b :I32) :I32 = a
      let foo = 0 + 2
    """))
    implicit val ctx = testContext("binop")
    trees foreach index
    trees foreach { tree => debugTree(out)(tree.typed()) }
  }

  @Test def testId () :Unit = {
    val trees = extract(program.parse("""
      fun id[A] (a :A) :A = a
    """))
    implicit val ctx = testContext("id")
    trees foreach index
    trees foreach { tree => debugTree(out)(tree.typed()) }
  }

  @Test def testInnerLoop () :Unit = {
    val trees = extract(program.parse("""
      fun foo () :I32 = {
        fun bar (a :Bool) :I32 = if (a) bar(false) else 0
        bar(true)
      }
      let baz = {
        fun bar (a :Bool) :I32 = if (a) bar(false) else 0
        bar(true)
      }
    """))
    implicit val ctx = testContext("innerLoop")
    trees foreach index
    trees foreach { tree => debugTree(out)(tree.typed()) }
  }

  @Test def testFib () :Unit = {
    val trees = extract(program.parse(MatchFib))
    implicit val ctx = testContext("fib")
    trees foreach index
    trees foreach { tree => debugTree(out)(tree.typed()) }
  }

  @Test def testFunParamInfer () :Unit = {
    val trees = extract(program.parse("""
      data List[A] = Nil | Cons(head :A, tail :List[A])
      fun id[A] (a :A) :A = a
      fun fst[A] (a :A, b :A) :A = a
      let foo :List[I32] = fst(id(id(Nil)), id(Cons(32, Nil)))
    """))
    implicit val ctx = testContext("funParamInfer")
    trees foreach index
    trees foreach { tree => debugTree(out)(tree.typed()) }
  }

  @Test def testParamFunDef () :Unit = {
    val trees = extract(program.parse("""
      fun less (i0 :I32, i1 :I32) :Bool = false
      fun sub (i0 :I32, i1 :I32) :I32 = i0
      fun id[A] (a :A) :A = a
      fun foo[A] (a1 :A, ii :I32) :I32 = if (ii < 1) ii else foo(id(id(a1)), ii-1)
    """))
    implicit val ctx = testContext("paramFunDef")
    trees foreach index
    trees foreach { tree => debugTree(out)(tree.typed()) }
  }

  @Test def testLambdaParamInfer () :Unit = {
    val trees = extract(program.parse("""
      fun compose[A,B,C] (f :A => B, g :B => C) :A => C = a => g(f(a))
      compose(a :I32 => 1, b :I32 => 2)
    """))
    implicit val ctx = testContext("lambdaParamInfer")
    trees foreach index
    trees foreach { tree => debugTree(out)(tree.typed()) }
  }

  @Test def testFaceImpl () :Unit = {
    val trees = extract(program.parse("""
      interface Foo[A] {
        fun pick (a1 :A, a2 :A) :A
      }
      fun first[T] (t1 :T, t2 :T) :T = t1
      impl i32Foo = Foo[I32](pick=first)
    """))
    implicit val ctx = testContext("faceImpl")
    trees foreach index
    trees foreach { tree => debugTree(out)(tree.typed()) }
  }

  @Test def testLiterals () :Unit = {
    val trees = extract(parseCode("tests/literals.cz"))
    implicit val ctx = testContext("literals")
    trees foreach index
    trees foreach { tree => debugTree(out)(tree.typed()) }
  }

  @Test def testData () :Unit = {
    val trees = extract(program.parse(OrdData))
    implicit val ctx = testContext("data")
    trees foreach index
    println(ctx.scope.lookup(typeName("Ordering")).info)
    trees foreach { tree => debugTree(out)(tree.typed()) }
  }

  @Test def testRecursiveData () :Unit = {
    val trees = extract(program.parse(ListData))
    implicit val ctx = testContext("data")
    trees foreach index
    println(ctx.scope.lookup(typeName("List")).info)
    println(ctx.scope.lookup(typeName("Nil")).info)
    println(ctx.scope.lookup(typeName("Cons")).info)
    trees foreach { tree => debugTree(out)(tree.typed()) }
  }

  @Test def testPrelude () :Unit = {
    parseAndType(Seq("std/prelude.cz")) foreach debugTree(out)
  }

  @Test def testLogic () :Unit = {
    val files = Seq("std/prelude.cz", "std/logic.cz")
    typeFiles(files) foreach debugTree(out)
  }

  @Test def testStdlib () :Unit = {
    val files = Seq("std/prelude.cz", "std/logic.cz", "std/rings.cz", "std/eq.cz")
    typeFiles(files) foreach debugTree(out)
  }

  @Test def testApply () :Unit = {
    val trees = extract(program.parse(ListApply))
    implicit val ctx = testContext("data")
    trees foreach index
    trees foreach { tree => debugTree(out)(tree.typed()) }
  }

  @Test def testApplyImpl () :Unit = {
    val trees = extract(program.parse(ApplyImpl))
    implicit val ctx = testContext("implApply")
    trees foreach index
    trees foreach { tree => debugTree(out)(tree.typed()) }
  }

  @Test def testParenBlock () :Unit = {
    val trees = parseAndType("code", ParenBlock)
    trees foreach { tree => debugTree(out)(tree) }
  }

  @Test def testBlockScope () :Unit = {
    val trees = extract(program.parse("""
      let foo = {
        let bar = 3
        bar
      }
      let baz = bar
    """))
    implicit val ctx = testContext("blockScope")
    trees foreach index
    trees foreach { tree => debugTree(out)(tree.typed()) }
  }

  @Test def testSimpleMatch () :Unit = {
    val trees = parseAndType("code", SimpleMatch)
    trees foreach { tree => debugTree(out)(tree) }
  }
  @Test def testTupleMatch () :Unit = {
    val trees = parseAndType("code", TupleMatch)
    trees foreach { tree => debugTree(out)(tree) }
  }
  @Test def testDestructMatch () :Unit = {
    val trees = parseAndType("code", DestructMatch)
    trees foreach { tree => debugTree(out)(tree) }
  }
  @Test def testParamDestructMatch () :Unit = {
    val trees = parseAndType("code", ParamDestructMatch)
    trees foreach { tree => debugTree(out)(tree) }
  }
  @Test def testGuardedMatch () :Unit = {
    val trees = parseAndType("code", GuardedMatch)
    trees foreach { tree => debugTree(out)(tree) }
  }
}
