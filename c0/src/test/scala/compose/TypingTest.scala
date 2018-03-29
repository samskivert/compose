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

  @Test def testFib () :Unit = {
    val trees = extract(program.parse(MatchFib))
    implicit val ctx = testContext("fib")
    trees foreach index
    trees foreach { tree => debugTree(out)(tree.typed()) }
  }

  @Test def testLiterals () :Unit = {
    val trees = extract(parseCode("literals.cz"))
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
    parseAndType(Seq("prelude.cz")) foreach debugTree(out)
  }

  @Test def testInterface () :Unit = {
    val eqtrees = parseAndType(Seq("prelude.cz", "eq.cz"))
    eqtrees foreach debugTree(out)
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
