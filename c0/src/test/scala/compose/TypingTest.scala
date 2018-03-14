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
    val trees = extract(program.parse("0 + 2"))
    implicit val ctx = testContext("binop")
    trees foreach { tree => debugTree(out)(tree.typed()) }
  }

  @Test def testFib () :Unit = {
    val fib = """
    fun add (a :I32, b :I32) :I32 = a
    fun sub (a :I32, b :I32) :I32 = a
    fun fib (n :I32) :I32 = match n
      case 0 = 0
      case 1 = 1
      case n = add(fib(sub(n, 2)), fib(sub(n, 1)))
    """
    val trees = extract(program.parse(fib))
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
    val data = """
    data Ordering = LT | EQ | GT
    """
    val trees = extract(program.parse(data))
    implicit val ctx = testContext("data")
    trees foreach index
    println(ctx.scope.lookup(typeName("Ordering")).info)
    trees foreach { tree => debugTree(out)(tree.typed()) }
  }

  @Test def testRecursiveData () :Unit = {
    val data = """
    data List[A] = Nil | Cons(head :A, tail :List[A])
    """
    val trees = extract(program.parse(data))
    implicit val ctx = testContext("data")
    trees foreach index
    println(ctx.scope.lookup(typeName("List")).info)
    println(ctx.scope.lookup(typeName("Nil")).info)
    println(ctx.scope.lookup(typeName("Cons")).info)
    trees foreach { tree => debugTree(out)(tree.typed()) }
  }

  @Test def testPrelude () :Unit = {
    val trees = extract(parseCode("prelude.cz"))
    implicit val ctx = testContext("prelude.cz")
    trees foreach index
    trees foreach { tree => debugTree(out)(tree.typed()) }
  }

  @Test def testInterface () :Unit = {
    val ptrees = extract(parseCode("prelude.cz"))
    val eqtrees = extract(parseCode("eq.cz"))
    implicit val ctx = testContext("eq.cz")
    (ptrees ++ eqtrees) foreach index
    ptrees foreach { _.typed() }
    eqtrees foreach { tree => debugTree(out)(tree.typed()) }
  }

  @Test def testApply () :Unit = {
    val data = """
    data List[A] = Nil | Cons(head :A, tail :List[A])
    fun id[A] (a :A) :A = a
    let a :List[I32] = Nil
    let b = id(5)
    let c :List[List[I32]] = Nil
    let d :List[I32] = Cons(5, Nil)
    let e = Cons(5, Nil), f = e.head, g = e.tail
    """
    val trees = extract(program.parse(data))
    implicit val ctx = testContext("data")
    trees foreach index
    trees foreach { tree => debugTree(out)(tree.typed()) }
  }

  @Test def testApplyImpl () :Unit = {
    val data = """
    interface Num[A] {
      fun add (a0 :A, a1 :A) :A
    }
    fun i32Add (a :I32, b :I32) :I32 = foreign
    impl i32Num = Num[I32](add=i32Add)
    fun i8Add (a :I8, b :I8) :I8 = foreign
    impl i8Num = Num[I8](add=i8Add)
    let a = 1, b = 2
    let c = add(a, b)
    """
    val trees = extract(program.parse(data))
    implicit val ctx = testContext("implApply")
    trees foreach index
    trees foreach { tree => debugTree(out)(tree.typed()) }
  }
}