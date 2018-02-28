//
// Compose Mark 0 compiler
// https://github.com/samskivert/compose/c0/LICENSE

package compose

import java.io.PrintWriter
import org.junit.Assert._
import org.junit._
import fastparse.core.Parsed

class TyperTest {
  import Contexts._
  import Indexer._
  import Parser._
  import Trees._
  import Typer._
  import TestCode._
  import Names._

  def extract[T] (result :Parsed[T, _, _]) :T = result.fold(
    (p, pos, extra) => {
      extra.traced.trace.split(" / " ).foreach(f => println(s"- $f"))
      fail(result.toString)
      ??? // unreachable
    },
    (res, pos) => res
  )

  def testContext (name :String) = moduleContext(termName(s"${name}.cz"))

  val out = new PrintWriter(System.out)

  @Test def testConstants () :Unit = {
    val trees = extract(program.parse("0"))
    implicit val ctx = testContext("constants")
    trees foreach { tree =>
      val typedTree = typed(tree)
      debugTree(out)(typedTree)
    }
  }

  @Test def testBinOp () :Unit = {
    val trees = extract(program.parse("0 + 2"))
    implicit val ctx = testContext("binop")
    trees foreach { tree =>
      val typedTree = typed(tree)
      debugTree(out)(typedTree)
    }
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
    trees foreach { tree =>
      val typedTree = typed(tree)
      debugTree(out)(typedTree)
    }
  }

  @Test def testLiterals () :Unit = {
    val trees = extract(parseCode("literals.cz"))
    implicit val ctx = testContext("literals")
    trees foreach index
    trees foreach { tree =>
      val typedTree = typed(tree)
      debugTree(out)(typedTree)
    }
  }

  @Test def testData () :Unit = {
    val data = """
    data Ordering = LT | EQ | GT
    """
    val trees = extract(program.parse(data))
    implicit val ctx = testContext("data")
    trees foreach index
    println(ctx.scope.lookup(typeName("Ordering")).info)
    trees foreach { tree =>
      val typedTree = typed(tree)
      debugTree(out)(typedTree)
    }
  }

  @Test def testInterface () :Unit = {
    val trees = extract(parseCode("prelude.cz")) ++
      extract(parseCode("eq.cz"))
    implicit val ctx = testContext("eq.cz")
    trees foreach index
    trees foreach { tree =>
      val typedTree = typed(tree)
      debugTree(out)(typedTree)
    }
  }
}
