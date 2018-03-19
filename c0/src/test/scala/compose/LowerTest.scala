//
// Compose Mark 0 compiler
// https://github.com/samskivert/compose/c0/LICENSE

package compose

import org.junit.Assert._
import org.junit._

class LowerTest {
  import Lower._
  import TestCode._

  @Test def testFib () :Unit = {
    val fib = """
    fun eq (a :I32, b :I32) :Bool = true
    fun add (a :I32, b :I32) :I32 = a
    fun sub (a :I32, b :I32) :I32 = a
    fun fib (n :I32) :I32 = cond
      eq(n, 0) = 0
      eq(n, 1) = 1
      else     = fib(n - 2) + fib(n - 1)
    """
    val trees = parseAndType("fib", fib)
    trees foreach { tree => lower(tree).stmts foreach print }
  }

  @Test def testParenBlock () :Unit = {
    val code = """
    data Foo(i :I32)
    ({
      let a = Foo(1), b = Foo(2)
      if false a else b
    }).i"""
    val trees = parseAndType("code", code)
    trees foreach { tree => lower(tree).stmts foreach print }
  }
}
