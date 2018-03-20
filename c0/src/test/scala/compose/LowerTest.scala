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
    fun add (a :I32, b :I32) :I32 = 0
    fun sub (a :I32, b :I32) :I32 = 0
    fun fib (n :I32) :I32 = cond
      eq(n, 0) = 0
      eq(n, 1) = 1
      else     = fib(n - 2) + fib(n - 1)
    """
    val trees = parseAndType("fib", fib)
    lower(trees) foreach print
  }

  @Test def testBindBlock () :Unit = {
    val code = """
    fun add (a :I32, b :I32) :I32 = 0
    fun less (a :I32, b :I32) :Bool = false
    var a = if true 1 else 2
    let b = if {
      let a = 1
      a + a < 5
    } 1 else 2
    let c = {
      let a = 1
      a + a
    }
    """
    val trees = parseAndType("code", code)
    lower(trees) foreach print
  }

  @Test def testIgnore () :Unit = {
    val code = """
    fun less (a :I32, b :I32) :Bool = false
    fun add (a :I32, b :I32) :I32 = 0
    var ii = 0
    while (ii < 10) ii = ii + (if (ii < 5) 1 else 2)
    """
    val trees = parseAndType("code", code)
    lower(trees) foreach print
  }

  @Test def testParenBlock () :Unit = {
    val code = """
    data Foo(i :I32)
    let a = ({
      let a = Foo(1), b = Foo(2)
      if false a else b
    }).i"""
    val trees = parseAndType("code", code)
    lower(trees) foreach print
  }
}
