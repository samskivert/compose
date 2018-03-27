//
// Compose Mark 0 compiler
// https://github.com/samskivert/compose/c0/LICENSE

package compose

import org.junit.Assert._
import org.junit._

class JSGenTest {
  import JSGen._
  import Lower._
  import Printing._
  import TestCode._

  @Test def testFib () :Unit = {
    val trees = parseAndType("fib", CondFib)
    gen(lower(trees), sysPrint)
    sysOut.println()
    sysOut.flush()
  }

  @Test def testIgnore () :Unit = {
    val code = """
    fun less (a :I32, b :I32) :Bool = false
    fun add (a :I32, b :I32) :I32 = 0
    var ii = 0
    while (ii < 10) {
      add(1, 2)
      ii = ii + (if (ii < 5) 1 else 2)
    }
    """
    val trees = parseAndType("code", code)
    gen(lower(trees), sysPrint)
    sysOut.println()
    sysOut.flush()
  }

  @Test def testApplyImpl () :Unit = {
    val trees = parseAndType("code", ApplyImpl)
    gen(lower(trees), sysPrint)
    sysOut.println()
    sysOut.flush()
  }
}
