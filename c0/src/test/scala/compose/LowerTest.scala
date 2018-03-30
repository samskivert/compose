//
// Compose Mark 0 compiler
// https://github.com/samskivert/compose/c0/LICENSE

package compose

import org.junit.Assert._
import org.junit._

class LowerTest {
  import Lower._
  import TestCode._

  def lowerCode (code :String) = lower(parseAndType("code", code))

  @Test def testCondFib () :Unit = lowerCode(CondFib) foreach print
  @Test def testMatchFib () :Unit = lowerCode(MatchFib) foreach print

  @Test def testBindBlock () :Unit = lowerCode("""
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
  """) foreach print

  @Test def testIgnore () :Unit = lowerCode("""
    fun less (a :I32, b :I32) :Bool = false
    fun add (a :I32, b :I32) :I32 = 0
    var ii = 0
    while (ii < 10) ii = ii + (if (ii < 5) 1 else 2)
  """) foreach print

  @Test def testParenBlock () :Unit = lowerCode(ParenBlock) foreach print

  @Test def testApplyImpl () :Unit = lowerCode(ApplyImpl) foreach print

  @Test def testEq () :Unit = {
    val eqtrees = parseAndType(Seq("prelude.cz", "eq.cz"))
    lower(eqtrees) foreach print
  }

  @Test def testSimpleMatch () :Unit = lowerCode(SimpleMatch) foreach print
  @Test def testTupleMatch () :Unit = lowerCode(TupleMatch) foreach print
  @Test def testDestructMatch () :Unit = lowerCode(DestructMatch) foreach print
  @Test def testParamDestructMatch () :Unit = lowerCode(ParamDestructMatch) foreach print
  @Test def testGuardedMatch () :Unit = lowerCode(GuardedMatch) foreach print

  @Test def testForeignOps () :Unit = lowerCode(ForeignOps) foreach print
}
