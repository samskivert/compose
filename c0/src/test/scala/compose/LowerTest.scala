//
// Compose Mark 0 compiler
// https://github.com/samskivert/compose/c0/LICENSE

package compose

import org.junit.Assert._
import org.junit._

class LowerTest {
  import Lower._
  import TestCode._

  def lowerTrees (trees :Seq[Trees.Tree]) = {
    val errs = trees.flatMap(Trees.errors)
    if (errs.isEmpty) lower(trees)
    else {
      errs foreach { case (path, err) =>
        println(s"Error: $err @ ${Trees.show(path.head)}")
        path foreach { t => println(s"- ${Trees.show(t)} : ${t.productPrefix}") }
      }
      Seq()
    }
  }

  def lowerCode (code :String) = lowerTrees(parseAndType("code", code))

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

  @Test def testForeigns () :Unit = lowerCode("""
    let Zero :I32 = foreign("0")
    fun length[A] (as :Array[A]) :I32 = foreign("as.length")
    fun clear[A] (as :Array[A]) :Unit = foreignBody("as.length = 0")
  """) foreach print

  @Test def testParenBlock () :Unit = lowerCode(ParenBlock) foreach print

  @Test def testApplyImpl () :Unit = lowerCode(ApplyImpl) foreach print

  @Test def testStdlib () :Unit = {
    val files = Seq("std/prelude.cz", "std/logic.cz", "std/rings.cz", "std/eq.cz")
    lowerTrees(typeFiles(files)) foreach print
  }

  @Test def testSimpleMatch () :Unit = lowerCode(SimpleMatch) foreach print
  @Test def testTupleMatch () :Unit = lowerCode(TupleMatch) foreach print
  @Test def testDestructMatch () :Unit = lowerCode(DestructMatch) foreach print
  @Test def testParamDestructMatch () :Unit = lowerCode(ParamDestructMatch) foreach print
  @Test def testGuardedMatch () :Unit = lowerCode(GuardedMatch) foreach print

  @Test def testForeignOps () :Unit = lowerCode(ForeignOps) foreach print
}
