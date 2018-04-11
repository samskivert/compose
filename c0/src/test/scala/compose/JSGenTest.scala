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

  def genTrees (trees :Seq[Trees.Tree]) = {
    val errs = trees.flatMap(Trees.errors)
    if (errs.isEmpty) {
      gen(lower(trees), sysPrint)
      sysOut.println()
      sysOut.flush()
    }
    else {
      errs foreach { case (tree, err) => println(s"Error: $err\n  $tree") }
      Seq()
    }
  }

  def genCode (code :String) :Unit = genTrees(parseAndType("code", code))

  @Test def testCondFib () :Unit = genCode(CondFib)
  @Test def testMatchFib () :Unit = genCode(MatchFib)

  @Test def testIgnore () :Unit = genCode("""
    fun less (a :I32, b :I32) :Bool = false
    fun add (a :I32, b :I32) :I32 = 0
    var ii = 0
    while (ii < 10) {
      add(1, 2)
      ii = ii + (if (ii < 5) 1 else 2)
    }
  """)

  @Test def testApplyImpl () :Unit = genCode(ApplyImpl)

  @Test def testMatches () :Unit =
    Seq(SimpleMatch, TupleMatch, DestructMatch, ParamDestructMatch, GuardedMatch) foreach genCode

  @Test def testStdlib () :Unit = {
    val files = Seq("std/prelude.cz", "std/logic.cz", "std/rings.cz", "std/eq.cz")
    genTrees(typeFiles(files))
  }
}
