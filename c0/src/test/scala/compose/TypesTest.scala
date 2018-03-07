//
// Compose Mark 0 compiler
// https://github.com/samskivert/compose/c0/LICENSE

package compose

import org.junit.Assert._
import org.junit._

class TypesTest {
  import Names._
  import Symbols._
  import Trees._
  import Types._

  def testSym = newModuleSymbol(termName("test"))
  def mkVar (name :String) = {
    val tname = typeName(name)
    Var(testSym.defineType(tname, Param(tname), Seq()), 1)
  }

  @Test def testUnify () :Unit = {
    val varA = mkVar("A")
    val varB = mkVar("B")
    println(unify(Seq((varA, varB))))

    val fakeList = Union(NoType, Seq(varA), Seq())
    println(unify(Seq(Apply(fakeList, Seq(varA)) -> Apply(fakeList, Seq(Prim.I32)))))

    val aToB = Arrow(Seq(), Seq(varA), varB)
    val aToInt = Arrow(Seq(), Seq(varA), Prim.I32)
    println(unify(Seq(aToB -> aToInt)))

    val cToInt = Arrow(Seq(), Seq(mkVar("C")), Prim.I32)
    println(unify(Seq(aToB -> cToInt)))

    val aToBool = Arrow(Seq(), Seq(varA), Prim.Bool)
    println(unify(Seq(aToB -> aToInt, aToB -> aToBool)))

    println(unify(Seq(Apply(fakeList, Seq(varA)) -> aToBool)))
  }
}
