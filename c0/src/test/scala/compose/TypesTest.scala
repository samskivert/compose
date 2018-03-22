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
    Var(testSym.defineType(tname, Param(tname)), 1)
  }

  def assertUnifies (constraints :Seq[(Type, Type)],
                     expect :Seq[(Var, Type)]) = unify(constraints) match {
    case Left(err) => fail(err.toString)
    case Right(subs) => assertEquals(expect.toMap, subs)
  }

  def assertUnifyFails (constraints :Seq[(Type, Type)]) = unify(constraints) match {
    case Left(err) => assertTrue(true)
    case Right(subs) => fail(s"$constraints should not have unified to $subs")
  }

  @Test def testUnify () :Unit = {
    val varA = mkVar("A")
    val varB = mkVar("B")
    assertUnifies(Seq(varA -> varB), Seq(varA -> varB))

    val fakeList = Union(NoType, Seq(varA), Seq())
    assertUnifies(Seq(Apply(fakeList, Seq(varA)) -> Apply(fakeList, Seq(Prim.I32))),
                  Seq(varA -> Prim.I32))

    val aToB = Arrow(NoTerm, Seq(), Seq(), Seq(varA), varB)
    val aToInt = Arrow(NoTerm, Seq(), Seq(), Seq(varA), Prim.I32)
    assertUnifies(Seq(aToB -> aToInt), Seq(varB -> Prim.I32))

    val varC = mkVar("C")
    val cToInt = Arrow(NoTerm, Seq(), Seq(), Seq(varC), Prim.I32)
    assertUnifies(Seq(aToB -> cToInt), Seq(varA -> varC, varB -> Prim.I32))

    val aToBool = Arrow(NoTerm, Seq(), Seq(), Seq(varA), Prim.Bool)
    assertUnifyFails(Seq(aToB -> aToInt, aToB -> aToBool))

    assertUnifyFails(Seq(Apply(fakeList, Seq(varA)) -> aToBool))

    assertUnifies(Seq(Array(varA) -> Array(Prim.I32)), Seq(varA -> Prim.I32))
  }
}
