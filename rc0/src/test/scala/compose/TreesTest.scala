package compose

import org.junit.Test
import org.junit.Assert._
import java.io.PrintWriter

class TreesTest {
  import Constants._
  import Names._
  import Symbols._
  import Trees._
  import Types._

  @Test def testResolve = {
    val p = Parsers.parser
    val tree = p.parseDef("def fst :: ∀A => ∀B => x:A -> y:B -> A = x")
    val rtree = tree.resolve(Scopes.emptyScope)
    // println(rtree)
    val TermDefTree(_, AllTree(asym, AllTree(bsym, AbsTree(xsym, TRefTree(xasym), AbsTree(
      ysym, TRefTree(ybsym), AscTree(TRefTree(aasym), RefTree(rxsym))))))) = rtree
    assertTrue(asym eq xasym)
    assertTrue(bsym eq ybsym)
    assertTrue(asym eq aasym)
    assertTrue(xsym eq rxsym)
  }

  @Test def testTyping = {
    val p = Parsers.parser
    val tree = p.parseDef("def fst :: ∀A => ∀B => x:A -> y:B -> A = x")
    val rtree = tree.resolve(Scopes.emptyScope)
    val itype = rtree.assignType(false)
    // rtree.debugPrint(new PrintWriter(System.out, true), "")
    // compare type sig via string repr to avoid symbol annoyances
    assertEquals("∀A ⇒ ∀B ⇒ A → B → A", itype.toString)

    // val fooTrue = new TermDefTree(termSym(termName("foo")), LitTree(True))
    // println(fooTrue)
    // println(fooTrue.assignType())
  }

  @Test def testBuiltins = {
    val p = Parsers.parser
    val tree = p.parseDef("def plus :: x:Int -> y:Int -> Int = x")
    val rtree = tree.resolve(Builtins.scope)
    val itype = rtree.assignType(false)
    // rtree.debugPrint(new PrintWriter(System.out, true), "")
    // compare type sig via string repr to avoid symbol annoyances
    assertEquals("Int → Int → Int", itype.toString)
  }

  def errors (tree :Tree) = tree.fold(Nil :List[Type])((errs, tree) => {
    println(s"$tree :: ${tree.treeType}")
    if (tree.treeType.isError) tree.treeType :: errs else errs
  })

  @Test def testTypeError = {
    val p = Parsers.parser
    val tree = p.parseDef("def plus :: x:String -> y:Int -> Int = x")
    val rtree = tree.resolve(Builtins.scope)
    val itype = rtree.assignType(true)
    rtree.debugPrint(new PrintWriter(System.out, true), "")
    // compare type sig via string repr to avoid symbol annoyances
    assertEquals("Int → Int → Int", itype.toString)
  }
}
