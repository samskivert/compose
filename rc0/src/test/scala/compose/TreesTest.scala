package compose

import org.junit.Test
import org.junit.Assert._
import java.io.PrintWriter

class TreesTest {
  import Constants._
  import Modules._
  import Names._
  import Symbols._
  import Trees._
  import Types._

  def treeErrors (tree :Tree) :List[Error] = tree.fold(List[Error]())(
    (errs, tree) => tree.treeType match {
      case err :Error => err :: errs
      case _ => errs
    })
  def assertNoErrors (tree :Tree) = {
    val errs = treeErrors(tree)
    errs foreach println
    assertTrue(errs.isEmpty)
  }

  @Test def testResolve = {
    val p = Parsers.parser
    val tree = p.parseDef("def fst :: ∀A => ∀B => x:A -> y:B -> A = x")
    val rtree = tree.resolve(emptyScope)
    // println(rtree)
    val TermDefTree(_, AllTree(asym, AllTree(bsym, AbsTree(xsym, TRefTree(xasym), AbsTree(
      ysym, TRefTree(ybsym), AscTree(TRefTree(aasym), RefTree(rxsym))))))) = rtree
    assertTrue(asym eq xasym)
    assertTrue(bsym eq ybsym)
    assertTrue(asym eq aasym)
    assertTrue(xsym eq rxsym)
  }

  // NOTE: we compare type sig via string repr to avoid symbol annoyances

  @Test def testTyping = {
    val p = Parsers.parser
    val tree = p.parseDef("def fst :: ∀A => ∀B => x:A -> y:B -> A = x")
    val rtree = tree.resolve(emptyScope)
    val itype = rtree.assignType(false)
    // rtree.debugPrint(new PrintWriter(System.out, true), "")
    assertEquals("∀A ⇒ ∀B ⇒ A → B → A", itype.toString)

    // val fooTrue = new TermDefTree(termSym(termName("foo")), LitTree(True))
    // println(fooTrue)
    // println(fooTrue.assignType())
  }

  @Test def testBuiltins = {
    val p = Parsers.parser
    val mod = new Module
    val add = mod.enter(p.parseDef("""def add :: x:Int -> y:Int -> Int = foreign "x + y" """))
    assertNoErrors(add)
    // add.debugPrint(new PrintWriter(System.out, true), "")
    assertEquals("Int → Int → Int", add.symType.toString)
  }

  @Test def testModuleSyms = {
    val p = Parsers.parser
    val mod = new Module
    val add = mod.enter(p.parseDef("""def add :: x:Int -> y:Int -> Int = foreign "x + y" """))
    assertNoErrors(add)
    // add.debugPrint(new PrintWriter(System.out, true), "")
    val incr = mod.enter(p.parseDef("""def incr :: x:Int -> Int = x + 1"""))
    assertNoErrors(incr)
    // incr.debugPrint(new PrintWriter(System.out, true), "")
    assertEquals("Int → Int", incr.symType.toString)
  }

  @Test def testTypeError = {
    val p = Parsers.parser
    val mod = new Module
    val plus = mod.enter(p.parseDef("def plus :: x:String -> y:Int -> Int = x"))
    // plus.debugPrint(new PrintWriter(System.out, true), "")
    val errs = treeErrors(plus)
    assertFalse("Expected type error.", errs.isEmpty)
    assertEquals(TypeMismatch(Builtins.intType, Builtins.stringType), errs.head)
  }
}
