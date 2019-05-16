package compose

import org.junit.Test
import org.junit.Assert._
import java.io.PrintWriter

class TreesTest {
  import Constants._
  import Modules._
  import Names._
  import Symbols._
  import TestUtils._
  import Trees._
  import Types._

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
    val mod = tempModule
    val add = mod.enter(p.parseDef("""def add :: x:Int -> y:Int -> Int = foreign "x + y" """))
    assertNoErrors(add)
    // add.debugPrint(new PrintWriter(System.out, true), "")
    assertEquals("Int → Int → Int", add.symType.toString)
  }

  @Test def testModuleSyms = {
    val p = Parsers.parser
    val mod = tempModule
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
    val mod = tempModule
    val plus = mod.enter(p.parseDef("def plus :: x:String -> y:Int -> Int = x"))
    // plus.debugPrint(new PrintWriter(System.out, true), "")
    val errs = treeErrors(plus)
    assertFalse("Expected type error.", errs.isEmpty)
    assertEquals(TypeMismatch(Builtins.intType, Builtins.stringType), errs.head)
  }

  @Test def testUnifyIf = {
    val p = Parsers.parser
    val mod = testModule
    val src = """def foo :: x :Int -> Int =
                   let bar = if x == 0 then 0 else if x == 1 then 1 else 2
                   in bar"""
    val foo = mod.enter(p.parseDef(src))
    // foo.debugPrint(new PrintWriter(System.out, true), "")
    assertNoErrors(foo)
    assertEquals("Int → Int", foo.symType.toString)
  }

  @Test def testUnifyFail = {
    val src = """let bar x = if x == 0 then 0 else "bob"
                 in bar"""
    val (tree, res) = typeExpr(src, false)
    assertEquals(Left(UnifyFailure(Const(int("0")), Const(string("bob")))), res)
  }

  @Test def testLet = {
    val src = """let bar x = if x == 0 then 0 else 1
                 in bar 5"""
    val (tree, res) = typeExpr(src, false)
    // tree.debugPrint(new PrintWriter(System.out, true), "")
    assertNoErrors(tree)
    assertEquals(Right("i8"), res.map(_.toString))
  }

  @Test def testRecursiveLet = {
    val src = """let bar x = if x == 0 then 0
                             else (bar (x-1)) + 1
                 in bar 5"""
    val (tree, res) = typeExpr(src, false)
    // tree.debugPrint(new PrintWriter(System.out, true), "")
    assertNoErrors(tree)
    assertEquals(Right("Int"), res.map(_.toString))
  }

  @Test def testRecursiveDef = {
    val p = Parsers.parser
    val mod = testModule
    val fib = mod.enter(p.parseDef("""def fib :: x:Int -> Int =
                                        if x == 0 then 0
                                        else if x == 1 then 1
                                        else fib (x-2) + fib (x-1)"""))
    // fib.debugPrint(new PrintWriter(System.out, true), "")
    assertNoErrors(fib)
    assertEquals("Int → Int", fib.symType.toString)
  }
}
