package compose

import org.junit.Test
import org.junit.Assert._

class ParserTest {
  import Constants._
  import Names._
  import Parsers._
  import Trees._

  def intLit (value :Int) = LitTree(int(value.toString))
  def ref (name :String) = URefTree(termName(name))
  def app (funTree :TermTree, argTree :TermTree) = AppTree(funTree, argTree)

  @Test def testParseLits = {
    val p = new Compose()
    assertEquals(intLit(25), p.parse("25"))
    assertEquals(LitTree(False), p.parse("false"))
    assertEquals(LitTree(True), p.parse("true"))
    assertEquals(LitTree(string("bob")), p.parse("\"bob\""))
    assertEquals(LitTree(float("1e7")), p.parse("1e7"))
    assertEquals(LitTree(float("1.1232")), p.parse("1.1232"))
    assertEquals(LitTree(float("1.4e3")), p.parse("1.4e3"))
  }

  @Test def testParsePreOps = {
    val p = new Compose()
    assertEquals(app(ref("-"), intLit(3)), p.parse("-3"))
    assertEquals(app(ref("!"), LitTree(False)), p.parse("!false"))
    assertEquals(app(ref("!"), app(ref("-"), ref("bar"))), p.parse("!-bar"))
    assertEquals(app(ref("!"), app(ref("+"), ref("foo"))), p.parse("!+foo"))
  }

  @Test def testParseBinOps = {
    val p = new Compose()
    assertEquals(app(app(ref("+"), intLit(3)), intLit(5)), p.parse("3+5"))
    assertEquals(app(app(ref("*"), intLit(3)), intLit(5)), p.parse("3*5"))
    assertEquals(app(app(ref("-"), intLit(3)), ref("foo")), p.parse("3-foo"))
    assertEquals(app(app(ref("/"), ref("foo")), intLit(5)), p.parse("foo/5"))
  }

  @Test def testParseLets = {
    val p = new Compose()
    assertEquals(LetTree(Bind(termName("foo"), THoleTree, intLit(5)), intLit(3)),
                 p.parse("let foo = 5 in 3"))

    val xPlus3 = AbsTree(termName("x"), THoleTree, app(app(ref("+"), ref("x")), intLit(3)))
    assertEquals(LetTree(Bind(termName("foo"), THoleTree, xPlus3), app(ref("foo"),intLit(5))),
                 p.parse("let foo x = x + 3 in foo 5"))
  }
}
