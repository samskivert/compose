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

  @Test def testParser = {
    val p = new Compose()
    // assertEquals(intLit(25), p.parse("25"))
    // assertEquals(LitTree(False), p.parse("false"))
    // assertEquals(LitTree(True), p.parse("true"))
    // assertEquals(LitTree(string("bob")), p.parse("\"bob\""))
    // assertEquals(LitTree(float("1e7")), p.parse("1e7"))
    // assertEquals(LitTree(float("1.1232")), p.parse("1.1232"))
    // assertEquals(LitTree(float("1.4e3")), p.parse("1.4e3"))

    // assertEquals(app(ref("-"), intLit(3)), p.parse("-3"))
    // assertEquals(app(ref("!"), LitTree(False)), p.parse("!false"))
    // assertEquals(app(ref("!"), app(ref("-"), ref("bar"))), p.parse("!-bar"))
    // assertEquals(app(ref("!"), app(ref("+"), ref("foo"))), p.parse("!+foo"))
    // assertEquals(app(app(ref("+"), intLit(3)), intLit(5)), p.parse("3+5"))

    println(p.parse("let foo = 5 in 3"))
    // println(p.parse("let foo x = x + 3 in foo 5"))

    // mkScanner("3 + 4 * 15")
    // println(p.termExpr())
  }
}
