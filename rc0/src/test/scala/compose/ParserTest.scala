package compose

import org.junit.Test
import org.junit.Assert._

class ParserTest {
  import Tokens._
  import Lexers._
  import Parsers._
  import Trees._

  def mkScanner (code :String) = new Scanner(code.toCharArray)
  def tokenize (code :String) :Seq[(Token,String)] = {
    val scanner = mkScanner(code)
    val toks = Seq.newBuilder[(Token,String)]
    var tok = scanner.gett()
    while (tok != EOF && tok != ERROR) {
      toks += ((tok, scanner.tokenText))
      tok = scanner.gett()
    }
    toks.result()
  }

  @Test def testScanner = {
    assertEquals(
      Seq((WORD, "let"), (WORD, "foo"), (EQUALS, ""), (LIT, "123")),
      tokenize("let foo = 123"))
    assertEquals(
      Seq((WORD, "let"), (WORD, "foo"), (EQUALS, ""), (LPAREN, ""),
          (LIT, "3"), (OPER, "+"), (LIT, "FF"), (RPAREN, "")),
      tokenize("let foo = (3 + 0xFF)"))
    assertEquals(
      Seq((WORD, "let"), (WORD, "foo"), (EQUALS, ""), (LIT, "1.4"),
          (OPER, "+"), (LIT, "1.5e-2")),
      tokenize("let foo = 1.4 + 1.5e-2"))
  }

  @Test def testParser = {
    val p = new Parser(mkScanner("3 + 4 * 15"))
    println(p.termExpr())
  }
}
