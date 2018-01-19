//
// Compose Mark 0 compiler
// https://github.com/samskivert/compose/c0/LICENSE

package compose

import java.nio.file.{Paths, Files}
import org.junit.Assert._
import org.junit._
import fastparse.core.Parsed

class ParserTest {
  import AST._
  import Parser._

  def testCode (path :String) = {
    val cwd = Paths.get(System.getProperty("user.dir"))
    val fullPath = cwd.resolve("tests").resolve(path)
    val sb = new java.lang.StringBuilder
    Files.lines(fullPath).forEach(sb.append(_).append("\n"))
    program.parse(sb.toString)
  }

  def printParse (result :Parsed[AST.Expr, _, _]) = result.fold(
    (p, pos, extra) => {
      extra.traced.trace.split(" / " ).foreach(f => println(s"- $f"))
      fail(result.toString)
    },
    (res, pos) => {
      // println(pos + ": " + AST.printTree(res))
      AST.printExpr(res)
    }
  )

  def testParse[T] (expect :T, result :Parsed[T, _, _]) = result.fold(
    (p, pos, extra) => {
      extra.traced.trace.split(" / " ).foreach(f => println(s"- $f"))
      fail(result.toString)
    },
    (res, pos) => {
      assertEquals(expect, res)
    }
  )

  def ident (name :String) = IdentRef(Sym(name))
  def binOp (op :String, e0 :Expr, e1 :Expr) = BinOp(Sym(op), e0, e1)
  def argDef (arg :String) = ArgDef(Sym(arg), None)
  def lit (value :String) = Literal(value)

  val helloSym = Sym("hello")
  val hello = ident("hello")
  val (a, b, c) = (ident("a"), ident("b"), ident("c"))

  // some simple isolated parses to be sure things aren't totally broken
  @Test def testExprs () :Unit = {
    testParse(lit("1"), literalExpr.parse("1"))
    testParse(lit("1f"), literalExpr.parse("1f"))
    testParse(lit("32.3f"), literalExpr.parse("32.3f"))
    testParse(ArrayLiteral(Seq(lit("1"), lit("2"), lit("3"))), bracketExpr.parse("[1, 2, 3]"))
    testParse(hello, identExpr.parse("hello"))

    testParse(binOp("*", binOp("+", a, b), c), parenExpr.parse("((a + b) * c)"))
    testParse(Block(Seq(), Seq(hello)), blockExpr.parse("{ hello }"))

    testParse(UnOp(Sym("+"), a), unaryExpr.parse("+a"))
    testParse(UnOp(Sym("-"), b), unaryExpr.parse("-b"))
    testParse(UnOp(Sym("~"), c), unaryExpr.parse("~c"))
    testParse(UnOp(Sym("!"), hello), unaryExpr.parse("!hello"))

    Seq("+", "-", "*", "/", "%", "<<", ">>", "&&", "||", "&", "|",
        "==", "!=", "<=", ">=", "<", ">") foreach { op =>
      testParse(binOp(op, a, b), pureOpExpr.parse(s"a $op b"))
    }
  }

  @Test def testSelect () :Unit = {
    testParse(Select(hello, Sym("thang")), atomExpr.parse("hello.thang"))
  }

  val aEqAPlus1 = Lambda(Seq(argDef("a")), binOp("+", a, lit("1")))

  @Test def testLambda () :Unit = {
    testParse(aEqAPlus1, lambdaExpr.parse("a => a + 1"))
    testParse(Lambda(Seq(argDef("a"), argDef("b")), binOp("*", a, b)),
              lambdaExpr.parse("(a, b) => a * b"))
  }

  @Test def testFunApply () :Unit = {
    testParse(FunApply(hello, Seq(a, b, c)), atomExpr.parse("hello(a, b, c)"))
    testParse(FunApply(hello, Seq(a, b, c)), atomExpr.parse("(hello)(a, b, c)"))
    testParse(FunApply(aEqAPlus1, Seq(b)), atomExpr.parse("(a => a + 1)(b)"))
    testParse(FunApply(Select(ident("foo"), helloSym), Seq(a, b, c)),
              atomExpr.parse("foo.hello(a, b, c)"))
  }

  @Test def testIf () :Unit = {
    testParse(If(binOp("<", a, b), binOp("+", a, lit("1")), binOp("-", b, lit("42"))),
              ifExpr.parse("if (a < b) a + 1 else b - 42"))
  }

  @Test def testMatch () :Unit = {
    // val caseClause = P( ws ~ (Key("case") ~ ws ~ pattern ~ ws ~ "=" ~ expr) ).
    //   map(data => Case(data._1, None, data._2))
    // val matchExpr = P( Key("match") ~ expr ~ caseClause.rep(1) ).map((Match.apply _).tupled)
  }

  @Test def testCond () :Unit = {
    // val condEnd = P( hs ~ newline )
    // val condClause = P( expr ~ hs ~ "=" ~ expr ~ condEnd ).map((Condition.apply _).tupled)
    // val elseClause = P( ws ~ Key("else") ~ hs ~ "=" ~ expr ~ condEnd )
    // val condExpr = P( Key("cond") ~ condClause.rep(1) ~ elseClause ).map((Cond.apply _).tupled)
  }

  @Test def testComp () :Unit = {
    val range = FunApply(ident("range"), Seq(lit("10"), lit("99")))
    val filter = binOp("==", binOp("%", binOp("+", a, b), lit("2")), lit("0"))
    // no brackets here because those are handled by bracketExpr
    val code = "a * b where a <- range(10, 99), b <- range(10, 99), (a + b) % 2 == 0"
    val clauses = Seq(Generator(Sym("a"), range), Generator(Sym("b"), range), Filter(filter))
    testParse(MonadComp(binOp("*", a, b), clauses), compExpr.parse(code))
  }

  @Test def testEffects () :Unit = {
    testParse(Assign(helloSym, binOp("%", a, b)), assignEffect.parse("hello = a % b"))
  }

  @Test def testEulers () :Unit = {
    1 to 7 foreach { ii =>
      val num = String.format("%02d", ii.asInstanceOf[AnyRef])
      println(s"-- Euler $num --------------")
      printParse(testCode(s"euler/euler$num.cz"))
    }
  }

  @Test def testNextEuler () :Unit = {
    printParse(testCode("euler/euler08.cz"))
  }

  @Test def testFib () :Unit = {
    printParse(testCode("fib.cz"))
  }

  @Test def testNestedFun () :Unit = {
    printParse(testCode("nested.cz"))
  }

  @Test def testRando () :Unit = {
    printParse(program.parse("3*1+5"))
    printParse(program.parse("(3, 4)"))
    printParse(program.parse("fun foo (a :Int, b :String) =\n   (a + b, -3)"))
    printParse(program.parse("""match 5 case i = 4 case z = 6+4"""))
  }
}
