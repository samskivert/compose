//
// Compose Mark 0 compiler
// https://github.com/samskivert/compose/c0/LICENSE

package compose

import org.junit.Assert._
import org.junit._
import fastparse.core.Parsed

class ParserTest {
  import Trees._
  import Parser._
  import Names._
  import TestCode._

  def printParse (result :Parsed[Seq[TermTree], _, _], tree :Boolean = false) = result.fold(
    (p, pos, extra) => {
      extra.traced.trace.split(" / " ).foreach(f => println(s"- $f"))
      fail(result.toString)
    },
    (res, pos) => res.foreach { expr =>
      if (tree) println(pos + ": " + showTree(expr))
      print(expr)
    }
  )

  def testParse[T] (expect :T, result :Parsed[T, _, _]) = result.fold(
    (p, pos, extra) => {
      extra.traced.trace.split(" / " ).foreach(f => println(s"- $f"))
      fail(result.toString)
    },
    // TODO: special tree equality assertion for better diffs?
    (res, pos) => assertEquals(expect, res)
  )

  def ident (name :String) = IdentRef(termName(name))
  def binOp (op :String, e0 :TermTree, e1 :TermTree) =
    FunApply(FunKind.BinOp, IdentRef(termName(op)), Seq(), Seq(e0, e1))
  def argDef (arg :String) = ArgDef(Seq(), termName(arg), OmittedType)
  def intlit (value :String) = Literal(Constants.int(value))
  def strlit (value :String) = Literal(Constants.string(value))

  val helloName = termName("hello")
  val hello = ident("hello")
  val (a, b, c) = (ident("a"), ident("b"), ident("c"))

  @Test def testLiterals () :Unit = {
    val expect = Seq(
      Literal(Constants.False),
      Literal(Constants.True),
      intlit("1"),
      Literal(Constants.float("1f")),
      Literal(Constants.float("32.3f")),
      Literal(Constants.char("c")),
      Literal(Constants.char("\\u1234")),
      strlit("hello"),
      strlit("hello\\n"),
      strlit("hello"),
      Literal(Constants.rawString("hello\\n")),
      Literal(Constants.rawString("hello")),
      ArrayLiteral(Seq(intlit("1"), intlit("2"), intlit("3")))
    )
    testParse(Tuple(expect), parseCode("literals.cz", parenExpr))
  }

  @Test def testExprs () :Unit = {
    testParse(hello, identExpr.parse("hello"))

    testParse(binOp("*", binOp("+", a, b), c), parenExpr.parse("((a + b) * c)"))
    testParse(Block(Seq(hello)), blockExpr.parse("{ hello }"))

    testParse(FunApply(FunKind.UnOp, ident("+"), Seq(), Seq(a)), unaryExpr.parse("+a"))
    testParse(FunApply(FunKind.UnOp, ident("-"), Seq(), Seq(b)), unaryExpr.parse("-b"))
    testParse(FunApply(FunKind.UnOp, ident("~"), Seq(), Seq(c)), unaryExpr.parse("~c"))
    testParse(FunApply(FunKind.UnOp, ident("!"), Seq(), Seq(hello)), unaryExpr.parse("!hello"))

    Seq("+", "-", "*", "/", "%", "<<", ">>", "&&", "||", "&", "|",
        "==", "!=", "<=", ">=", "<", ">") foreach { op =>
      testParse(binOp(op, a, b), pureOpExpr.parse(s"a $op b"))
    }
  }

  @Test def testLet () :Unit = {
    testParse(LetDef(Seq(Binding(termName("a"), OmittedType, strlit("hello")))),
              letDef.parse("let a = \"hello\""))
  }

  @Test def testData () :Unit = {
    val aVar = Param(typeName("A"))
    val nilCase = RecordDef(Seq(), typeName("Nil"), Seq(), Seq())
    val consCase = RecordDef(Seq(), typeName("Cons"), Seq(), Seq(
      FieldDef(Seq(), termName("head"), TypeRef(typeName("A"))),
      FieldDef(Seq(), termName("tail"), TypeApply(typeName("List"), Seq(TypeRef(typeName("A")))))
    ))
    testParse(UnionDef(Seq(), typeName("List"), Seq(aVar), Seq(nilCase, consCase)),
              unionDef.parse("data List[A] = Nil | Cons (head :A, tail :List[A])"))
  }

  @Test def testSelect () :Unit = {
    testParse(Select(hello, termName("thang")), atomExpr.parse("hello.thang"))
  }

  val aEqAPlus1 = Lambda(Seq(argDef("a")), binOp("+", a, intlit("1")))

  @Test def testLambda () :Unit = {
    testParse(aEqAPlus1, lambdaExpr.parse("a => a + 1"))
    testParse(Lambda(Seq(argDef("a"), argDef("b")), binOp("*", a, b)),
              lambdaExpr.parse("(a, b) => a * b"))
  }

  @Test def testFunApply () :Unit = {
    testParse(FunApply(FunKind.Normal, hello, Seq(), Seq(a, b, c)), atomExpr.parse("hello(a, b, c)"))
    testParse(FunApply(FunKind.Normal, hello, Seq(), Seq(a, b, c)), atomExpr.parse("(hello)(a, b, c)"))
    testParse(FunApply(FunKind.Normal, aEqAPlus1, Seq(), Seq(b)), atomExpr.parse("(a => a + 1)(b)"))
    testParse(FunApply(FunKind.Normal, Select(ident("foo"), helloName), Seq(), Seq(a, b, c)),
              atomExpr.parse("foo.hello(a, b, c)"))
  }

  @Test def testIf () :Unit = {
    testParse(If(binOp("<", a, b), binOp("+", a, intlit("1")), binOp("-", b, intlit("42"))),
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
    val range = FunApply(FunKind.Normal, ident("range"), Seq(), Seq(intlit("10"), intlit("99")))
    val filter = binOp("==", binOp("%", binOp("+", a, b), intlit("2")), intlit("0"))
    // no brackets here because those are handled by bracketExpr
    val code = "a * b where a <- range(10, 99), b <- range(10, 99), (a + b) % 2 == 0"
    val clauses = Seq(Generator(termName("a"), range), Generator(termName("b"), range),
                      Filter(filter))
    testParse(MonadComp(binOp("*", a, b), clauses), compExpr.parse(code))
  }

  @Test def testEffects () :Unit = {
    testParse(Assign(helloName, binOp("%", a, b)), assignEffect.parse("hello = a % b"))
  }

  @Test def testTypes () :Unit = {
    val intType = TypeRef(typeName("Int"))
    testParse(intType, typeRef.parse("Int"))
    val intListType = TypeApply(typeName("List"), Seq(intType))
    testParse(intListType, typeRef.parse("List[Int]"))
    val intListToIntType = TypeArrow(Seq(intListType), intType)
    testParse(intListToIntType, typeRef.parse("List[Int] => Int"))
    val intsToIntType = TypeArrow(Seq(intType, intType), intType)
    testParse(intsToIntType, typeRef.parse("(Int, Int) => Int"))
  }

  @Test def testEulers () :Unit = {
    1 to 10 foreach { ii =>
      val num = String.format("%02d", ii.asInstanceOf[AnyRef])
      println(s"-- Euler $num --------------")
      printParse(parseCode(s"euler/euler$num.cz"))
    }
  }

  @Test def testDataDefs () :Unit = {
    printParse(parseCode("data.cz"))
  }

  @Test def testInterfaceDefs () :Unit = {
    printParse(parseCode("eq.cz"))
    printParse(parseCode("ord.cz"))
  }

  @Test def testNextEuler () :Unit = {
    // printParse(parseCode("euler/euler11.cz"), false)
  }

  @Test def testFib () :Unit = {
    printParse(parseCode("fib.cz"))
  }

  @Test def testNestedFun () :Unit = {
    printParse(parseCode("nested.cz"))
  }

  @Test def testRando () :Unit = {
    printParse(program.parse("3*1+5"))
    printParse(program.parse("(3, 4)"))
    printParse(program.parse("fun foo (a :Int, b :String) =\n   (a + b, -3)"))
    printParse(program.parse("""match 5 case i = 4 case z = 6+4"""))
  }
}
