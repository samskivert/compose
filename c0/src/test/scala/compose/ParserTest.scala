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

  val PrintParsedTrees = false

  def check (result :Parsed[Seq[TermTree], _, _], tree :Boolean = false) = result.fold(
    (p, pos, extra) => {
      extra.traced.trace.split(" / " ).foreach(f => println(s"- $f"))
      fail(result.toString)
    },
    (res, pos) => res.foreach { expr =>
      if (tree) println(pos + ": " + showTree(expr))
      if (PrintParsedTrees) print(expr)
    }
  )

  def assertTreeEquals[T] (expect :T, result :T) = result match {
    case tree :Tree => if (tree != expect) fail(
      s"Expected:\n     ${show(expect.asInstanceOf[Tree])}\nGot: ${show(tree)}")
    case _          => assertEquals(expect, result)
  }

  def testParse[T] (expect :T, result :Parsed[T, _, _]) = result.fold(
    (p, pos, extra) => {
      extra.traced.trace.split(" / " ).foreach(f => println(s"- $f"))
      fail(result.toString)
    },
    (res, pos) => res match {
      case trees :Seq[_] => (trees zip expect.asInstanceOf[Seq[Any]]) foreach
        (assertTreeEquals _).tupled
      case _             => assertTreeEquals(expect, res)
    }
  )

  def ident (name :String) = IdentRef(termName(name))
  def unOp (op :String, exp :TermTree) = FunApply(FunKind.UnOp, ident(op), Seq(), Seq(exp))
  def binOp (op :String, e0 :TermTree, e1 :TermTree) =
    FunApply(FunKind.BinOp, IdentRef(termName(op)), Seq(), Seq(e0, e1))
  def argDef (arg :String) = ArgDef(Seq(), termName(arg), OmittedType)
  def intlit (value :String) = Literal(Constants.int(value))
  def strlit (value :String) = Literal(Constants.string(value))

  val helloName = termName("hello")
  val hello = ident("hello")
  val (a, b, c) = (ident("a"), ident("b"), ident("c"))

  @Test def testDocComments () :Unit = {
    testParse(Seq("Foo"), docComments.parse("/// Foo\n"))
    testParse(Seq("Foo", "Bar"), docComments.parse("/// Foo\n/// Bar"))
    testParse(Seq("Foo", "", "Bar"), docComments.parse("/// Foo\n///\n/// Bar"))
  }

  val Lit1 = intlit("1")
  val Lit2 = intlit("2")
  val Lit3 = intlit("3")
  val Lit4 = intlit("4")
  val Lit5 = intlit("5")

  @Test def testLiterals () :Unit = {
    val expect = Seq(
      Literal(Constants.False),
      Literal(Constants.True),
      Lit1,
      Literal(Constants.float("1f")),
      Literal(Constants.float("32.3f")),
      Literal(Constants.char("c")),
      Literal(Constants.char("\\u1234")),
      strlit("hello"),
      strlit("hello\\n"),
      strlit("hello"),
      Literal(Constants.rawString("hello\\n")),
      Literal(Constants.rawString("hello")),
      ArrayLiteral(Seq(Lit1, Lit2, Lit3))
    )
    testParse(FunApply(FunKind.Normal, IdentRef(tupleName(13)), Seq(), expect),
              parseCode("tests/literals.cz", parenExpr))

    testParse(Seq(FunApply(FunKind.Normal, ident("Tuple2"), Seq(), Seq(Lit3, Lit4))),
              program.parse("(3, 4)"))
  }

  @Test def testExprs () :Unit = {
    testParse(hello, identExpr.parse("hello"))

    testParse(binOp("*", binOp("+", a, b), c), parenExpr.parse("((a + b) * c)"))
    testParse(Block(Seq(hello)), blockExpr.parse("{ hello }"))

    testParse(unOp("+", a), unaryExpr.parse("+a"))
    testParse(unOp("-", b), unaryExpr.parse("-b"))
    testParse(unOp("~", c), unaryExpr.parse("~c"))
    testParse(unOp("!", hello), unaryExpr.parse("!hello"))

    Seq("+", "-", "*", "/", "%", "<<", ">>", "&&", "||", "&", "|",
        "==", "!=", "<=", ">=", "<", ">") foreach { op =>
      testParse(binOp(op, a, b), pureOpExpr.parse(s"a $op b"))
    }

    testParse(Seq(binOp("+", binOp("*", Lit3, Lit1), Lit5)), program.parse("3*1+5"))
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

  val aEqAPlus1 = Lambda(Seq(argDef("a")), binOp("+", a, Lit1))

  @Test def testLambda () :Unit = {
    testParse(aEqAPlus1, lambdaExpr.parse("a => a + 1"))
    testParse(Lambda(Seq(argDef("a"), argDef("b")), binOp("*", a, b)),
              lambdaExpr.parse("(a, b) => a * b"))
  }

  @Test def testFunApply () :Unit = {
    testParse(FunApply(FunKind.Normal, hello, Seq(), Seq(a, b, c)),
              atomExpr.parse("hello(a, b, c)"))
    testParse(FunApply(FunKind.Normal, hello, Seq(), Seq(a, b, c)),
              atomExpr.parse("(hello)(a, b, c)"))
    testParse(FunApply(FunKind.Normal, aEqAPlus1, Seq(), Seq(b)),
              atomExpr.parse("(a => a + 1)(b)"))
    testParse(FunApply(FunKind.Normal, Select(ident("foo"), helloName), Seq(), Seq(a, b, c)),
              atomExpr.parse("foo.hello(a, b, c)"))
  }

  @Test def testFunDef () :Unit = {
    val args = Seq(ArgDef(Seq(), termName("a"), TypeRef(typeName("Int"))),
                   ArgDef(Seq(), termName("b"), TypeRef(typeName("String"))))
    val body = FunApply(FunKind.Normal, ident("Tuple2"), Seq(),
                        Seq(binOp("+", a, b), unOp("-", intlit("3"))))
    testParse(Seq(DefExpr(FunDef(Seq(), termName("foo"), Seq(), Seq(), args, OmittedType, body))),
              program.parse("fun foo (a :Int, b :String) =\n   (a + b, -3)"))
  }

  @Test def testIf () :Unit = {
    testParse(If(binOp("<", a, b), binOp("+", a, Lit1), binOp("-", b, intlit("42"))),
              ifExpr.parse("if (a < b) a + 1 else b - 42"))
  }

  @Test def testMatch () :Unit = {
    testParse(LiteralPat(Constants.int("0")), pattern.parse("0"))
    testParse(LetPat(termName("a")), pattern.parse("a"))
    testParse(IdentPat(termName("N")), pattern.parse("N"))
    testParse(DestructPat(termName("Cons"), Seq(LetPat(termName("h")), LetPat(termName("t")))),
              pattern.parse("Cons(h, t)"))

    check(program.parse("""match 5 case i = 4 case z = 6+4"""))
    // check(program.parse(TupleMatch), true)
    // check(program.parse(DestructMatch))
  }

  @Test def testCond () :Unit = {
    // val condEnd = P( hs ~ newline )
    // val condClause = P( expr ~ hs ~ "=" ~ expr ~ condEnd ).map((Condition.apply _).tupled)
    // val elseClause = P( ws ~ Key("else") ~ hs ~ "=" ~ expr ~ condEnd )
    // val condExpr = P( Key("cond") ~ condClause.rep(1) ~ elseClause ).map((Cond.apply _).tupled)
  }

  @Test def testComp () :Unit = {
    val range = FunApply(FunKind.Normal, ident("range"), Seq(), Seq(intlit("10"), intlit("99")))
    val filter = binOp("==", binOp("%", binOp("+", a, b), Lit2), intlit("0"))
    // no brackets here because those are handled by bracketExpr
    val code = "a * b where a <- range(10, 99), b <- range(10, 99), (a + b) % 2 == 0"
    val clauses = Seq(Generator(termName("a"), range), Generator(termName("b"), range),
                      Filter(filter))
    testParse(MonadComp(binOp("*", a, b), clauses), compExpr.parse(code))
  }

  @Test def testEffects () :Unit = {
    testParse(Assign(helloName, binOp("%", a, b)), assignEffect.parse("hello = a % b"))
  }

  @Test def testImplDefs () :Unit = {
    check(program.parse("impl eqI32 = Eq[I32](eq=i32Eq)"))
    check(program.parse("impl eqArray[A:Eq] = Eq[Array[A]](eq=arrayEq)"))
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
      check(parseCode(s"tests/euler/euler$num.cz"))
    }
  }

  @Test def testDataDefs () :Unit = {
    check(parseCode("tests/data.cz"))
  }

  @Test def testInterfaceDefs () :Unit = {
    check(parseCode("std/eq.cz"))
    check(parseCode("std/ord.cz"))
  }

  @Test def testNextEuler () :Unit = {
    // check(parseCode("euler/euler11.cz"), false)
  }

  @Test def testFib () :Unit = {
    check(parseCode("tests/fib.cz"))
  }

  @Test def testNestedFun () :Unit = {
    check(parseCode("tests/nested.cz"))
  }
}
