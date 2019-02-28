//
// Compose Mark 0 compiler
// https://github.com/samskivert/compose/c0/LICENSE

package compose

import org.parboiled.MatcherContext
import org.parboiled.errors.{ErrorUtils, ParsingException}
import org.parboiled.matchers.CustomMatcher
import org.parboiled.scala._

object Parsers {
  import Trees._
  import Names._
  import Symbols._
  import Constants._

  def parser :Compose = new Compose()

  class Compose extends Parser {
    import scala.language.implicitConversions

    // redefine the default string-to-rule conversion to also match trailing whitespace if the
    // string ends with a blank, this keeps the rules free from most whitespace matching clutter
    override implicit def toRule (string: String) =
      if (string.endsWith(" ")) str(string.trim) ~ WhiteSpace
      else str(string)

    // literals
    def Digit = rule { "0" - "9" }
    def HexDigit = rule { "0" - "9" | "a" - "f" | "A" - "F" }
    def Digits = rule { oneOrMore(Digit) }
    def Integer = rule { (("1" - "9") ~ Digits | Digit) }
    def Frac = rule { "." ~ Digits }
    def Exp = rule { ignoreCase("e") ~ optional(anyOf("+-")) ~ Digits }

    def Unicode = rule { "u" ~ HexDigit ~ HexDigit ~ HexDigit ~ HexDigit }
    def EscapedChar = rule { "\\" ~ (anyOf("\"\\/bfnrt") | Unicode) }
    def NormalChar = rule { !anyOf("\"\\") ~ ANY }
    def Character = rule { EscapedChar | NormalChar }

    def WhiteSpace :Rule0 = rule { zeroOrMore(anyOf(" \n\r\t\f")) }

    def TrueLit = rule { "true " ~ push(True) }
    def FalseLit = rule { "false " ~ push(False) }
    def StringLit = rule { "\"" ~ zeroOrMore(Character) ~> string ~ "\" " }

    def mkNumberLit (v :String) = if (v.contains(".") || v.contains("e")) float(v) else int(v)
    def NumberLit = rule {
      group(Integer ~ optional(Frac) ~ optional(Exp)) ~> mkNumberLit ~ WhiteSpace
    }

    // identifiers
    abstract class CharMatcher (label :String) extends CustomMatcher(label) {
      override def isSingleCharMatcher = true
      override def canMatchEmpty = false
      override def isStarterChar (c :Char) = acceptChar(c)
      override def getStarterChar = 'a'
      override def `match`[V] (ctx :MatcherContext[V]) = {
        if (!acceptChar(ctx.getCurrentChar())) false
        else {
          ctx.advanceIndex(1)
          ctx.createNode()
          return true
        }
      }
      def acceptChar (c :Char) :Boolean
    }
    val JavaLetter :Rule0 = new CharMatcher("Letter") {
      def acceptChar (c :Char) = java.lang.Character.isJavaIdentifierStart(c)
    }
    val JavaLetterOrDigit :Rule0 = new CharMatcher("LetterOrDigit") {
      def acceptChar (c :Char) = java.lang.Character.isJavaIdentifierPart(c)
    }

    def Keyword = rule { ( "let" | "in" ) ~ !JavaLetterOrDigit }
    def RawIdent = rule { !Keyword ~ JavaLetter ~ zeroOrMore(JavaLetterOrDigit) }
    def Ident = rule { RawIdent ~> termName ~ WhiteSpace }

    // TODO: TypeDef: prod, sum, alias, etc.

    /// Type Expressions

    // TypeRef := Ident // TODO: tapp, etc.
    def TypeRef = Ident ~~> (name => UTRefTree(name.toTypeName))

    /// Term Expressions

    // Lit := TrueLit | FalseLit | StringLit | NumberLit
    def Lit = rule { ( TrueLit | FalseLit | StringLit | NumberLit ) ~~> LitTree }

    // TermRef := Ident
    def TermRef = Ident ~~> URefTree

    // AtomTerm := Lit | TermRef | ("(" Expr ")")
    def AtomTerm :Rule1[TermTree] = rule { ( Lit | TermRef | "(" ~ Expr ~ ")" ) }

    // IndexTerm := AtomTerm ("@" AtomTerm)*
    def IndexTerm = AtomTerm // TODO: when we have array indexing

    // AppTerm := IndexTerm IndexTerm*
    def AppTerm = rule { oneOrMore(IndexTerm) ~~> (
      terms => (terms.head /: terms.tail)((fn, arg) => AppTree(fn, arg))) }

    // PreOp := "+" | "-" | "!" // TODO: "~" if we add bitwise ops
    def PreOp = anyOf("+-!") ~> identity
    // PreTerm := PreOp* AppTerm
    def PreTerm = rule { zeroOrMore(PreOp) ~ AppTerm ~~> (
      (preOp, term) => (term /: preOp.reverse)((tm, op) =>  AppTree(URefTree(termName(op)), tm))) }

    def binOpApps (pre :TermTree, ops :List[(String, TermTree)]) = (pre /: ops)(
      (tm, opTm) => AppTree(AppTree(URefTree(termName(opTm._1)), tm), opTm._2))

    // MulOp := "*" | "/" | "%"
    def MulOp = anyOf("*/%") ~> identity ~ WhiteSpace
    // MulTerm := AppTerm (MulOp AppTerm)*
    def MulTerm = rule { PreTerm ~ zeroOrMore(MulOp ~ PreTerm) ~~> binOpApps}

    // AddOp = "+" | "-"
    def AddOp = anyOf("+-") ~> identity ~ WhiteSpace
    // AddTerm := MulTerm (AddOp MulTerm)*
    def AddTerm = rule { MulTerm ~ zeroOrMore(AddOp ~ MulTerm) ~~> binOpApps }

    // EqOp := "==" | "!=" | "is"
    def EqOp = ( "==" | "!=" | "is" ) ~> identity ~ WhiteSpace
    // EqTerm := AddTerm (EqOp AddTerm)*
    def EqTerm = rule { AddTerm ~ zeroOrMore(EqOp ~ AddTerm) ~~> binOpApps }

    // TODO: bitwise ops?

    // RelOp := "<" | ">" | "<=" | ">="
    def RelOp = ( "<=" | "<" | ">=" | ">" ) ~> identity ~ WhiteSpace
    // RelTerm := EqTerm (RelOp EqTerm)*
    def RelTerm = rule { EqTerm ~ zeroOrMore(RelOp ~ EqTerm) ~~> binOpApps }

    // ConjTerm := RelTerm ("&&" RelTerm)*
    def ConjOp = ( "&&" ) ~> identity ~ WhiteSpace
    def ConjTerm = rule { RelTerm ~ zeroOrMore(ConjOp ~ RelTerm) ~~> binOpApps }

    // DisjTerm := ConjTerm ("||" ConjTerm)*
    def DisjOp = ( "||" ) ~> identity ~ WhiteSpace
    def DisjTerm = rule { ConjTerm ~ zeroOrMore(DisjOp ~ ConjTerm) ~~> binOpApps }

    // TermExpr := DisjTerm
    def TermExpr = DisjTerm

    // let foo ∀A ⇒ Num A ⇒ x:A → y:Seq A → z:Bool → Bool = z
    // let foo ∀A ∀B Num A x y z = z
    // let foo x y z = z

    // let foo → Bool = false
    // let foo:Bool = false
    // let foo = false

    // let foo x:Bool → Bool = false
    // let foo x = false

    // let foo x:Bool → y:Bool → Bool = false
    // let foo x y = false

    def mkAbs (args :List[(Name, TypeTree)], body :TermTree) = (args :\ body)(
      (arg, tm) => AbsTree(termSym(arg._1.toTermName), arg._2, tm))
    def mkAll (args :List[Name], body :TermTree) = (args :\ body)(
      (arg, tm) => AllTree(typeSym(arg.toTypeName), tm))

    // OptTypeAnn := (":" TypeRef)?
    def OptTypeAnn :Rule1[TypeTree] = optional(":" ~ TypeRef) ~~> (_ getOrElse THoleTree)
    // LetArgDef := Ident OptTypeAnn
    def LetArgDef = Ident ~ OptTypeAnn
    // LetArgs :=  LetArgDef ("->" LetArgDef)*
    def LetArgs = LetArgDef ~ zeroOrMore("-> " ~ LetArgDef) ~~> (
      (arg0, ann0, rest) => (arg0, ann0) :: rest)
    // LetBind := Ident LetArgs? = Expr
    def LetBind = rule { Ident ~ optional(LetArgs) ~ "= " ~ Expr ~~> ((name, args, body) => {
      // TODO: allow trailing `-> TypeAnn` to annotate the bind
      Bind(termSym(name), THoleTree, (body /: (args getOrElse Nil))(
        (tm, arg) => AbsTree(termSym(arg._1.toTermName), arg._2, tm)))
    })}
    // LetBinds := LetBind (";" LetBind)*
    def LetBinds = LetBind ~ zeroOrMore("; " ~ LetBind) ~~> ((b0, bs) => b0 :: bs)
    // LetExpr := "let" LetBinds "in" Expr
    def LetExpr = rule { "let " ~ LetBind ~ WhiteSpace ~ "in " ~ Expr ~~> LetTree }

    // TODO: support multiple bindings in a single let (essentially letrec)
    // LetExpr := "let" LetBinds "in" Expr
    // def LetExpr = rule { "let" ~ LetBinds ~ "in" ~ Expr }

    // TODO: CaseExpr

    // Expr := LetExpr | CaseExpr | TermExpr
    def Expr :Rule1[TermTree] = rule { WhiteSpace ~ ( LetExpr /*| CaseExpr*/ | TermExpr ) }

    /// Top-level Definitions

    // def fst :: ∀A => ∀B => x:A -> y:B -> A = x
    // def c :: Int = 30

    // TypeArg := "∀" Ident
    def TypeArg = "∀" ~ Ident ~ "=> "
    // Arg := Ident ":" TypeRef
    def Arg = Ident ~ ":" ~ TypeRef ~ "-> "
    // TermDef := "def" Ident "::" TypeArg* Arg* TypeRef = Expr
    def TermDef = rule {
      WhiteSpace ~ "def " ~ Ident ~ ":: " ~ zeroOrMore(TypeArg) ~ zeroOrMore(Arg) ~ TypeRef ~ "= " ~ Expr ~~> (
        (name, targs, args, asc, body) => TermDefTree(
          termSym(name), mkAll(targs, mkAbs(args, AscTree(asc, body))))) }

    // TODO: Def = TermDef | TypeDef & have parseDef use Def

    def parseExpr (code :String) = parse(Expr, code)
    def parseDef (code :String) = parse(TermDef, code)

    def parse [T <: Tree] (root :Rule1[T], code :String) :T = {
      // val parsingResult = TracingParseRunner(root).run(code)
      val parsingResult = ReportingParseRunner(root).run(code)
      parsingResult.result match {
        case Some(tree) => tree
        case None => throw new ParsingException(
          "Invalid source:\n" + ErrorUtils.printParseErrors(parsingResult))
      }
    }
  }
}
