//
// Compose Mark 0 compiler
// https://github.com/samskivert/compose/c0/LICENSE

package compose

import fastparse.all._
import fastparse.CharPredicates._

object Lexer {

  // Numbers
  val digits = "0123456789"
  val Digit = P( CharIn(digits) )
  val hexDigits = digits + "abcdefABCDEF"
  val HexDigit = P( CharIn(hexDigits) )
  val HexNum = P( "0x" ~ CharsWhileIn(hexDigits) )
  val DecNum = P( CharsWhileIn(digits) )
  val Exp = P( CharIn("Ee") ~ CharIn("+-").? ~ DecNum )
  val FloatType = P( CharIn("fFdD") )

  // Strings (TODO)
  val UnicodeEscape = P( "u" ~ HexDigit ~ HexDigit ~ HexDigit ~ HexDigit )

  // Operators (not currently used)
  val OpChar = P ( CharPred(isOpChar) )
  def isOpChar(c: Char) = c match{
    case '!' | '#' | '%' | '&' | '*' | '+' | '-' | '/' |
         ':' | '<' | '=' | '>' | '?' | '@' | '\\' | '^' | '|' | '~' => true
    case _ => isOtherSymbol(c) || isMathSymbol(c)
  }
  val Op = P( OpChar.rep(1) ).!

  // Identifiers
  val Letter = P( CharPred(isLower) | CharPred(isUpper) )
  val IdentStart = P( Letter | "_" )
  val IdentCont = P( Letter | Digit | "_" | "$" )

  // Whitespace
  val newline = P( "\n" | "\r\n" | "\r" | "\f")
  val horizspace = P( " " | "\t" )
  val whitespace = P( horizspace | newline )
  val hs = P( horizspace.rep )
  val ws = P( whitespace.rep )
  val ws1 = P( whitespace.rep(1) )

  // Literals
  val Int = P( (HexNum | DecNum) ~ CharIn("Ll").? ).!

  val Float = {
    def Thing = P( DecNum ~ Exp.? ~ FloatType.? )
    def Thing2 = P( "." ~ Thing | Exp ~ FloatType.? | Exp.? ~ FloatType )
    P( "." ~ Thing | DecNum ~ Thing2 )
  }.!

  // Keywords & identifiers
  def mkKeyP (s: String) = s ~ !IdentCont
  val Key = Map() ++ Seq(
    "let", "var", "fun", "do", "while", "if", "else", "cond", "for", "match", "case", "where"
  ).map(id => (id, mkKeyP(id)))

  val Name = P( IdentStart ~ IdentCont.rep ).!.filter(id => !Key.contains(id))
}

object Parser {
  import Lexer._
  import AST._

  val number = P( Float | Int )
  val ident :P[Sym] = P( Name ).map(Sym)
  // TODO: string literals

  // types
  val typeRef :P[Type] = P( ident ).map(Named) // TODO: type constructor application, etc.
  val optTypeRef = P( (ws ~ ":" ~ typeRef ).? )

  // definitions
  val argDef :P[ArgDef] = P( ws ~ ident ~ optTypeRef ).map((ArgDef.apply _).tupled)

  val funArgs :P[Seq[ArgDef]] = P( ("(" ~ argDef.rep(sep=",") ~ ws ~ ")") )
  val optFunArgs = P( funArgs.?.map(_ getOrElse Seq()) )
  val funDef :P[FunDef] = P(
    Key("fun") ~ ws1 ~/ ident ~ ws ~ optFunArgs ~ optTypeRef ~ ws ~ "=" ~/ ws ~ expr
  ).map((FunDef.apply _).tupled)

  val binding = P( ws ~ ident ~ optTypeRef ~ ws ~ "=" ~ expr ).map((Binding.apply _).tupled)
  val letDef = P( Key("let") ~ ws1 ~/ binding.rep(sep=",") ).map(LetDef)
  val varDef = P( Key("var") ~ ws1 ~/ binding.rep(sep=",") ).map(VarDef)

  val defs = P( (ws ~ (funDef | letDef | varDef)).rep )

  // patterns
  val identPat = P( ident ).map(IdentPat)
  val literalPat = P( number ).map(LiteralPat)
  val destructPat = P( ident ~ "[" ~ pattern.rep(sep=",") ~ "]" ).
    map((DestructPat.apply _).tupled)
  val pattern :P[Pattern] = P( identPat | literalPat | destructPat )

  // expressions
  def op (glyph :String) :P[Sym] = ws ~ glyph.!.map(Sym)
  def binOp (term: P[Expr], op: P[Sym]) = (ws ~ term ~ (op ~ ws ~ term).rep).map {
    case (lhs, chunks) => chunks.foldLeft(lhs) { case (lhs, (op, rhs)) => BinOp(op, lhs, rhs) }
  }

  val literalExpr = number.map(Literal)
  val identExpr = P( ident ).map(IdentRef)

  val parenExpr = P( "(" ~/ expr.rep(sep=",") ~ ")" ).map {
    case Seq(expr) => expr
    case exps      => Tuple(exps)
  }

  val blockExpr = P( "{" ~/ defs ~ stmts.rep(1) ~ ws ~ "}" ).map((Block.apply _).tupled)

  val selectSuff = ("." ~ ident)
  val applySuff = P( "(" ~ expr.rep(sep=",") ~ ws ~ ")" )

  val atomExpr = P( (literalExpr | identExpr | parenExpr) ~ selectSuff.? ~ applySuff.?).map {
    case (expr, None,        None) => expr
    case (expr, Some(ident), None) => Select(expr, ident)
    case (expr, None,        Some(args)) => FunApply(expr, args)
    case (expr, Some(ident), Some(args)) => FunApply(Select(expr, ident), args)
  }

  val unaryExpr :P[UnOp] = P( (op("+") | op("-") | op("~") | op("!")) ~ atomExpr ).
    map((UnOp.apply _).tupled)

  val termExpr = P( atomExpr | unaryExpr )
  val multiExpr = P( binOp(termExpr, op("*") | op("/") | op("%")) )
  val addExpr = P( binOp(multiExpr, op("+") | op("-")) )

  val bitshiftExpr = P( binOp(addExpr, op("<<") | op(">>")) )
  val bitAndExpr = P( binOp(bitshiftExpr, op("&")) )
  val bitXorExpr = P( binOp(bitAndExpr, op("^")) )
  val bitOrExpr = P( binOp(bitXorExpr, op("|")) )

  val equalExpr = P( binOp(bitOrExpr, op("==") | op("!=")) )
  val relExpr = P( binOp(equalExpr, op("<=") | op(">=") | op("<") | op(">")) )
  val logAndExpr = P( binOp(relExpr, op("&&")) )
  val logOrExpr = P( binOp(logAndExpr, op("||")) )
  // TODO: ::?
  // TODO: elvis op?

  // the top of the precedence chain for pure unary & binary op expressions
  val pureOpExpr = P( logOrExpr )

  val bareArg = P( argDef ).map(ad => Seq(ad))
  val lambdaExpr = P( (funArgs | bareArg) ~ hs ~ "=>" ~ expr ).map((Lambda.apply _).tupled)

  val ifExpr = P( Key("if") ~ expr ~ expr ~ ws ~ Key("else") ~ expr ).map((If.apply _).tupled)
  // TODO: if let?

  val caseClause = P( ws ~ (Key("case") ~ ws ~ pattern ~ ws ~ "=" ~ expr) ).
    map(data => Case(data._1, None, data._2))
  val matchExpr = P( Key("match") ~ expr ~ caseClause.rep(1) ).map((Match.apply _).tupled)

  val condEnd = P( hs ~ newline )
  val condClause = P( expr ~ hs ~ "=" ~ expr ~ condEnd ).map((Condition.apply _).tupled)
  val elseClause = P( ws ~ Key("else") ~ hs ~ "=" ~ expr ~ condEnd )
  val condExpr = P( Key("cond") ~ condClause.rep(1) ~ elseClause ).map((Cond.apply _).tupled)

  val generator = P( ident ~ hs ~ "<-" ~ expr ).map((Generator.apply _).tupled)
  val compClause :P[CompClause] = P( ws ~ (generator | expr.map(Filter)) )
  val compExpr = P( expr ~ hs ~ Key("where") ~ compClause.rep(sep = ",") ).
    map((MonadComp.apply _).tupled)

  val arrayLiteral = P( expr.rep(sep=",") ).map(ArrayLiteral)
  // TODO: .. and ... ranges?
  val bracketExpr = P( "[" ~ ( compExpr | arrayLiteral ) ~ ws ~ "]" )

  val pureExpr = lambdaExpr | pureOpExpr | blockExpr | bracketExpr | ifExpr | matchExpr | condExpr
  val expr :P[Expr] = P( ws ~ pureExpr )

  // side effecting operations
  val assignEffect = P( ident ~ hs ~ "=" ~ ws ~ expr ).map((Assign.apply _).tupled)
  // TODO: assign ops (+= etc.)?

  val whileEffect = P( Key("while") ~ expr ~ expr ).map((While.apply _).tupled)
  val doWhileEffect = P( Key("do") ~ expr ~ ws ~ Key("while") ~ expr ).map((DoWhile.apply _).tupled)
  val forEffect = P( Key("for") ~ generator.rep(1, sep=",") ~ expr ).map((For.apply _).tupled)

  val effects = (assignEffect | whileEffect | doWhileEffect | forEffect )

  // "statements" are either effects (assignments, etc.) or expressions
  val stmts :P[Expr] = P( ws ~ (effects | pureExpr) )

  val program :P[Expr] = P( defs ~ stmts.rep ).map((Block.apply _).tupled)

  def trace[A <: Expr] (p :fastparse.all.P[A], id :String) =
    p.map(e => { AST.printExpr(e) ; e }).log(id)
}
