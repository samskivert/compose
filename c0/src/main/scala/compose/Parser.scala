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

  // Strings
  val UnicodeEscape = P( "\\u" ~ HexDigit ~ HexDigit ~ HexDigit ~ HexDigit )
  val CharEscape = P( "\\" ~ AnyChar )
  def string(delimiter: String) = P( delimiter ~ stringelem(delimiter).rep.! ~ delimiter)
  def stringelem(quote: String): P0 = P( stringchar(quote) | CharEscape | !quote ~ quote.take(1)  )
  def stringchar(quote: String): P0 = P( CharsWhile(!s"\\${quote(0)}".contains(_)) )
  val QuoteString = P( "\"" ~ ( CharsWhile(!"\\\n\"".contains(_)) | CharEscape ).rep.! ~ "\"")
  val TickString = P( "`" ~ CharsWhile(_ != '`').! ~ "`" )
  val TripleQuoteString = string("\"\"\"")
  val TripleTickString = string("```")

  // Operators (not currently used)
  val OpChar = P ( CharPred(_ match {
    case '!' | '#' | '%' | '&' | '*' | '+'  | '-' | '/' | ':' |
         '<' | '=' | '>' | '?' | '@' | '\\' | '^' | '|' | '~' => true
    case c => isOtherSymbol(c) || isMathSymbol(c)
  }) )
  val Op = P( OpChar.rep(1) ).!

  // Identifiers
  val Letter = P( CharPred(isLower) | CharPred(isUpper) )
  val IdentStart = P( Letter | "_" )
  val IdentCont = P( Letter | Digit | "_" | "$" )

  // Whitespace
  val newline = P( "\n" | "\r\n" | "\r" | "\f" )
  val hspace = P( " " | "\t" )
  val comment = P( "//" ~ !"/" ~ CharsWhile(_ != '\n') )
  val hs = P( hspace.rep )
  val ws = P( ( hspace | newline | comment ).rep )

  // Literals
  val BoolLit = P( "false" | "true" ).!
  val IntLit = P( (HexNum | DecNum) ~ CharIn("Ll").? ).!
  val FloatLit = {
    def Thing = P( DecNum ~ Exp.? ~ FloatType.? )
    def Thing2 = P( "." ~ Thing | Exp ~ FloatType.? | Exp.? ~ FloatType )
    P( "." ~ Thing | DecNum ~ Thing2 )
  }.!
  val CharLit :P[String] = P( "'" ~ ( UnicodeEscape | CharEscape | AnyChar ).! ~ "'" )
  val StringLit :P[String] = P( TripleQuoteString | QuoteString )
  val RawStringLit :P[String] = P( TripleTickString | TickString )

  // Keywords & identifiers
  def mkKeyP (s: String) = s ~ !IdentCont
  val Key = Map() ++ Seq(
    "let", "var", "fun", "do", "while", "if", "else", "cond", "for", "match", "case", "where"
  ).map(id => (id, mkKeyP(id)))

  val Name = P( IdentStart ~ IdentCont.rep ).!.filter(id => !Key.contains(id))
}

object Parser {
  import Lexer._
  import Trees._
  import Names._

  val literal :P[Literal] = (
    BoolLit.map(_.toBoolean).map(BoolLiteral) |
    FloatLit.map(FloatLiteral) |
    IntLit.map(IntLiteral) |
    CharLit.map(CharLiteral) |
    StringLit.map(StringLiteral) |
    RawStringLit.map(RawStringLiteral) )
  val arrayLiteral = P( expr.rep(sep=",") ).map(ArrayLiteral)
  val ident :P[Name] = P( Name ).map(termName)

  // types
  val typeApplySuff = P( "[" ~ (ws ~ typeRef).rep(1, ",") ~ ws ~ "]" )
  val namedOrApply :P[TypeTree] = P( ident ~ typeApplySuff.? ).map {
    case (ident, None) => Named(ident)
    case (ident, Some(args)) => TypeApply(ident, args)
  }
  val funTypeSuff = P( hs ~ "=> " ~ hs ~ typeRef )
  val namedOrApplyOrFun :P[TypeTree] = P( namedOrApply ~ funTypeSuff.? ).map {
    case (pref, None) => pref
    case (pref, Some(ret)) => Arrow(Seq(pref), ret)
  }
  val parenFun = P( "(" ~ (ws ~ typeRef).rep(1, ",") ~ ws ~ ")" ~ hs ~ "=>" ~ hs ~ typeRef ).
    map((Arrow.apply _).tupled)
  val typeRef :P[TypeTree] = P( namedOrApplyOrFun | parenFun )
  val optTypeRef = P( (ws ~ ":" ~ typeRef ).? )
  val optBounds = P( (ws ~ ":" ~ namedOrApply ).? )

  // comments: doc comments are part of AST, other comments are whitespace
  val docComment = P( "///" ~ " ".? ~ CharsWhile(_ != '\n').! ~ "\n" )
  val docComments = P( hs ~ docComment.rep(sep="\n") )

  // definitions
  val argDef :P[ArgDef] = P( docComments ~ ws ~ ident ~ optTypeRef ).map(ArgDef.tupled)
  val typeArgDef = P( ws ~ ident ~ optBounds ).map(TypeArgDef.tupled)

  val funArgs :P[Seq[ArgDef]] = P( "(" ~ argDef.rep(sep=",") ~ ws ~ ")" )
  val optFunArgs = P( ws ~ funArgs.?.map(_ getOrElse Seq()) )
  val typeArgs = P( "[" ~ typeArgDef.rep(1, sep=",") ~ ws ~ "]" )
  val optTypeArgs = P( hs ~ typeArgs.?.map(_ getOrElse Seq()) )
  val funDef :P[FunDef] = P(docComments ~ ws ~ Key("fun") ~ hs ~/ ident ~ optTypeArgs ~ optFunArgs ~
                            optTypeRef ~ ws ~ "=" ~/ ws ~ expr).map(FunDef.tupled)

  val binding = P( ws ~ ident ~ optTypeRef ~ ws ~ "=" ~ expr ).map(Binding.tupled)
  val letDef = P( Key("let") ~ hs ~/ binding.rep(1, sep=",") ).map(LetDef)
  val varDef = P( Key("var") ~ hs ~/ binding.rep(1, sep=",") ).map(VarDef)

  val defExpr = P( (funDef | letDef | varDef) ).map(DefExpr)

  // patterns
  val identPat = P( ident ).map(IdentPat)
  val literalPat = P( literal ).map(LiteralPat)
  val destructPat = P( ident ~ "[" ~ pattern.rep(sep=",") ~ "]" ).map(DestructPat.tupled)
  val pattern :P[PatTree] = P( identPat | literalPat | destructPat )

  // expressions
  def op (glyph :String) :P[Name] = ws ~ glyph.!.map(termName)
  def binOp (term: P[Expr], op: P[Name]) = (ws ~ term ~ (op ~ ws ~ term).rep).map {
    case (lhs, chunks) => chunks.foldLeft(lhs) { case (lhs, (op, rhs)) => BinOp(op, lhs, rhs) }
  }

  val constExpr = P( literal ).map(Constant)
  val identExpr = P( ident ).map(IdentRef)

  val parenExpr = P( "(" ~/ expr.rep(sep=",") ~ ws ~ ")" ).map {
    case Seq(expr) => expr
    case exps      => Tuple(exps)
  }

  val blockExpr = P( "{" ~/ stmt.rep(1) ~ ws ~ "}" ).map(Block)

  // TODO: .. and ... ranges?
  val bracketExpr = P( "[" ~ ( compExpr | arrayLiteral ) ~ ws ~ "]" )

  val selectSuff = ("." ~ ident)
  val applySuff = P( ("[" ~ typeRef.rep(1, ",") ~ "]").? ~ "(" ~ expr.rep(sep=",") ~ ws ~ ")" )
  val atomSuffs = P( selectSuff | applySuff ).rep
  val atomExpr = P( (constExpr | identExpr | parenExpr | bracketExpr) ~ atomSuffs).map { es =>
    es._2.foldLeft(es._1)((expr, suff) => suff match {
      case ident :Name   => Select(expr, ident)
      case (typeArgs :Option[_], args :Seq[_]) => FunApply(
        expr, (typeArgs getOrElse Seq()).asInstanceOf[Seq[TypeTree]],
        args.asInstanceOf[Seq[Expr]])
    })
  }

  val unaryExpr :P[UnOp] = P( (op("+") | op("-") | op("~") | op("!")) ~ atomExpr ).map(UnOp.tupled)

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
  val lambdaExpr = P( (funArgs | bareArg) ~ hs ~ "=>" ~ expr ).map(Lambda.tupled)

  val ifExpr = P( Key("if") ~ expr ~ expr ~ ws ~ Key("else") ~ expr ).map(If.tupled)
  // TODO: if let?

  val caseClause = P( ws ~ (Key("case") ~ ws ~ pattern ~ ws ~ "=" ~ expr) ).
    map(data => Case(data._1, None, data._2))
  val matchExpr = P( Key("match") ~ expr ~ caseClause.rep(1) ).map(Match.tupled)

  val condEnd = P( hs ~ newline )
  val condClause = P( expr ~ hs ~ "=" ~ expr ~ condEnd ).map(Condition.tupled)
  val elseClause = P( ws ~ Key("else") ~ hs ~ "=" ~ expr ~ condEnd )
  val condExpr = P( Key("cond") ~ condClause.rep(1) ~ elseClause ).map(Cond.tupled)

  val generator = P( ident ~ hs ~ "<-" ~ expr ).map(Generator.tupled)
  val compClause :P[CompClause] = P( ws ~ (generator | expr.map(Filter)) )
  val compExpr = P( expr ~ hs ~ Key("where") ~ compClause.rep(sep = ",") ).map(MonadComp.tupled)

  val pureExpr = lambdaExpr | pureOpExpr | blockExpr | ifExpr | matchExpr | condExpr
  val expr :P[Expr] = P( ws ~ pureExpr )

  // side effecting operations
  val assignEffect = P( ident ~ hs ~ "=" ~ ws ~ expr ).map(Assign.tupled)
  // TODO: assign ops (+= etc.)?

  val whileEffect = P( Key("while") ~ expr ~ impureExpr ).map(While.tupled)
  val doWhileEffect = P( Key("do") ~ impureExpr ~ ws ~ Key("while") ~ expr ).map(DoWhile.tupled)
  val forEffect = P( Key("for") ~ (ws ~ generator).rep(1, sep=",") ~ impureExpr ).map(For.tupled)

  val effect = (assignEffect | whileEffect | doWhileEffect | forEffect )
  val impureExpr :P[Expr] = P( ws ~ (effect | pureExpr) )

  // a "statement" is either a def, an effect (assignments, etc.) or an expression
  val stmt :P[Expr] = P( ws ~ (defExpr | effect | pureExpr) )

  // a program is a sequence of expressions
  val program :P[Seq[Expr]] = P( stmt.rep )

  def trace[A <: Expr] (p :fastparse.all.P[A], id :String) =
    p.map(e => { Trees.printExpr(e) ; e }).log(id)
}
