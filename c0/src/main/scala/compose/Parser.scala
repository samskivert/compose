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
  val comment = P( "//" ~ !"/" ~ CharsWhile(_ != '\n').? )
  val hs = P( hspace.rep )
  val ws = P( ( hspace | newline | comment ).rep )

  // Constants
  val BoolConst = P( "false" | "true" ).!
  val IntConst = P( (HexNum | DecNum) ~ CharIn("Ll").? ).!
  val FloatConst = {
    def Thing = P( DecNum ~ Exp.? ~ FloatType.? )
    def Thing2 = P( "." ~ Thing | Exp ~ FloatType.? | Exp.? ~ FloatType )
    P( "." ~ Thing | DecNum ~ Thing2 )
  }.!
  val CharConst :P[String] = P( "'" ~ ( UnicodeEscape | CharEscape | AnyChar ).! ~ "'" )
  val StringConst :P[String] = P( TripleQuoteString | QuoteString )
  val RawStringConst :P[String] = P( TripleTickString | TickString )

  // Keywords & identifiers
  def mkKeyP (s: String) = s ~ !IdentCont
  val Key = Map() ++ Seq(
    "case", "cond", "data", "do", "else", "for", "fun", "if", "interface", "impl", "let", "match",
    "var", "where", "while"
  ).map(id => (id, mkKeyP(id)))

  val Name = P( IdentStart ~ IdentCont.rep ).!.filter(id => !Key.contains(id))
}

object Parser {
  import Lexer._
  import Trees._
  import Names._
  import Constants._

  val constant :P[Constant] = (
    BoolConst.map(Constants.bool) |
    FloatConst.map(Constants.float) |
    IntConst.map(Constants.int) |
    CharConst.map(Constants.char) |
    StringConst.map(Constants.string) |
    RawStringConst.map(Constants.rawString) )
  val arrayLiteral = P( expr.rep(sep=",") ).map(ArrayLiteral)
  val ident :P[TermName] = P( Name ).map(termName)
  val typeIdent :P[TypeName] = P( ident.map(_.toTypeName) )

  // types
  val typeApplySuff = P( "[" ~ (ws ~ typeRef).rep(1, ",") ~ ws ~ "]" )
  val namedOrApply :P[TypeTree] = P( typeIdent ~ typeApplySuff.? ).map {
    case (ident, None) => TypeRef(ident)
    case (ident, Some(args)) => TypeApply(ident, args)
  }
  val funTypeSuff = P( hs ~ "=> " ~ hs ~ typeRef )
  val namedOrApplyOrFun :P[TypeTree] = P( namedOrApply ~ funTypeSuff.? ).map {
    case (pref, None) => pref
    case (pref, Some(ret)) => TypeArrow(Seq(pref), ret)
  }
  val parenFun = P( "(" ~ (ws ~ typeRef).rep(1, ",") ~ ws ~ ")" ~ hs ~ "=>" ~ hs ~ typeRef ).
    map((TypeArrow.apply _).tupled)
  val typeRef :P[TypeTree] = P( namedOrApplyOrFun | parenFun )
  val optTypeRef = P( (ws ~ ":" ~ typeRef ).? ).map(_ getOrElse OmittedType)

  // comments: doc comments are part of AST, other comments are whitespace
  val docComment = P( hs ~ "///" ~ " ".? ~ CharsWhile(_ != '\n').?.! )
  val docComments = P( ws ~ docComment.rep(sep="\n") ~ ws )

  // definitions
  val simpleConstraint = P( hs ~ ":" ~/ typeIdent )
  val paramApply = P( "[" ~/ (hs ~ typeIdent.map(TypeRef)).rep(1, ",") ~ hs ~ "]" )
  val cstParamDef :P[ParamOrConst] =
    P( ws ~ typeIdent ~ (simpleConstraint | paramApply).? ).map {
      case (name, None) => Param(name)
      case (name, Some(params :Seq[_])) => Constraint(name, params.asInstanceOf[Seq[TypeRef]])
      case (name, Some(ident)) =>
        ConstrainedParam(Param(name), Constraint(ident.asInstanceOf[TypeName], Seq(TypeRef(name))))
    }
  val cstParams = P( "[" ~ cstParamDef.rep(1, sep=",") ~ ws ~ "]" )
  val param = P( ws ~ typeIdent ).map(Param)
  val params = P( "[" ~ param.rep(1, sep=",") ~ ws ~ "]" )
  def optSeq[T] (parser :P[Seq[T]]) = parser.?.map(_ getOrElse Seq())

  val argDef :P[ArgDef] = P( docComments ~ ws ~ ident ~ optTypeRef ).map(ArgDef.tupled)
  val funArgs :P[Seq[ArgDef]] = P( "(" ~ argDef.rep(sep=",") ~ ws ~ ")" )
  val optFunArgs = P( ws ~ funArgs.?.map(_ getOrElse Seq()) )
  val optFunBody = P( (ws ~ "=" ~/ ws ~ expr).?.map(_ getOrElse OmittedBody) )
  val funDef :P[FunDef] = P( docComments ~ ws ~ Key("fun") ~ hs ~/ ident ~ optSeq(cstParams) ~
                             optFunArgs ~ optTypeRef ~ optFunBody).map((FunDef.mk _).tupled)

  val binding = P( ws ~ ident ~ optTypeRef ~ ws ~ "=" ~ expr ).map(Binding.tupled)
  val letDef = P( Key("let") ~ hs ~/ binding.rep(1, sep=",") ).map(LetDef)
  val varDef = P( Key("var") ~ hs ~/ binding.rep(1, sep=",") ).map(VarDef)

  val fieldDef = P( docComments ~ ws ~ ident ~ hs ~ ":" ~ typeRef ).map(FieldDef.tupled)
  val fieldDefs = P( "(" ~/ fieldDef.rep(sep=",") ~ ws ~ ")" )
  val recordDef = P( docComments ~ Key("data") ~ hs ~ typeIdent ~ optSeq(params) ~ ws ~
                    fieldDefs ).map(RecordDef.tupled)

  val caseDef = P( docComments ~ ws ~ typeIdent ~ optSeq(params) ~ ws ~
                   fieldDefs.?.map(_ getOrElse Seq()) ~ ws ).map(RecordDef.tupled)
  val unionDef = P( docComments ~ Key("data") ~ hs ~ typeIdent ~ optSeq(params) ~ ws ~ "=" ~/
                    // TODO: do we want/need to allow unions with a single case?
                    caseDef.rep(2, sep="|") ).map(UnionDef.tupled)

  val parentRef = P( ws ~ typeIdent ~ paramApply ).map(Constraint.tupled)
  val xtends = P( ws ~ ":" ~/ hs ~ parentRef.rep(1, sep=",") )
  val optExtends = P( xtends.? ).map(_ getOrElse Seq())
  val faceDef = P( docComments ~ Key("interface") ~ hs ~/ typeIdent ~ optSeq(params) ~ ws ~
                   optExtends ~ ws ~ "{" ~/ funDef.rep ~ ws ~ "}" ).map(FaceDef.tupled)
  val methBind = P( ws ~ ident ~ hs ~ "=" ~/ hs ~ ident ).map(MethodBinding.tupled)
  val implDef = P( docComments ~ Key("impl") ~ hs ~ ident ~ hs ~ optSeq(cstParams) ~ hs ~ "=" ~/
                  ws ~ typeRef ~ ws ~ "(" ~/ methBind.rep(sep=",") ~ ws ~ ")" ).
    map((ImplDef.mk _).tupled)

  val topLevelDef = P( funDef | unionDef | recordDef | faceDef | implDef ).map(DefExpr)
  val defExpr = P( topLevelDef | letDef | varDef )

  // patterns that start with an upper case letter are 'ref' (ref an existing binding),
  // those that start with a lower case letter are 'def' (define a new binding)
  def isRefPat (nm :TermName) = Character.isUpperCase(nm.toString.charAt(0))

  // patterns
  val letOrIdentPat = P( ident ).map(nm => if (isRefPat(nm)) IdentPat(nm) else LetPat(nm))
  val literalPat = P( constant ).map(LiteralPat)
  val destructPat = P( ident ~ "(" ~/ pattern.rep(sep=",") ~ ")" ).map(DestructPat.tupled)
  val tuplePat = P( "(" ~/ pattern.rep(sep=",") ~ ")" ).
    map(pats => DestructPat(tupleName(pats.size), pats))
  val pattern :P[TermTree] = P( hs ~ (tuplePat | destructPat | letOrIdentPat | literalPat) )

  // expressions
  def op (glyph :String) :P[TermName] = ws ~ glyph.!.map(termName)
  def binOp (term: P[TermTree], op: P[TermName]) = (ws ~ term ~ (op ~ ws ~ term).rep).map {
    case (lhs, chunks) => chunks.foldLeft(lhs) {
      case (lhs, (op, rhs)) => FunApply(FunKind.BinOp, IdentRef(op), Seq(), Seq(lhs, rhs))
    }
  }

  val literalExpr = P( constant ).map(Literal)
  val identExpr = P( ident ).map(IdentRef)

  val parenExpr = P( "(" ~/ expr.rep(sep=",") ~ ws ~ ")" ).map {
    case Seq()     => Literal(Constants.Unit)
    case Seq(expr) => expr
    case exps      => FunApply(FunKind.Normal, IdentRef(tupleName(exps.size)), Seq(), exps)
  }

  val blockExpr = P( "{" ~/ stmt.rep(1) ~ ws ~ "}" ).map(Block)

  // TODO: .. and ... ranges?
  val bracketExpr = P( "[" ~ ( compExpr | arrayLiteral ) ~ ws ~ "]" )

  val selectSuff = P( "." ~ ident ).map(Select(OmittedBody, _))
  val applyParams = P( ("[" ~ typeRef.rep(1, ",") ~ "]").? ).map(_ getOrElse Seq())
  val applySuff = P( applyParams ~ "(" ~ expr.rep(sep=",") ~ ws ~ ")" ).map {
    case (params, args) => FunApply(FunKind.Normal, OmittedBody, params, args)
  }
  val atomSuffs = P( selectSuff | applySuff ).rep
  val atomExpr = P( (literalExpr | identExpr | parenExpr | bracketExpr) ~ atomSuffs).map {
    case (expr, suffs) => suffs.foldLeft(expr)((expr, suff) => suff match {
      case Select(_, ident) => Select(expr, ident)
      case FunApply(kind, _, params, args) => FunApply(kind, expr, params, args)
      case _ => ??? // not reachable, but scalac can't prove it
    })
  }

  val unaryExpr = P( (op("+") | op("-") | op("~") | op("!")) ~ atomExpr ).map {
    case (op, expr) => FunApply(FunKind.UnOp, IdentRef(op), Seq(), Seq(expr))
  }

  val termExpr = P( atomExpr | unaryExpr )
  val indexExpr = P( ws ~ termExpr ~ (op("@") ~ ws ~ termExpr).rep).map {
    case (lhs, chunks) => chunks.foldLeft(lhs) {
      case (lhs, (op, rhs)) => Index(lhs, rhs)
    }
  }
  val multiExpr = P( binOp(indexExpr, op("*") | op("/") | op("%")) )
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
  // TODO: binding if (if let)

  // TODO: change if to 'where'; this will be nicer for guarded binding ifs:
  // if let Foo(bar) where bar > 3 { expr }
  val guardClause = P( hs ~ Key("if") ~ expr ).?
  val caseClause = P( ws ~ (Key("case") ~/ pattern ~ guardClause ~ ws ~ "=" ~ expr) ).
    map(Case.tupled)
  val matchExpr = P( Key("match") ~ expr ~ caseClause.rep(1) ).map(Match.tupled)

  val condEnd = P( hs ~ newline )
  val condClause = P( expr ~ hs ~ "=" ~ expr ~ condEnd ).map(Condition.tupled)
  val elseClause = P( ws ~ Key("else") ~ hs ~ "=" ~ expr ~ condEnd )
  val condExpr = P( Key("cond") ~ condClause.rep(1) ~ elseClause ).map(Cond.tupled)

  val generator = P( ident ~ hs ~ "<-" ~ expr ).map(Generator.tupled)
  val compClause :P[TermTree] = P( ws ~ (generator | expr.map(Filter)) )
  val compExpr = P( expr ~ hs ~ Key("where") ~ compClause.rep(sep = ",") ).map(MonadComp.tupled)

  val pureExpr = P( lambdaExpr | pureOpExpr | blockExpr | ifExpr | matchExpr | condExpr )
  val expr :P[TermTree] = P( ws ~ pureExpr )

  // side effecting operations
  val assignEffect = P( ident ~ hs ~ "=" ~ ws ~ expr ).map(Assign.tupled)
  // TODO: assign ops (+= etc.)?

  val whileEffect = P( Key("while") ~ expr ~ impureExpr ).map(While.tupled)
  val doWhileEffect = P( Key("do") ~ impureExpr ~ ws ~ Key("while") ~ expr ).map(DoWhile.tupled)
  val forEffect = P( Key("for") ~ (ws ~ generator).rep(1, sep=",") ~ impureExpr ).map(For.tupled)

  val effect = (assignEffect | whileEffect | doWhileEffect | forEffect )
  val impureExpr :P[TermTree] = P( ws ~ (effect | pureExpr) )

  // a "statement" is either a def, an effect (assignments, etc.) or an expression
  val stmt :P[TermTree] = P( ws ~ (defExpr | effect | pureExpr) )

  // a program is a sequence of expressions
  val program :P[Seq[TermTree]] = P( stmt.rep ) ~ ws ~ End

  def trace[A] (p :fastparse.all.P[A], id :String) =
    p.map(e => { println(e) ; e }).log(id)

  def traceExpr[A <: TermTree] (p :fastparse.all.P[A], id :String) =
    p.map(e => { Trees.print(e) ; e }).log(id)
}
