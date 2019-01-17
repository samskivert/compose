//
// Compose Mark 0 compiler
// https://github.com/samskivert/compose/c0/LICENSE

package compose

import java.lang.{Character => JCharacter, StringBuilder => JStringBuilder}

object Tokens {
  type Token = Int

  final val EMPTY = 0 ; enter(EMPTY, "empty")
  final val ERROR = 1 ; enter(ERROR, "error")
  final val EOF = 2   ; enter(EOF, "eof")
  final val LIT = 3   ; enter(LIT, "literal")
  final val WORD = 4  ; enter(WORD, "word")
  final val OPER = 5  ; enter(OPER, "oper")

  final val ARROW  = 10 ; enter(ARROW, "->")
  final val EQUALS = 11 ; enter(EQUALS, "=")
  final val COLON  = 12 ; enter(COLON, ":")
  final val FORALL = 13 ; enter(FORALL, "∀")
  final val LPAREN = 14 ; enter(LPAREN, "(")
  final val RPAREN = 15 ; enter(RPAREN, ")")

  final val TOKS = 32

  def tokname (token :Token) :String = names(token)

  private val names = Array.tabulate(TOKS)(ii => s"token:$ii")
  private def enter (token :Token, name :String) :Unit = names(token) = name
}

object Lexers {
  import Tokens._
  import Constants._

  final val LF = '\u000A'
  final val FF = '\u000C'
  final val CR = '\u000D'
  final val SU = '\u001A'

  final def isSpecial (c: Char) :Boolean = {
    val chtp = JCharacter.getType(c)
    chtp == JCharacter.MATH_SYMBOL.toInt || chtp == JCharacter.OTHER_SYMBOL.toInt
  }
  final def isDigit (ch :Char) = (ch >= '0' && ch <= '9')
  final def isHexDigit (ch :Char) =
    (ch >= '0' && ch <= '9') || (ch >= 'a' && ch <= 'f') || (ch >= 'A' && ch <= 'F')
  final def isWhitespace (ch :Char) =
    (ch <= ' ') && (ch == ' ' || ch == '\t' || ch == CR || ch == LF || ch == FF)

  class CharReader (val buf :Array[Char]) {
    var ch :Char = _
    var charOffset :Int = 0
    var lineStartOffset :Int = 0
    nextChar()

    def isAtEnd :Boolean = charOffset >= buf.length
    def getc () :Char = { nextChar() ; ch }

    def nextChar () :Unit = {
      val idx = charOffset
      charOffset = idx + 1
      if (idx >= buf.length) {
        ch = SU
      } else {
        val c = buf(idx)
        ch = c
        if (c < ' ') {
          // skip over the CR in CRLF
          if (ch == CR && charOffset < buf.length && buf(charOffset) == LF) {
            charOffset += 1
            ch = LF
          }
          // make a note of line starts
          if (ch == LF || ch == FF) {
            // lastLineStartOffset = lineStartOffset
            lineStartOffset = charOffset
          }
        }
      }
    }

    def peekChar (n :Int) :Char = {
      val idx = charOffset+n
      if (idx >= buf.length) SU
      else buf(idx)
    }
  }

  class Scanner (buf :Array[Char]) extends CharReader(buf) {
    var token :Token = EMPTY
    var tokenConst :Constant = Unit

    def tokenText = litBuf.toString
    private val litBuf = new JStringBuilder

    def gett () :Token = { nextToken() ; token }

    def nextToken () :Unit = {
      while (isWhitespace(ch)) nextChar()
      litBuf.setLength(0)
      token = scanToken()
    }

    private def scanToken () :Token = {
      println(s"scanToken '$ch'")
      if (isDigit(ch)) scanNumber()
      else if (JCharacter.isLetter(ch)) scanWord()
      else if (ch == '(') scanOp(LPAREN)
      else if (ch == ')') scanOp(RPAREN)
      else if (ch == '=') scanOp(EQUALS)
      else if (ch == ':') scanOp(COLON)
      else if (ch == '∀') scanOp(COLON)
      else if (ch == '→') scanOp(ARROW)
      else if (ch == '-' && peekChar(0) == '>') scanOp(ARROW)
      else if (ch == SU && isAtEnd) EOF
      // TODO: comments
      // TODO: strings
      else scanOper()
    }

    private def scanOp (tok :Token) :Token = { nextChar() ; tok }

    // numbers: 123, 0xFF, 1.4, 123e10, 1.4e-2
    private def scanNumber () :Token = {
      val p0 = peekChar(0) ; val p1 = peekChar(1)
      if (ch == '0' && (p0 == 'x' || p0 == 'X') && isHexDigit(p1)) {
        nextChar()
        nextChar()
        do {
          litBuf.append(ch)
          nextChar()
        } while (isHexDigit(ch))
        tokenConst = hex(tokenText)
        LIT
      } else {
        var isFloat = false
        var isFloatChar = false
        do {
          litBuf.append(ch)
          nextChar()
          isFloatChar = (ch == '.' || ch == 'e' || ch == 'E' || ch == '-')
          if (isFloatChar) isFloat = true
        } while (isFloatChar || isDigit(ch))
        tokenConst = if (isFloat) float(tokenText) else int(tokenText)
        LIT
      }
    }

    private def scanWord () :Token = {
      do {
        litBuf.append(ch)
        nextChar()
      } while (JCharacter.isLetterOrDigit(ch))
      if ("true".contentEquals(litBuf)) { tokenConst = True ; LIT }
      else if ("false".contentEquals(litBuf)) { tokenConst = False ; LIT }
      else WORD
    }

    private def scanOper () :Token = {
      do {
        litBuf.append(ch)
        nextChar()
      } while (!isWhitespace(ch) && !JCharacter.isLetterOrDigit(ch))
      OPER
    }
  }
}

// Grammar:
//
// TermDef := "def" Ident Args = Expr
// Args :=  ArgDef ("->" ArgDef)*
// ArgDef := Ident ":" TypeRef
//
// TODO: TypeDef: prod, sum, alias, etc.
//
// TypeRef := Ident // TODO: tapp, etc.
//
// Expr := LetExpr | CaseExpr | TermExpr
//
// LetExpr := "let" LetBinds "in" Expr
// LetBinds := LetBind (";" LetBind)*
// LetBind := Ident OptArgs? = Expr
// OptArgs :=  OptArgDef ("->" OptArgDef)*
// OptArgDef := Ident (":" TypeRef)?
//
// TODO: CaseExpr
//
// TermExpr := DisjTerm
// DisjTerm := ConjTerm ("||" ConjTerm)*
// ConjTerm := RelTerm ("&&" RelTerm)*
// RelTerm := EqTerm (RelOp EqTerm)*
// RelOp := "<" | ">" | "<=" | ">="
// TODO: bitwise ops?
// EqTerm := AddTerm (EqOp AddTerm)*
// EqOp := "==" | "!=" | "is"
// AddTerm := MulTerm (AddOp MulTerm)*
// AddOp = "+" | "-"
// MulTerm := AppTerm (MulOp AppTerm)*
// MulOp := "*" | "/" | "%"
// AppTerm := PreTerm PreTerm*
// PreTerm := PreOp? IndexTerm
// PreOp := "+" | "-" | "!" // TODO: "~" if we add bitwise ops
// IndexTerm := AtomTerm ("@" AtomTerm)*
// AtomTerm := TermRef | Lit | ("(" Expr ")")
// TermRef := Ident
//
// Ident and Lit are produced by the scanner.

object Parsers {
  import Tokens._
  import Lexers._
  import Trees._
  import Names._
  import Symbols._
  import Constants._

  class ParseException (msg :String) extends Exception(msg)

  var AtomToks = Set(LIT, LPAREN, WORD)
  val PreOp = Set("+", "-", "!") // TODO: "~" if we add bitwise ops
  val MulOp = Set("*", "/", "%")
  val AddOp = Set("+", "-")
  val EqOp  = Set("==", "!=", "is") // TODO: make scanner treat `is` as an op?
  val RelOp = Set("<", ">", "<=", ">=")

  class Parser (scanner :Scanner) {
    def error (msg :String) = throw new ParseException(msg)
    def expectTok (token :Token) = if (scanner.token != token) error(
      s"Expected ${tokname(token)}; got ${tokname(scanner.token)}")
    def consume () = scanner.nextToken()

    def peekOp (op :String)       = scanner.token == OPER && scanner.tokenText == op
    def peekOp (ops :Set[String]) = scanner.token == OPER && ops(scanner.tokenText)

    /// productions
    def ident (tok :Token) = { expectTok(tok) ; try termName(scanner.tokenText) finally consume() }
    def opRef () :TermTree = RefTree(new ParsedSym(ident(OPER)))

    // AtomTerm := TermRef | Lit | ("(" Expr ")")
    def atomTerm () :TermTree = scanner.token match {
      case LIT    => try LitTree(scanner.tokenConst) finally consume()
      case LPAREN => consume() ; val term = expr() ; expectTok(RPAREN) ; term
      case WORD   => RefTree(new ParsedSym(ident(WORD)))
      case tok    => error(s"Unexpected: ${tokname(tok)}; expected name, literal or (expr).")
    }

    // IndexTerm := AtomTerm ("@" AtomTerm)*
    def indexTerm () :TermTree = {
      var term = atomTerm()
      // we don't have an array indexing tree node yet
      // while (peekOp("@")) term = IndexTree(term, atomTerm())
      term
    }
    // PreTerm := PreOp? IndexTerm
    def preTerm () :TermTree = if (!peekOp(PreOp)) indexTerm() else AppTree(opRef(), indexTerm())
    // AppTerm := PreTerm PreTerm*
    def appTerm () :TermTree = {
      var term = preTerm()
      // if the next production will be a preterm, apply it to our fun term
      while (peekOp(PreOp) || AtomToks(scanner.token)) term = AppTree(term, preTerm())
      term
    }
    // MulTerm := AppTerm (MulOp AppTerm)*
    def mulTerm () :TermTree = {
      var term = appTerm()
      while (peekOp(MulOp)) term = AppTree(AppTree(opRef(), term), appTerm())
      term
    }
    // AddTerm := MulTerm (AddOp MulTerm)*
    def addTerm () :TermTree = {
      var term = mulTerm()
      while (peekOp(AddOp)) term = AppTree(AppTree(opRef(), term), mulTerm())
      term
    }
    // EqTerm := AddTerm (EqOp AddTerm)*
    def eqTerm () :TermTree = {
      var term = addTerm()
      while (peekOp(EqOp)) term = AppTree(AppTree(opRef(), term), addTerm())
      term
    }
    // RelTerm := EqTerm (RelOp EqTerm)*
    def relTerm () :TermTree = {
      var term = eqTerm()
      while (peekOp(RelOp)) term = AppTree(AppTree(opRef(), term), eqTerm())
      term
    }
    // ConjTerm := RelTerm ("&&" RelTerm)*
    def conjTerm () :TermTree = {
      var term = relTerm()
      while (peekOp("&&")) term = AppTree(AppTree(opRef(), term), relTerm())
      term
    }
    // DisjTerm := ConjTerm ("||" ConjTerm)*
    def disjTerm () :TermTree = {
      var term = conjTerm()
      while (peekOp("||")) term = AppTree(AppTree(opRef(), term), conjTerm())
      term
    }
    // TermExpr := DisjTerm
    def termExpr () :TermTree = disjTerm()

    def letExpr () :TermTree = ???
    def letBind () :TermTree = ???
    def optArgs () :TermTree = ???
    def optArgDef () :TermTree = ???

    def expr () :TermTree = ???
  }
}
