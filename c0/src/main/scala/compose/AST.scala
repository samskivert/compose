//
// Compose Mark 0 compiler
// https://github.com/samskivert/compose/c0/LICENSE

package compose

import java.io.PrintWriter

object AST {

  case class Sym (name :String) {
    override def toString = name
  }

  sealed trait Type
  case class Named (name :Sym) extends Type {
    override def toString = name.toString
  }
  case class TypeApply (args :Seq[Type]) extends Type

  def typeToString (typ :Option[Type]) = typ.map(tp => s" :$tp").getOrElse("")

  sealed trait Literal
  case class BoolLiteral (value :Boolean) extends Literal {
    override def toString = value.toString
  }
  case class IntLiteral (value :String) extends Literal {
    override def toString = value
  }
  case class FloatLiteral (value :String) extends Literal {
    override def toString = value
  }
  case class CharLiteral (value :String) extends Literal {
    override def toString = s"'$value'"
  }
  case class StringLiteral (value :String) extends Literal {
    override def toString = '"' + value + '"'
  }
  case class RawStringLiteral (value :String) extends Literal {
    override def toString = s"`${value}`"
  }

  //
  // Patterns

  sealed trait Pattern

  // TODO: optional type annotation? (needed for destructuring funargs)
  case class IdentPat (ident :Sym) extends Pattern {
    override def toString = ident.toString
  }
  case class LiteralPat (value :Literal) extends Pattern {
    override def toString = value.toString
  }
  case class DestructPat (ctor :Sym, bindings :Seq[Pattern]) extends Pattern {
    override def toString = s"$ctor(${bindings.mkString(", ")})"
  }
  // TODO: named destructors? (i.e. Node[left @ Node[ll lr], right])

  //
  // Definitions

  sealed trait Def

  // TODO: destructuring argument bindings
  case class ArgDef (name :Sym, typ :Option[Type]) {
    override def toString = s"$name${typeToString(typ)}"
  }
  case class FunDef (name :Sym, args :Seq[ArgDef], rtype :Option[Type], body :Expr) extends Def

  // TODO: destructuring bindings
  case class Binding (name :Sym, typ :Option[Type], value :Expr)
  case class LetDef (bindings :Seq[Binding]) extends Def
  case class VarDef (bindings :Seq[Binding]) extends Def

  case class Ctor (name :Sym, args :Seq[ArgDef])
  case class DataDef (name :Sym, variants :Seq[Ctor]) extends Def

  // case class TypeDef (name :Sym, TODO)

  //
  // Expressions

  sealed trait Expr extends Product

  // pure
  case class Constant (value :Literal) extends Expr
  case class ArrayLiteral (values :Seq[Expr]) extends Expr

  case class UnOp (op :Sym, expr :Expr) extends Expr
  case class BinOp (op :Sym, left :Expr, right :Expr) extends Expr

  case class IdentRef (ident :Sym) extends Expr

  case class Select (expr :Expr, field :Sym) extends Expr

  case class Tuple (exprs :Seq[Expr]) extends Expr

  case class Lambda (args :Seq[ArgDef], body :Expr) extends Expr

  case class FunApply (fun :Expr, args :Seq[Expr]) extends Expr

  case class If (cond :Expr, ifTrue :Expr, ifFalse :Expr) extends Expr
  // TODO: if+let? or maybe "let Ctor(arg) = expr" is a LetExpr which evaluates to true/false?
  // latter might be fiddly due to having to affect the naming environment

  case class Case (pattern :Pattern, guard :Option[Expr], result :Expr)
  case class Match (cond :Expr, cases :Seq[Case]) extends Expr

  case class Condition (guard :Expr, result :Expr)
  case class Cond (conds :Seq[Condition], elseResult :Expr) extends Expr

  sealed trait CompClause
  case class Generator (name :Sym, expr :Expr) extends CompClause
  case class Filter (expr :Expr) extends CompClause
  case class MonadComp (elem :Expr, clauses :Seq[CompClause]) extends Expr

  case class Block (defs :Seq[Def], exprs :Seq[Expr]) extends Expr

  // naughty
  case class Assign (ident :Sym, value :Expr) extends Expr

  case class While (cond :Expr, body :Expr) extends Expr
  case class DoWhile (body :Expr, cond :Expr) extends Expr
  case class For (gens :Seq[Generator], body :Expr) extends Expr

  //
  // Printing, pretty and otherwise

  def printTree (root :Product) :String = {
    import java.lang.{StringBuilder => JStringBuilder}
    val sb = new JStringBuilder

    def print (indent :String, node :Any) :Unit = node match {
      case pnode :Product =>
        sb.append(pnode.productPrefix).append("(")
        val nindent = indent + " "
        pnode.productArity match {
          case 0 => sb.append(")")
          case 1 =>
            print(nindent, pnode.productElement(0))
            sb.append(")")
          case n =>
            sb.append("\n").append(nindent)
            print(nindent, pnode.productIterator)
            sb.append("\n").append(indent).append(")")
        }
      case iter :Iterator[_] =>
        var first = true
        iter.foreach { elem =>
          if (!first) sb.append("\n").append(indent)
          print(indent, elem)
          first = false
        }
      case iter :Iterable[_] =>
        if (iter.isEmpty) sb.append("<empty>")
        else print(indent, iter.iterator)
      case _ => sb.append(node)
    }

    print("", root)
    sb.toString
  }

  def printType (out :PrintWriter, indent :String = "")(typ :Type) :Unit = {
    out.print(typ) // TODO
  }

  def printDef (out :PrintWriter, indent :String = "")(df :Def) :Unit = {
    def printOptType (optType :Option[Type]) = optType.foreach {
      typ => out.print(" :") ; printType(out, indent)(typ) }
    def printBindings (bindings :Seq[Binding]) = {
      var first = true
      bindings foreach { case Binding (name, btype, value) =>
        if (!first) out.print(", ")
        out.print(name) ; printOptType(btype) ; out.print(" = ") ; printExpr(out, indent)(value)
        first = false
      }
    }
    df match {
      case FunDef(name, args, rtype, body) =>
        out.print("fun ") ; out.print(name) ; out.print(args.mkString(" (", ", ", ")"))
        printOptType(rtype) ; out.print(" = ")
        printExpr(out, indent)(body)
      case LetDef(bindings) => out.print("let ") ; printBindings(bindings)
      case VarDef(bindings) => out.print("var ") ; printBindings(bindings)
      case DataDef(name, variants) => out.print(s"$name = ${variants.mkString(" | ")}") // TODO
    }
  }

  def printExpr (out :PrintWriter, indent :String = "")(expr :Expr) :Unit = {
    def nindent = indent + " "
    def print0 (expr :Expr) = printExpr(out, indent)(expr)
    def print1 (expr :Expr) = printExpr(out, nindent)(expr)
    def printSep[T] (sep :String, ts :Seq[T], printT :T => Unit) = {
      var first = true
      ts foreach { t =>
        if (!first) out.print(sep)
        printT(t)
        first = false
      }
    }
    def printTuple (exprs :Seq[Expr]) = {
      out.print("(") ; printSep(", ", exprs, print0) ; out.print(")")
    }

    expr match {
      case Constant(value) => out.print(value)
      case ArrayLiteral(values) =>
        out.print("[") ; printSep(", ", values, print0) ; out.print("]")
      case UnOp (op, expr) => out.print(op) ; print0(expr)
      case BinOp (op, left, right) =>
        out.print("(") ; print0(left)
        out.print(" ") ; out.print(op)
        out.print(" ") ; print0(right) ; out.print(")")
      case IdentRef (ident) => out.print(ident)
      case Select (lhs, field) => print0(lhs) ; out.print(".") ; out.print(field)
      case Tuple (exprs) => printTuple(exprs)
      case Lambda (args, body) =>
        if (args.size == 1) out.print(args(0))
        else out.print(args.mkString("(", ", ", ")"))
        out.print(" => ") ; print0(body)
      case FunApply (ident, args) => print0(ident) ; printTuple(args)
      case If (cond, ifTrue, ifFalse) =>
        out.print("if (") ; print0(cond) ; out.print(") ") ; print0(ifTrue)
        out.print(" else ") ; print0(ifFalse)
      case Match (cond, cases) =>
        out.print("match ") ; print0(cond)
        cases foreach { case Case(pattern, guard, result) =>
          out.println() ; out.print(nindent)
          out.print("case ") ; out.print(pattern) ; out.print(" = ") ; print1(result)
        }
      case Cond(conds, elseResult) => out.print("cond")
        conds foreach { case Condition(guard, result) =>
          out.println() ; out.print(nindent)
          print1(guard) ; out.print(" = ") ; print1(result)
        }
        out.println() ; out.print(nindent) ; out.print("else = ") ; print1(elseResult)
      case MonadComp(elem, clauses) =>
        out.print("[") ; print0(elem) ; out.print(" where ")
        printSep[CompClause](", ", clauses, _ match {
          case Generator(name, expr) => out.print(name) ; out.print(" <- ") ; print0(expr)
          case Filter(expr) => print0(expr)
        })
        out.print("]")
      case Block (defs, exprs) =>
        out.print("{")
        def printSep () = { out.println() ; out.print(nindent) }
        defs foreach { df => printSep() ; printDef(out, nindent)(df) }
        exprs foreach { expr => printSep() ; printExpr(out, nindent)(expr) }
        out.println() ; out.print(indent) ; out.print("}")

      case Assign (ident, value) => out.print(ident) ; out.print(" = ") ; print0(value)
      case While (cond, body) => out.print("while ") ; print0(cond) ; out.print(" ") ; print0(body)
      case DoWhile (body, cond) =>
        out.print("do ") ; print0(body)
        out.print(" while ") ; print0(cond)
      case For (gens, body) =>
        out.print("for ") ; printSep[Generator](", ", gens, {
          case Generator(name, expr) => out.print(name) ; out.print(" <- ") ; print0(expr)
        })
        out.print(" ") ; print0(body)
    }
  }

  def printExpr (expr :Expr) :Unit = {
    val out = new PrintWriter(System.out)
    printExpr(out, "")(expr)
    out.println()
    out.flush()
  }
}
