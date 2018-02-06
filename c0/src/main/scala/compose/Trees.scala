//
// Compose Mark 0 compiler
// https://github.com/samskivert/compose/c0/LICENSE

package compose

import java.io.PrintWriter

object Trees {
  import Names._
  import Types._

  class UntypedTreeException (tree :Tree) extends RuntimeException {
    override def getMessage :String = s"Type of $tree is not assigned"
  }

  /** A node in the AST. The type parameter reflects whether a type has been assigned to the tree
    * node.
    */
  abstract class Tree extends Cloneable {
    private[this] var treeType :Type = _
    private def setType (tpe :Type) :this.type = { treeType = tpe ; this }

    /** The type  constructor at the root of the tree */
    type ThisTree <: Tree

    /** Returns the type of this tree. Throws an exception if called on an untyped tree. */
    def tpe :Type = {
      if (treeType == null) throw new UntypedTreeException(this)
      treeType
    }

    /** Returns a typed tree isomorphic to `this` with the given `tpe`. */
    def withType (tpe :Type) :ThisTree = {
      val tree = if (treeType == null || treeType == tpe) this else clone
      tree.asInstanceOf[ThisTree].setType(tpe)
    }

    /** Does this tree represent a type? */
    def isType: Boolean = false

    /** Does this tree represent a term? */
    def isTerm: Boolean = false

    /** Is this a legal part of a pattern which is not at the same time a term? */
    def isPattern: Boolean = false

    /** Does this tree define a new symbol that is not defined elsewhere? */
    def isDef: Boolean = false
  }

  sealed trait TypeTree extends Tree {
    type ThisTree <: TypeTree
    override def isType = true
  }
  case class Named (name :Name) extends TypeTree {
    override def toString = name.toString
  }
  case class Arrow (args :Seq[TypeTree], ret :TypeTree) extends TypeTree {
    override def toString = s"${args.mkString("(", ", ", ")")} => $ret"
  }
  case class TypeApply (ctor :Name, args :Seq[TypeTree]) extends TypeTree {
    override def toString = s"$ctor${args.mkString("[", ", ", "]")}"
  }

  def typeToString (typ :Option[TypeTree]) = typ.map(tp => s" :$tp").getOrElse("")

  trait TermTree extends Tree {
    type ThisTree <: TermTree
    override def isTerm = true
  }

  sealed trait Literal extends TermTree
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

  sealed trait PatTree extends Tree {
    type ThisTree <: PatTree
    override def isPattern = true
  }

  // TODO: optional type annotation? (needed for destructuring funargs)
  case class IdentPat (ident :Name) extends PatTree {
    override def toString = ident.toString
  }
  case class LiteralPat (value :Literal) extends PatTree {
    override def toString = value.toString
  }
  case class DestructPat (ctor :Name, bindings :Seq[PatTree]) extends PatTree {
    override def toString = s"$ctor(${bindings.mkString(", ")})"
  }
  // TODO: named destructors? (i.e. Node[left @ Node[ll lr], right])

  //
  // Definitions

  sealed trait DefTree extends Tree {
    type ThisTree <: DefTree
    override def isDef = true
  }

  // TODO: destructuring argument bindings
  case class ArgDef (docs :Seq[String], name :Name, typ :Option[TypeTree]) {
    override def toString = s"$name${typeToString(typ)}"
  }
  case class TypeArgDef (name :Name, bound :Option[TypeTree]) { // TODO: separate AST for bounds?
    override def toString = s"$name${typeToString(bound)}"
  }
  case class FunDef (docs :Seq[String], name :Name, typeArgs :Seq[TypeArgDef],
                     args :Seq[ArgDef], retType :Option[TypeTree], body :Expr) extends DefTree

  // TODO: destructuring bindings
  case class Binding (name :Name, typ :Option[TypeTree], value :Expr)
  case class LetDef (bindings :Seq[Binding]) extends DefTree
  case class VarDef (bindings :Seq[Binding]) extends DefTree

  case class Ctor (docs :Seq[String], name :Name, args :Seq[ArgDef])
  case class DataDef (docs :Seq[String], name :Name, variants :Seq[Ctor]) extends DefTree

  // case class TypeDef (name :Name, TODO)

  //
  // Expressions

  sealed trait Expr extends TermTree with Product

  // pure
  case class Constant (value :Literal) extends Expr
  case class ArrayLiteral (values :Seq[Expr]) extends Expr

  case class UnOp (op :Name, expr :Expr) extends Expr
  case class BinOp (op :Name, left :Expr, right :Expr) extends Expr

  case class IdentRef (ident :Name) extends Expr

  case class Select (expr :Expr, field :Name) extends Expr

  case class Tuple (exprs :Seq[Expr]) extends Expr

  case class Lambda (args :Seq[ArgDef], body :Expr) extends Expr

  case class FunApply (fun :Expr, typeArgs :Seq[TypeTree], args :Seq[Expr]) extends Expr

  case class If (cond :Expr, ifTrue :Expr, ifFalse :Expr) extends Expr
  // TODO: if+let? or maybe "let Ctor(arg) = expr" is a LetExpr which evaluates to true/false?
  // latter might be fiddly due to having to affect the naming environment

  case class Case (pattern :PatTree, guard :Option[Expr], result :Expr)
  case class Match (cond :Expr, cases :Seq[Case]) extends Expr

  case class Condition (guard :Expr, result :Expr)
  case class Cond (conds :Seq[Condition], elseResult :Expr) extends Expr

  sealed trait CompClause
  case class Generator (name :Name, expr :Expr) extends CompClause
  case class Filter (expr :Expr) extends CompClause
  case class MonadComp (elem :Expr, clauses :Seq[CompClause]) extends Expr

  case class DefExpr (df :DefTree) extends Expr
  case class Block (exprs :Seq[Expr]) extends Expr

  // naughty
  case class Assign (ident :Name, value :Expr) extends Expr

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

  def printType (out :PrintWriter, indent :String = "")(typ :TypeTree) :Unit = {
    out.print(typ) // TODO
  }

  def printDef (out :PrintWriter, indent :String = "")(df :DefTree) :Unit = {
    def printOptType (optType :Option[TypeTree]) = optType.foreach {
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
      case DataDef(docs, name, variants) =>
        docs.foreach { doc => out.print(s"/// $doc") }
        out.print(s"$name = ${variants.mkString(" | ")}") // TODO
      case FunDef(docs, name, typeArgs, args, ret, body) =>
        docs.foreach { doc => out.println(s"/// $doc") }
        out.print("fun ") ; out.print(name)
        if (!typeArgs.isEmpty) out.print(typeArgs.mkString("[", ", ", "]"))
        out.print(args.mkString(" (", ", ", ")")) ; printOptType(ret) ; out.print(" = ")
        printExpr(out, indent)(body)
      case LetDef(bindings) => out.print("let ") ; printBindings(bindings)
      case VarDef(bindings) => out.print("var ") ; printBindings(bindings)
    }
  }

  def printExpr (out :PrintWriter, indent :String = "")(expr :Expr) :Unit = {
    def nindent = indent + " "
    def print0 (expr :Expr) = printExpr(out, indent)(expr)
    def print1 (expr :Expr) = printExpr(out, nindent)(expr)
    def printSep[T] (ts :Seq[T], printT :T => Unit, open :String, sep :String, close :String) = {
      var first = true
      out.print(open)
      ts foreach { t =>
        if (!first) out.print(sep)
        printT(t)
        first = false
      }
      out.print(close)
    }
    def printTuple (exprs :Seq[Expr]) = printSep(exprs, print0, "(", ", ", ")")

    expr match {
      case Constant(value) => out.print(value)
      case ArrayLiteral(values) => printSep(values, print0, "[", ", ", "]")
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
      case FunApply (ident, typeArgs, args) =>
        print0(ident)
        if (!typeArgs.isEmpty) printSep(typeArgs, printType(out, indent), "[", ", ", "]")
        printTuple(args)
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
        out.print("[") ; print0(elem)
        printSep[CompClause](clauses, _ match {
          case Generator(name, expr) => out.print(name) ; out.print(" <- ") ; print0(expr)
          case Filter(expr) => print0(expr)
        }, " where ", ", ", "]")
      case DefExpr(df) => printDef(out, indent)(df)
      case Block (exprs) =>
        out.print("{")
        def printSep () = { out.println() ; out.print(nindent) }
        exprs foreach { expr => printSep() ; printExpr(out, nindent)(expr) }
        out.println() ; out.print(indent) ; out.print("}")

      case Assign (ident, value) => out.print(ident) ; out.print(" = ") ; print0(value)
      case While (cond, body) => out.print("while ") ; print0(cond) ; out.print(" ") ; print0(body)
      case DoWhile (body, cond) =>
        out.print("do ") ; print0(body)
        out.print(" while ") ; print0(cond)
      case For (gens, body) =>
        printSep[Generator](gens, {
          case Generator(name, expr) => out.print(name) ; out.print(" <- ") ; print0(expr)
        }, "for ", ", ", " ")
        print0(body)
    }
  }

  def printExpr (expr :Expr) :Unit = {
    val out = new PrintWriter(System.out)
    printExpr(out, "")(expr)
    out.println()
    out.flush()
  }
}
