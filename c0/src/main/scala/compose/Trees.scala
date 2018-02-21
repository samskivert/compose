//
// Compose Mark 0 compiler
// https://github.com/samskivert/compose/c0/LICENSE

package compose

import java.io.PrintWriter

object Trees {
  import Names._
  import Types._
  import Constants._
  import Contexts._

  class UntypedTreeException (tree :Tree) extends RuntimeException {
    override def getMessage :String = s"Type of $tree is not assigned"
  }

  /** A node in the AST. The type parameter reflects whether a type has been assigned to the tree
    * node.
    */
  abstract class Tree extends Cloneable with Product {
    private[this] var treeType :Type = _
    private def setType (tpe :Type) :this.type = { treeType = tpe ; this }

    /** The type  constructor at the root of the tree */
    type ThisTree <: Tree

    /** Returns the type of this tree. Throws an exception if called on an untyped tree. */
    def tpe :Type = {
      if (treeType == null) throw new UntypedTreeException(this)
      treeType
    }

    def isTyped = treeType != null

    /** Returns a typed tree isomorphic to `this` with the given `tpe`. */
    def withType (tpe :Type) :ThisTree = {
      val tree = if (treeType == null || treeType == tpe) this else clone
      tree.asInstanceOf[ThisTree].setType(tpe)
    }

    /** Does this tree represent a type? */
    def isType: Boolean = false

    /** Does this tree represent a term? */
    def isTerm: Boolean = false

    /** Does this tree define a new symbol? */
    def isDef: Boolean = false

    /** Is this a part of a pattern (but not a term or def)? */
    def isPattern: Boolean = false

    /** Is this part of a comprehension expression (but not a term or def)? */
    def isComprehension :Boolean = false
  }

  sealed trait TypeTree extends Tree {
    type ThisTree <: TypeTree
    override def isType = true
  }
  case object OmittedType extends TypeTree {
    override def toString = ""
  }
  case class TypeRef (name :TypeName) extends TypeTree {
    override def toString = name.toString
  }
  case class TypeApply (ctor :TypeName, args :Seq[TypeTree]) extends TypeTree {
    override def toString = s"$ctor${args.mkString("[", ", ", "]")}"
  }
  // TODO: do we need a syntax for universally quantified arrows?
  case class TypeArrow (args :Seq[TypeTree], ret :TypeTree) extends TypeTree {
    override def toString = s"${args.mkString("(", ", ", ")")} => $ret"
  }

  //
  // Definitions

  sealed trait DefTree extends Tree {
    type ThisTree <: DefTree
    override def isDef = true
  }

  // TODO: destructuring fun arg bindings
  case class ArgDef (docs :Seq[String], name :TermName, typ :TypeTree) extends DefTree {
    type ThisTree <: ArgDef
  }
  case class ParamDef (name :TypeName, bound :TypeTree) extends DefTree {
    type ThisTree <: ParamDef
  }
  case class FunDef (docs :Seq[String], name :TermName, params :Seq[ParamDef],
                     args :Seq[ArgDef], result :TypeTree, body :TermTree) extends DefTree {
    type ThisTree <: FunDef
  }

  // TODO: destructuring let/var bindings
  case class Binding (name :TermName, typ :TypeTree, value :TermTree) extends DefTree
  case class LetDef (bindings :Seq[Binding]) extends DefTree
  case class VarDef (bindings :Seq[Binding]) extends DefTree

  case class FieldDef (docs :Seq[String], name :TermName, typ :TypeTree) extends DefTree
  case class RecordDef (docs :Seq[String], name :TypeName, params :Seq[ParamDef],
                        fields :Seq[FieldDef]) extends DefTree

  case class UnionDef (docs :Seq[String], name :TypeName, params :Seq[ParamDef],
                       cases :Seq[DefTree]) extends DefTree

  // case class TypeDef (name :TypeName, TODO)

  //
  // Patterns and conditions

  sealed trait PatTree extends Tree {
    type ThisTree <: PatTree
    override def isPattern = true
  }

  // TODO: optional type annotation? (needed for destructuring funargs)
  case class IdentPat (ident :TermName) extends PatTree {
    override def toString = ident.toString
  }
  case class LiteralPat (const :Constant) extends PatTree {
    override def toString = const.toString
  }
  case class DestructPat (ctor :TermName, bindings :Seq[PatTree]) extends PatTree {
    override def toString = s"$ctor(${bindings.mkString(", ")})"
  }
  // TODO: named destructors? (i.e. Node[left @ Node[ll lr], right])
  case class Case (pattern :PatTree, guard :Option[TermTree], result :TermTree) extends PatTree {
    type ThisTree <: Case
  }
  case class Condition (guard :TermTree, result :TermTree) extends PatTree {
    type ThisTree <: Condition
  }

  //
  // Comprehensions

  sealed trait CompTree extends Tree {
    type ThisTree <: CompTree
    override def isComprehension = true
  }

  case class Generator (name :TermName, expr :TermTree) extends CompTree
  case class Filter (expr :TermTree) extends CompTree

  //
  // Terms / expressions

  sealed trait TermTree extends Tree {
    type ThisTree <: TermTree
    override def isTerm = true
  }

  // pure
  case class Literal (const :Constant) extends TermTree
  case class ArrayLiteral (values :Seq[TermTree]) extends TermTree

  case class IdentRef (ident :TermName) extends TermTree

  case class Select (expr :TermTree, field :TermName) extends TermTree

  case class Tuple (exprs :Seq[TermTree]) extends TermTree

  case class Lambda (args :Seq[ArgDef], body :TermTree) extends TermTree

  final val Normal = 0
  final val UnOp = 1
  final val BinOp = 2
  final val Method = 3
  case class FunApply (kind :Int, fun :TermTree, params :Seq[TypeTree], args :Seq[TermTree]) extends TermTree

  case class If (cond :TermTree, ifTrue :TermTree, ifFalse :TermTree) extends TermTree
  // TODO: if+let? or maybe "let Ctor(arg) = expr" is a LetTermTree which evaluates to true/false?
  // latter might be fiddly due to it being an expr that introduces defs in a scope up the AST

  case class Match (cond :TermTree, cases :Seq[Case]) extends TermTree
  case class Cond (conds :Seq[Condition], elseResult :TermTree) extends TermTree
  case class MonadComp (elem :TermTree, clauses :Seq[CompTree]) extends TermTree

  case class DefExpr (df :DefTree) extends TermTree
  case class Block (exprs :Seq[TermTree]) extends TermTree

  // naughty
  case class Assign (ident :TermName, value :TermTree) extends TermTree

  case class While (cond :TermTree, body :TermTree) extends TermTree
  case class DoWhile (body :TermTree, cond :TermTree) extends TermTree
  case class For (gens :Seq[Generator], body :TermTree) extends TermTree

  //
  // Traversals

  /** Folds an operation over a tree, allowing control over when and whether to recurse. */
  abstract class Accumulator[T] {

    /** Applies an operation to a tree node.
      * The implementation must call `foldOver` to recurse into the node's children. */
    def apply (t :T, tree :Tree)(implicit ctx :Context) :T

    /** Folds this accumulator over a sequence of `trees`. */
    def apply (t :T, trees :Traversable[Tree])(implicit ctx :Context) :T = (t /: trees)(apply)

    /** Folds this accumulator over the children of `tree`. */
    def foldOver (t :T, tree :Tree)(implicit ctx :Context) :T = tree match {
      // type trees
      case OmittedType => t
      case TypeRef(name) => t
      case TypeApply(ctor, args) => apply(t, args)
      case TypeArrow(args, ret) => apply(apply(t, args), ret)

      // def trees
      case ArgDef(docs, name, typ) => apply(t, typ)
      case ParamDef(name, bound) => apply(t, bound)
      case FunDef(docs, name, params, args, result, body) =>
        apply(apply(apply(apply(t, params), args), result), body)
      case Binding (name, typ, value) => apply(apply(t, typ), value)
      case LetDef(bindings) => apply(t, bindings)
      case VarDef(bindings) => apply(t, bindings)
      case FieldDef(docs, name, typ) => apply(t, typ)
      case RecordDef(docs, name, args, fields) => apply(apply(t, args), fields)
      case UnionDef(docs, name, args, cases) => apply(apply(t, args), cases)

      // term trees
      case Literal (const) => t
      case ArrayLiteral (values) => apply(t, values)
      case IdentRef(ident) => t
      case Select (expr, field) => apply(t, expr)
      case Tuple (exprs) => apply(t, exprs)
      case Lambda (args, body) => apply(apply(t, args), body)
      case FunApply (kind, fun, params, args) => apply(apply(apply(t, fun), params), args)
      case If (cond, ifTrue, ifFalse) => apply(apply(apply(t, cond), ifTrue), ifFalse)
      case Match(cond, cases) => apply(apply(t, cond), cases)
      case Cond (conds, elseResult) => apply(apply(t, conds), elseResult)
      case Generator (name, expr) => apply(t, expr)
      case Filter (expr) => apply(t, expr)
      case MonadComp(elem, clauses) => apply(apply(t, elem), clauses)
      case DefExpr(df) => apply(t, df)
      case Block(exprs) => apply(t, exprs)
      case Assign(ident, value) => apply(t, value)
      case While (cond, body) => apply(apply(t, cond), body)
      case DoWhile(body, cond) => apply(apply(t, body), cond)
      case For(gens, body) => apply(apply(t, gens), body)

      // pat trees
      case IdentPat (ident) => t
      case LiteralPat (const) => t
      case DestructPat (ctor, bindings) => apply(t, bindings)
      case Case (pattern, guard, result) => apply(apply(apply(t, pattern), guard), result)
      case Condition (guard, result) => apply(apply(t, guard), result)
    }
  }

  /** Facilitates tree traversals: folds done for side effects. */
  abstract class Traverser extends Accumulator[Unit] {

    /** Traverse the given tree node.
      * The implementation must call `foldOver` to recurse into the node's children. */
    def traverse (tree :Tree)(implicit ctx :Context) :Unit

    /** Traverses the children of `tree`. */
    def foldOver (tree :Tree)(implicit ctx :Context) :Unit = foldOver((), tree)

    override final def apply (t :Unit, tree :Tree)(implicit ctx :Context) = traverse(tree)
  }

  //
  // Printing, pretty and otherwise

  class Printer (out :PrintWriter, indent :String = "") {
    def nest = new Printer(out, indent + " ")
    def print (value :Any) = { out.print(value) ; this }
    def print (v0 :Any, v1 :Any, rest :Any*) = {
      out.print(v0) ; out.print(v1) ; rest.foreach(out.print) ; this
    }
    def println () :this.type = { out.println() ; this }
    def println (values :Any*) :this.type = { values.foreach(out.print) ; println() ; this }
    def printIndent (values :Any*) = { out.print(indent) ; values.foreach(out.print) ; this }
  }

  def printType (typ :TypeTree)(implicit pr :Printer) :Unit = {
    pr.print(typ) // TODO
  }
  def printOptType (typ :TypeTree)(implicit pr :Printer) = typ match {
    case OmittedType => // nothing
    case _ => pr.print(" :") ; printType(typ)
  }

  def printMemberDef (df :DefTree)(implicit pr :Printer) = printDef(df, true)(pr.nest)
  def printDefList (defs :Seq[DefTree], open :String, close :String)(implicit pr :Printer) =
    if (!defs.isEmpty) printSep(defs, printMemberDef, open, ", ", close)

  def printDef (df :DefTree, member :Boolean = false)(implicit pr :Printer) :Printer = {
    df match {
      case Binding(name, typ, value) =>
        pr.print(name) ; printOptType(typ) ; pr.print(" = ") ; printExpr(value)
      case LetDef(bindings) => pr.print("let ") ; printDefList(bindings, "", "")
      case VarDef(bindings) => pr.print("var ") ; printDefList(bindings, "", "")
      case FieldDef(docs, name, typ) =>
        if (!docs.isEmpty) pr.println()
        docs.foreach { doc => pr.printIndent(s"/// $doc").println() }
        if (!docs.isEmpty) pr.printIndent("")
        pr.print(name, " :") ; printType(typ)
      case RecordDef(docs, name, args, fields) =>
        if (!docs.isEmpty) pr.println()
        docs.foreach { doc => pr.printIndent(s"/// $doc").println() }
        if (!docs.isEmpty) pr.printIndent("")
        if (!member) pr.print("data ")
        pr.print(name)
        printDefList(args, "[", "]")
        printDefList(fields, "(", ")")
      case UnionDef(docs, name, args, cases) =>
        docs.foreach { doc => pr.println(s"/// $doc") }
        pr.print("data ", name)
        printDefList(args, "[", "]")
        if (!cases.isEmpty) {
          pr.print(" = ")
          printSep(cases, printMemberDef, "", " | ", "")
        }
      case ArgDef(docs, name, typ) => pr.print(name) ; printOptType(typ)
      case ParamDef(name, bounds) => pr.print(name) ; printOptType(bounds)
      case FunDef(docs, name, params, args, ret, body) =>
        docs.foreach { doc => pr.println(s"/// $doc") }
        pr.print("fun ", name)
        printDefList(params, "[", "]")
        printDefList(args, " (", ")")
        printOptType(ret) ; pr.print(" = ") ; printExpr(body)
    }
    pr
  }

  def printPat (pat :PatTree)(implicit pr :Printer) :Printer = pat match {
    case IdentPat(ident) => pr.print(ident)
    case LiteralPat(const) => pr.print(const)
    case DestructPat(ctor, bindings) =>
      pr.print(ctor) ; printSep(bindings, printPat, "", ", ", "")
    case Case(pattern, guard, result) =>
      pr.printIndent("case ", pattern, " = ") ; printExpr(result)(pr.nest)
    case Condition(guard, result) =>
      pr.printIndent("") ; printExpr(guard) ; pr.print(" = ") ; printExpr(result)(pr.nest)
  }

  def printComp (comp :CompTree)(implicit pr :Printer) :Printer = comp match {
    case Generator(name, expr) => pr.print(name, " <- ") ; printExpr(expr)
    case Filter(expr) => printExpr(expr)
  }

  def printExpr (expr :TermTree)(implicit pr :Printer) :Printer = {
    def print1 (expr :TermTree) = printExpr(expr)(pr.nest)
    def printTuple (exprs :Seq[TermTree]) = printSep(exprs, printExpr, "(", ", ", ")")

    expr match {
      case Literal(const) => pr.print(const)
      case ArrayLiteral(values) => printSep(values, printExpr, "[", ", ", "]")
      case IdentRef (ident) => pr.print(ident)
      case Select(lhs, field) => printExpr(lhs).print(".", field)
      case Tuple(exprs) => printTuple(exprs)
      case Lambda(args, body) =>
        if (args.size == 1) pr.print(args(0))
        else printSep[DefTree](args, printDef(_, false), " (", ", ", ")")
        pr.print(" => ") ; printExpr(body)
      case FunApply(kind, ident, params, args) => kind match {
          case UnOp =>
            printExpr(ident) ; printExpr(args(0))
          case BinOp =>
            pr.print("(") ; printExpr(args(0)).print(" ")
            printExpr(ident).print(" ")
            printExpr(args(1)).print(")")
          case Normal =>
            printExpr(ident)
            if (!params.isEmpty) printSep(params, printType, "[", ", ", "]")
            printTuple(args)
        }
      case If(cond, ifTrue, ifFalse) =>
        pr.print("if (") ; printExpr(cond) ; pr.print(") ") ; printExpr(ifTrue)
        pr.print(" else ") ; printExpr(ifFalse)
      case Match(cond, cases) =>
        pr.print("match ") ; printExpr(cond)
        cases foreach { printPat(_)(pr.println().nest) }
      case Cond(conds, elseResult) => pr.print("cond")
        conds foreach { printPat(_)(pr.println().nest) }
        pr.println().nest.printIndent("else = ") ; print1(elseResult)
      case MonadComp(elem, clauses) =>
        pr.print("[") ; printExpr(elem)
        printSep(clauses, printComp, " where ", ", ", "]")
      case DefExpr(df) => printDef(df)
      case Block(exprs) =>
        pr.print("{")
        def printSep () = pr.println().nest.printIndent()
        exprs foreach { expr => printSep() ; print1(expr) }
        pr.println().printIndent("}")

      case Assign(ident, value) => pr.print(ident, " = ") ; printExpr(value)
      case While(cond, body) =>
        pr.print("while ") ; printExpr(cond).print(" ") ; printExpr(body)
      case DoWhile(body, cond) =>
        pr.print("do ") ; printExpr(body)
        pr.print(" while ") ; printExpr(cond)
      case For(gens, body) =>
        printSep[Generator](gens, {
          case Generator(name, expr) => pr.print(name, " <- ") ; printExpr(expr)
        }, "for ", ", ", " ")
        printExpr(body)
    }
    pr
  }

  def printSep[T] (ts :Seq[T], printT :T => Unit, open :String, sep :String, close :String)
                  (implicit pr :Printer) = {
    var first = true
    pr.print(open)
    ts foreach { t =>
      if (!first) pr.print(sep)
      printT(t)
      first = false
    }
    pr.print(close)
  }

  def showTree (root :Product) :String = {
    import java.lang.{StringBuilder => JStringBuilder}
    val sb = new JStringBuilder

    def show (indent :String, node :Any) :Unit = node match {
      case pnode :Product =>
        sb.append(pnode.productPrefix).append("(")
        val nindent = indent + " "
        pnode.productArity match {
          case 0 => sb.append(")")
          case 1 =>
            show(nindent, pnode.productElement(0))
            sb.append(")")
          case n =>
            sb.append("\n").append(nindent)
            show(nindent, pnode.productIterator)
            sb.append("\n").append(indent).append(")")
        }
      case iter :Iterator[_] =>
        var first = true
        iter.foreach { elem =>
          if (!first) sb.append("\n").append(indent)
          show(indent, elem)
          first = false
        }
      case iter :Iterable[_] =>
        if (iter.isEmpty) sb.append("<empty>")
        else show(indent, iter.iterator)
      case _ => sb.append(node)
    }

    show("", root)
    sb.toString
  }

  def debugTree (out :PrintWriter)(tree :Tree) :Unit = {
    val acc = new Accumulator[Printer]() {
      override def apply (pr :Printer, tree :Tree)(implicit ctx :Context) = {
        def treeType = if (tree.isTyped) tree.tpe else Prim.Missing
        tree match {
          case DefExpr(dt) => apply(pr, dt)
          case FunDef(docs, name, params, args, result, body) =>
            pr.printIndent("fun ", name, " ")
            printDefList(params, "[", "]")(pr)
            printDefList(args, "(", ")")(pr)
            pr.print(" :", result)
            pr.println()
            apply(pr.nest, body)
            pr
          case Literal (const) =>
            pr.printIndent(const).println(" :", treeType)
          case IdentRef(ident) =>
            pr.printIndent(ident).println(" :", treeType)
          case FunApply (kind, fun, params, args) =>
            pr.printIndent("<apply>").println(" :", treeType)
            apply(pr.nest, fun)
            apply(pr.nest, args)
            pr
          case Case (pattern, guard, result) =>
            pr.printIndent("<case>").println(" :", treeType)
            foldOver(pr.nest, tree)
            pr
          case Match(cond, cases) =>
            pr.printIndent("<match>").println(" :", treeType)
            apply(pr.nest, cond)
            apply(pr.nest, cases)
            pr
          case Tuple (exprs) =>
            pr.printIndent(s"<tuple${exprs.size}>").println(" :", treeType)
            apply(pr.nest, exprs)
            pr
          case _ =>
            pr.printIndent(tree).println(" :", treeType)
            foldOver(pr.nest, tree)
            pr
        }
        // tree match {
        //   case TypeRef(name) => pr.print(name)
        //   case TypeApply(ctor, args) => pr.print(out.println()
        //   case TypeArrow(args, ret) => 
        //   case ArgDef(docs, name, typ) => 
        //   case ParamDef(name, bound) => 
        //   case Binding (name, typ, value) => 
        //   case LetDef(bindings) => 
        //   case VarDef(bindings) => 
        //   case FieldDef(docs, name, typ) => 
        //   case RecordDef(docs, name, args, fields) => 
        //   case UnionDef(docs, name, args, cases) => 
        //   case Literal (const) => 
        //   case ArrayLiteral (values) => 
        //   case Select (expr, field) => 
        //   case Lambda (args, body) => 
        //   case If (cond, ifTrue, ifFalse) => 
        //   case Condition (guard, result) => 
        //   case Cond (conds, elseResult) => 
        //   case Generator (name, expr) => 
        //   case Filter (expr) => 
        //   case MonadComp(elem, clauses) => 
        //   case DefExpr(df) => 
        //   case Block(exprs) => 
        //   case Assign(ident, value) => 
        //   case While (cond, body) => 
        //   case DoWhile(body, cond) => 
        //   case For(gens, body) => 
        // }
      }
    }
    acc.apply(new Printer(out), tree)(moduleContext(termName("debugTree")))
    out.flush()
  }

  def print (expr :TermTree) :Unit = {
    val out = new PrintWriter(System.out)
    printExpr(expr)(new Printer(out))
    out.println()
    out.flush()
  }
}
