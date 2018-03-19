//
// Compose Mark 0 compiler
// https://github.com/samskivert/compose/c0/LICENSE

package compose

import java.util.HashMap
import scala.collection.mutable.ArrayBuffer

object Lower {
  import Constants._
  import Names._
  import Printing._
  import Types._
  import compose.{Symbols => hsym, Trees => high}

  // Lowering maps high symbols to low symbols (when it creates a low def tree from a high def
  // tree) and it also creates new low symbols when it hoists sub-expressions out into bindings.

  class Symbol (val name :Name, val sig :Type) {
    private[this] var _tree :DefTree = _

    def tree :DefTree =
      if (_tree == null) fail(s"No tree assigned to $this")
      else _tree

    def setTree (tree :DefTree) :Unit =
      if (_tree == null) _tree = tree
      else fail(s"Symbol already has assigned tree: $this")

    override def toString = s"$name :: {_tree}"
    override def hashCode = System.identityHashCode(this)
    override def equals (that: Any) = this eq that.asInstanceOf[AnyRef]
  }

  // Lowered trees
  sealed abstract class Tree
  sealed trait ExprTree extends Tree
  sealed trait StmtTree extends Tree
  sealed trait DefTree extends StmtTree

  abstract class Block { val stmts :Seq[StmtTree] }

  // defs
  case class LetDef (ident :Symbol, value :ExprTree) extends DefTree
  case class VarDef (ident :Symbol, value :ExprTree) extends DefTree
  case class BareVarDef (ident :Symbol) extends DefTree
  case class FunDef (ident :Symbol, dicts :Seq[Symbol], args :Seq[Symbol],
                     body :Block) extends DefTree
  case class RecordDef (ident :Symbol, fields :Seq[Symbol]) extends DefTree
  case class UnionDef (ident :Symbol, cases :Seq[Symbol]) extends DefTree

  // expressions
  case class Literal (const :Constant) extends ExprTree
  case class IdentRef (sym :Symbol) extends ExprTree
  case class Select (expr :ExprTree, field :Symbol) extends ExprTree
  case class Index (array :Symbol, index :Symbol) extends ExprTree
  case class Lambda (args :Seq[Symbol], body :Block) extends ExprTree
  case class Apply (fun :Symbol, dicts :Seq[Symbol], args :Seq[Symbol]) extends ExprTree

  // statements
  case class Assign (lhs :Symbol, value :ExprTree) extends StmtTree
  case class Return (value :ExprTree) extends StmtTree
  case class If (cond :Symbol, ifTrue :Block, ifFalse :Block) extends StmtTree
  case class While (cond :ExprTree, body :Block) extends StmtTree
  case class DoWhile (body :Block, cond :ExprTree) extends StmtTree

  val UnitExpr = Literal(Unit)

  def lower (tree :high.Tree) :Block = {
    implicit val ctx = new Context()
    val block = new MutableBlock()
    lower(tree, block, false)
    block
  }

  def print (tree :Tree) :Unit = {
    printTree(tree)(sysPrint)
    sysOut.println()
    sysOut.flush()
  }

  private class MutableBlock extends Block {
    val stmts = ArrayBuffer[StmtTree]()
    def += (stmt :StmtTree) :Unit = stmts += stmt
  }

  private class Context {
    private val syms = new HashMap[hsym.Symbol, Symbol]()
    private var nextIdent = 1

    /** Returns the low sym for high sym `source`, creating one if necessary. */
    def sym (source :hsym.Symbol) :Symbol = {
      val low = syms.get(source)
      if (low != null) low else {
        val newlow = new Symbol(source.name, source.info)
        syms.put(source, newlow)
        newlow
      }
    }

    def freshIdent (tpe :Type) :Symbol =
      try new Symbol(termName(s"$$$nextIdent"), tpe)
      finally nextIdent += 1
  }

  private def hoist (block :MutableBlock)(expr :high.TermTree)
                    (implicit ctx :Context) :Symbol = lower(expr, block, true) match {
    case IdentRef(sym) => sym
    case value =>
      val argSym = ctx.freshIdent(expr.tpe)
      block += LetDef(argSym, value)
      argSym
  }

  private def lower (tree :high.Tree, block :MutableBlock, asExpr :Boolean)
                    (implicit ctx :Context) :ExprTree = tree match {
    case high.OmittedBody =>
      ???
    case high.Literal(const) =>
      Literal(const)
    case high.ArrayLiteral(values) =>
      ???
    case tree :high.IdentRef =>
      IdentRef(ctx.sym(tree.sym))
    case tree @ high.Select(expr, field) => tree.sym.info match {
      // TODO: whence the dictionary args
      case arr :Arrow => Apply(ctx.sym(arr.sym), Seq(), Seq(hoist(block)(expr)))
      case fld :Field => Select(lower(expr, block, true), ctx.sym(fld.sym))
      case tpe        => fail(s"Unexpected field type: $tpe (of $tree)")
    }
    case high.Index(expr, index) =>
      Index(hoist(block)(expr), hoist(block)(index))
    case high.Tuple(exprs) =>
      ???
    case high.Lambda(args, body) =>
      ???
    case tree @ high.FunApply(kind, fun, params, args) =>
      // TODO: dictionary args
      Apply(hoist(block)(fun), Seq(), args map hoist(block))
    case high.If(cond, ifTrue, ifFalse) =>
      val condSym = hoist(block)(cond)
      if (asExpr) {
        val resultSym = ctx.freshIdent(tree.tpe)
        block += BareVarDef(resultSym)
        val trueBlock = new MutableBlock()
        trueBlock += Assign(resultSym, lower(ifTrue, trueBlock, true))
        val falseBlock = new MutableBlock()
        falseBlock += Assign(resultSym, lower(ifFalse, falseBlock, true))
        block += If(condSym, trueBlock, falseBlock)
        IdentRef(resultSym)
      } else {
        val trueBlock = new MutableBlock()
        lower(ifTrue, trueBlock, false)
        val falseBlock = new MutableBlock()
        lower(ifFalse, falseBlock, false)
        block += If(condSym, trueBlock, falseBlock)
        UnitExpr
      }
    case high.Match(cond, cases) =>
      ???
    case high.Cond(conds, elseResult) =>
      if (asExpr) {
        val resultSym = ctx.freshIdent(tree.tpe)
        block += BareVarDef(resultSym)
        def loop (block :MutableBlock, conds :Seq[high.Condition]) :Unit = {
          val cond = conds.head
          val condSym = hoist(block)(cond.guard)
          val condBlock = new MutableBlock()
          condBlock += Assign(resultSym, lower(cond.result, condBlock, true))
          val elseBlock = new MutableBlock()
          if (conds.size == 1) elseBlock += Assign(resultSym, lower(elseResult, elseBlock, true))
          else loop(elseBlock, conds.tail)
          block += If(condSym, condBlock, elseBlock)
        }
        loop(block, conds)
        IdentRef(resultSym)
      } else {
        def loop (block :MutableBlock, conds :Seq[high.Condition]) :Unit = {
          val cond = conds.head
          val condSym = hoist(block)(cond.guard)
          val condBlock = new MutableBlock()
          lower(cond.result, condBlock, false)
          val elseBlock = new MutableBlock()
          if (conds.size == 1) lower(elseResult, elseBlock, false)
          else loop(elseBlock, conds.tail)
          block += If(condSym, condBlock, elseBlock)
        }
        loop(block, conds)
        UnitExpr
      }
    case high.MonadComp(elem, clauses) =>
      ???
    case high.DefExpr(df) =>
      lowerDef(df, block)
    case high.Block(exprs) =>
      if (asExpr) {
        val resultSym = ctx.freshIdent(tree.tpe)
        block += BareVarDef(resultSym)
        // TODO: we need to rename shadowed vars...
        exprs.take(exprs.size-1) foreach { expr => lower(expr, block, false) }
        block += Assign(resultSym, lower(exprs.last, block, true))
        IdentRef(resultSym)
      } else {
        // TODO: we need to rename shadowed vars...
        exprs.foreach { expr => lower(expr, block, false) }
        UnitExpr
      }
    case tree @ high.Assign(ident, value) =>
      block += Assign(ctx.sym(tree.sym), lower(value, block, true))
      UnitExpr
    case high.While(cond, body) =>
      ???
    case high.DoWhile(body, cond) =>
      ???
    case high.For(gens, body) =>
      ???
  }

  private def lowerDef (tree :high.DefTree, block :MutableBlock)
                       (implicit ctx :Context) :ExprTree = {
    def lowerBinding (ctor :(Symbol, ExprTree) => DefTree)(bind :high.Binding) :Unit = {
      block += ctor(ctx.sym(bind.sym), lower(bind.value, block, true))
    }
    tree match {
      case high.FunDef(docs, name, params, csts, args, result, body) =>
        val bodyBlock = new MutableBlock()
        bodyBlock += Return(lower(body, bodyBlock, true))
        // TODO: extract closed over values and pass them as arguments
        block += FunDef(ctx.sym(tree.sym), csts.map(_.sym).map(ctx.sym),
                        args.map(_.sym).map(ctx.sym), bodyBlock)
      case high.LetDef(binds) =>
        binds foreach lowerBinding(LetDef)
      case high.VarDef(binds) =>
        binds foreach lowerBinding(VarDef)
      case high.RecordDef(docs, name, args, fields) =>
        block += RecordDef(ctx.sym(tree.sym), fields.map(_.sym).map(ctx.sym))
      case high.UnionDef(docs, name, args, cases) =>
        ???
      case high.FaceDef(docs, name, params, parents, meths) =>
        // TODO: extract default methods into standalone funs
      case high.MethodBinding(meth, fun) =>
        ???
      case high.ImplDef(docs, name, params, csts, parent, binds) =>
        ???
      case _ => fail("unreachable")
    }
    UnitExpr
  }

  private def printSym (sym :Symbol)(implicit pr :Printer) :Unit = pr.print(sym.name)
  private def printBlock (block :Block)(implicit pr :Printer) :Unit = {
    pr.println("{")
    block.stmts.foreach { stmt => printTree(stmt)(pr.nest.printIndent()) ; pr.println() }
    pr.printIndent("}")
  }
  private def printTree (tree :Tree)(implicit pr :Printer) :Unit = tree match {
    case LetDef(ident, value) =>
      pr.print("let ", ident.name) ; pr.print(" = ") ; printTree(value)
    case VarDef(ident, value) =>
      pr.print("var ", ident.name) ; pr.print(" = ") ; printTree(value)
    case BareVarDef(ident) =>
      pr.print("var ", ident.name)
    case FunDef(ident, dicts, args, body) =>
      pr.print("fun ", ident.name) ; printSep(dicts, printSym, Square)
      printSep(args, printSym, Paren) ; pr.print(" ") ; printBlock(body)
    case RecordDef(ident, fields) =>
      // TODO
    case UnionDef(ident, cases) =>
      // TODO
    case Literal(const) =>
      pr.print(const)
    case IdentRef(sym) =>
      printSym(sym)
    case Select(expr, field) =>
      printTree(expr) ; pr.print(".") ; printSym(field)
    case Index(array, index) =>
      pr.print(array.name, "[", index.name, "]")
    case Lambda(args, body) =>
      printSep(args, printSym, Paren) ; pr.print(" => ") ; printBlock(body)
    case Apply(fun, dicts, args) =>
      printSym(fun) ; printSep(dicts, printSym, Square) ; printSep(args, printSym, Paren)
    case Assign(lhs, value) =>
      printSym(lhs) ; pr.print(" = ") ; printTree(value)
    case Return(value) =>
      pr.print("return ") ; printTree(value)
    case If(cond, ifTrue, ifFalse) =>
      pr.print("if (") ; printSym(cond) ; pr.print(") ") ; printBlock(ifTrue)
      pr.print(" else ") ; printBlock(ifFalse)
    case While(cond, body) =>
      pr.print("while (") ; printTree(cond) ; pr.print(") ") ; printBlock(body)
    case DoWhile(body, cond) =>
      pr.print("do ") ; printBlock(body) ; pr.print(" while (") ; printTree(cond) ; pr.print(")")
  }

  private def fail (msg :String) :Nothing = throw new AssertionError(msg)
}
