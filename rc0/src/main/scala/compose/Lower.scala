package compose

import java.util.HashMap
import scala.collection.mutable.ArrayBuffer

object Lower {
  import Constants._
  import Names._
  import Printing._
  import Symbols._
  import Types._
  import compose.{Trees => high}

  // Lowering maps high symbols to low symbols (when it creates a low def tree from a high def
  // tree) and it also creates new low symbols when it hoists sub-expressions out into bindings.

  class Symbol (val name :Name, val sig :Type, val exists :Boolean) {
    private[this] var _tree :DefTree = _

    def tree :DefTree =
      if (_tree == null) fail(s"No tree assigned to $this")
      else _tree

    def setTree (tree :DefTree) :Unit =
      if (_tree == null) _tree = tree
      else fail(s"Symbol already has assigned tree: $this")

    override def toString = s"$name :$sig ::${_tree}"
    override def hashCode = System.identityHashCode(this)
    override def equals (that: Any) = this eq that.asInstanceOf[AnyRef]
  }

  // Lowered trees
  sealed abstract class Tree
  sealed trait ExprTree extends Tree
  sealed trait StmtTree extends Tree
  sealed trait DefTree extends StmtTree

  // defs
  case class LetDef (ident :Symbol, mut :Boolean, value :Option[ExprTree]) extends DefTree
  case class RecordDef (ident :Symbol, fields :Seq[Symbol]) extends DefTree
  case class UnionDef (ident :Symbol, cases :Seq[RecordDef]) extends DefTree
  case class FunDef (ident :Symbol, dicts :Seq[Symbol], args :Seq[Symbol],
                     body :StmtTree) extends DefTree

  // expressions
  case class Literal (const :Constant) extends ExprTree
  case class IdentRef (sym :Symbol) extends ExprTree
  case class Select (expr :ExprTree, field :Symbol) extends ExprTree
  case class Index (expr :ExprTree, index :ExprTree) extends ExprTree
  case class Is (expr :ExprTree, what :ExprTree) extends ExprTree
  case class And (left :ExprTree, right :ExprTree) extends ExprTree
  case class Lambda (dicts :Seq[Symbol], args :Seq[Symbol], body :StmtTree) extends ExprTree
  case class Call (fun :ExprTree, dicts :Seq[ExprTree], args :Seq[ExprTree]) extends ExprTree
  case class Construct (obj :Symbol, args :Seq[ExprTree]) extends ExprTree
  case class ForeignExpr (body :String) extends ExprTree

  // statements
  case class Block (stmts :Seq[Tree], label :Option[Int]) extends StmtTree
  case class Assign (lhs :Symbol, value :ExprTree) extends StmtTree
  case class Return (value :ExprTree) extends StmtTree
  case class Break (label :Int) extends StmtTree
  case class Raise (value :ExprTree) extends ExprTree
  case class ExprStmt (expr :ExprTree) extends StmtTree
  case class If (cond :ExprTree, ifTrue :StmtTree) extends StmtTree
  case class IfElse (cond :ExprTree, ifTrue :StmtTree, ifFalse :StmtTree) extends StmtTree
  case class While (cond :ExprTree, body :StmtTree) extends StmtTree
  case class DoWhile (body :StmtTree, cond :ExprTree) extends StmtTree
  case class ForeignBody (body :String) extends StmtTree

  val UnitExpr = Literal(Unit)

  def lower (trees :Seq[high.Tree]) :Seq[StmtTree] = {
    implicit val ctx = new Context()
    val bb = new BlockBuilder()
    trees foreach { tree => lowerTree(tree, bb) }
    bb.toStmts()
  }

  def lower (tree :high.Tree) :Seq[StmtTree] = lower(Seq(tree))

  def print (tree :Tree) :Unit = {
    printTree(tree)(sysPrint)
    sysOut.println()
    sysOut.flush()
  }

  private class BlockBuilder {
    private var stmts = ArrayBuffer[StmtTree]()
    def toStmts () :Seq[StmtTree] = try stmts finally stmts = null
    def build (label :Option[Int] = None, forceBlock :Boolean = false) :StmtTree =
      if (stmts.size == 1 && !forceBlock) stmts.head else Block(toStmts(), label)
    def += (stmt :StmtTree) :Unit = stmts += stmt
  }

  private class Context {
    private val syms = new HashMap[Sym, Symbol]()
    private var nextIdent = 1
    private var nextLabel = 1

    // whether we're lowering a var or a let
    var mut = false

    /** Returns the low sym for high sym `source`, creating one if necessary. */
    def sym (source :Sym) :Symbol = {
      val low = syms.get(source)
      if (low != null) low else {
        val sig = source match {
          case tpe :TypeSym => tpe.sig
          case trm :TermSym => trm.tpe // TODO: want sig from DefTree...
        }
        val newlow = new Symbol(source.name, sig, source.isDefined)
        syms.put(source, newlow)
        newlow
      }
    }

    def freshIdent (tpe :Type) :Symbol =
      try new Symbol(termName(s"$$$nextIdent"), tpe, true)
      finally nextIdent += 1

    def freshLabel () :Int =
      try nextLabel
      finally nextLabel += 1
  }

  private def lowerTree (tree :high.Tree, bb :BlockBuilder)
                        (implicit ctx :Context) :ExprTree = tree match {
    case tt :high.TermTree => lowerTerm(tt, bb, bindStmt)
    case _                 => unreachable(tree)
  }

  private def lowerBind (bind :high.Bind, bb :BlockBuilder, target :BindTarget)
                        (implicit ctx :Context) :ExprTree = {
    val high.Bind(sym, tpe, value) = bind
    val bindSym = ctx.sym(sym)
    if (needsHoist(value)) {
      if (ctx.mut) {
        bb += LetDef(bindSym, true, None)
        lowerTerm(value, bb, bindTo(bindSym))
      } else {
        val hoistSym = ctx.freshIdent(sym.tpe)
        bb += LetDef(hoistSym, true, None)
        lowerTerm(value, bb, bindTo(hoistSym))
        bb += LetDef(bindSym, false, Some(IdentRef(hoistSym)))
      }
    } else {
      bb += LetDef(bindSym, ctx.mut, Some(lowerTerm(value, bb, bindNone)))
    }
    UnitExpr
  }

  private def lowerTerm (tree :high.TermTree, bb :BlockBuilder, target :BindTarget)
                        (implicit ctx :Context) :ExprTree = tree match {
    case high.LetTree(bind, expr) =>
      lowerBind(bind, bb, target)
      lowerTerm(expr, bb, target)

    case high.LetRecTree(binds, expr) =>
      fail("TODO")

    case high.EmptyTermTree =>
      ???
    case high.HoleTree =>
      ???

    case high.LitTree(const) =>
      // TODO: include expected type or manually represent implicit widening in lowered tree
      target.bind(Literal(const), bb)

    case tree :high.URefTree =>
      unreachable(tree)
    case tree :high.RefTree =>
      target.bind(IdentRef(ctx.sym(tree.sym)), bb)

    // TODO: LetRecTree, URefTree
    case high.AllTree(sym, body) =>
      lowerTerm(body, bb, target)
    case high.AscTree(ann, body) =>
      lowerTerm(body, bb, target)

    case tree :high.AbsTree =>
      val (args, body) = tree.uncurry(Nil)
      val bodyStmt = if (needsHoist(body)) {
        val bodyBB = new BlockBuilder()
        lowerTerm(body, bodyBB, bindReturn)
        bodyBB.build(None, true)
      } else ExprStmt(lowerTerm(body, bb, bindNone))
      val dicts = Seq[Symbol]() // TODO: dictionary args
      target.bind(Lambda(dicts, args.map(arg => ctx.sym(arg.sym)), bodyStmt), bb)

    case tree :high.AppTree =>
      // uncurry any nested apps into the inner-most fun + arg list
      val (fun, args) = tree.uncurry(Nil)

      // TODO: generate a partial app lambda term: `foo a b` => `c => foo(a, b, c)`
      if (fun.treeType.arity != args.length) fail(
        s"TODO: support unsaturated calls ${fun} :: ${fun.treeType} @ ${args}")

      val funSym = fun match {
        case ident :high.RefTree =>
          ctx.sym(ident.sym)
        case value =>
          val argSym = ctx.freshIdent(fun.treeType)
          bb += LetDef(argSym, false, None)
          lowerTerm(fun, bb, bindTo(argSym))
          argSym
      }
      def foreignCode = {
        assert(args.length == 1)
        val farg = args.head.asInstanceOf[high.LitTree]
        assert(farg.cnst.tag == StringTag)
        farg.cnst.value
      }

      val callExpr = if (funSym eq ctx.sym(Builtins.foreignSym)) {
        ForeignExpr(foreignCode)
      } else {
        // val funType = fun.treeType.asInstanceOf[Arrow]
        val funExpr = /*if (tree.impl == NoImpl)*/ IdentRef(funSym)
        // TODO: if the impl is static and the method unadapted, inline it
        /*else Select(lowerImpl(tree.impl), ctx.sym(funType.sym))*/
        val implArgs = Seq() // tree.implArgs.map(lowerImpl)
        Call(funExpr, implArgs, lowerHoist(args, bb))
      }
      target.bind(callExpr, bb)

    case high.IfTree(cond, ifTrue, ifFalse) =>
      val condExpr = lowerHoist(cond, bb)
      val trueBB = new BlockBuilder()
      lowerTerm(ifTrue, trueBB, target.enterBlock)
      val falseBB = new BlockBuilder()
      // we return the result of the else block, but either is fine (they are same)
      try lowerTerm(ifFalse, falseBB, target.enterBlock)
      finally bb += IfElse(condExpr, trueBB.build(), falseBB.build())

    case high.TermDefTree(sym, body) =>
      val termSym = ctx.sym(sym)
      if (needsHoist(body)) {
        // TODO: may need to express this as a "block with return value" if we end up needing to
        // generate JS diffrently (i.e. define and call an anonymous nullary function)
        bb += LetDef(ctx.sym(sym), false, None)
        val initBB = new BlockBuilder()
        lowerTerm(body, initBB, bindTo(termSym))
        bb += initBB.build()
      } else {
        bb += LetDef(ctx.sym(sym), false, Some(lowerTerm(body, bb, bindNone)))
      }
      IdentRef(termSym)

    case tree :high.TermSymTree => fail("TODO")
  }

  private abstract class BindTarget {
    def bind (expr :ExprTree, bb :BlockBuilder) :ExprTree
    def enterBlock :BindTarget = this
  }
  private val bindStmt :BindTarget = new BindTarget() {
    def bind (expr :ExprTree, bb :BlockBuilder) :ExprTree = { bb += ExprStmt(expr) ; expr }
  }
  private val bindNone :BindTarget = new BindTarget() {
    def bind (expr :ExprTree, bb :BlockBuilder) :ExprTree = expr
    override def enterBlock = fail("Encountered block in non-binding context.")
  }
  private val bindReturn :BindTarget = new BindTarget() {
    def bind (expr :ExprTree, bb :BlockBuilder) :ExprTree = { bb += Return(expr) ; UnitExpr }
  }
  private def bindFresh (tpe :Type, bb :BlockBuilder)(implicit ctx :Context) :BindTarget = {
    val bindSym = ctx.freshIdent(tpe)
    bb += LetDef(bindSym, true, None)
    bindTo(bindSym)
  }
  private def bindTo (sym :Symbol) :BindTarget = new BindTarget() {
    def bind (expr :ExprTree, bb :BlockBuilder) :ExprTree = {
      bb += Assign(sym, expr)
      IdentRef(sym)
    }
  }

  private def lowerHoist (expr :high.TermTree, bb :BlockBuilder)(implicit ctx :Context) :ExprTree =
    if (needsHoist(expr)) lowerFresh(expr, bb) else lowerTerm(expr, bb, bindNone)

  private def lowerHoist (exprs :Seq[high.TermTree], bb :BlockBuilder)
                         (implicit ctx :Context) :Seq[ExprTree] =
    if (exprs.exists(needsHoist)) {
      exprs map { expr => lowerFresh(expr, bb) }
    } else {
      exprs map { expr => lowerTerm(expr, bb, bindNone) }
    }

  private def lowerFresh (expr :high.TermTree, bb :BlockBuilder)
                         (implicit ctx :Context) :ExprTree = expr match {
    case ident @ high.RefTree(sym) => IdentRef(ctx.sym(ident.sym))
    // TODO: also keep constants inline?
    case expr => lowerTerm(expr, bb, bindFresh(expr.treeType, bb))
  }

  private def needsHoist (tree :high.TermTree) :Boolean = tree match {
    case _ :high.LitTree => false
    case _ :high.RefTree => false
    case _ :high.AbsTree => false
    case high.AscTree(_, body)  => needsHoist(body)
    case high.AppTree(fun, arg) => needsHoist(fun) || needsHoist(arg)
    case _ => true
  }

  private def printSym (sym :Symbol)(implicit pr :Printer) :Unit =
    if (sym.exists) pr.print(sym.name) else pr.print("{!", sym, "!}")

  private def printTree (tree :Tree)(implicit pr :Printer) :Unit = tree match {
    case LetDef(ident, mut, valopt) =>
      pr.print(if (mut) "var " else "let ", ident.name)
      valopt foreach { value => pr.print(" = ") ; printTree(value) }
    case FunDef(ident, dicts, args, body) =>
      pr.print("fun ", ident.name)
      if (!dicts.isEmpty) printSep(dicts, printSym, Square)
      printSep(args, printSym, Paren) ; pr.print(" ") ; printTree(body)
    case RecordDef(ident, fields) =>
      pr.print("data ", ident.name) ; printSep(fields, printSym, Paren)
    case UnionDef(ident, cases) =>
      pr.print("data ", ident.name) ; printSep(cases, printTree, Blank, " | ")
    case Literal(const) =>
      pr.print(const)
    case IdentRef(sym) =>
      printSym(sym)
    case Select(expr, field) =>
      printTree(expr) ; pr.print(".") ; printSym(field)
    case Is(expr, what) =>
      printTree(expr) ; pr.print(" is ") ; printTree(what)
    case And(left, right) =>
      printTree(left) ; pr.print(" && ") ; printTree(right)
    case Index(array, index) =>
      printTree(array) ; pr.print("[") ; printTree(index) ; pr.print("]")
    case Lambda(dicts, args, body) =>
      if (!dicts.isEmpty) printSep(dicts, printSym, Square)
      printSep(args, printSym, Paren) ; pr.print(" => ") ; printTree(body)
    case Call(fun, dicts, args) =>
      printTree(fun)
      if (!dicts.isEmpty) printSep(dicts, printTree, Square)
      printSep(args, printTree, Paren)
    case Construct(obj, args) =>
      printSym(obj) ; printSep(args, printTree, Paren)
    case Block(stmts, labelOpt) =>
      labelOpt foreach { lbl => pr.print(s"L$lbl: ") }
      pr.println("{")
      stmts.foreach { stmt => printTree(stmt)(pr.nest.printIndent()) ; pr.println() }
      pr.printIndent("}")
    case Assign(lhs, value) =>
      printSym(lhs) ; pr.print(" = ") ; printTree(value)
    case Return(value) =>
      pr.print("return ") ; printTree(value)
    case Break(label) =>
      pr.print(s"break L$label")
    case Raise(value) =>
      pr.print("raise ") ; printTree(value)
    case ExprStmt(expr) =>
      printTree(expr)
    case If(cond, ifTrue) =>
      pr.print("if (") ; printTree(cond) ; pr.print(") ") ; printTree(ifTrue)
    case IfElse(cond, ifTrue, ifFalse) =>
      pr.print("if (") ; printTree(cond) ; pr.print(") ") ; printTree(ifTrue)
      pr.println().printIndent("else ") ; printTree(ifFalse)
    case While(cond, body) =>
      pr.print("while (") ; printTree(cond) ; pr.print(") ") ; printTree(body)
    case DoWhile(body, cond) =>
      pr.print("do ") ; printTree(body) ; pr.print(" while (") ; printTree(cond) ; pr.print(")")
    case ForeignExpr(body) =>
      pr.print("foreign(", body, ")")
    case ForeignBody(body) =>
      pr.print("foreign {", body, "}")
  }
}
