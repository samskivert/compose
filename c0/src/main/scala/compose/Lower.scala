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
  import Resolve._
  import Types._
  import compose.{Symbols => hsym, Trees => high}

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

  final val NoSymbol = new Symbol(NoName, Untyped, false)

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
  case class Apply (fun :ExprTree, dicts :Seq[ExprTree], args :Seq[ExprTree]) extends ExprTree
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
    def build (label :Option[Int] = None) :StmtTree =
      if (stmts.size == 1) stmts.head else Block(toStmts, label)
    def += (stmt :StmtTree) :Unit = stmts += stmt
  }

  private class Context {
    private val syms = new HashMap[hsym.Symbol, Symbol]()
    private var nextIdent = 1
    private var nextLabel = 1

    // whether we're lowering a var or a let
    var mut = false

    /** Returns the low sym for high sym `source`, creating one if necessary. */
    def sym (source :hsym.Symbol) :Symbol = {
      val low = syms.get(source)
      if (low != null) low else {
        val newlow = new Symbol(source.name, source.info, source.exists)
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

  private def lowerTerm (tree :high.TermTree, bb :BlockBuilder, target :BindTarget)
                        (implicit ctx :Context) :ExprTree = tree match {
    case tree @ high.Binding(name, tpe, value) =>
      val bindSym = ctx.sym(tree.sym)
      if (needsHoist(value)) {
        if (ctx.mut) {
          bb += LetDef(bindSym, true, None)
          lowerTerm(value, bb, bindTo(bindSym))
        } else {
          val hoistSym = ctx.freshIdent(tree.sym.info)
          bb += LetDef(hoistSym, true, None)
          lowerTerm(value, bb, bindTo(hoistSym))
          bb += LetDef(bindSym, false, Some(IdentRef(hoistSym)))
        }
      } else {
        bb += LetDef(bindSym, ctx.mut, Some(lowerTerm(value, bb, bindNone)))
      }
      UnitExpr
    case high.LetDef(binds) =>
      binds foreach { bind => lowerTerm(bind, bb, target) }
      UnitExpr
    case high.VarDef(binds) =>
      try {
        ctx.mut = true
        binds foreach { bind => lowerTerm(bind, bb, target) }
        UnitExpr
      } finally ctx.mut = false

    case high.OmittedBody =>
      ???
    case high.Literal(const) =>
      // TODO: include expected type or manually represent implicit widening in lowered tree
      target.bind(Literal(const), bb)
    case high.ArrayLiteral(values) =>
      ???
    case tree :high.IdentRef =>
      target.bind(IdentRef(ctx.sym(tree.sym)), bb)
    case tree @ high.Select(expr, field) => target.bind(tree.sym.info match {
      // TODO: whence the dictionary args
      case arr :Arrow => Apply(IdentRef(ctx.sym(arr.sym)), Seq(), Seq(lowerHoist(expr, bb)))
      case fld :Field => Select(lowerHoist(expr, bb), ctx.sym(fld.sym))
      case tpe        => fail(s"Unexpected field type: $tpe (of $tree)")
    }, bb)
    case high.Index(expr, index) =>
      target.bind(Index(lowerHoist(expr, bb), lowerHoist(index, bb)), bb)

    case high.Lambda(args, body) =>
      val bodyStmt = if (needsHoist(body)) {
        val bodyBB = new BlockBuilder()
        lowerTerm(body, bodyBB, bindReturn)
        bodyBB.build()
      } else ExprStmt(lowerTerm(body, bb, bindNone))
      val dicts = Seq[Symbol]() // TODO: dictionary args?
      target.bind(Lambda(dicts, args.map(_.sym).map(ctx.sym), bodyStmt), bb)

    case tree @ high.FunApply(kind, fun, params, args) =>
      val funSym = fun match {
        case ident :high.IdentRef => ctx.sym(ident.sym)
        case value =>
          val argSym = ctx.freshIdent(fun.tpe)
          bb += LetDef(argSym, false, None)
          lowerTerm(fun, bb, bindTo(argSym))
          argSym
      }
      def foreignCode = {
        assert(args.size == 1)
        val arg = args(0).asInstanceOf[high.Literal]
        assert(arg.const.tag == StringTag)
        arg.const.value
      }
      if (funSym == ctx.sym(Prim.foreign)) {
        target.bind(ForeignExpr(foreignCode), bb)
      } else if (funSym == ctx.sym(Prim.foreignBody)) {
        bb += ForeignBody(foreignCode)
        UnitExpr
      } else {
        val funType = fun.tpe.asInstanceOf[Arrow]
        val funExpr = if (tree.impl == NoImpl) IdentRef(funSym)
                      // TODO: if the impl is static and the method unadapted, inline it
                      else Select(lowerImpl(tree.impl), ctx.sym(funType.sym))
        target.bind(Apply(funExpr, tree.implArgs.map(lowerImpl), lowerHoist(args, bb)), bb)
      }

    case high.If(cond, ifTrue, ifFalse) =>
      val condExpr = lowerHoist(cond, bb)
      val trueBB = new BlockBuilder()
      lowerTerm(ifTrue, trueBB, target.enterBlock)
      val falseBB = new BlockBuilder()
      // we return the result of the else block, but either is fine (they are same)
      try lowerTerm(ifFalse, falseBB, target.enterBlock)
      finally bb += IfElse(condExpr, trueBB.build(), falseBB.build())

    case tree :high.Match => lowerMatch(tree, bb, target)

    case high.Cond(conds, elseResult) =>
      def loop (bb :BlockBuilder, conds :Seq[high.Condition]) :ExprTree = {
        val cond = conds.head
        val condExpr = lowerHoist(cond.guard, bb)
        val condBB = new BlockBuilder()
        lowerTerm(cond.result, condBB, target.enterBlock)
        val elseBB = new BlockBuilder()
        try if (conds.size == 1) lowerTerm(elseResult, elseBB, target.enterBlock)
            else loop(elseBB, conds.tail)
        finally bb += IfElse(condExpr, condBB.build(), elseBB.build())
      }
      loop(bb, conds)

    case high.Generator(name, expr) => ???
    case high.Filter(expr) => ???
    case high.MonadComp(elem, clauses) => ???

    case high.DefExpr(df) =>
      lowerDef(df, bb)
    case high.Block(exprs) =>
      val nestBB = new BlockBuilder()
      exprs.take(exprs.size-1) foreach { expr => lowerTerm(expr, nestBB, bindStmt) }
      try lowerTerm(exprs.last, nestBB, target.enterBlock)
      finally bb += nestBB.build()
    case tree @ high.Assign(ident, value) =>
      lowerTerm(value, bb, bindTo(ctx.sym(tree.sym)))
    case high.While(cond, body) =>
      // TODO: this is not correct; if a while's conditional expression contains a nested block,
      // we need to either hoist it into a function or replicate it before the first loop entry
      // and at the end of the loop
      val whileBB = new BlockBuilder()
      lowerTerm(body, whileBB, bindStmt)
      bb += While(lowerHoist(cond, bb), whileBB.build())
      UnitExpr
    case high.DoWhile(body, cond) =>
      // TODO: this is also not correct; it's easier to make do/while support blocks in the
      // condition expression, we just need to declare a variable outside the while body and then
      // execute the condition at the end of the loop and assign the result to that var (note: if
      // we ever get a `continue` statement this would have to change)
      val whileBB = new BlockBuilder()
      lowerTerm(body, whileBB, bindStmt)
      bb += DoWhile(whileBB.build(), lowerHoist(cond, bb))
      UnitExpr
    case high.For(gens, body) =>
      ???

    // handled by ImplDef
    case tree :high.MethodBinding => unreachable(tree)
    // handled by Cond
    case tree :high.Condition => unreachable(tree)
    // handled by lowerMatch
    case tree :high.LetPat => unreachable(tree)
    case tree :high.IdentPat => unreachable(tree)
    case tree :high.LiteralPat => unreachable(tree)
    case tree :high.DestructPat => unreachable(tree)
    case tree :high.Case => unreachable(tree)
  }

  private def lowerImpl (tree :ImplTree)(implicit ctx :Context) :ExprTree = tree match {
    case ImplRef(sym) => IdentRef(ctx.sym(sym))
    case ImplApply(impl, args) => Apply(IdentRef(ctx.sym(impl)), Seq(), args map lowerImpl)
    case _ => unreachable(tree)
  }

  private def lowerDef (tree :high.DefTree, bb :BlockBuilder)
                       (implicit ctx :Context) :ExprTree = {
    def lowerRecord (tree :high.RecordDef) =
      RecordDef(ctx.sym(tree.sym), tree.fields.map(_.sym).map(ctx.sym))
    tree match {
      case high.FunDef(docs, name, params, csts, args, result, body) =>
        val bodyBB = new BlockBuilder()
        lowerTerm(body, bodyBB, bindReturn)
        // TODO: extract closed over values and pass them as arguments
        bb += FunDef(ctx.sym(tree.sym), csts.map(_.sym).map(ctx.sym),
                     args.map(_.sym).map(ctx.sym), bodyBB.build())
      case tree :high.RecordDef =>
        bb += lowerRecord(tree)
      case high.UnionDef(docs, name, args, cases) =>
        bb += UnionDef(ctx.sym(tree.sym), cases map lowerRecord)
      case high.FaceDef(docs, name, params, parents, meths) =>
        // TODO: extract default methods into standalone funs
      case high.ImplDef(docs, name, params, csts, parent, binds) =>
        val implSym = ctx.sym(tree.sym)
        val Record(faceSym, _, fieldTypes) = tree.tpe
        def ctorTree = Construct(
          ctx.sym(faceSym),
          fieldTypes.map(f => lowerMethod(binds.find(_.meth == f.name).get, f)))
        // if the impl has no constraints, it's lowered into a dictionary value
        if (csts.isEmpty) bb += LetDef(implSym, false, Some(ctorTree))
        // otherwise it's lowered into a function from constraints to dictionary
        else {
          val bodyBB = new BlockBuilder()
          bodyBB += Return(ctorTree)
          bb += FunDef(implSym, csts.map(_.sym).map(ctx.sym), Seq(), bodyBB.build())
        }
      case _ => unreachable(tree)
    }
    UnitExpr
  }

  private def lowerMethod (bind :high.MethodBinding, field :Field)
                          (implicit ctx :Context) :ExprTree = {
    val boundFun = IdentRef(ctx.sym(bind.sym))
    // if we require no dictionaries, lower directly to the bound fun
    if (bind.impls.isEmpty) boundFun
    // otherwise lower to a lambda which closes over the necessary dictionaries
    else {
      // pull arg names from the method fundef instead of using meaningless names
      val Arrow(methSym, methParams, methCsts, methArgs, methResult) = field.tpe
      val lambdaSyms = (methSym.funArgs.map(_.name) zip methArgs).
        map(nt => new Symbol(nt._1, nt._2, true))
      val dictArgs = bind.impls map lowerImpl
      val lambdaArgs = lambdaSyms map IdentRef
      Lambda(Seq(), lambdaSyms, ExprStmt(Apply(boundFun, dictArgs, lambdaArgs)))
    }
  }

  private def lowerMatch (tree :high.Match, bb :BlockBuilder, target :BindTarget)
                         (implicit ctx :Context) :ExprTree = {

    // wrap the match in a labeled block, which we'll use to forward jump out of the match code
    // after each match (the combination of tests, binds and guards prevents a simple translation
    // to a chain of if/elses)
    val matchLabel = ctx.freshLabel()
    val matchBB = new BlockBuilder()
    val matchExp = lowerFresh(tree.cond, matchBB)

    def collectTests (disc :ExprTree, tree :high.TermTree) :Seq[ExprTree] = tree match {
      case tree :high.LetPat => Seq()
      // NOTE: the use of `is` here is a bit loosey goosey; pattern matching is a structural
      // operation, so we don't want to use `eq`, but we need to be stricter about which built-in
      // types are (mathematically) variants (Unit, Bool, In and Un) and which are not (Fn and
      // String)
      case tree @ high.IdentPat(ident) => Seq(Is(disc, IdentRef(ctx.sym(tree.sym))))
      case high.LiteralPat(const) => Seq(Is(disc, Literal(const)))
      case tree @ high.DestructPat(ctor, binds) =>
        val recType = tree.tpe.asInstanceOf[Record]
        Seq(Is(disc, IdentRef(ctx.sym(tree.sym)))) ++ (binds zip recType.fields).flatMap {
          case (bind, field) => collectTests(Select(disc, ctx.sym(field.sym)), bind)
        }
      case _ => unreachable(tree)
    }

    def collectBinds (disc :ExprTree, tree :high.TermTree, bb :BlockBuilder) :Unit = tree match {
      case tree @ high.LetPat(ident) =>
        if (ident != IgnoreName) bb += LetDef(ctx.sym(tree.sym), false, Some(disc))
      case tree :high.IdentPat => // no bindings
      case tree :high.LiteralPat => // no bindings
      case tree @ high.DestructPat(ctor, binds) =>
        val recType = tree.tpe.asInstanceOf[Record]
        (binds zip recType.fields).foreach {
          case (bind, field) => collectBinds(Select(disc, ctx.sym(field.sym)), bind, bb)
        }
      case _ => unreachable(tree)
    }

    var fullyMatched = false
    tree.cases foreach { cse =>
      // TODO: if we're already fully matched, any subsequent cases will be ignored, so we could
      // issue a warning; but we're not doing any other exhaustivity analysis in C0 so maybe we
      // just won't bother; DFU!
      val tests = collectTests(matchExp, cse.pattern)
      val caseBB = new BlockBuilder()
      collectBinds(matchExp, cse.pattern, caseBB)
      cse.guard match {
        case Some(guard) =>
          val guardExp = lowerHoist(guard, caseBB)
          val ifBB = new BlockBuilder()
          lowerTerm(cse.result, ifBB, target)
          // if we're returning the result, no need to break
          if (target != bindReturn) ifBB += Break(matchLabel)
          caseBB += If(guardExp, ifBB.build())
        case None =>
          if (tests.isEmpty) fullyMatched = true
          lowerTerm(cse.result, caseBB, target)
          // if we're returning the result, no need to break
          if (target != bindReturn) caseBB += Break(matchLabel)
      }
      if (tests.isEmpty) matchBB += caseBB.build()
      else matchBB += If(tests reduce And, caseBB.build())
    }

    if (!fullyMatched) matchBB += ExprStmt(Raise(Literal(string("match error"))))

    // if we're returning our result, we don't need to label the block
    bb += matchBB.build(if (target == bindReturn) None else Some(matchLabel))
    UnitExpr
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
    case ident @ high.IdentRef(sym) => IdentRef(ctx.sym(ident.sym))
    case expr => lowerTerm(expr, bb, bindFresh(expr.tpe, bb))
  }

  private def needsHoist (tree :high.TermTree) :Boolean = tree match {
    case high.OmittedBody => false
    case _ :high.Literal => false
    case _ :high.ArrayLiteral => false
    case _ :high.IdentRef => false
    case _ :high.Lambda => false
    case high.Select(expr, field) => needsHoist(expr)
    case high.Index(expr, index) => needsHoist(expr) || needsHoist(index)
    case high.FunApply(kind, fun, params, args) => needsHoist(fun) || args.exists(needsHoist)
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
    case Apply(fun, dicts, args) =>
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
