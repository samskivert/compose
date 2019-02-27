package compose

object Trees {
  import Analysis._
  import Constants._
  import Names._
  import Scopes._
  import Symbols._
  import Types._

  sealed trait Tree {
    def treeType :Type
    def signature :Type
    def isHole = false

    /** Returns a tree with names resolved (via `scope`). */
    def resolve (scope :Scope) :Tree

    def format :String
    override def toString = format
  }

  sealed trait TypeTree extends Tree {
    def treeType = signature
    override def resolve (scope :Scope) :TypeTree
  }

  sealed trait TermTree extends Tree {

    protected var inferredType :Type = Hole0 // TODO: Unknown or Unset?
    def treeType = inferredType
    def signature = Hole0
    override def resolve (scope :Scope) :TermTree

    /** Checks that this tree has type `tpe` with input context `ctx`.
      * @return the output context or an error. */
    def check (ctx :Context, tpe :Type) :Either[String, Context] = {
      ctx.tracer.trace(s"Tree.check $this :: $tpe")
      tpe match {
        case abs @ Abs(_, body) =>
          // ∀I :: (e, ∀α.A)
          val checkCtx = ctx.extend(abs.uv)
          ctx.tracer.trace(s"- ∀I ($this <= $body) in $checkCtx")
          check(checkCtx, body) map { checkedCtx => // Γ,α ⊢ e ⇐ A ⊣ ∆,α,Θ
            this.applyContext(checkedCtx)
            checkedCtx.peel(abs.uv) // ∆
          }
        case _ =>
          // Sub :: (e, B)
          inferSave(ctx) flatMap { (expType, theta) => // Γ ⊢ e ⇒ A ⊣ Θ
            ctx.tracer.trace(s"- Sub ($this => $expType) ; [Θ]$expType <: [Θ]$tpe in $theta")
            subtype(theta, expType.apply(theta), tpe.apply(theta)) // Θ ⊢ [Θ]A <: [Θ]B ⊣ ∆
          }
      }
    }

    /** Infers a type for this tree with input context `ctx` and records its as the tree's type. The
      * recorded type is not yet applied to the context until inference is complete for the entire
      * expression (which happens in `DefTree.infer`).
      * @return the inferred type and the output context. */
    def inferSave (ctx :Context) :Either[String, (Type, Context)] = {
      ctx.tracer.trace(s"infer $this")
      infer(ctx) map { (tpe, delta) =>
        inferredType = tpe
        ctx.tracer.trace(s"> $this :: $tpe // ∆ = $delta")
        (tpe, delta)
      }
    }

    protected def infer (ctx :Context) :Either[String, (Type, Context)]

    /** Applies this tree's type to the supplied `ctx` (subbing existential vars for their
      * solutions).
      * @return the supplied `ctx`, unmodified. */
    protected def applyContext (ctx :Context) :Unit = {
      // for (let id of this.branchIds) {
      //   const branch = this.branch(id)
      //   if (branch instanceof TermTree) branch.applyContext(ctx)
      // }
      inferredType = inferredType.apply(ctx)
      ctx.tracer.trace(s">> $this :: $inferredType")
    }
  }

  // sealed abstract class PatTree extends TermTree {
  // }

  // empty trees
  case object EmptyTermTree extends TermTree {
    def resolve (scope :Scope) = this
    // def owner = emptySym
    // get scope () :S.Scope { return S.emptyScope }
    // link (parent :Tree, parentId :string) { /*NOOP!*/ }
    def format = "<empty>"
    protected def infer (ctx :Context) = Right((Hole0, ctx))
  }

  case object EmptyTypeTree extends TypeTree {
    // def owner = emptySym
    def signature = Hole0
    def resolve (scope :Scope) = this
    def format = "<empty>"
    // get scope () :S.Scope { return S.emptyScope }
    // link (parent :Tree, parentId :string) { /*NOOP!*/ }
  }

  // type trees
  case object THoleTree extends TypeTree {
    def signature = Hole0
    def resolve (scope :Scope) = this
    def format = "<hole>"
    override def isHole = true
  }
  case class TConstTree (cnst :Constant) extends TypeTree {
    def signature = Const(cnst)
    def format = cnst.value
    def resolve (scope :Scope) = this
  }
  case class UTRefTree (name :Name) extends TypeTree {
    def signature = ??? // should never be called
    def format = s"?$name"
    def resolve (scope :Scope) = TRefTree(scope.lookupType(name))
  }
  case class TRefTree (sym :Symbol) extends TypeTree {
    def signature = sym.tpe
    def format = sym.toString
    def resolve (scope :Scope) = this
  }
  case class ArrowTree (arg :TypeTree, res :TypeTree) extends TypeTree {
    def signature = new Arrow(arg.signature, res.signature)
    def format = s"${arg.format} → ${res.format}"
    def resolve (scope :Scope) = ArrowTree(arg.resolve(scope), res.resolve(scope))
  }
  case class TAppTree (ctor :TypeTree, arg :TypeTree) extends TypeTree {
    def signature = new App(ctor.signature, arg.signature)
    def format = s"${ctor.format} ${arg.format}"
    def resolve (scope :Scope) = TAppTree(ctor.resolve(scope), arg.resolve(scope))
  }
  case class ProdTree (fields :Seq[TypeTree]) extends TypeTree {
    def signature = ???
    def format = fields.map(_.format).mkString(" + ")
    def resolve (scope :Scope) = ProdTree(fields.map(_.resolve(scope)))
  }
  case class SumTree (cases :Seq[TypeTree]) extends TypeTree {
    def signature = ???
    def format = cases.map(_.format).mkString(" | ")
    def resolve (scope :Scope) = ProdTree(cases.map(_.resolve(scope)))
  }
  case class FieldTree (name :Name, tpe :TypeTree) extends TypeTree {
    def signature = ???
    def format = s"$name:${tpe.format}"
    def resolve (scope :Scope) = FieldTree(name, tpe.resolve(scope))
  }
  case class CtorTree (name :Name, prod :TypeTree) extends TypeTree {
    def signature = ???
    def format = s"$name ${prod.format}"
    def resolve (scope :Scope) = CtorTree(name, prod.resolve(scope))
  }
  // case class PrimTree (primType :Type) extends TypeTree

  // trees that define local symbols
  // abstract class PatSymTree extends PatTree with SymTree {
  //   protected var symType :Type = Hole0
  // }
  abstract class TermSymTree extends TermTree with SymTree {
    protected var _symType :Type = Hole0
    def symType = _symType
  }

  // // pattern trees
  // case object PHoleTree extends PatTree {
  //   protected def infer (ctx :Context) = (Hole0, ctx)
  // }
  // case class PLitTree (cnst :Constant) extends PatTree {
  //   protected def infer (ctx :Context) = ???
  // }
  // case class PBindTree (sym :Symbol) extends PatSymTree {
  //   protected def infer (ctx :Context) = ???
  // }
  // case class PDtorTree (ctor :Symbol) extends PatTree {
  //   protected def infer (ctx :Context) = ???
  // }
  // case class PAppTree (fun :PatTree, arg :PatTree) extends PatTree {
  //   protected def infer (ctx :Context) = ???
  // }

  // abstraction trees
  case class AbsTree (sym :LexicalSym, ann :TypeTree, body :TermTree) extends TermSymTree {
    override def signature = Arrow(ann.signature, body.signature)
    def format = body match {
      case _ :AbsTree => s"$sym:${ann.format} → ${body.format}"
      case a :AscTree => s"$sym:${ann.format} → ${a.ann.format} = ${a.body.format}"
      case _          => s"$sym:${ann.format} = ${body.format}"
    }
    def resolve (scope :Scope) = sym.setTree(
      AbsTree(sym, ann.resolve(scope), body.resolve(sym.scope(scope))))
    override def check (ctx :Context, tpe :Type) :Either[String,Context] = tpe match {
      case Arrow(arg, res) =>
        inferredType = tpe // lambda types are not always synthesized, so we also assign abs
        _symType = arg      // trees a type during checking, ditto for the lambda arg sym
        val argAssump = NAssump(sym, arg) // x:A
        val checkCtx = ctx.extend(argAssump)
        ctx.tracer.trace(s"- ->I ($body <= $res) in $checkCtx")
        body.check(checkCtx, res) map { // Γ,x:A ⊢ e ⇐ B ⊣ ∆,x:A,Θ
          checkedCtx => checkedCtx.peel(argAssump) } // ∆
      case _ => super.check(ctx, tpe)
    }
    // ->I=> :: λx.e
    protected def infer (ctx :Context) = {
      val eA = freshEVar("a") // â
      val eC = freshEVar("c") // ĉ
      val assump = NAssump(this.sym, eA) // x:â
      _symType = eA // propagate type to arg sym
      val checkCtx = ctx.extend(eA).extend(eC).extend(assump) // Γ,â,ĉ,x:â
      ctx.tracer.trace(s"- ->I=> ($body <= $eC) in $checkCtx")
      body.check(checkCtx, eC) map {
        checkedCtx => (Arrow(eA, eC), checkedCtx.peel(assump)) } // e ⇐ ĉ ⊣ ∆,x:â,Θ
    }
  }

  case class AllTree (sym :LexicalSym, body :TermTree) extends TermTree with SymTree {
    def symType :UVar = UVar(sym)
    override def signature = Abs(symType, body.signature)
    def format = s"∀$sym => ${body.format}"
    def resolve (scope :Scope) = sym.setTree(AllTree(sym, body.resolve(sym.scope(scope))))
    override def check (ctx :Context, tpe :Type) :Either[String, Context] = body.check(ctx, tpe)
    protected def infer (ctx :Context) = body.inferSave(ctx)
  }

  case class Bind (sym :LexicalSym, ann :TypeTree, body :TermTree) extends SymTree {
    protected var _symType :Type = Hole0
    def symType = _symType
    def assignType (tpe :Type) = { _symType = tpe }
    def format = s"$sym:${ann.format} = ${body.format}"
    def resolve (scope :Scope) = sym.setTree(
      // TODO: is it OK that we're always doing a recursive bind?
      Bind(sym, ann.resolve(scope), body.resolve(sym.scope(scope))))
  }

  case class LetTree (bind :Bind, expr :TermTree) extends TermTree {
    def format = s"let ${bind.format} in ${expr.format}"
    def resolve (scope :Scope) = {
      val nbind = bind.resolve(scope)
      LetTree(nbind, expr.resolve(nbind.sym.scope(scope)))
    }
    // TODO: if we have a type annotation, use that when typing the body
    protected def infer (ctx :Context) = bind.body.inferSave(ctx) flatMap { (expType, theta) =>
      bind.assignType(expType) // assign type to binding symbol
      val eC = freshEVar("c")
      val assump = new NAssump(bind.sym, expType)
      val checkCtx = theta.extend(eC).extend(assump)
      ctx.tracer.trace(s"- Let=> ($expr <= $eC) in $checkCtx")
      expr.check(checkCtx, eC) map { checkedCtx => (eC, checkedCtx.peel(assump)) }
    }
  }

  case class LetRecTree (binds :Seq[Bind], expr :TermTree) extends TermTree {
    def format = s"let ${binds.map(_.format).mkString(" ; ")} in ${expr.format}"
    def resolve (scope :Scope) = ???
    protected def infer (ctx :Context) = ???
  }

  // expression trees
  case class LitTree (cnst :Constant) extends TermTree {
    def format = cnst.value
    def resolve (scope :Scope) = this
    protected def infer (ctx :Context) = Right((Const(cnst), ctx))
  }
  case class URefTree (name :Name) extends TermTree {
    def format = s"?$name"
    def resolve (scope :Scope) = RefTree(scope.lookupTerm(name))
    protected def infer (ctx :Context) = ??? // should never be called...
  }
  // TODO: ref and xref? (former comes from our context, latter has type that we use as is)
  case class RefTree (sym :Symbol) extends TermTree {
    def format = sym.toString
    def resolve (scope :Scope) = this
    protected def infer (ctx :Context) = Right(
      (ctx.assump(this.sym) getOrElse sym.tpe, ctx)) // A ⊣ Γ
  }
  case class AscTree (ann :TypeTree, body :TermTree) extends TermTree {
    def format = s"(${body.format} : ${ann.format})"
    def resolve (scope :Scope) = AscTree(ann.resolve(scope), body.resolve(scope))
    override def check (ctx :Context, tpe :Type) :Either[String, Context] = body.check(ctx, tpe)
    override def signature = ann.signature
    protected def infer (ctx :Context) = body.inferSave(ctx)
  }
  case object HoleTree extends TermTree {
    def format = "<hole>"
    def resolve (scope :Scope) = this
    protected def infer (ctx :Context) = Right((Hole0, ctx))
  }
  case class AppTree (fun :TermTree, arg :TermTree) extends TermTree {
    def format = s"${fun.format} ${arg.format}"
    def resolve (scope :Scope) = AppTree(fun.resolve(scope), arg.resolve(scope))
    // ->E :: (e1 e2)
    protected def infer (ctx :Context) = fun.inferSave(ctx) flatMap { // e1 ⇒ A ⊣ Θ
      (funType, theta) =>
      val reducedFun = funType.apply(theta) // [Θ]A
      ctx.tracer.trace(s"- ->E $fun => $funType ; $reducedFun ● $arg in $theta")
      reducedFun.inferApp(this.arg, theta) map { (resType, delta) => // C ⊣ ∆
        // TODO: I think this is handled by a recursive apply of the final context with all the
        // solutions
        // this.fun.sig = funType.apply(delta)
        (resType, delta)
      }
    }
  }
  case class IfTree (test :TermTree, texp :TermTree, fexp :TermTree) extends TermTree {
    def format = s"if ${test.format} then ${texp.format} else ${fexp.format}"
    def resolve (scope :Scope) = IfTree(
      test.resolve(scope), texp.resolve(scope), fexp.resolve(scope))
    protected def infer (ctx :Context) = {
      // // If=> :: if test ifTrue else ifFalse
      // case XIf(test, ifTrue, ifFalse) =>
      //   check(test, TBool)
      //   val (trueType, theta) = infer(ifTrue)
      //   ctx.tracer.trace(s"- If=> ($ifTrue => $trueType) ; ($ifFalse <= $trueType) in $theta")
      //   val delta = check(ifFalse, trueType)(theta)
      //   (trueType, delta) // TODO: peel?
      ???
    }
  }
  // case class MatchTree (scrut :TermTree, cases :Seq[CaseTree]) extends TermTree {
  //   protected def infer (ctx :Context) = ???
  // }
  // case class CaseTree (pat :PatTree, body :TermTree) extends TermTree {
  //   protected def infer (ctx :Context) = ???
  // }

  // top-level def trees
  case class TermDefTree (sym :LexicalSym, body :TermTree) extends TermTree with SymTree {
    def symType = inferredType
    def format = body match {
      case AscTree(ann, body) => s"def ${sym} :: ${ann.format} = ${body.format}"
      case _                  => s"def ${sym} :: ${body.format}"
    }
    def resolve (scope :Scope) :TermDefTree =
      sym.setTree(TermDefTree(sym, body.resolve(sym.scope(scope))))

    /** Infers and records a type for this definition and all its subtrees.
      * @return the inferred type for this tree. */
    def inferType (trace :Boolean = false) :Type = {
      val ctx = newCtx(trace)
      val ann = body.signature
      ctx.tracer.trace(s"$this ${sym.name} // inferType :: $ann")
      if (ann == Hole0) {
        body.inferSave(ctx) map { (tpe, delta) =>
          inferredType = tpe
          applyContext(delta)
        }
      } else {
        // TODO: ann.checkWellFormed
        body.check(ctx, ann) map { delta =>  // A ⊣ ∆
          // TODO: do we still need to apply delta here? only forall trees auto-apply
          inferredType = ann
          applyContext(delta)
        }
      }
      if (trace) println(s"$this :: $treeType\n--\n" +
                         // debugShow.join("\n") + "\n--\n" +
                         ctx.tracer.result.mkString("\n"))
      inferredType
    }

    protected def infer (ctx :Context) = body.inferSave(ctx)
  }

  case class TypeDefTree (sym :LexicalSym, body :TypeTree) extends TypeTree with SymTree {
    def symType = signature
    def signature = body.signature
    def format = s"type ${sym} = ${body.format}"
    def resolve (scope :Scope) = sym.setTree(TypeDefTree(sym, body.resolve(sym.scope(scope))))
  }
}
