package compose

object Trees {
  import Analysis._
  import Constants._
  import Names._
  import Symbols._
  import Types._

  sealed trait Tree {
    def treeType :Type
    def signature :Type
    def isHole = false
  }

  sealed abstract class TypeTree extends Tree {
    def treeType = signature
  }

  sealed abstract class TermTree extends Tree {

    protected var inferredType :Type = Hole0 // TODO: Unknown or Unset?
    def treeType = inferredType
    def signature = Hole0

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
    // def owner = emptySym
    // get scope () :S.Scope { return S.emptyScope }
    // link (parent :Tree, parentId :string) { /*NOOP!*/ }
    protected def infer (ctx :Context) = Right((Hole0, ctx))
  }

  case object EmptyTypeTree extends TypeTree {
    // def owner = emptySym
    def signature = Hole0
    // get scope () :S.Scope { return S.emptyScope }
    // link (parent :Tree, parentId :string) { /*NOOP!*/ }
  }

  // type trees
  case object THoleTree extends TypeTree {
    def signature = Hole0
    override def isHole = true
  }
  case class TConstTree (cnst :Constant) extends TypeTree {
    def signature = Const(cnst)
  }
  case class TRefTree (sym :Symbol) extends TypeTree {
    def signature = ??? // TODO
  }
  case class ArrowTree (arg :TypeTree, res :TypeTree) extends TypeTree {
    def signature = new Arrow(arg.signature, res.signature)
  }
  case class TAppTree (ctor :TypeTree, arg :TypeTree) extends TypeTree {
    def signature = new App(ctor.signature, arg.signature)
  }
  case class ProdTree (fields :Seq[TypeTree]) extends TypeTree {
    def signature = ???
  }
  case class SumTree (cases :Seq[TypeTree]) extends TypeTree {
    def signature = ???
  }
  case class FieldTree (name :Name, tpe :TypeTree) extends TypeTree {
    def signature = ???
  }
  case class CtorTree (name :Name, prod :TypeTree) extends TypeTree {
    def signature = ???
  }
  // case class PrimTree (primType :Type) extends TypeTree

  // trees that define local symbols
  sealed trait SymTree extends Tree {
    def sym :Symbol
    def symType :Type
  }
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
  case class LetTree (name :Name, ann :TypeTree, body :TermTree,
                      expr :TermTree) extends TermSymTree {
    val sym = new LexicalSym(name, this, Kind.Term)
    protected def infer (ctx :Context) = body.inferSave(ctx) flatMap { (expType, theta) =>
      val eC = freshEVar("c")
      val assump = new NAssump(sym, expType)
      _symType = expType // assign type to symbol, which is not separately entyped
      val checkCtx = theta.extend(eC).extend(assump)
      ctx.tracer.trace(s"- Let=> ($expr <= $eC) in $checkCtx")
      expr.check(checkCtx, eC) map { checkedCtx => (eC, checkedCtx.peel(assump)) }
    }
  }

  case class AbsTree (name :Name, ann :TypeTree, body :TermTree) extends TermSymTree {
    val sym = new LexicalSym(name, this, Kind.Term)
    override def signature = Arrow(ann.signature, body.signature)
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

  case class AllTree (name :Name, body :TermTree) extends TermTree with SymTree {
    val sym = new LexicalSym(name, this, Kind.Type)
    val symType :UVar = UVar(sym)
    override def signature = Abs(symType, body.signature)
    protected def infer (ctx :Context) = body.inferSave(ctx)
  }

  // expression trees
  case class LitTree (cnst :Constant) extends TermTree {
    protected def infer (ctx :Context) = Right((Const(cnst), ctx))
  }
  case class RefTree (sym :Symbol) extends TermTree {
    // TODO: ref and xref (former comes from our context, latter has type that we use as is)
    protected def infer (ctx :Context) = Right(
      (ctx.assump(this.sym) getOrElse sym.tpe, ctx)) // A ⊣ Γ
  }
  case class AscTree (ann :TypeTree, body :TermTree) extends TermTree {
    override def signature = ann.signature
    protected def infer (ctx :Context) = body.inferSave(ctx)
  }
  case object HoleTree extends TermTree {
    protected def infer (ctx :Context) = Right((Hole0, ctx))
  }
  case class AppTree (fun :TermTree, arg :TermTree) extends TermTree {
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
  case class TermDefTree (name :Name, body :TermTree) extends TermTree {

    /** Infers and records a type for this definition and all its subtrees.
      * @return the inferred type for this tree. */
    def inferType () :Type = {
      val ctx = newCtx
      val ann = Hole0 // TODO: body.signature
      ctx.tracer.trace(s"$this $name // inferType :: $ann")
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
      println(s"$this :: $treeType\n--\n" +
              // debugShow.join("\n") + "\n--\n" +
              ctx.tracer.result.mkString("\n"))
      inferredType
    }

    protected def infer (ctx :Context) = body.inferSave(ctx)
  }
  case class TypeDefTree (name :Name, body :TypeTree) extends TypeTree {
    def signature = body.signature
  }
}
