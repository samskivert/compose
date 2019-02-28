package compose

object Types {
  import Analysis._
  import Constants._
  import Symbols.Sym
  import Trees.TermTree

  // kinds
  abstract sealed class Kind
  case object Star extends Kind {
    override def toString = "*"
  }
  case class KArrow (arg :Kind, res :Kind) extends Kind {
    override def toString = s"$arg → $res"
  }
  case class KError (msg :String) extends Kind {
    override def toString = s"!!$msg!!"
  }

  def kindApply (fun :Kind, arg :Kind) :Kind = fun match {
    case arr :KArrow => if (arg == Star) arr.res
                        else new KError(s"Cannot apply type arrow ($fun) to non-star kind $arg")
    case _           => new KError(s"Cannot apply type arg (kind: $arg) to non-arrow kind $fun")
  }

  // types
  sealed abstract class Type {
    def kind :Kind
    def arity = 0
    def isError = false
    def isArrow = false
    def isMono = true

    /** Returns whether `eV` is in the free variables of this type. */
    def containsFree (ev :EVar) :Boolean

    /** Returns whether this type is well-formed with respect to `ctx`. */
    def isWellFormed (ctx :Context) :Boolean = !this.checkMalformed(ctx).isDefined
    /** Checks that this type is malformed with respect to `ctx`.
      * @return some error message if it is malformed, `None` if it is well-formed. */
    def checkMalformed (ctx :Context) :Option[String]

    /** Applies `ctx` to this type (substituting existential vars for their solutions). */
    def apply (ctx :Context) :Type = this

    /** Returns this type with `thatT` replaced by `thisT`. */
    def subst (thisT :EVar, thatT :UVar) :Type = this

    /** Infers the type of an application of this type to `tree`.
      * @return the inferred type and the output context. */
    def inferApp (tree :TermTree, ctx :Context) :Either[String, (Type, Context)] =
      Left(s"Cannot apply term of type '$this' to '$tree'")
  }

  abstract class GroundType extends Type {
    def kind = Star
    def containsFree (ev :EVar) = false
    def checkMalformed (ctx :Context) = None
  }

  case class Hole (kind :Kind) extends Type {
    def containsFree (ev :EVar) = false
    def checkMalformed (ctx :Context) = None
  }
  val Hole0 = Hole(Star)

  case class Error (msg :String) extends GroundType {
    override def isError = true
    override def toString = s"!!$msg!!"
  }

  case class Const (cnst :Constant) extends GroundType {
    override def toString = cnst.value
  }

  case class UVar (sym :Sym) extends Type with Note {
    def kind = Star
    def containsFree (ev :EVar) = false
    def checkMalformed (ctx :Context) = if (ctx.contains(this)) None
                                        else Some(s"Unbound type variable '${sym.name}'")
    override def subst (thisT :EVar, thatT :UVar) :Type = if (thatT.equals(this)) thisT else this
    override def toString = s"${sym.name}"
  }

  case class EVar (name :String) extends Type with Note {
    def kind = Star
    def containsFree (ev :EVar) = ev == this
    def checkMalformed (ctx :Context) = if (ctx.contains(this) || ctx.solution(this).isDefined) None
                                        else Some(s"Unbound existential variable '$name'")
    override def apply (ctx :Context) :Type = ctx.solution(this).map(_.apply(ctx)) getOrElse this

    override def inferApp (tree :TermTree, ctx :Context) = { // âApp
      val a1 = freshEVar("a₁") // â₁
      val a2 = freshEVar("a₂") // â₂
      val aArrow = new Arrow(a1, a2) // â₁→â₂
      split(ctx, this) flatMap { (postCtx, preCtx) => // Γpre[â]post
        val checkCtx = preCtx.extend(a2).extend(a1).extend(new NSol(this, aArrow))
                             .concat(postCtx) // Γpre[â₂,â₁,â=â₁→â₂]post
        ctx.tracer.trace(s"- âApp $tree <= $a1 in $checkCtx")
        tree.check(checkCtx, a1) map { checkedCtx => (a2, checkedCtx) } // â₂ ⊣ ∆
      }
    }

    override def toString = s"#$name"
  }

  case class Abs (uv :UVar, body :Type) extends Type {
    def kind = KArrow(Star, body.kind)
    override def arity = body.arity
    override def isArrow = body.isArrow
    override def isMono = false

    def containsFree (ev :EVar) = body.containsFree(ev)
    def checkMalformed (ctx :Context) = body.checkMalformed(ctx.extend(this.uv))

    override def apply (ctx :Context) = new Abs(uv, body.apply(ctx))
    override def subst (thisT :EVar, thatT :UVar) = new Abs(uv, body.subst(thisT, thatT))

    override def inferApp (tree :TermTree, ctx :Context) = { // ∀App
      // case TAll(uv, tpe) =>
      val eA = freshEVar("a") // â
      val reduced = this.body.subst(eA, this.uv) // [â/α]A
      val appCtx = ctx.extend(eA) // Γ,â
      ctx.tracer.trace(s"- ∀App ${reduced} ● ${this} in ${appCtx}")
      reduced.inferApp(tree, appCtx) // C ⊣ ∆
    }

    override def toString = s"∀${uv.sym.name} ⇒ $body"
  }

  case class Arrow (arg :Type, res :Type) extends Type {
    def kind = Star
    override def arity = 1 + res.arity
    override def isArrow = true
    override def isMono = arg.isMono && res.isMono

    def containsFree (ev :EVar) = arg.containsFree(ev) || res.containsFree(ev)
    def checkMalformed (ctx :Context) = arg.checkMalformed(ctx) orElse res.checkMalformed(ctx)

    override def apply (ctx :Context) = new Arrow(arg.apply(ctx), res.apply(ctx))
    override def subst (thisT :EVar, thatT :UVar) = new Arrow(
      arg.subst(thisT, thatT), res.subst(thisT, thatT))

    override def inferApp (tree :TermTree, ctx :Context) = // ->App
      // A→C
      tree.check(ctx, arg).map(checkedCtx => (res, checkedCtx)) // C ⊣ ∆

    override def toString = s"$arg → $res"
  }

  case class App (ctor :Type, arg :Type) extends Type {
    def kind = kindApply(ctor.kind, arg.kind)
    // TODO
    def containsFree (ev :EVar) = false
    def checkMalformed (ctx :Context) = None
    override def toString = s"$ctor $arg"
  }

  case class Scalar (name :String, tag :Char, size :Int) extends GroundType {
    override def toString = s"$name"
  }

  case class Prod (operands :Seq[Type]) extends Type {
    def kind = Star
    // TODO: hrm...
    def checkMalformed (ctx :Context) = None
    def containsFree (ev :EVar) = false
    override def toString = if (operands.isEmpty) "()" else operands.mkString("*")
  }

  case class Sum (cases :Seq[Type]) extends Type {
    def kind = Star
    // TODO: hrm...
    def checkMalformed (ctx :Context) = None
    def containsFree (ev :EVar) = false
    override def toString = cases.mkString(" + ")
  }

  case class Nominal (sym :Sym, bodyFn :() => Type) extends Type {
    def kind = bodyFn().kind
    // TODO: hrm...
    def checkMalformed (ctx :Context) = None
    def containsFree (ev :EVar) = false
    override def toString = sym.name.toString // TODO: signature?
  }

  /** Derives a subtyping relationship `tpeA <: tpeB` within `ctx`.
    * @return the output context or a string describing an error. */
  def subtype (ctx :Context, tpeA :Type, tpeB :Type) :Either[String, Context] = (tpeA, tpeB) match {
    // <:Unit :: Γ ⊢ 1 <: 1 ⊣ Γ
    case (_, _) if (tpeA.equals(tpeB)) => Right(ctx) // Γ

    // TODO: handle widening primitives? coercing sum cases to sum type?

    // <:Var :: Γ[α] ⊢ α <: α ⊣ Γ[α]
    case (uvA :UVar, uvB :UVar) if (uvA == uvB) => Right(ctx) // Γ

    // <:Exvar :: Γ[â] ⊢ â <: â ⊣ Γ[â]
    case (evA :EVar, evB :EVar) if (evA == evB) =>
      if (ctx.contains(evA)) Right(ctx)
      else Left(s"Unbound existential '$evA'") // Γ

    // <:→ :: Γ ⊢ A1→A2 <: B1→B2 ⊣ ∆
    case (arA :Arrow, arB :Arrow) =>
      subtype(ctx, arB.arg, arA.arg) flatMap { theta => // Γ ⊢ B1 <: A1 ⊣ Θ
        subtype(theta, arA.res.apply(theta), arB.res.apply(theta)) // Θ ⊢ [Θ]A2 <: [Θ]B2 ⊣ ∆
      }

      // <:∀L :: Γ ⊢ ∀α.A <: B ⊣ ∆
    case (absA :Abs, _) =>
      val eA = freshEVar("a")
      val eAMark = NMark(eA)
      val subCtx = ctx.extend(eAMark).extend(eA) // Γ,▶â,â
      subtype(subCtx, absA.body.subst(eA, absA.uv), tpeB) map { // [â/α]A <: B ⊣ ∆,▶â,Θ
        deltaEtc => deltaEtc.peel(eAMark) // ∆
      }

      // <:∀R :: Γ ⊢ A <: ∀α.B ⊣ ∆
    case (_, absB :Abs) =>
      subtype(ctx.extend(absB.uv), tpeA, absB.body) map { // Γ,α ⊢ A <: B ⊣ ∆,α,Θ
        deltaEtc => deltaEtc.peel(absB.uv) // ∆
      }

      // <:InstantiateL :: Γ[â] ⊢ â <: A ⊣ ∆
    case (evA :EVar, _) if (ctx.contains(evA) && !tpeB.containsFree(evA)) =>
      ctx.tracer.trace(s"- <:InstL $evA :=< $tpeB")
      instantiateL(ctx, evA, tpeB) // Γ[â] ⊢ â :=< A ⊣ ∆

      // <:InstantiateR :: Γ[â] ⊢ A <: â ⊣ ∆
    case (_, evB :EVar) if (ctx.contains(evB) && !tpeA.containsFree(evB)) =>
      ctx.tracer.trace(s"- <:InstR $tpeA :=< $evB")
      instantiateR(ctx, tpeA, evB) // Γ[â] ⊢ A <: â ⊣ ∆

    case _ => Left(s"Type mismatch: expected '$tpeB', given: '$tpeA'")
  }

  /** Instantiates `eA` such that `eA <: a` in `ctx`.
    * @return the output context or a string describing an error. */
  private def instantiateL (ctx :Context, eA :EVar, t :Type) :Either[String, Context] = t match {
    // InstLSolve :: Γ,â,Γ′ ⊢ â :=< τ ⊣ Γ,â=τ,Γ′
    case _ if (t.isMono && t.isWellFormed(ctx.peel(eA))) => // Γ ⊢ τ
      split(ctx, eA) map { (postCtx, preCtx) =>
        ctx.tracer.trace(s"- InstLSolve $eA :=< $t")
        preCtx.extend(new NSol(eA, t)).concat(postCtx) // Γ,â=τ,Γ′
      }

    // InstLReach :: Γ[â][ĉ] ⊢ â :=< ĉ ⊣ Γ[â][ĉ=â]
    case eC :EVar if (ctx.peel(eC).contains(eA)) =>
      split(ctx, eC) map { (postCtx, preCtx) =>
        ctx.tracer.trace(s"- InstLReach $eA :=< $eC")
        preCtx.extend(new NSol(eC, eA)).concat(postCtx) // Γ[â][ĉ=â]
      }

    // InstLArr :: Γ[â] ⊢ â :=< A1 → A2 ⊣ ∆
    case Arrow(a1, a2) if (ctx.contains(eA)) =>
      split(ctx, eA) flatMap { (postCtx, preCtx) =>
        val eA1 = freshEVar("a₁") ; val eA2 = freshEVar("a₂")
        val nsol = new NSol(eA, new Arrow(eA1, eA1))
        val a1ctx = preCtx.extend(eA2).extend(eA1).extend(nsol).concat(postCtx)
        ctx.tracer.trace(s"- InstLArr(1) $a1 :=< $eA1 in $a1ctx")
        instantiateR(a1ctx, a1, eA1) flatMap { theta => // Γ[â₂,â₁,â=â₁→â2] ⊢ A1 :=< â₁ ⊣ Θ
          ctx.tracer.trace(s"- InstRArr(2) $eA2 :=< ${a2.apply(theta)} in $theta")
          instantiateL(theta, eA2, a2.apply(theta)) // Θ ⊢ â₂ :=< [Θ]A2 ⊣ ∆
        }
      }

    // InstLAllR :: Γ[â] ⊢ â :=< ∀β.B ⊣ ∆
    case absT @ Abs(uB, b) if (ctx.contains(eA)) =>
      ctx.tracer.trace(s"- InstLAllR $eA :=< $b in ${ctx.extend(uB)}")
      instantiateL(ctx.extend(uB), eA, b) map { // Γ[â],β ⊢ â :=< B ⊣ ∆,β,∆′
        deltaEtc => deltaEtc.peel(uB) // ∆
      }

    case _ => Left(s"Failed to instantiate '$eA' to '$t'")
  }

  /** Instantiates `eA` such that `a <: eA` in `ctx`.
    * @return the output context or a string describing an error. */
  private def instantiateR (ctx :Context, t :Type, eA :EVar) :Either[String, Context] = t match {
    // InstRSolve :: Γ,â,Γ′ ⊢ τ :=< â ⊣ Γ,â=τ,Γ′
    case _ if (t.isMono && t.isWellFormed(ctx.peel(eA))) => // Γ ⊢ τ
      split(ctx, eA) map { (postCtx, preCtx) =>
        ctx.tracer.trace(s"- InstRSolve $t :=< $eA")
        preCtx.extend(new NSol(eA, t)).concat(postCtx) // Γ,â=τ,Γ′
      }

    // InstRReach :: Γ[â][ĉ] ⊢ ĉ :=< â ⊣ Γ[â][ĉ=â]
    case (eC :EVar) if (ctx.peel(eC) contains eA) =>
      split(ctx, eC) map { (postCtx, preCtx) =>
        ctx.tracer.trace(s"- InstRReach $t :=< $eA")
        preCtx.extend(new NSol(eC, eA)).concat(postCtx) // Γ[â][ĉ = â]
      }

    // InstRArr :: Γ[â] ⊢ A1 → A2 :=< â ⊣ ∆
    case Arrow(a1, a2) if (ctx.contains(eA)) =>
      split(ctx, eA) flatMap { (postCtx, preCtx) =>
        val eA1 = freshEVar("a₁") ; val eA2 = freshEVar("a₂")
        val nsol = new NSol(eA, new Arrow(eA1, eA1))
        val a1ctx = preCtx.extend(eA2).extend(eA1).extend(nsol).concat(postCtx)
        ctx.tracer.trace(s"- InstRArr(1) $eA1 :=< $a1 in $a1ctx")
        instantiateL(a1ctx, eA1, a1) flatMap { theta => // Γ[â₂,â₁,â=â₁→â₂] ⊢ â₁ :=< A1 ⊣ Θ
          ctx.tracer.trace(s"- InstRArr(2) ${a2.apply(theta)} :=< $eA2 in $theta")
          instantiateR(theta, a2.apply(theta), eA2) // Θ ⊢ [Θ]A2 :=< â₂ ⊣ ∆
        }
      }

    // InstRAllL :: Γ[â],▶ĉ,ĉ ⊢ [ĉ/β]B :=< â ⊣ ∆,▶ĉ,∆′
    case absT @ Abs(uB, b) if ctx.contains(eA) =>
      val eC = freshEVar("c")
      val mC = new NMark(eC)
      val instCtx = ctx.extend(mC).extend(eC) // Γ[â],▶ĉ,ĉ
      ctx.tracer.trace(s"- InstRAllL [$eC/$uB]$b :=< $eA in $instCtx")
      instantiateR(instCtx, b.subst(eC, uB), eA) map { // Γic ⊢ [ĉ/β]B :=< â ⊣ ∆,▶ĉ,∆′
        deltaEtc => deltaEtc.peel(mC) // ∆
      }

    case _ => Left(s"Failed to instantiate '$t' to '$eA'\n (context: $this)")
  }

  private def split (ctx :Context, ev :EVar) :Either[String, (Context, Context)] =
    ctx.split(ev) match {
      case None       => Left(s"Unable to split context on $ev")
      case Some(ctxs) => Right(ctxs)
    }
}
