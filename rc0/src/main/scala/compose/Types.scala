package compose

object Types {
  import Constants._
  import Trees.TermTree
  import Symbols.Symbol

  // kinds
  abstract sealed class Kind
  case object Star extends Kind {
    override def toString = "*"
  }
  case class KArrow (arg :Kind, res :Kind) extends Kind {
    def toString = s"$arg → $res"
  }
  case class KError (msg :String) extends Kind {
    def toString = s"!!$msg!!"
  }

  def kindApply (fun :Kind, arg :Kind) :Kind = fun match {
    case arr :KArrow => if (arg == Star) arr.res
                        else new KError(s"Cannot apply type arrow ($fun) to non-star kind $arg")
    case _           => new KError(s"Cannot apply type arg (kind: $arg) to non-arrow kind $fun")
  }

  // types
  sealed abstract class Type {
    def kind :Kind
    def isError = false
    def isArrow = false
    def arity = 0

    /** Whether this type is a monotype. */
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
    def inferApp (tree :TermTree, ctx :Context) :(Type, Context) =
      (new Error(s"Cannot apply term of type '$this' to '$tree'"), ctx)
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

  case class Error (msg :String) extends GroundType {
    def isError = true
    def toString = s"!!$msg!!"
  }

  case class Const (cnst :Constant) extends GroundType {
    def toString = cnst.value
  }

  case class UVar (sym :Symbol) extends Type with Note {
    def kind = Star
    def containsFree (ev :EVar) = false
    def checkMalformed (ctx :Context) = if (ctx.contains(this)) None
                                        else Some(s"Unbound type variable '${sym.name}'")
    def subst (thisT :EVar, thatT :UVar) :Type = if (thatT.equals(this)) thisT else this
    def toString = s"${sym.name}"
  }

  case class EVar (name :String) extends Type with Note {
    def kind = Star
    def containsFree (ev :EVar) = ev == this
    def checkMalformed (ctx :Context) = if (ctx.contains(this) || ctx.solution(this).isDefined) None
                                        else Some(s"Unbound existential variable '$name'")
    def apply (ctx :Context) :Type = ctx.solution(this).map(_.apply(ctx)) getOrElse this

    def inferApp (tree :TermTree, ctx :Context) :(Type, Context) = { // âApp
      val a1 = freshEVar("a₁") // â₁
      val a2 = freshEVar("a₂") // â₂
      val aArrow = new Arrow(a1, a2) // â₁→â₂
      (ctx.split(this) map { (postCtx, preCtx) => // Γpre[â]post
        val checkCtx = preCtx.extend(a2).extend(a1).extend(new NSol(this, aArrow)).
        concat(postCtx) // Γpre[â₂,â₁,â=â₁→â₂]post
        ctx.tracer.trace(s"- âApp $tree <= $a1 in $checkCtx")
        check(tree, checkCtx, a1, a2) // â₂ ⊣ ∆
      }) match {
        case Left(err) => return (new Error(err), ctx)
        case Right(res) => res
      }
    }

    def toString = s"#$name"
  }

  case class Abs (sym :Symbol, body :Type) extends Type {
    val uv = new UVar(sym)
    def kind = KArrow(Star, body.kind)
    def arity = body.arity
    def isArrow = body.isArrow

    def isMono = false
    def containsFree (ev :EVar) = body.containsFree(ev)
    def checkMalformed (ctx :Context) = body.checkMalformed(ctx.extend(this.uv))

    def apply (ctx :Context) = new Abs(sym, body.apply(ctx))
    def subst (thisT :EVar, thatT :UVar) = new Abs(sym, body.subst(thisT, thatT))

    def inferApp (tree :TermTree, ctx :Context) :(Type, Context) = { // ∀App
      // case TAll(uv, tpe) =>
      val eA = freshEVar("a") // â
      val reduced = this.body.subst(eA, this.uv) // [â/α]A
      val appCtx = ctx.extend(eA) // Γ,â
      ctx.tracer.trace(s"- ∀App ${reduced} ● ${this} in ${appCtx}")
      reduced.inferApp(tree, appCtx) // C ⊣ ∆
    }

    def toString = s"`∀${sym.name} $body"
  }

  case class Arrow (arg :Type, res :Type) extends Type {
    def kind = Star
    def isArrow = true
    def arity = 1 + res.arity

    def isMono = arg.isMono && res.isMono
    def containsFree (ev :EVar) = arg.containsFree(ev) || res.containsFree(ev)
    def checkMalformed (ctx :Context) = arg.checkMalformed(ctx) orElse res.checkMalformed(ctx)

    def apply (ctx :Context) = new Arrow(arg.apply(ctx), res.apply(ctx))
    def subst (thisT :EVar, thatT :UVar) = new Arrow(
      arg.subst(thisT, thatT), res.subst(thisT, thatT))

    def inferApp (tree :TermTree, ctx :Context) :(Type, Context) = // ->App
      // A→C
      check(tree, ctx, this.arg, this.res) // C ⊣ ∆

    def toString = s"$arg → $res"
  }

  case class App (ctor :Type, arg :Type) extends Type {
    def kind = kindApply(ctor.kind, arg.kind)
    // TODO
    def containsFree (ev :EVar) = false
    def checkMalformed (ctx :Context) = None
    def toString = s"$ctor $arg"
  }

  case class Scalar (tag :Char, size :Int) extends GroundType {
    def kind = Star
    def toString = s"Scalar:$tag$size"
  }

  case class Prod (operands :Seq[Type]) extends Type {
    def kind = Star
    // TODO: hrm...
    def checkMalformed (ctx :Context) = None
    def containsFree (ev :EVar) = false
    def toString = if (operands.isEmpty) "()" else operands.mkString("*")
  }

  case class Sum (cases :Seq[Type]) extends Type {
    def kind = Star
    // TODO: hrm...
    def checkMalformed (ctx :Context) = None
    def containsFree (ev :EVar) = false
    def toString = cases.mkString(" + ")
  }

  case class Nominal (sym :Symbol, bodyFn :() => Type) extends Type {
    def kind = bodyFn().kind
    // TODO: hrm...
    def checkMalformed (ctx :Context) = None
    def containsFree (ev :EVar) = false
    def toString = sym.name // TODO: signature?
  }

  // checking
  trait Note {}
  case class NAssump (sym :Symbol, tpe :Type) extends Note {
    def toString = s"$sym~$tpe"
  }
  case class NSol (ev :EVar, tpe :Type) extends Note {
    def toString = s"$ev=$tpe"
  }
  case class NMark (ev :EVar) extends Note {
    def toString = s"*$ev"
  }
  // UVar and EVar are also notes

  class Tracer {
    val msgs = Seq.newBuilder[String]
    def trace (msg :String) :Unit = msgs += msg
  }

  var nextEVar = 1
  def freshEVar (name :String) :EVar = try EVar(s"$name$nextEVar") finally nextEVar += 1

  class Context (val tracer :Tracer, val notes :List[Note]) {

    /** Returns whether this context contains `note`. */
    def contains (note :Note) = notes.contains(note)
    /** Creates a new context which extends this context with `note`. */
    def extend (note :Note) :Context = new Context(tracer, note :: notes)
    /** Creates a new context which extends this context with the entirety of `other`. */
    def concat (other :Context) :Context = new Context(tracer, other.notes ++ notes)

    /** Peels off the end of a context up to and including `note`. */
    def peel (note :Note) :Context = {
      val nidx = notes.indexOf(note)
      if (nidx < 0) {
        println(s"Peeled unknown note from context $note :: $notes")
        new Context(tracer, Nil)
      } else {
        tracer.trace(s"-- peeled $notes > $note < ${notes.drop(nidx)}")
        new Context(tracer, notes.drop(nidx))
      }
    }

    /** Splits this context into the part after `note` and the part before. `note` itself is not
      * included. Recall that contexts list notes in reverse order, hence the `(post, pre)` return
      * order. If `note` is not in this context `None` is returned. */
    def split (note :Note) :Either[String, (Context,Context)] = {
      val nidx = notes.indexOf(note)
      if (nidx < 0) return Left(s"Unable to split context on $note: $this")
      else Right(new Context(tracer, notes.take(nidx-1)) -> new Context(tracer, notes.drop(nidx)))
    }

    /** Looks up the assumption for `sym`. */
    def assump (sym :Symbol) :Option[Type] = (notes.collect {
      case assump :NAssump if (assump.sym == sym) => assump
    }) match {
      case Nil          => None
      case List(assump) => Some(assump.tpe)
      case assumps      => Some(new Error(s"Multiple assumptions for '$sym': $assumps"))
    }

    /** Looks up the solution for `ev` in `ctx`. */
    def solution (ev :EVar) :Option[Type] = (notes.collect {
      case sol :NSol if (sol.ev == ev) => sol
    }) match {
      case Nil       => None
      case List(sol) => Some(sol.tpe)
      case sols      => Some(new Error(s"Multiple solutions for '$ev': $sols"))
    }

    /** Derives a subtyping relationship `tpeA <: tpeB` within this context.
      * @return the output context or a string describing an error. */
    def subtype (tpeA :Type, tpeB :Type) :Either[String, Context] = (tpeA, tpeB) match {
      // <:Unit :: Γ ⊢ 1 <: 1 ⊣ Γ
      case (_, _) if (tpeA.equals(tpeB)) => Right(this) // Γ

      // TODO: handle widening primitives? coercing sum cases to sum type?

      // <:Var :: Γ[α] ⊢ α <: α ⊣ Γ[α]
      case (uvA :UVar, uvB :UVar) if (uvA == uvB) => Right(this) // Γ

      // <:Exvar :: Γ[â] ⊢ â <: â ⊣ Γ[â]
      case (evA :EVar, evB :EVar) if (evA == evB) => if (contains(evA)) Right(this)
                                  else Left(s"Unbound existential '$evA'") // Γ

      // <:→ :: Γ ⊢ A1→A2 <: B1→B2 ⊣ ∆
      case (arA :Arrow, arB :Arrow) =>
        subtype(arB.arg, arA.arg) flatMap { theta => // Γ ⊢ B1 <: A1 ⊣ Θ
          theta.subtype(arA.res.apply(theta), arB.res.apply(theta)) // Θ ⊢ [Θ]A2 <: [Θ]B2 ⊣ ∆
        }

      // <:∀L :: Γ ⊢ ∀α.A <: B ⊣ ∆
      case (absA :Abs, _) =>
        val eA = freshEVar("a")
        val eAMark = NMark(eA)
        val subCtx = extend(eAMark).extend(eA) // Γ,▶â,â
        subCtx.subtype(absA.body.subst(eA, absA.uv), tpeB) map { // [â/α]A <: B ⊣ ∆,▶â,Θ
          deltaEtc => deltaEtc.peel(eAMark) // ∆
        }

      // <:∀R :: Γ ⊢ A <: ∀α.B ⊣ ∆
      case (_, absB :Abs) =>
        extend(absB.uv).subtype(tpeA, absB.body) map { // Γ,α ⊢ A <: B ⊣ ∆,α,Θ
          deltaEtc => deltaEtc.peel(absB.uv) // ∆
        }

      // <:InstantiateL :: Γ[â] ⊢ â <: A ⊣ ∆
      case (evA :EVar, _) if (contains(evA) && !tpeB.containsFree(evA)) =>
        tracer.trace(s"- <:InstL $evA :=< $tpeB")
        instantiateL(evA, tpeB) // Γ[â] ⊢ â :=< A ⊣ ∆

      // <:InstantiateR :: Γ[â] ⊢ A <: â ⊣ ∆
      case (_, evB :EVar) if (contains(evB) && !tpeA.containsFree(evB)) =>
        tracer.trace(s"- <:InstR $tpeA :=< $evB")
        instantiateR(tpeA, evB) // Γ[â] ⊢ A <: â ⊣ ∆

      case _ => Left(s"Type mismatch: expected '$tpeB', given: '$tpeA'")
    }

    /** Instantiates `eA` such that `eA <: a` in this context.
      * @return the output context or a string describing an error. */
    def instantiateL (eA :EVar, t :Type) :Either[String, Context] = t match {
      // InstLSolve :: Γ,â,Γ′ ⊢ â :=< τ ⊣ Γ,â=τ,Γ′
      case _ if (t.isMono && t.isWellFormed(peel(eA))) => // Γ ⊢ τ
        split(eA) map { (postCtx, preCtx) =>
          tracer.trace(s"- InstLSolve $eA :=< $t")
          preCtx.extend(new NSol(eA, t)).concat(postCtx) // Γ,â=τ,Γ′
        }

      // InstLReach :: Γ[â][ĉ] ⊢ â :=< ĉ ⊣ Γ[â][ĉ=â]
      case eC :EVar if (peel(eC).contains(eA)) =>
        split(eC) map { (postCtx, preCtx) =>
          tracer.trace(s"- InstLReach $eA :=< $eC")
          preCtx.extend(new NSol(eC, eA)).concat(postCtx) // Γ[â][ĉ=â]
        }

      // InstLArr :: Γ[â] ⊢ â :=< A1 → A2 ⊣ ∆
      case Arrow(a1, a2) if (contains(eA)) =>
        split(eA) flatMap { (postCtx, preCtx) =>
          val eA1 = freshEVar("a₁") ; val eA2 = freshEVar("a₂")
          val nsol = new NSol(eA, new Arrow(eA1, eA1))
          val a1ctx = preCtx.extend(eA2).extend(eA1).extend(nsol).concat(postCtx)
          tracer.trace(s"- InstLArr(1) $a1 :=< $eA1 in $a1ctx")
          a1ctx.instantiateR(a1, eA1) flatMap { theta => // Γ[â₂,â₁,â=â₁→â2] ⊢ A1 :=< â₁ ⊣ Θ
            tracer.trace(s"- InstRArr(2) $eA2 :=< ${a2.apply(theta)} in $theta")
            theta.instantiateL(eA2, a2.apply(theta)) // Θ ⊢ â₂ :=< [Θ]A2 ⊣ ∆
          }
        }

      // InstLAllR :: Γ[â] ⊢ â :=< ∀β.B ⊣ ∆
      case absT @ Abs(_, b) if (contains(eA)) =>
        val uB = absT.uv
        tracer.trace(s"- InstLAllR $eA :=< $b in ${extend(uB)}")
        extend(uB).instantiateL(eA, b) map { // Γ[â],β ⊢ â :=< B ⊣ ∆,β,∆′
          deltaEtc => deltaEtc.peel(uB) // ∆
        }

      case _ => Left(s"Failed to instantiate '$eA' to '$t'")
    }

    /** Instantiates `eA` such that `a <: eA` in this context.
      * @return the output context or a string describing an error. */
    def instantiateR (t :Type, eA :EVar) :Either[String, Context] = t match {
      // InstRSolve :: Γ,â,Γ′ ⊢ τ :=< â ⊣ Γ,â=τ,Γ′
      case _ if (t.isMono && t.isWellFormed(peel(eA))) => // Γ ⊢ τ
        split(eA) map { (postCtx, preCtx) =>
          tracer.trace(s"- InstRSolve $t :=< $eA")
          preCtx.extend(new NSol(eA, t)).concat(postCtx) // Γ,â=τ,Γ′
        }

      // InstRReach :: Γ[â][ĉ] ⊢ ĉ :=< â ⊣ Γ[â][ĉ=â]
      case (eC :EVar) if (peel(eC) contains eA) =>
        split(eC) map { (postCtx, preCtx) =>
          tracer.trace(s"- InstRReach $t :=< $eA")
          preCtx.extend(new NSol(eC, eA)).concat(postCtx) // Γ[â][ĉ = â]
        }

      // InstRArr :: Γ[â] ⊢ A1 → A2 :=< â ⊣ ∆
      case Arrow(a1, a2) if (contains(eA)) =>
        split(eA) flatMap { (postCtx, preCtx) =>
          val eA1 = freshEVar("a₁") ; val eA2 = freshEVar("a₂")
          val nsol = new NSol(eA, new Arrow(eA1, eA1))
          val a1ctx = preCtx.extend(eA2).extend(eA1).extend(nsol).concat(postCtx)
          tracer.trace(s"- InstRArr(1) $eA1 :=< $a1 in $a1ctx")
          a1ctx.instantiateL(eA1, a1) flatMap { theta => // Γ[â₂,â₁,â=â₁→â₂] ⊢ â₁ :=< A1 ⊣ Θ
            tracer.trace(s"- InstRArr(2) ${a2.apply(theta)} :=< $eA2 in $theta")
            theta.instantiateR(a2.apply(theta), eA2) // Θ ⊢ [Θ]A2 :=< â₂ ⊣ ∆
          }
        }

      // InstRAllL :: Γ[â],▶ĉ,ĉ ⊢ [ĉ/β]B :=< â ⊣ ∆,▶ĉ,∆′
      case absT @ Abs(_, b) if contains(eA) =>
        val uB = absT.uv
        val eC = freshEVar("c")
        val mC = new NMark(eC)
        val instCtx = extend(mC).extend(eC) // Γ[â],▶ĉ,ĉ
        tracer.trace(s"- InstRAllL [$eC/$uB]$b :=< $eA in $instCtx")
        instCtx.instantiateR(b.subst(eC, uB), eA) map { // Γic ⊢ [ĉ/β]B :=< â ⊣ ∆,▶ĉ,∆′
          deltaEtc => deltaEtc.peel(mC) // ∆
        }

      case _ => Left(s"Failed to instantiate '$t' to '$eA'\n (context: $this)")
    }

    def toString = notes.toString
  }

  def check (
    tree :TermTree, checkCtx :Context, checkType :Type, resType :Type,
    peeler :(ctx :Context) => Context = ctx => ctx
  ) :(Type, Context) = tree.check(checkCtx, checkType) match {
    case Left(msg) => (new Error(msg), checkCtx)
    case Right(checkedCtx) => (resType, peeler(checkedCtx))
  }

//   interface DefTree {
//   readonly signature :Type
// }

// export function check (tree :Tree, checkCtx :Context, checkType :Type, resType :Type,
//                        peeler :(ctx :Context) => Context = ctx => ctx) :[Type, Context] {
//   const checkedCtx = tree.check(checkCtx, checkType)
//   return typeof checkedCtx === 'string' ?
//     [new Error(checkedCtx), checkCtx] :
//     [resType, peeler(checkedCtx)]
// }

}
