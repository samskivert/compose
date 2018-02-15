//
// Compose Mark 0 compiler
// https://github.com/samskivert/compose/c0/LICENSE

package compose

object Typer {
  import Trees._
  import Types._
  import Contexts._
  import Symbols._
  import Names._

  def typeFor (ident :TypeName)(implicit ctx :Context) :Type = ctx.scope.lookup(ident) match {
    case NoType => Unknown(ident)
    case sym    => sym.info
  }

  def typeFor (tree :TypeTree)(implicit ctx :Context) :Type = tree match {
    case TypeRef(name)         => typeFor(name)
    case TypeArrow(args, ret)  => Arrow(Seq(), args.map(typeFor), typeFor(ret))
    case TypeApply(ctor, args) => Apply(typeFor(ctor), args.map(typeFor))
  }

  def typed (tree :Tree)(implicit ctx :Context) :Tree = tree match {
    case typet :TypeTree => typet.withType(typeFor(typet))
    case termt :TermTree => typed(termt)
    case patt  :PatTree  => typed(patt)
    case deft  :DefTree  => typed(deft)
  }

  def typed (tree :TermTree)(implicit ctx :Context) :TermTree = tree match {
    case expr :Expr => typed(expr)
  }
  def typed (tree :PatTree)(implicit ctx :Context) :PatTree = ???

  // def trees define types, they don't have types in the expression sense
  def typed (tree :DefTree)(implicit ctx :Context) :DefTree = tree.withType(Prims.None)

  // TODO: tree copier?

  def typed (tree :Expr)(implicit ctx :Context) :Expr = tree match {
    case Literal(const) => tree.withType(Const(const))
    case ArrayLiteral(values) =>
      val typedValues = values.map(typed) // TODO: what about empty array?
      ArrayLiteral(typedValues).withType(unify(typedValues.map(_.tpe)))
    case IdentRef(ident) => ???
    case Select(expr, field) => ???
    case Tuple(exprs) => ???
    case Lambda(args, body) => ???
    case FunApply(kind, fun, typeArgs, args) => ???
    case If(cond, ifTrue, ifFalse) => ???
    case Match(cond, cases) => ???
    case Cond(conds, elseResult) => ???
    case MonadComp(elem, clauses) => ???
    case DefExpr(df) => ???
    case Block(exprs) => ???
    case Assign(ident, value) => ???
    case While(cond, body) => ???
    case DoWhile(body, cond) => ???
    case For(gens, body) => ???
  }

  def unify (types :Seq[Type]) :Type = ???
}
