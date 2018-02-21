//
// Compose Mark 0 compiler
// https://github.com/samskivert/compose/c0/LICENSE

package compose

object Typer {
  import Trees._
  import Types._
  import Contexts._
  import Names._
  import Indexer._

  def typeFor (ident :TypeName)(implicit ctx :Context) :Type =
    ctx.scope.lookup(ident).map(_.info) getOrElse Unknown(ident)
  def typeFor (ident :TermName)(implicit ctx :Context) :Type =
    ctx.scope.lookup(ident).map(_.info) getOrElse Unknown(ident)

  def typeFor (tree :TypeTree)(implicit ctx :Context) :Type = tree match {
    case OmittedType           => Prim.None
    case TypeRef(name)         => typeFor(name)
    case TypeArrow(args, ret)  => Arrow(Seq(), args.map(typeFor), typeFor(ret))
    case TypeApply(ctor, args) => Apply(typeFor(ctor), args.map(typeFor))
  }

  // TODO: tree copier?

  // TODO: pass "proto"-type into typed calls for local type inference

  def typed (tree :Tree)(implicit ctx :Context) :Tree = {
    println(s"typing $tree")
    tree match {
      case typet :TypeTree => typedType(typet)
      case termt :TermTree => typedTerm(termt)
      case patt  :PatTree  => typedPat(patt)
      case cmpt  :CompTree => typedComp(cmpt)
      case deft  :DefTree  => typedDef(deft)
    }
  }
  def typedType (tree :TypeTree)(implicit ctx :Context) :TypeTree = tree.withType(typeFor(tree))

  def typedTerm (tree :TermTree)(implicit ctx :Context) :TermTree = tree match {
    case Literal(const) =>
      tree.withType(Const(const))

    case ArrayLiteral(values) =>
      val typedValues = values map typedTerm // TODO: disallow empty array literal?
      ArrayLiteral(typedValues).withType(unify(typedValues.map(_.tpe)))

    case IdentRef(ident) =>
      tree.withType(typeFor(ident))

    case Select(expr, field) =>
      val typedExpr = typedTerm(expr)
      Select(typedExpr, field).withType(typedExpr.tpe match {
        case Record(name, params, fields) =>
          fields.find(_.name == field).map(_.tpe) getOrElse Error(
            s"'${name}' record does not define a '${field}' field.")
        case _ => Error("Target of select must be a record type")
      })

    case Tuple(exprs) =>
      val typedExprs = exprs map typedTerm
      Tuple(typedExprs).withType(Apply(Prim.tuple(exprs.size), typedExprs.map(_.tpe)))

    case Lambda(args, body) =>
      val lamSym = ctx.owner.createTerm(NoName)
      val lamCtx = ctx.withOwner(lamSym)
      val argSyms = args map { arg => index(arg)(lamCtx) }
      val typedBody = typedTerm(body)(lamCtx)
      val typedArgs = args zip argSyms map {
        case (arg, argSym) => arg.withType(argSym.info)
      }
      Lambda(typedArgs, typedBody).withType(typedBody.tpe)

    case FunApply(kind, fun, params, args) =>
      val typedFun = typedTerm(fun)
      val typedParams = params map typedType
      val (argTypes, resultType) = apply(typedFun.tpe, typedParams.map(_.tpe)) match {
        case Arrow(params, args, result) =>
          // TODO: type application should leave no unapplied params (so assert?), or maybe it does
          // and we attempt to infer those params, or...
          (args, result)
        case tpe =>
          (Seq(), Error(s"Target of apply must be an arrow type (got: $tpe)"))
      }
      // TODO: use argTypes as prototypes when typing args
      val typedArgs = args map typedTerm
      FunApply(kind, typedFun, typedParams, typedArgs).withType(resultType)

    case If(cond, ifTrue, ifFalse) =>
      val typedCond = typedTerm(cond) // TODO: "expect" a Bool
      val typedTrue = typedTerm(ifTrue)
      val typedFalse = typedTerm(ifFalse)
      If(typedCond, typedTrue, typedFalse).withType(unify(Seq(typedTrue.tpe, typedFalse.tpe)))

    case Match(cond, cases) =>
      val typedCond = typedTerm(cond)
      val typedCases = cases map { cs => typedPat(cs).asInstanceOf[Case] }
      Match(typedCond, typedCases).withType(unify(typedCases.map(_.result.tpe)))

    case Cond(conds, elseResult) =>
      val typedConds = conds map { cd => typedPat(cd).asInstanceOf[Condition] }
      val typedElse = typedTerm(elseResult)
      Cond(typedConds, typedElse).withType(unify(typedConds.map(_.result.tpe) :+ typedElse.tpe))

    case MonadComp(elem, clauses) => ???

    case DefExpr(df) =>
      DefExpr(typedDef(df)).withType(Prim.None) // TODO: real type? or symbol is all we need...

    case Block(exprs) =>
      // first index the block, then type all of its statements
      exprs foreach index
      val typedExprs = exprs map typedTerm
      Block(typedExprs).withType(typedExprs.last.tpe)

    case Assign(ident, value) =>
      Assign(ident, typedTerm(value)).withType(Prim.None)

    case While(cond, body) =>
      While(typedTerm(cond), typedTerm(body)).withType(Prim.None)

    case DoWhile(body, cond) =>
      DoWhile(typedTerm(body), typedTerm(cond)).withType(Prim.None)

    case For(gens, body) => ???
  }

  def typedPat (tree :PatTree)(implicit ctx :Context) :PatTree = tree match {
    case IdentPat(ident) => tree.withType(typeFor(ident))
    case LiteralPat(const) => tree.withType(Const(const))
    case DestructPat(ctor, bindings) =>
      // TODO: resolve constructor in symbol table; use that to type the bindings
      tree.withType(Prim.None)
    case Case(pattern, guard, result) =>
      val typedPattern = typedPat(pattern)
      val typedGuard = guard map typedTerm // TODO: expect Bool
      // TODO: use ctx obtained by typing pattern as it has necessary name bindings
      val typedResult = typedTerm(result)
      Case(typedPattern, typedGuard, typedResult).withType(typedResult.tpe)
    case Condition(guard, result) =>
      val typedGuard = typedTerm(guard) // TODO: expect bool
      val typedResult = typedTerm(result)
      Condition(typedGuard, typedResult).withType(typedResult.tpe)
  }

  def typedComp (tree :CompTree)(implicit ctx :Context) :CompTree = ???

  def typedDef (tree :DefTree)(implicit ctx :Context) :DefTree = tree match {
    case ArgDef (docs, name, typ) => tree.withType(Prim.None)
    case ParamDef (name, bound) => tree.withType(Prim.None)
    case FunDef (docs, name, params, args, result, body) =>
      println(s"typing fundef $name => ${ctx.scope.lookup(name)}")
      val funSym = ctx.scope.lookup(name)
      assert(funSym.exists, s"Missing symbol for fundef $name")
      val Arrow(paramTypes, argTypes, resultType) = funSym.info
      val typedParams = params zip paramTypes map(pt => pt._1 withType pt._2)
      val typedArgs = args zip argTypes map(at => at._1 withType at._2)
      val typedResult = result.withType(resultType)
      val typedBody = typedTerm(body)(ctx.withOwner(funSym))
      FunDef(docs, name, typedParams, typedArgs, typedResult, typedBody).withType(Prim.None)
    case Binding (name, typ, value) => tree.withType(Prim.None)
    case LetDef (bindings) => tree.withType(Prim.None)
    case VarDef (bindings) => tree.withType(Prim.None)
    case FieldDef (docs, name, typ) => tree.withType(Prim.None)
    case RecordDef (docs, name, args, fields) => tree.withType(Prim.None)
    case UnionDef (docs, name, args, cases) => tree.withType(Prim.None)
  }

  // TODO: at least check that all the types are the same
  def unify (types :Seq[Type]) :Type = if (types.isEmpty) ??? else types.head
}
