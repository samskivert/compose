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
    case TypeApply(ctor, args) =>
      if (ctor != ArrayName) Apply(typeFor(ctor), args.map(typeFor))
      else if (args.size != 1) Error("Array requires one type parameter")
      else Array(typeFor(args(0)))
  }

  // TODO: tree copier?

  // TODO: pass "proto"-type into typed calls for local type inference

  private def tag[A] (msg :String, a :A) :A = { println(msg + ":" + a); a }

  def typed (tree :Tree)(implicit ctx :Context) :Tree = tree match {
    case typet :TypeTree => typedType(typet)
    case termt :TermTree => typedTerm(termt)
    case patt  :PatTree  => typedPat(patt)
    case cmpt  :CompTree => typedComp(cmpt)
    case deft  :DefTree  => typedDef(deft)
  }

  def typedType (tree :TypeTree)(implicit ctx :Context) :TypeTree = tree.withType(typeFor(tree))

  def typedTerm (tree :TermTree)(implicit ctx :Context) :TermTree = tree match {
    case OmittedBody =>
      tree.withType(Prim.None)

    case Literal(const) =>
      tree.withType(Const(const))

    case ArrayLiteral(values) =>
      val typedValues = values map typedTerm // TODO: disallow empty array literal?
      ArrayLiteral(typedValues).withType(unify(typedValues.map(_.tpe)))

    case IdentRef(ident) =>
      tree.withType(typeFor(ident))

    case Select(expr, field) =>
      val typedExpr = typedTerm(expr)
      def asReceiverFun (errmsg :String) = ctx.scope.lookup(field).info match {
        case funType @ Arrow(_, _, resultType) =>
          FunApply(FunKind.Receiver, IdentRef(field).withType(funType), Seq(), Seq(typedExpr)).
            withType(resultType)
        case _ => tree.withType(Error(errmsg))
      }
      // if the receiver is not a record, or does not define the selected field, attempt to resolve
      // that field as a function and pass the receiver as a single argument to it
      typedExpr.tpe match {
        case Record(name, _, fields) => fields.find(_.name == field) match {
          case Some(field) => Select(typedExpr, field.name).withType(field.tpe)
          case None => asReceiverFun(
            s"'${name}' record does not define a '${field}' field, " +
              s"nor does a '${field}' function exist.")
        }
        case _ => asReceiverFun(s"No function named '${field}' could be found")
      }

    case Index(expr, index) =>
      val typedExpr = typedTerm(expr) // TODO: expect array
      val typedIndex = typedTerm(index) // TODO: expect integral
      Index(typedExpr, typedIndex).withType(typedExpr.tpe match {
        case Array(elem) => elem
        case _           => Error("Target of index must be an array type")
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
      // if `fun` is a Select, we type the receiver and if it's not a record (or if it is a record
      // and lacks selected a field, or if it has the field and the field's type is not a fun),
      // then assume we're looking at a "method invocation style" apply and flip the receiver into
      // the first arg and the selected field into an ident ref
      val (typedFun, fullKind, fullArgs) = fun match {
        case Select(recv, field) =>
          val typedRecv = typedTerm(recv)
          typedRecv.tpe match {
            // TODO: make sure the type of the matching field is a fun...
            case Record(_, _, fields) if (fields.exists(_.name == field)) =>
              (typedTerm(fun), kind, args)
            case _ => (typedTerm(IdentRef(field)), FunKind.Receiver, recv +: args)
          }
        case _ => (typedTerm(fun), kind, args)
      }
      val typedParams = params map typedType
      val (argTypes, resultType) = applyType(typedFun.tpe, typedParams.map(_.tpe)) match {
        case Arrow(params, args, result) =>
          // TODO: type application should leave no unapplied params (so assert?), or maybe it does
          // and we attempt to infer those params, or...
          (args, result)
        case tpe =>
          (Seq(), Error(s"Target of apply must be an arrow type (got: $tpe)"))
      }
      // TODO: use argTypes as prototypes when typing args
      val typedArgs = fullArgs map typedTerm
      FunApply(fullKind, typedFun, typedParams, typedArgs).withType(resultType)

    case If(cond, ifTrue, ifFalse) =>
      val typedCond = typedTerm(cond) // TODO: expect Bool
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
    case IdentPat(ident) =>
      tree.withType(typeFor(ident))
    case LiteralPat(const) =>
      tree.withType(Const(const))
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

  def typedComp (tree :CompTree)(implicit ctx :Context) :CompTree = tree match {
    case Generator(name, expr) =>
      val typedExpr = typedTerm(expr) // TODO: any expected type? probably not because generators
                                      // will presumable be driven by a traversable type class
      Generator(name, typedExpr).withType(Prim.None) // TODO: should probably have "generated" type
    case Filter(expr) =>
      val typedExpr = typedTerm(expr) // TODO: expect Bool
      Filter(typedExpr).withType(typedExpr.tpe)
  }

  // TODO: these should properly type all the subtrees instead of reusing the types computed
  // during indexing OR we should actually type the subtrees during indexing, but that seems
  // fiddlier so maybe since this is just c0 we'll just do this work twice

  def typedDef (tree :DefTree)(implicit ctx :Context) :DefTree = tree match {
    case Param(name) =>
      Param(name).withType(typeFor(name))
    case Constraint(name, params) =>
      val typedParams = params map { param => typedDef(param).asInstanceOf[Param] }
      val ctorSym = ctx.scope.lookup(name)
      Constraint(name, typedParams).withType(Apply(ctorSym.info, typedParams.map(_.tpe)))

    case ArgDef(docs, name, typ) =>
      val typedTyp = typedType(typ)
      ArgDef(docs, name, typedTyp).withType(typedTyp.tpe)

    case FunDef(docs, name, params, args, result, body) =>
      println(s"typing fundef $name => ${ctx.scope.lookup(name)}")
      val funSym = ctx.scope.lookup(name)
      assert(funSym.exists, s"Missing symbol for fun def $name")
      val Arrow(paramTypes, argTypes, resultType) = funSym.info
      val typedParams = params zip paramTypes map(pt => pt._1 withType pt._2)
      val typedArgs = args zip argTypes map(at => at._1 withType at._2)
      val typedResult = result.withType(resultType)
      val typedBody = typedTerm(body)(ctx.withOwner(funSym))
      FunDef(docs, name, typedParams, typedArgs, typedResult, typedBody).withType(funSym.info)

    case Binding(name, typ, value) =>
      Binding(name, typedType(typ), typedTerm(value)).withType(Prim.None)
    case LetDef(binds) =>
      val typedBinds = binds map { bind => typedDef(bind).asInstanceOf[Binding] }
      LetDef(typedBinds).withType(Prim.None)
    case VarDef(binds) =>
      val typedBinds = binds map { bind => typedDef(bind).asInstanceOf[Binding] }
      VarDef(typedBinds).withType(Prim.None)

    case FieldDef(docs, name, typ) =>
      FieldDef(docs, name, typedType(typ)).withType(Prim.None)
    case RecordDef(docs, name, params, fields) =>
      val recSym = ctx.scope.lookup(name)
      assert(recSym.exists, s"Missing symbol for record def $name")
      val Record(_, paramTypes, fieldTypes) = recSym.info
      val typedParams = params zip paramTypes map(pt => pt._1 withType pt._2)
      val typedFields = fields zip fieldTypes map(fd => fd._1 withType fd._2.tpe)
      RecordDef(docs, name, typedParams, typedFields).withType(recSym.info)

    case UnionDef(docs, name, params, cases) =>
      val unionSym = ctx.scope.lookup(name)
      assert(unionSym.exists, s"Missing symbol for union def $name")
      val Union(_, paramTypes, caseTypes) = unionSym.info
      val typedParams = params zip paramTypes map(pt => pt._1 withType pt._2)
      val typedCases = cases zip caseTypes map(cs => cs._1 withType cs._2)
      UnionDef(docs, name, typedParams, typedCases).withType(unionSym.info)

    case FaceDef(docs, name, params, parents, meths) =>
      val faceSym = ctx.scope.lookup(name)
      println(s"typing facedef $name => $faceSym")
      assert(faceSym.exists, s"Missing symbol for interface def $name")
      val Interface(_, paramTypes, methTypes) = faceSym.info
      val typedParams = params zip paramTypes map(pt => pt._1 withType pt._2)
      val typedParents = parents map { parent => typedDef(parent).asInstanceOf[Constraint] }
      val faceCtx = ctx.withOwner(faceSym)
      val typedMeths = meths map { meth => typedDef(meth)(faceCtx).asInstanceOf[FunDef] }
      FaceDef(docs, name, typedParams, typedParents, typedMeths).withType(faceSym.info)

    case MethodBinding(meth, fun) =>
      // TODO
      tree.withType(Prim.None)
    case ImplDef(docs, name, params, face, funs) =>
      // TODO
      tree.withType(Prim.None)
  }
}
