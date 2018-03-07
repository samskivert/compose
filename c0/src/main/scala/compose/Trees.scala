//
// Compose Mark 0 compiler
// https://github.com/samskivert/compose/c0/LICENSE

package compose

import java.io.{StringWriter, PrintWriter}
import scala.collection.mutable.ArrayBuffer

object Trees {
  import Constants._
  import Contexts._
  import Names._
  import Types._
  import Indexer._
  import Symbols._

  private var treeId = 0

  /** A node in the AST. Nodes know how to type themselves, given a Context.
    *
    * Typing procedes in two phases: indexing (creating symbols for all definitions) then typing
    * (assigning a type to every node in the tree). Indexing is performed for a given scope prior
    * to typing the tree at that scope.
    *
    * Tree nodes contain a mutable type variable that is assigned during the typing process. Typing
    * is initiated by calling `typed`. TODO: explain procedure for indexing and typing a tree.
    */
  sealed abstract class Tree extends Cloneable with Product {
    private[this] var treeType :Type = _
    val id = { treeId += 1 ; treeId }

    /** The type constructor at the root of this tree */
    type ThisTree <: Tree

    /** Does this tree represent a type? */
    def isType: Boolean = false

    /** Does this tree represent a term? */
    def isTerm: Boolean = false

    /** Does this tree define a new symbol? */
    def isDef: Boolean = false

    /** Is this a part of a pattern (but not a term or def)? */
    def isPattern: Boolean = false

    /** Is this part of a comprehension expression (but not a term or def)? */
    def isComprehension :Boolean = false

    /** Whether or not this tree has been typed. */
    def isTyped = treeType != null

    /** Returns the type of this tree. Throws an exception if called on an untyped tree. */
    def tpe :Type = {
      if (treeType == null) throw new Exception(s"Type of $id#$this is not assigned")
      treeType
    }

    /** Ensures this tree (and its children) are typed, and returns it.
      * @param proto the expected type of the tree (if known, otherwise `None`). Used for local
      * type inference and type checking.
      */
    def typed (proto :Type = Untyped)(implicit ctx :Context) :this.type = {
      // TODO: save ctx and ensure we don't request the type later with an invalid context?
      if (!isTyped) {
        treeType = computeType(proto)
        computeBodyType()
      }
      this
    }

    // TEMP: what's the "proper" way to do this?
    def retype (tpe :Type) :Unit = { treeType = tpe }

    protected def computeType (proto :Type)(implicit ctx :Context) :Type
    protected def computeBodyType ()(implicit ctx :Context) :Unit = {}
  }

  def typed[T <: Tree](trees :Seq[T], protos :Seq[Type])(implicit ctx :Context) :Seq[T] =
    (trees zip protos).map(tp => (tp._1 typed tp._2))
  def typedTypes[T <: Tree](trees :Seq[T], protos :Seq[Type])(implicit ctx :Context) :Seq[Type] =
    (trees zip protos).map(tp => (tp._1 typed tp._2).tpe)

  sealed trait TypeTree extends Tree {
    type ThisTree <: TypeTree
    override def isType = true
  }
  case object OmittedType extends TypeTree {
    protected def computeType (proto :Type)(implicit ctx :Context) = Untyped
    override def toString = ""
  }
  case class TypeRef (name :TypeName) extends TypeTree {
    protected def computeType (proto :Type)(implicit ctx :Context) = {
      // TODO: check that types match
      typeFor(name)
    }
    override def toString = name.toString
  }
  case class TypeApply (ctor :TypeName, params :Seq[TypeTree]) extends TypeTree {
    protected def computeType (proto :Type)(implicit ctx :Context) = {
      val paramProtos = proto match {
        case Apply(pctorType, pparamTypes) => pparamTypes
        // TODO: should anything other than None here be a type mismatch?
        case _ => params.map(_ => Untyped)
      }
      val paramTypes = typedTypes(params, paramProtos)
      if (ctor == ArrayName) {
        if (paramTypes.size != 1) Error(s"Array must be applied to exactly one parameter")
        else Array(paramTypes(0))
      } else {
        val ctorType = typeFor(ctor)
        // if we're typing a def, we want to leave the type unapplied so as to enable recursive type
        // definitions; but if we're typing an expression, we want to apply the type now
        if (ctx.typingDef) Apply(ctorType, paramTypes) else applyType(ctorType, paramTypes)
      }
    }
    override def toString = s"$ctor${params.mkString("[", ", ", "]")}"
  }
  // TODO: do we need a syntax for universally quantified arrows?
  case class TypeArrow (args :Seq[TypeTree], ret :TypeTree) extends TypeTree {
    protected def computeType (proto :Type)(implicit ctx :Context) = {
      val (argProtos, retProto) = proto match {
        case Arrow(_, argProtos, retProto) => (argProtos, retProto)
        // TODO: same as above re: error on mismatch
        case _ => (args.map(_ => Untyped), Untyped)
      }
      Arrow(Seq(), typedTypes(args, argProtos), ret.typed(retProto).tpe)
    }
    override def toString = s"${args.mkString("(", ", ", ")")} => $ret"
  }

  //
  // Definitions

  sealed trait DefTree extends Tree {
    private[this] var treeSym :Symbol = _
    type ThisTree <: DefTree

    /** Whether or not the symbol defined by this tree has been created. This happens during the
      * indexing step which precedes type checking. */
    def isIndexed = treeSym != null

    /** Returns the symbol for the type defined by this tree. Throws an exception if called on an
      * unindexed tree. */
    def sym :Symbol = {
      if (treeSym == null) throw new Exception(s"Symbol of $id#$this is not created")
      treeSym
    }

    /** Assigns a symbol to this tree (called by `Indexer`). */
    def index (sym :Symbol) :sym.type = {
      if (treeSym != null) throw new Exception(s"Symbol of $id#$this already assigned")
      treeSym = sym
      sym
    }

    override def isDef = true
  }

  // used to intertwingle params and constraints into a single list
  // (I don't want to complicate the syntax with a separate list for constraints)
  sealed trait ParamOrConst {
    def accum (params :ArrayBuffer[Param], csts :ArrayBuffer[Constraint]) :Unit
  }
  case class ConstrainedParam (param :Param, cst :Constraint) extends ParamOrConst {
    def accum (params :ArrayBuffer[Param], csts :ArrayBuffer[Constraint]) :Unit = {
      params += param
      csts += cst
    }
  }

  case class Param (name :TypeName) extends DefTree with ParamOrConst {
    type ThisTree <: Param
    def accum (params :ArrayBuffer[Param], csts :ArrayBuffer[Constraint]) :Unit = params += this
    protected def computeType (proto :Type)(implicit ctx :Context) = Var(sym.asType, ctx.scope.id)
  }
  // TODO: a constraint is technically not a def tree, it's a ref tree... change?
  case class Constraint (name :TypeName, params :Seq[TypeRef]) extends DefTree with ParamOrConst {
    type ThisTree <: Constraint
    def accum (params :ArrayBuffer[Param], csts :ArrayBuffer[Constraint]) :Unit = csts += this
    protected def computeType (proto :Type)(implicit ctx :Context) =
      // TODO: will we ever have an expected type for a constraint?
      applyType(typeFor(name), params.map(_.typed().tpe))
  }

  // TODO: destructuring fun arg bindings
  case class ArgDef (docs :Seq[String], name :TermName, typ :TypeTree) extends DefTree {
    type ThisTree <: ArgDef
    protected def computeType (proto :Type)(implicit ctx :Context) = typ.typed(proto).tpe
  }
  case class FunDef (docs :Seq[String], name :TermName, params :Seq[Param], csts :Seq[Constraint],
                     args :Seq[ArgDef], result :TypeTree, body :TermTree) extends DefTree {
    type ThisTree <: FunDef
    protected def computeType (proto :Type)(implicit ctx :Context) = {
      val funSym = ctx.scope.lookup(name)
      assert(funSym.exists, s"Missing symbol for fun def $name")
      val fullParams = ctx.owner.params ++ params
      def typedWith (implicit ctx :Context) = Arrow(
        fullParams.map(_.typed().tpe), args.map(_.typed().tpe), result.typed().tpe)
      typedWith(ctx.withOwner(funSym))
    }
    override protected def computeBodyType ()(implicit ctx :Context) = {
      val funSym = ctx.scope.lookup(name)
      assert(funSym.exists, s"Missing symbol for fun def $name")
      val Arrow(_, _, retType) = this.tpe
      body.typed(retType)(ctx.withOwner(funSym).withoutFlag(TypingDef))
    }
  }
  object FunDef {
    def mk (docs :Seq[String], name :TermName, paramsCsts :Seq[ParamOrConst],
            args :Seq[ArgDef], result :TypeTree, body :TermTree) :FunDef = {
      val params = ArrayBuffer[Param]() ; val csts = ArrayBuffer[Constraint]()
      paramsCsts.foreach(_.accum(params, csts))
      FunDef(docs, name, params, csts, args, result, body)
    }
  }

  // TODO: destructuring let/var bindings
  case class Binding (name :TermName, typ :TypeTree, value :TermTree) extends DefTree {
    protected def computeType (proto :Type)(implicit ctx :Context) = {
      val declType = typ.typed().tpe
      val exprType = value.typed(declType).tpe
      if (typ == OmittedType) exprType else declType
    }
  }
  case class LetDef (binds :Seq[Binding]) extends DefTree {
    protected def computeType (proto :Type)(implicit ctx :Context) = {
      val bindCtx = ctx.withoutFlag(TypingDef)
      binds.foreach(_.typed()(bindCtx))
      Untyped
    }
  }
  case class VarDef (binds :Seq[Binding]) extends DefTree {
    protected def computeType (proto :Type)(implicit ctx :Context) =  {
      val bindCtx = ctx.withoutFlag(TypingDef)
      binds.foreach(_.typed()(bindCtx))
      Untyped
    }
  }

  case class FieldDef (docs :Seq[String], name :TermName, typ :TypeTree) extends DefTree {
    type ThisTree <: FieldDef
    protected def computeType (proto :Type)(implicit ctx :Context) = typ.typed(proto).tpe
  }
  case class RecordDef (
    docs :Seq[String], name :TypeName, params :Seq[Param], fields :Seq[FieldDef]
  ) extends DefTree {
    protected def computeType (proto :Type)(implicit ctx :Context) = {
      val recSym = ctx.scope.lookup(name)
      assert(recSym.exists, s"Missing symbol for record def $name")
      val fullParams = ctx.owner.params ++ params
      def typedWith (implicit ctx :Context) = Record(
        sym.asType, fullParams.map(_.typed().tpe), fields.map(f => Field(f.name, f.typed().tpe)))
      typedWith(ctx.withOwner(recSym))
    }
  }

  case class UnionDef (
    docs :Seq[String], name :TypeName, params :Seq[Param], cases :Seq[DefTree]
  ) extends DefTree {
    protected def computeType (proto :Type)(implicit ctx :Context) = {
      val unionSym = ctx.scope.lookup(name)
      assert(unionSym.exists, s"Missing symbol for union def $name")
      def typedWith (implicit ctx :Context) =
        Union(sym.asType, params.map(_.typed().tpe), cases.map(_.typed().tpe))
      typedWith(ctx.withOwner(unionSym))
    }
  }

  case class FaceDef (
    docs :Seq[String], name :TypeName, params :Seq[Param], parents :Seq[Constraint],
    meths :Seq[FunDef]
  ) extends DefTree {
    protected def computeType (proto :Type)(implicit ctx :Context) = {
      val faceSym = ctx.scope.lookup(name)
      assert(faceSym.exists, s"Missing symbol for interface def $name")
      def typedWith (implicit ctx :Context) = {
        params.foreach(_.typed())
        parents.foreach(_.typed())
        // TODO: parents should be in type?
        Interface(sym.asType, params.map(_.tpe), meths.map(m => Method(m.name, m.typed().tpe)))
      }
      typedWith(ctx.withOwner(faceSym))
    }
  }

  case class MethodBinding (meth :TermName, fun :TermName) extends DefTree {
    protected def computeType (proto :Type)(implicit ctx :Context) = Untyped // TODO
  }
  case class ImplDef (
    docs :Seq[String], name :TermName, params :Seq[Param], csts :Seq[Constraint], face :TypeTree,
    binds :Seq[MethodBinding]
  ) extends DefTree {
    protected def computeType (proto :Type)(implicit ctx :Context) = Untyped // TODO
    // TEMP: impl is typed as function from constraints to record
    // implSym.initInfoLazy(() => Arrow(paramSyms.map(_.info), Seq(), typeFor(face)(implCtx)))
  }
  object ImplDef {
    def mk (docs :Seq[String], name :TermName, paramsCsts :Seq[ParamOrConst],
            face :TypeTree, binds :Seq[MethodBinding]) :ImplDef = {
      val params = ArrayBuffer[Param]() ; val csts = ArrayBuffer[Constraint]()
      paramsCsts.foreach(_.accum(params, csts))
      ImplDef(docs, name, params, csts, face, binds)
    }
  }

  // case class TypeDef (name :TypeName, TODO)

  //
  // Patterns and conditions

  sealed trait PatTree extends Tree {
    type ThisTree <: PatTree
    override def isPattern = true
  }

  // TODO: optional type annotation? (needed for destructuring funargs)
  case class IdentPat (ident :TermName) extends PatTree {
    protected def computeType (proto :Type)(implicit ctx :Context) = typeFor(ident)
    override def toString = ident.toString
  }
  case class LiteralPat (const :Constant) extends PatTree {
    protected def computeType (proto :Type)(implicit ctx :Context) = Const(const)
    override def toString = const.toString
  }
  case class DestructPat (ctor :TermName, binds :Seq[PatTree]) extends PatTree {
    // TODO: resolve constructor in symbol table; use that to type the bindings
    protected def computeType (proto :Type)(implicit ctx :Context) = ???
    override def toString = s"$ctor(${binds.mkString(", ")})"
  }
  // TODO: named destructors? (i.e. Node[left @ Node[ll lr], right])
  case class Case (pattern :PatTree, guard :Option[TermTree], result :TermTree) extends PatTree {
    type ThisTree <: Case
    protected def computeType (proto :Type)(implicit ctx :Context) = {
      // TODO: we need a way to pass in the expected type of the pattern (via context?)...
      pattern.typed()
      guard.foreach(_.typed(Prim.Bool))
      // TODO: use ctx obtained by typing pattern as it has necessary name bindings
      result.typed(proto).tpe
    }
  }
  case class Condition (guard :TermTree, result :TermTree) extends PatTree {
    type ThisTree <: Condition
    protected def computeType (proto :Type)(implicit ctx :Context) = {
      guard.typed(Prim.Bool)
      result.typed(proto).tpe
    }
  }

  //
  // Comprehensions

  sealed trait CompTree extends Tree {
    type ThisTree <: CompTree
    override def isComprehension = true
  }

  case class Generator (name :TermName, expr :TermTree) extends CompTree {
    // TODO: any expected type? probably not because generators will presumably be driven by a
    // traversable type class
    protected def computeType (proto :Type)(implicit ctx :Context) = expr.typed(proto).tpe
  }
  case class Filter (expr :TermTree) extends CompTree {
    protected def computeType (proto :Type)(implicit ctx :Context) = expr.typed(Prim.Bool).tpe
  }

  //
  // Terms / expressions

  sealed trait TermTree extends Tree {
    type ThisTree <: TermTree
    override def isTerm = true
  }
  case object OmittedBody extends TermTree {
    protected def computeType (proto :Type)(implicit ctx :Context) = Untyped
  }

  // pure
  case class Literal (const :Constant) extends TermTree {
    protected def computeType (proto :Type)(implicit ctx :Context) = Const(const)
  }
  case class ArrayLiteral (values :Seq[TermTree]) extends TermTree {
    protected def computeType (proto :Type)(implicit ctx :Context) = {
      val elemProto = proto match {
        case Array(elemType) => elemType
        // TODO: handle type mismatch here?
        case _ => Untyped
      }
      Array(if (values.isEmpty) elemProto else join(values.map(_.typed(elemProto).tpe)))
    }
  }

  case class IdentRef (ident :TermName) extends TermTree {
    protected def computeType (proto :Type)(implicit ctx :Context) = {
      // TODO: check that type matches proto?
      typeFor(ident)
    }
  }

  case class Select (expr :TermTree, field :TermName) extends TermTree {
    protected def computeType (proto :Type)(implicit ctx :Context) = {
      // if the receiver is not a record, or does not define the selected field, attempt to resolve
      // that field as a function and pass the receiver as a single argument to it
      def asReceiverFun (errmsg :String) = ctx.scope.lookup(field).info match {
        // TODO: we need to record the fun chosen here for funapply, infer args (using proto), etc.
        case Arrow(_, _, resultType) => resultType
        case _ => Error(errmsg)
      }
      // TODO: how to use prototype here? just check that we match?
      expr.typed().tpe match {
        case Record(name, _, fields) => fields.find(_.name == field) match {
          // because a record type may be recursive, we unfold once here
          case Some(field) => unfold(field.tpe)
          case None => asReceiverFun(
            s"'${name}' record does not define a '${field}' field, " +
              s"nor does a '${field}' function exist.")
        }
        case _ => asReceiverFun(s"No function named '${field}' could be found")
      }
    }
  }

  case class Index (expr :TermTree, index :TermTree) extends TermTree {
    protected def computeType (proto :Type)(implicit ctx :Context) = {
      // (TODO: expect generic integral rather than I32?)
      index.typed(Prim.I32).tpe // compute type of index for side effect
      expr.typed(Array(proto)).tpe match {
        case Array(elem) => elem
        case tpe         => Error(s"Target of @ must be an array type (got $tpe)")
      }
    }
  }

  case class Tuple (exprs :Seq[TermTree]) extends TermTree {
    protected def computeType (proto :Type)(implicit ctx :Context) = {
      val elemTypes = proto match {
        // TODO: fail if record name does not match tuple? (adapt?)
        case Record(_, _, fields) => fields.map(_.tpe)
        // TODO: fail if we don't expect a Tuple type?
        case _ => exprs.map(_ => Untyped)
      }
      applyType(Prim.tuple(exprs.size), typedTypes(exprs, elemTypes))
    }
  }

  case class Lambda (args :Seq[ArgDef], body :TermTree) extends TermTree {
    protected def computeType (proto :Type)(implicit ctx :Context) = {
      val lamSym = ctx.owner.createTerm(NoName, this) // TODO: synthesize lambda name
      val lamCtx = ctx.withOwner(lamSym)
      args foreach { arg => index(arg)(lamCtx) } // index args in lambda context
      val (argProtos, retProto) = proto match {
        case Arrow(_, argTypes, retType) => (argTypes, retType)
        // TODO: fail if expected type not arrow?
        case _ => (args.map(_ => Untyped), Untyped)
      }
      typedTypes(args, argProtos)(lamCtx) // and type them in same context
      body.typed(retProto)(lamCtx).tpe
    }
  }

  sealed trait FunKind
  object FunKind {
    case object Normal extends FunKind
    case object UnOp extends FunKind
    case object BinOp extends FunKind
    case object Receiver extends FunKind
  }
  case class FunApply (kind :FunKind, fun :TermTree, params :Seq[TypeTree],
                       args :Seq[TermTree]) extends TermTree {
    protected def computeType (proto :Type)(implicit ctx :Context) = {
      // TODO: sort out handling of foo.bar[B](baz)

      // // if `fun` is a Select, we type the receiver and if it's not a record (or if it is a record
      // // and lacks selected a field, or if it has the field and the field's type is not a fun),
      // // then assume we're looking at a "method invocation style" apply and flip the receiver into
      // // the first arg and the selected field into an ident ref
      // val (xfFun, fullKind, fullArgs) = fun match {
      //   case Select(recv, field) =>
      //     recv.typed.tpe match {
      //       // TODO: make sure the type of the matching field is a fun...
      //       case Record(_, _, fields) if (fields.exists(_.name == field)) => (fun, kind, args)
      //       case _ => (IdentRef(field), FunKind.Receiver, recv +: args)
      //     }
      //   case _ => (fun, kind, args)
      // }
      // val typedParams = mapConserve(params, typedType)
      // val typedFun = typedTerm(xfFun)
      // val appliedFunType = applyType(typedFun.tpe, typedParams.map(_.tpe))
      // val appliedTypedFun = xfFun.withType(appliedFunType)
      // val (argTypes, resultType) = appliedFunType match {
      //   case Arrow(params, args, result) =>
      //     // TODO: type application should leave no unapplied params (so assert?), or maybe it does
      //     // and we attempt to infer those params, or...
      //     (args, result)
      //   case tpe =>
      //     (Seq(), Error(s"Target of apply must be an arrow type (got: $tpe)"))
      // }
      // // TODO: use argTypes as prototypes when typing args
      // val typedArgs = mapConserve(fullArgs, typedTerm)
      // FunApply(fullKind, appliedTypedFun, typedParams, typedArgs).withType(resultType)

      val funProto = Arrow(Seq(), args.map(_ => Untyped), proto)
      fun.typed(funProto).tpe match {
        case funType @ Arrow(formalParams, formalArgs, formalResult) =>
          def unifyApply (csts :Seq[(Type, Type)]) = unify(csts).map(
            sols => applyType(funType, formalParams map subst(sols)))
          // first constrain fun type based on explicit parameters and expected return type
          val applyParams = params.map(_.typed().tpe)
          val applyCsts = (formalParams zip applyParams) :+ (formalResult -> proto)
          unifyApply(applyCsts).map(cstFunType => {
            // use the partially constrained formal argument types to type the actual arguments
            val argProtos = cstFunType.asInstanceOf[Arrow].args
            val argTypes = typedTypes(args, argProtos)
            // finally re-unify the function type using the apply constraints and any new
            // constraints generated by the typed arguments
            unifyApply(applyCsts ++ (formalArgs zip argTypes)).map(newFunType => {
              fun.retype(newFunType)
              newFunType.asInstanceOf[Arrow].result
            }).merge
          }).merge
        case tpe => Error(s"Target of apply must be an arrow type (got: $tpe)")
      }
    }
  }

  case class If (cond :TermTree, ifTrue :TermTree, ifFalse :TermTree) extends TermTree {
    protected def computeType (proto :Type)(implicit ctx :Context) = {
      cond.typed(Prim.Bool)
      join(Seq(ifTrue.typed(proto).tpe, ifFalse.typed(proto).tpe))
    }
  }
  // TODO: if+let? or maybe "let Ctor(arg) = expr" is a LetTermTree which evaluates to true/false?
  // latter might be fiddly due to it being an expr that introduces defs in a scope up the AST

  case class Match (cond :TermTree, cases :Seq[Case]) extends TermTree {
    protected def computeType (proto :Type)(implicit ctx :Context) = {
      cond.typed() // compute type of condition
      // TODO: use type of cond as proto for patterns (via context?)
      join(cases.map(_.typed(proto).result.tpe))
    }
  }
  case class Cond (conds :Seq[Condition], elseResult :TermTree) extends TermTree {
    protected def computeType (proto :Type)(implicit ctx :Context) =
      join(conds.map(_.typed(proto).result.tpe) :+ elseResult.typed(proto).tpe)
  }
  case class MonadComp (elem :TermTree, clauses :Seq[CompTree]) extends TermTree {
    protected def computeType (proto :Type)(implicit ctx :Context) = ???
  }

  case class DefExpr (df :DefTree) extends TermTree {
    protected def computeType (proto :Type)(implicit ctx :Context) =
      df.typed(proto)(ctx.withFlag(TypingDef)).tpe
  }
  case class Block (exprs :Seq[TermTree]) extends TermTree {
    protected def computeType (proto :Type)(implicit ctx :Context) = {
      // first index contents, then type contents, then use type of last expr as block type
      val stats = exprs.take(exprs.size-1)
      stats foreach index
      stats foreach { _.typed() }
      exprs.last.typed(proto).tpe
    }
  }

  // naughty
  case class Assign (ident :TermName, value :TermTree) extends TermTree {
    protected def computeType (proto :Type)(implicit ctx :Context) = { value.typed() ; Untyped }
  }

  case class While (cond :TermTree, body :TermTree) extends TermTree {
    protected def computeType (proto :Type)(implicit ctx :Context) = {
      cond.typed(Prim.Bool) ; body.typed() ; Untyped
    }
  }
  case class DoWhile (body :TermTree, cond :TermTree) extends TermTree {
    protected def computeType (proto :Type)(implicit ctx :Context) = {
      body.typed() ; cond.typed(Prim.Bool) ; Untyped
    }
  }
  case class For (gens :Seq[Generator], body :TermTree) extends TermTree {
    protected def computeType (proto :Type)(implicit ctx :Context) = ???
  }

  /** Creates a `Record` tree for a `Tuple` of rank `rank`. This tree is typed and the resulting
    * type is used to type the special tuple syntax. */
  def tupleTree (rank :Int) :Tree = {
    val paramNames = 1 to rank map { n => typeName(s"T$n") }
    val fields = 1 to rank map { n => FieldDef(Seq(), termName(s"_$n"), TypeRef(paramNames(n-1))) }
    RecordDef(Seq(), typeName(s"Tuple$rank"), paramNames map Param, fields)
  }

  //
  // Traversals

  /** Folds an operation over a tree, allowing control over when and whether to recurse. */
  abstract class Accumulator[T] {

    /** Applies an operation to a tree node.
      * The implementation must call `foldOver` to recurse into the node's children. */
    def apply (t :T, tree :Tree)(implicit ctx :Context) :T

    /** Folds this accumulator over a sequence of `trees`. */
    def apply (t :T, trees :Traversable[Tree])(implicit ctx :Context) :T = (t /: trees)(apply)

    /** Folds this accumulator over the children of `tree`. */
    def foldOver (t :T, tree :Tree)(implicit ctx :Context) :T = tree match {
      // type trees
      case OmittedType => t
      case TypeRef(name) => t
      case TypeApply(ctor, args) => apply(t, args)
      case TypeArrow(args, ret) => apply(apply(t, args), ret)

      // def trees
      case Param(name) => t
      case Constraint(name, params) => apply(t, params)
      case ArgDef(docs, name, typ) => apply(t, typ)
      case FunDef(docs, name, params, args, csts, result, body) =>
        apply(apply(apply(apply(apply(t, params), args), csts), result), body)
      case Binding (name, typ, value) => apply(apply(t, typ), value)
      case LetDef(binds) => apply(t, binds)
      case VarDef(binds) => apply(t, binds)
      case FieldDef(docs, name, typ) => apply(t, typ)
      case RecordDef(docs, name, args, fields) => apply(apply(t, args), fields)
      case UnionDef(docs, name, args, cases) => apply(apply(t, args), cases)
      case FaceDef(docs, name, params, parents, meths) =>
        apply(apply(apply(t, params), parents), meths)
      case MethodBinding(meth, fun) => t
      case ImplDef(docs, name, params, csts, parent, binds) =>
        apply(apply(apply(apply(t, params), csts), parent), binds)

      // pat trees
      case IdentPat (ident) => t
      case LiteralPat (const) => t
      case DestructPat (ctor, binds) => apply(t, binds)
      case Case (pattern, guard, result) => apply(apply(apply(t, pattern), guard), result)
      case Condition (guard, result) => apply(apply(t, guard), result)

      // comp trees
      case Generator (name, expr) => apply(t, expr)
      case Filter (expr) => apply(t, expr)

      // term trees
      case OmittedBody => t
      case Literal (const) => t
      case ArrayLiteral (values) => apply(t, values)
      case IdentRef(ident) => t
      case Select (expr, field) => apply(t, expr)
      case Index (expr, index) => apply(apply(t, expr), index)
      case Tuple (exprs) => apply(t, exprs)
      case Lambda (args, body) => apply(apply(t, args), body)
      case FunApply (kind, fun, params, args) => apply(apply(apply(t, fun), params), args)
      case If (cond, ifTrue, ifFalse) => apply(apply(apply(t, cond), ifTrue), ifFalse)
      case Match(cond, cases) => apply(apply(t, cond), cases)
      case Cond (conds, elseResult) => apply(apply(t, conds), elseResult)
      case MonadComp(elem, clauses) => apply(apply(t, elem), clauses)
      case DefExpr(df) => apply(t, df)
      case Block(exprs) => apply(t, exprs)
      case Assign(ident, value) => apply(t, value)
      case While (cond, body) => apply(apply(t, cond), body)
      case DoWhile(body, cond) => apply(apply(t, body), cond)
      case For(gens, body) => apply(apply(t, gens), body)
    }
  }

  /** Facilitates tree traversals: folds done for side effects. */
  abstract class Traverser extends Accumulator[Unit] {

    /** Traverse the given tree node.
      * The implementation must call `foldOver` to recurse into the node's children. */
    def traverse (tree :Tree)(implicit ctx :Context) :Unit

    /** Traverses the children of `tree`. */
    def foldOver (tree :Tree)(implicit ctx :Context) :Unit = foldOver((), tree)

    override final def apply (t :Unit, tree :Tree)(implicit ctx :Context) = traverse(tree)
  }

  //
  // Printing, pretty and otherwise

  class Printer (out :PrintWriter, indent :String = "") {
    def nest = new Printer(out, indent + "  ")
    def print (value :Any) = { out.print(value) ; this }
    def print (v0 :Any, v1 :Any, rest :Any*) = {
      out.print(v0) ; out.print(v1) ; rest.foreach(out.print) ; this
    }
    def println () :this.type = { out.println() ; this }
    def println (values :Any*) :this.type = { values.foreach(out.print) ; println() ; this }
    def printIndent (values :Any*) = { out.print(indent) ; values.foreach(out.print) ; this }
  }

  def printType (typ :TypeTree)(implicit pr :Printer) :Unit = {
    pr.print(typ) // TODO
  }
  def printOptType (typ :TypeTree)(implicit pr :Printer) = typ match {
    case OmittedType => // nothing
    case _ => pr.print(" :") ; printType(typ)
  }

  def printMemberDef (df :DefTree)(implicit pr :Printer) = printDef(df, true)(pr.nest)
  def printDefList (defs :Seq[DefTree], open :String, close :String)(implicit pr :Printer) =
    if (!defs.isEmpty) printSep(defs, printMemberDef, open, ", ", close)

  def printDef (df :DefTree, member :Boolean = false)(implicit pr :Printer) :Printer = {
    df match {
      case Binding(name, typ, value) =>
        pr.print(name) ; printOptType(typ) ; pr.print(" = ") ; printExpr(value)
      case LetDef(binds) => pr.print("let ") ; printDefList(binds, "", "")
      case VarDef(binds) => pr.print("var ") ; printDefList(binds, "", "")
      case FieldDef(docs, name, typ) =>
        if (!docs.isEmpty) pr.println()
        docs.foreach { doc => pr.printIndent(s"/// $doc").println() }
        if (!docs.isEmpty) pr.printIndent("")
        pr.print(name, " :") ; printType(typ)
      case RecordDef(docs, name, params, fields) =>
        if (!docs.isEmpty) pr.println()
        docs.foreach { doc => pr.printIndent(s"/// $doc").println() }
        if (!docs.isEmpty) pr.printIndent("")
        if (!member) pr.print("data ")
        pr.print(name)
        printDefList(params, "[", "]")
        printDefList(fields, "(", ")")
      case UnionDef(docs, name, args, cases) =>
        docs.foreach { doc => pr.println(s"/// $doc") }
        pr.print("data ", name)
        printDefList(args, "[", "]")
        if (!cases.isEmpty) {
          pr.print(" = ")
          printSep(cases, printMemberDef, "", " | ", "")
        }
      case Param(name) => pr.print(name)
      case Constraint(name, params) => pr.print(name) ; printSep(params, printType, "[", ", ", "]")
      case ArgDef(docs, name, typ) => pr.print(name) ; printOptType(typ)
      case FunDef(docs, name, params, csts, args, ret, body) =>
        docs.foreach { doc => pr.printIndent(s"/// $doc").println() }
        pr.printIndent("fun ", name)
        printDefList(params ++ csts, "[", "]")
        printDefList(args, " (", ")")
        printOptType(ret)
        if (body != OmittedBody) { pr.print(" = ") ; printExpr(body) }
      case FaceDef(docs, name, params, parents, meths) =>
        docs.foreach { doc => pr.println(s"/// $doc") }
        pr.print("interface ", name)
        printDefList(params, "[", "]")
        printDefList(parents, " : ", "")
        pr.print(" {")
        meths foreach { meth => printDef(meth)(pr.println().nest) }
        pr.println().printIndent("}")
      case MethodBinding(meth, fun) => pr.print(meth, " = ", fun)
      case ImplDef(docs, name, params, csts, parent, binds) =>
        docs.foreach { doc => pr.println(s"/// $doc") }
        pr.print("impl ", name)
        printDefList(params ++ csts, "[", "]")
        pr.print(" = ") ; printType(parent)
        printDefList(binds, "(", ")")
    }
    pr
  }

  def printPat (pat :PatTree)(implicit pr :Printer) :Printer = pat match {
    case IdentPat(ident) => pr.print(ident)
    case LiteralPat(const) => pr.print(const)
    case DestructPat(ctor, binds) =>
      pr.print(ctor) ; printSep(binds, printPat, "", ", ", "")
    case Case(pattern, guard, result) =>
      pr.printIndent("case ", pattern, " = ") ; printExpr(result)(pr.nest)
    case Condition(guard, result) =>
      pr.printIndent("") ; printExpr(guard) ; pr.print(" = ") ; printExpr(result)(pr.nest)
  }

  def printComp (comp :CompTree)(implicit pr :Printer) :Printer = comp match {
    case Generator(name, expr) => pr.print(name, " <- ") ; printExpr(expr)
    case Filter(expr) => printExpr(expr)
  }

  def printExpr (expr :TermTree)(implicit pr :Printer) :Printer = {
    def print1 (expr :TermTree) = printExpr(expr)(pr.nest)
    def printTuple (exprs :Seq[TermTree]) = printSep(exprs, printExpr, "(", ", ", ")")

    expr match {
      case Literal(const) => pr.print(const)
      case ArrayLiteral(values) => printSep(values, printExpr, "[", ", ", "]")
      case IdentRef (ident) => pr.print(ident)
      case OmittedBody => // nada
      case Select(expr, field) => printExpr(expr).print(".", field)
      case Index(expr, index) => printExpr(expr).print("@") ; printExpr(index)
      case Tuple(exprs) => printTuple(exprs)
      case Lambda(args, body) =>
        if (args.size == 1) printDef(args(0))
        else printSep[DefTree](args, printDef(_, false), " (", ", ", ")")
        pr.print(" => ") ; printExpr(body)
      case FunApply(kind, ident, params, args) => kind match {
          case FunKind.UnOp =>
            printExpr(ident) ; printExpr(args(0))
          case FunKind.BinOp =>
            pr.print("(") ; printExpr(args(0)).print(" ")
            printExpr(ident).print(" ")
            printExpr(args(1)).print(")")
          case FunKind.Normal =>
            printExpr(ident)
            if (!params.isEmpty) printSep(params, printType, "[", ", ", "]")
            printTuple(args)
          case FunKind.Receiver =>
            printExpr(args(0)) ; pr.print(".") ; printExpr(ident)
            printTuple(args.drop(1))
        }
      case If(cond, ifTrue, ifFalse) =>
        pr.print("if (") ; printExpr(cond) ; pr.print(") ") ; printExpr(ifTrue)
        pr.print(" else ") ; printExpr(ifFalse)
      case Match(cond, cases) =>
        pr.print("match ") ; printExpr(cond)
        cases foreach { printPat(_)(pr.println().nest) }
      case Cond(conds, elseResult) => pr.print("cond")
        conds foreach { printPat(_)(pr.println().nest) }
        pr.println().nest.printIndent("else = ") ; print1(elseResult)
      case MonadComp(elem, clauses) =>
        pr.print("[") ; printExpr(elem)
        printSep(clauses, printComp, " where ", ", ", "]")
      case DefExpr(df) => printDef(df)
      case Block(exprs) =>
        pr.print("{")
        def printSep () = pr.println().nest.printIndent()
        exprs foreach { expr => printSep() ; print1(expr) }
        pr.println().printIndent("}")

      case Assign(ident, value) => pr.print(ident, " = ") ; printExpr(value)
      case While(cond, body) =>
        pr.print("while ") ; printExpr(cond).print(" ") ; printExpr(body)
      case DoWhile(body, cond) =>
        pr.print("do ") ; printExpr(body)
        pr.print(" while ") ; printExpr(cond)
      case For(gens, body) =>
        printSep[Generator](gens, {
          case Generator(name, expr) => pr.print(name, " <- ") ; printExpr(expr)
        }, "for ", ", ", " ")
        printExpr(body)
    }
    pr
  }

  def printSep[T] (ts :Seq[T], printT :T => Unit, open :String, sep :String, close :String)
                  (implicit pr :Printer) = {
    var first = true
    pr.print(open)
    ts foreach { t =>
      if (!first) pr.print(sep)
      printT(t)
      first = false
    }
    pr.print(close)
  }

  def showTree (root :Product) :String = {
    import java.lang.{StringBuilder => JStringBuilder}
    val sb = new JStringBuilder

    def show (indent :String, node :Any) :Unit = node match {
      case pnode :Product =>
        sb.append(pnode.productPrefix).append("(")
        val nindent = indent + " "
        pnode.productArity match {
          case 0 => sb.append(")")
          case 1 =>
            show(nindent, pnode.productElement(0))
            sb.append(")")
          case n =>
            sb.append("\n").append(nindent)
            show(nindent, pnode.productIterator)
            sb.append("\n").append(indent).append(")")
        }
      case iter :Iterator[_] =>
        var first = true
        iter.foreach { elem =>
          if (!first) sb.append("\n").append(indent)
          show(indent, elem)
          first = false
        }
      case iter :Iterable[_] =>
        if (iter.isEmpty) sb.append("<empty>")
        else show(indent, iter.iterator)
      case _ => sb.append(node)
    }

    show("", root)
    sb.toString
  }

  def debugTree (out :PrintWriter)(tree :Tree) :Unit = {
    val acc = new Accumulator[Printer]() {
      override def apply (pr :Printer, tree :Tree)(implicit ctx :Context) = {
        def treeType = if (tree.isTyped) tree.tpe else Error("Missing")
        tree match {
          case DefExpr(dt) => apply(pr, dt)
          case FunDef(docs, name, params, csts, args, result, body) =>
            pr.printIndent("fun ", name)
            printDefList(params ++ csts, "[", "]")(pr)
            printDefList(args, "(", ")")(pr)
            pr.print(" :", result)
            pr.println(" ::", treeType)
            apply(pr.nest, body)
            pr
          case Literal(const) =>
            pr.printIndent(const).println(" ::", treeType)
          case IdentRef(ident) =>
            pr.printIndent(ident).println(" ::", treeType)
          case Select(expr, field) =>
            pr.printIndent("<select> ", field, " ::", treeType).println()
            apply(pr.nest, expr)
            pr
          case Index(expr, index) =>
            pr.printIndent("<index> ::", treeType).println()
            apply(pr.nest, expr)
            apply(pr.nest, index)
            pr
          case FunApply(kind, fun, params, args) =>
            pr.printIndent("<apply>").println(" ::", treeType)
            apply(pr.nest, fun)
            apply(pr.nest, args)
            pr
          case Case(pattern, guard, result) =>
            pr.printIndent("<case>").println(" ::", treeType)
            foldOver(pr.nest, tree)
            pr
          case Match(cond, cases) =>
            pr.printIndent("<match>").println(" ::", treeType)
            apply(pr.nest, cond)
            apply(pr.nest, cases)
            pr
          case Tuple(exprs) =>
            pr.printIndent(s"<tuple${exprs.size}>").println(" ::", treeType)
            apply(pr.nest, exprs)
            pr
          case If(cond, ifTrue, ifFalse) =>
            pr.printIndent(s"<if> ::", treeType).println()
            apply(pr.nest, cond)
            apply(pr.nest, ifTrue)
            apply(pr.nest, ifFalse)
            pr
          case Binding (name, typ, value) =>
            pr.printIndent(name).println(" ::", treeType)
            apply(pr.nest, typ)
            apply(pr.nest, value)
            pr
          case LetDef(binds) =>
            pr.printIndent(s"<let>").println()
            apply(pr.nest, binds)
            pr
          case VarDef(binds) =>
            pr.printIndent(s"<var>").println()
            apply(pr, binds)
            pr
          case FieldDef(docs, name, typ) =>
            pr.printIndent("field ", name).println(" ::", treeType)
            apply(pr.nest, typ)
            pr
          case RecordDef(docs, name, params, fields) =>
            pr.printIndent("rec ", name)
            printDefList(params, "[", "]")(pr)
            pr.println(" ::", treeType)
            apply(pr.nest, fields)
            pr
          case UnionDef(docs, name, params, cases) =>
            pr.printIndent("data ", name)
            printDefList(params, "[", "]")(pr)
            pr.println(" ::", treeType)
            apply(pr.nest, cases)
            pr
          case FaceDef(docs, name, params, parents, meths) =>
            pr.print("face ", name)
            printDefList(params, "[", "]")(pr)
            printDefList(parents, " : ", "")(pr)
            pr.println(" ::", treeType)
            apply(pr.nest, parents)
            apply(pr.nest, meths)
            pr
          case _ =>
            pr.printIndent(tree).println(" ::", treeType)
            foldOver(pr.nest, tree)
            pr
        }
        // tree match {
        //   case TypeRef(name) => pr.print(name)
        //   case TypeApply(ctor, args) => pr.print(out.println()
        //   case TypeArrow(args, ret) =>
        //   case Param(name) =>
        //   case Constraint(name, params) =>
        //   case ArgDef(docs, name, typ) =>
        //   case Binding (name, typ, value) =>
        //   case Literal (const) =>
        //   case ArrayLiteral (values) =>
        //   case Select (expr, field) =>
        //   case Index (expr, index) =>
        //   case Lambda (args, body) =>
        //   case Condition (guard, result) =>
        //   case Cond (conds, elseResult) =>
        //   case Generator (name, expr) =>
        //   case Filter (expr) =>
        //   case MonadComp(elem, clauses) =>
        //   case DefExpr(df) =>
        //   case Block(exprs) =>
        //   case Assign(ident, value) =>
        //   case While (cond, body) =>
        //   case DoWhile(body, cond) =>
        //   case For(gens, body) =>
        // }
      }
    }
    acc.apply(new Printer(out), tree)(moduleContext(termName("debugTree")))
    out.flush()
  }

  def show (expr :TermTree) :String = {
    val str = new StringWriter()
    val out = new PrintWriter(str)
    printExpr(expr)(new Printer(out))
    out.flush()
    str.toString
  }

  def print (expr :TermTree) :Unit = {
    val out = new PrintWriter(System.out)
    printExpr(expr)(new Printer(out))
    out.println()
    out.flush()
  }
}
