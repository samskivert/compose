//
// Compose Mark 0 compiler
// https://github.com/samskivert/compose/c0/LICENSE

package compose

import java.io.{StringWriter, PrintWriter}
import scala.collection.mutable.ArrayBuffer

object Trees {
  import Constants._
  import Contexts._
  import Indexer._
  import Names._
  import Printing._
  import Resolve._
  import Symbols._
  import Types._

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

    /** Whether or not this tree has been typed. */
    def isTyped = treeType != null

    /** Returns the type of this tree. Throws an exception if called on an untyped tree. */
    def tpe :Type =
      if (treeType == null) fail(s"Type of $id#$this is not assigned")
      else treeType

    /** Ensures this tree (and its children) are typed, and returns it.
      * @param proto the expected type of the tree (if known, otherwise `None`). Used for local
      * type inference and type checking.
      */
    def typed (proto :Type = Untyped)(implicit ctx :Context) :this.type = {
      // TODO: save ctx and ensure we don't request the type later with an invalid context?
      if (!isTyped) {
        // println(s"computeType $id#$this ($proto)")
        treeType = computeType(proto)
      }
      this
    }

    // TEMP: what's the "proper" way to do this?
    def retype (tpe :Type) :Unit = { treeType = tpe }

    protected def computeType (proto :Type)(implicit ctx :Context) :Type
  }

  def typed[T <: Tree](trees :Seq[T], protos :Seq[Type])(implicit ctx :Context) :Seq[T] =
    (trees zip protos).map(tp => (tp._1 typed tp._2))
  def typedTypes[T <: Tree](trees :Seq[T], protos :Seq[Type])(implicit ctx :Context) :Seq[Type] =
    (trees zip protos).map(tp => (tp._1 typed tp._2).tpe)

  def rootTypeName (tree :TypeTree) :TypeName = tree match {
    case TypeRef(name) => name
    case TypeApply(ctor, _) => ctor
    case _ => NoName.toTypeName
  }

  sealed abstract class TypeTree extends Tree {
    type ThisTree <: TypeTree
    override def isType = true
  }
  case object OmittedType extends TypeTree {
    protected def computeType (proto :Type)(implicit ctx :Context) = Untyped
    override def toString = "<omitted>"
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
        case Arrow(_, _, _, argProtos, retProto) => (argProtos, retProto)
        // TODO: same as above re: error on mismatch
        case _ => (args.map(_ => Untyped), Untyped)
      }
      Arrow(NoType, Seq(), Seq(), typedTypes(args, argProtos), ret.typed(retProto).tpe)
    }
    override def toString = s"${args.mkString("(", ", ", ")")} => $ret"
  }

  //
  // Defs: participate in indexing, have signatures, define a symbol

  sealed abstract class DefTree extends Tree {
    private[this] var treeSig :Type = _
    private[this] var treeSym :Symbol = _
    private[this] var computingSig = false
    type ThisTree <: DefTree

    /** Whether or not the symbol defined by this tree has been created. This happens during the
      * indexing step which precedes type checking. */
    def isIndexed = treeSym != null

    def hasSig :Boolean = treeSig != null

    /** Returns the type signature for the definition expressed by this tree.
      * Note: this is computed on demand the first time it is requested. */
    def sig :Type = {
      if (treeSig != null) treeSig
      else if (computingSig) Lazy(this)
      else {
        computingSig = true
        // println(s"computeSig($id#$this)")
        treeSig = computeSig(Context(sym, sym.scope, computeSigFlags))
        computingSig = false
        treeSig
      }
    }

    /** Returns the symbol for the type defined by this tree. Throws an exception if called on an
      * unindexed tree. */
    def sym :Symbol =
      if (treeSym == null) fail(s"Symbol of $id#$this is not created")
      else treeSym

    /** Assigns a symbol to this tree (called by `Indexer`). */
    def index (sym :Symbol) :sym.type = {
      if (treeSym != null) fail(s"Symbol of $id#$this already assigned")
      // println(s"index($id#$this)")
      treeSym = sym
      sym
    }

    /** Type parameters introduced by this def. */
    def defParams :Seq[Param] = Seq()

    /** Type constraints introduced by this def. */
    def defCsts :Seq[Constraint] = Seq()

    override def isDef = true

    // TEMP: this is a terrible hack, hackity hack hack
    protected def resig (sig :Type) :Unit = { treeSig = sig }

    protected def computeSigFlags :Int = TypingDef
    protected def computeSig (implicit ctx :Context) :Type
    protected def computeType (proto :Type)(implicit ctx :Context) = sig
  }

  // this is used to intertwingle params and constraints into a single list
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
    protected def computeSig (implicit ctx :Context) = Var(sym.asType, ctx.scope.id)
    // override protected def computeType (proto :Type)(implicit ctx :Context) =
    //   Var(sym.asType, ctx.scope.id, true)
  }
  case class Constraint (name :TypeName, params :Seq[TypeRef]) extends DefTree with ParamOrConst {
    type ThisTree <: Constraint
    def accum (params :ArrayBuffer[Param], csts :ArrayBuffer[Constraint]) :Unit = csts += this
    protected def computeSig (implicit ctx :Context) =
      // TODO: the signature should be the type of the dictionary record
      applyType(typeFor(name), params.map(_.typed().tpe))
    override protected def computeType (proto :Type)(implicit ctx :Context) =
      applyType(typeFor(name), params.map(_.typed().tpe))
  }

  // TODO: destructuring fun arg bindings
  case class ArgDef (docs :Seq[String], name :TermName, typ :TypeTree) extends DefTree {
    type ThisTree <: ArgDef
    protected def computeSig (implicit ctx :Context) =
      if (typ == OmittedType) Error(s"Sig used before type computed: $name")
      else typ.typed().tpe
    override protected def computeType (proto :Type)(implicit ctx :Context) = {
      if (typ == OmittedType) {
        // HACK: when we compute the signature of an ArgDef we only have the declared type to work
        // with, but for lambdas we usually want to infer the type; so we set the initial sig to an
        // Error and then when we later type the ArgDef tree, we sneakily replace the signature
        // with the inferred type; nothing uses the sig for lambdas prior to the tree being typed,
        // so that works out "just fine"; in C1 we'll probably use full Algo W and avoid these
        // shenanigans
        resig(proto)
        proto
      } else typ.typed().tpe
    }
  }
  case class FunDef (docs :Seq[String], name :TermName, params :Seq[Param], csts :Seq[Constraint],
                     args :Seq[ArgDef], result :TypeTree, body :TermTree) extends DefTree {
    type ThisTree <: FunDef
    override def defParams = params
    override def defCsts = csts

    protected def computeSig (implicit ctx :Context) = {
      enterCsts(csts)
      val fullParams = sym.owner.params ++ params
      val paramTypes = fullParams.map(_.typed().tpe.asInstanceOf[Var])
      Arrow(sym, paramTypes, csts.map(_.typed().tpe), args.map(_.typed().tpe), result.typed().tpe)
    }
    override protected def computeType (proto :Type)(implicit ctx :Context) = {
      try sig // make sure sig is computed before we type our body
      finally body.typed(sig.asInstanceOf[Arrow].result)(ctx.withOwner(sym))
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

  case class FieldDef (docs :Seq[String], name :TermName, typ :TypeTree) extends DefTree {
    type ThisTree <: FieldDef
    protected def computeSig (implicit ctx :Context) = Field(sym.asTerm, typ.typed().tpe)
  }
  case class RecordDef (
    docs :Seq[String], name :TypeName, params :Seq[Param], fields :Seq[FieldDef]
  ) extends DefTree {
    override def defParams = params
    protected def computeSig (implicit ctx :Context) = {
      val fullParams = sym.owner.params ++ params
      Record(sym.asType, fullParams.map(_.typed().tpe),
             fields.map(_.typed().tpe.asInstanceOf[Field]))
    }
  }

  case class UnionDef (
    docs :Seq[String], name :TypeName, params :Seq[Param], cases :Seq[RecordDef]
  ) extends DefTree {
    override def defParams = params
    protected def computeSig (implicit ctx :Context) = {
      Union(sym.asType, params.map(_.typed().tpe), cases.map(_.typed().tpe))
    }
  }

  case class FaceDef (
    docs :Seq[String], name :TypeName, params :Seq[Param], parents :Seq[Constraint],
    meths :Seq[FunDef]
  ) extends DefTree {
    override def defParams = params
    override def defCsts = parents
    protected def computeSig (implicit ctx :Context) = {
      params.foreach(_.typed())
      parents.foreach(_.typed()) // TODO: should parents be in type?
      Interface(sym.asType, params.map(_.tpe), meths.map(m => Method(m.sym.asTerm, m.sig)))
    }
    override protected def computeType (proto :Type)(implicit ctx :Context) = {
      try sig // make sure sig is computed before we type our methods
      finally meths.foreach(_.typed())
    }
  }

  case class MethodBinding (meth :TermName, fun :TermName) extends RefTree {
    private[this] var _impls = Seq[ImplTree]()
    def impls :Seq[ImplTree] = _impls

    protected def computeType (proto :Type)(implicit ctx :Context) = proto match {
      case err :Error => err
      case _ =>
        val targetSym = setSym(ctx.scope.lookup(fun))
        if (!targetSym.exists) Unknown(fun, ctx.scope.id) else targetSym.info match {
          case tgtType :Arrow =>
            // TODO: check that it matches proto (that we can join? that the types are equal modulo
            // dictionaries after application?)
            if (tgtType.params.isEmpty) tgtType
            else {
              val Arrow(protoSym, protoParams, protoCsts, protoArgs, protoResult) = proto
              val bindCsts = (tgtType.args zip protoArgs) :+ (tgtType.result -> protoResult)
              unifyApply(bindCsts, tgtType) match {
                case methType :Arrow =>
                  // TODO: when resolving constraints for our bound methods, we will often encounter
                  // a situation where we need to pass in `this` impl to meet a constraint; we don't
                  // want to recursively resolve it (indeed we can't without infinitely looping), so
                  // we need to tell the constraint resolution process about `this` so that it can
                  // use a special ImplThis tree to cut the Gordian knot in this case
                  _impls = resolveCsts(methType.asInstanceOf[Arrow].csts)
                  // if any impl failed to resolve, type ourselves as error
                  _impls collectFirst { case ErrorImpl(msg) => Error(msg) } getOrElse methType
                case error => error
              }
            }
          case tpe => Error(
            s"Cannot bind interface method to non-function: '${targetSym.name} :$tpe'")
        }
    }
  }

  case class ImplDef (
    docs :Seq[String], name :TermName, params :Seq[Param], csts :Seq[Constraint], face :TypeTree,
    binds :Seq[MethodBinding]
  ) extends DefTree {
    override def defParams = params
    override def defCsts = csts
    override protected def computeSigFlags = 0
    protected def computeSig (implicit ctx :Context) = {
      enterCsts(csts)
      val paramTypes = params.map(_.typed().tpe)
      val cstTypes = csts.map(_.typed().tpe)
      face.typed().tpe match {
        // our signature is a function from our parameters and constraints to our implemented
        // interface, applied to those parameters
        case faceType :Interface => Arrow(sym, paramTypes, cstTypes, Seq(), faceType)
        case tpe => Error(s"Expected interface type, got: $tpe")
      }
    }
    override protected def computeType (proto :Type)(implicit ctx :Context) = {
      def typedWith (implicit ctx :Context) = {
        enterCsts(csts)
        params.map(_.typed().tpe)
        csts.map(_.typed().tpe)
        face.typed().tpe match {
          case Interface(faceSym, faceParams, faceMeths) =>
            // type the bindings using the interface method types as prototypes
            binds.foreach { bind => bind.typed(faceMeths.find(_.name == bind.meth).map(_.tpe).
              getOrElse(Error(s"No matching interface method for impl binding '${bind.meth}'"))) }
            def bindType (m :Method) = if (binds.exists(_.meth == m.name)) m.tpe
                                       else Error(s"Missing binding for method ${m.name}")
            Record(faceSym, faceParams, faceMeths.map(m => Field(m.sym, bindType(m))))
          case tpe => Error(s"Expected interface type, got: $tpe")
        }
      }
      typedWith(ctx.withOwner(sym))
    }
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
  // Terms: not types, not (top-level) defs, may be refs (not the clearest category)

  sealed abstract class TermTree extends Tree {
    type ThisTree <: TermTree
    override def isTerm = true
  }

  //
  // Refs: a term that references a def/symbol

  sealed abstract class RefTree extends TermTree {
    private[this] var treeSym :Symbol = _
    type ThisTree <: RefTree

    /** Returns the symbol for the referent of this tree. Referent is computed at typing time.
      * Throws an exception if called on an untyped tree. */
    def sym :Symbol = {
      if (treeSym == null) fail(s"Symbol of $id#$this is not resolved")
      else treeSym
    }

    protected def setSym (sym :Symbol) :sym.type = {
      if (treeSym != null) fail(s"Symbol of $id#$this already assigned (to $treeSym)")
      treeSym = sym
      sym
    }
  }

  //
  // Local defs: no indexing, no sigs, but still define a symbol

  sealed abstract class LocalDefTree extends TermTree {
    private[this] var treeSym :Symbol = _
    type ThisTree <: LocalDefTree

    override def isDef = true

    /** Returns the symbol for the term defined by this tree.
      * @throws Exception if called on an untyped tree. */
    def sym :Symbol =
      if (treeSym == null) fail(s"Symbol of $id#$this is not created")
      else treeSym

    protected def setSym (sym :Symbol) :sym.type = {
      if (treeSym != null) fail(s"Symbol of $id#$this already assigned (to $treeSym)")
      treeSym = sym
      sym
    }
  }

  // TODO: destructuring let/var bindings
  case class Binding (name :TermName, typ :TypeTree, value :TermTree) extends LocalDefTree {
    protected def computeType (proto :Type)(implicit ctx :Context) = {
      val declType = typ.typed().tpe
      val exprType = value.typed(declType).tpe
      val treeType = if (typ == OmittedType) exprType else declType
      // TODO: put context in let or var mode, so Binding can note mutability
      setSym(ctx.defineTerm(name, this, _ => treeType))
      treeType
    }
  }
  case class LetDef (binds :Seq[Binding]) extends TermTree {
    protected def computeType (proto :Type)(implicit ctx :Context) = {
      binds.foreach(_.typed())
      Untyped
    }
  }
  case class VarDef (binds :Seq[Binding]) extends TermTree {
    protected def computeType (proto :Type)(implicit ctx :Context) = {
      binds.foreach(_.typed())
      Untyped
    }
  }

  //
  // Expression terms

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

  case class IdentRef (ident :TermName) extends RefTree {
    protected def computeType (proto :Type)(implicit ctx :Context) = {
      val idSym = setSym(ctx.scope.lookup(ident))
      // TODO: check that type matches proto?
      idSym.map(_.info) getOrElse Unknown(ident, ctx.scope.id)
    }
  }

  case class Select (expr :TermTree, field :TermName) extends RefTree {
    protected def computeType (proto :Type)(implicit ctx :Context) = {
      // if the receiver is not a record, or does not define the selected field, attempt to resolve
      // that field as a function and pass the receiver as a single argument to it
      def asReceiverFun (errmsg :String) = ctx.scope.lookup(field).info match {
        // TODO: we need to record the fun chosen here for funapply, infer args (using proto), etc.
        case Arrow(funSym, _, _, _, resultType) => setSym(funSym) ; resultType
        case _ => Error(errmsg)
      }
      // TODO: how to use prototype here? just check that we match?
      expr.typed().tpe match {
        case Record(name, _, fields) => fields.find(_.name == field) match {
          // because a record type may be recursive, we unfold once here
          case Some(field) => setSym(field.sym) ; unfold(field.tpe)
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

  case class Lambda (args :Seq[ArgDef], body :TermTree) extends TermTree {
    protected def computeType (proto :Type)(implicit ctx :Context) = {
      val lamSym = ctx.createTerm(NoName, this, _ => Untyped) // TODO: synthesize lambda name
      val lamCtx = ctx.withOwner(lamSym)
      args foreach { arg => index(arg)(lamCtx) } // index args in lambda context
      val (argProtos, retProto) = proto match {
        case Arrow(_, _, _, argTypes, retType) => (argTypes, retType)
        // TODO: fail if expected type not arrow?
        case _ => (args.map(_ => Untyped), Untyped)
      }
      // and type them in same context; TODO: if the prototype is parameterized, we could perhaps
      // infer a parameterized type for this lambda, but maybe we'll just punt on that and wait for
      // full inference in C1
      val argTypes = typedTypes(args, argProtos)(lamCtx)
      val retType = body.typed(retProto)(lamCtx).tpe
      Arrow(lamSym, Seq(), Seq(), argTypes, retType)
    }
  }

  sealed trait FunKind
  object FunKind {
    case object Normal extends FunKind
    case object UnOp extends FunKind
    case object BinOp extends FunKind
    case object Receiver extends FunKind
    // TODO: Tuple type, which we'd use to omit TupleN name when printing?
  }
  case class FunApply (kind :FunKind, fun :TermTree, params :Seq[TypeTree],
                       args :Seq[TermTree]) extends TermTree {
    private[this] var _impl :ImplTree = NoImpl
    def impl :ImplTree = _impl

    private[this] var _implArgs = Seq[ImplTree]()
    def implArgs :Seq[ImplTree] = _implArgs

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

      // TODO: we need to look at the type constraints on the function we're calling and ensure
      // that we can resolve implementations to meet them

      def typed (funType :Arrow) = {
        // println(s"funApply (proto: $proto) $fun :: $funType")
        val Arrow(funSym, formalParams, formalCsts, formalArgs, formalResult) = funType
        // next constrain the fun type based on explicit parameters and expected return type
        val applyParams = params.map(_.typed().tpe)
        val applyCsts = (formalParams zip applyParams) :+ (formalResult -> proto)
        // println(s"- applyCsts $fun :: $applyCsts")
        unifyApplyMap(applyCsts, funType) { cstFunType =>
          // any unapplied params in the partially constrained fun type cannot be used as
          // prototypes when typing the arguments, so replace with Untyped
          val argProtos = cstFunType.args.map(
            tpe => if (formalParams contains tpe) Untyped else tpe)
          // use the partially constrained formal argument types to type the actual arguments
          val argTypes = typedTypes(args, argProtos)
          // finally unify the resolved function with the apply constraints and any new
          // constraints generated by the typed arguments
          val cstFunCsts = applyCsts ++ (cstFunType.args zip argTypes)
          unify(cstFunCsts).map(sols => {
            val appParams = funType.params map subst(sols)
            applyType(funType, appParams).map(appliedType => {
              // println(s"- appliedType $fun :: $appliedType")
              val appFunType = appliedType.asInstanceOf[Arrow]
              // retype the fun tree with this resolved and applied arrow type
              fun.retype(appFunType)
              // if there are still unapplied params, then we failed to infer everything
              if (appFunType.params.exists(formalParams.contains)) Error(
                s"Unable to infer params: $appFunType")
              else {
                // resolve the symbols for any implementations we need to pass
                _implArgs = resolveCsts(appFunType.csts)
                // TODO: if any of _implArgs could not be resolved, type the apply as an error?
                if (!funSym.isMethod) appFunType.result
                // if this is a method application, resolve the impl via which we will obtain the
                // method (and give ourselves an error type if we cannot resolve an impl)
                else resolveImpl(funSym.owner.asType, appParams) match {
                  case ErrorImpl(msg) => Error(
                    s"Unable to resolve impl for method: ${appFunType.debugString}: ${msg}")
                  case impl => _impl = impl ; appFunType.result
                }
              }
            })
          }).merge
        }
      }

      // resolve the raw (unapplied) fun type from the fun tree
      val funProto = Arrow(NoTerm, Seq(), Seq(), args.map(_ => Untyped), proto)
      fun.typed(funProto).tpe match {
        // skolemize the type variables in the fun signature so that if this happens to be a
        // recursive call to the fun we're currently typing, we don't mix up the type variables for
        // this invocation of the fun with those of the recursive apply
        case funType :Arrow => typed(funType.skolemize)
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

  // TODO: optional type annotation? (needed for destructuring funargs); maybe we just use a
  // different AST node for that...
  case class LetPat (ident :TermName) extends LocalDefTree {
    override protected def computeType (proto :Type)(implicit ctx :Context) = {
      setSym(ctx.defineTerm(ident, this, _ => proto))
      proto
    }
    override def toString = ident.toString
  }
  case class IdentPat (ident :TermName) extends RefTree {
    protected def computeType (proto :Type)(implicit ctx :Context) = {
      val idSym = setSym(ctx.scope.lookup(ident))
      val identType = idSym.map(_.info) getOrElse Unknown(ident, ctx.scope.id)
      // TODO: this type application should probably be more robust; we technically only want to
      // do it when the prototype is a union/record type and the referent is a singleton record
      // case
      if (!proto.params.isEmpty) applyType(identType, proto.params) else identType
      // TODO: check that type matches proto?
    }
    override def toString = ident.toString
  }
  case class LiteralPat (const :Constant) extends TermTree {
    protected def computeType (proto :Type)(implicit ctx :Context) = Const(const)
    override def toString = const.toString
  }
  case class DestructPat (ctor :TermName, binds :Seq[TermTree]) extends RefTree {
    protected def computeType (proto :Type)(implicit ctx :Context) = {
      val recSym = setSym(ctx.scope.lookup(ctor.toTypeName))
      // TODO: error if record has parameters and proto provides none & vice versa
      val recType = if (proto.params.isEmpty) recSym.info
                    else applyType(recSym.info, proto.params)
      val fields = recType.asInstanceOf[Record].fields
      (binds zip fields) foreach { bf => bf._1.typed(bf._2.tpe) }
      recType
    }
    override def toString = s"$ctor(${binds.mkString(", ")})"
  }
  // TODO: named destructors? (i.e. Node[left @ Node[ll lr], right])
  case class Case (pattern :TermTree, guard :Option[TermTree], result :TermTree) extends TermTree {
    type ThisTree <: Case
    protected def computeType (proto :Type)(implicit ctx :Context) = {
      def typedInScope (implicit ctx :Context) = {
        pattern.typed(ctx.patternProto)
        guard.foreach(_.typed(Prim.Bool))
        // TODO: use ctx obtained by typing pattern as it has necessary name bindings
        result.typed(proto).tpe
      }
      typedInScope(ctx.withNestedScope(NoName))
    }
  }
  case class Match (cond :TermTree, cases :Seq[Case]) extends TermTree {
    protected def computeType (proto :Type)(implicit ctx :Context) = {
      val condType = cond.typed().tpe
      val caseCtx = ctx.withPatternProto(condType)
      def typedCase (cse :Case)(implicit ctx :Context) = cse.typed(proto).result.tpe
      join(cases.map(typedCase(_)(caseCtx)))
    }
  }

  case class Condition (guard :TermTree, result :TermTree) extends TermTree {
    type ThisTree <: Condition
    protected def computeType (proto :Type)(implicit ctx :Context) = {
      guard.typed(Prim.Bool)
      result.typed(proto).tpe
    }
  }
  case class Cond (conds :Seq[Condition], elseResult :TermTree) extends TermTree {
    protected def computeType (proto :Type)(implicit ctx :Context) =
      join(conds.map(_.typed(proto).result.tpe) :+ elseResult.typed(proto).tpe)
  }

  case class Generator (name :TermName, expr :TermTree) extends LocalDefTree {
    // TODO: any expected type? probably not because generators will presumably be driven by a
    // traversable type class
    protected def computeType (proto :Type)(implicit ctx :Context) = expr.typed(proto).tpe
  }
  case class Filter (expr :TermTree) extends TermTree {
    protected def computeType (proto :Type)(implicit ctx :Context) = expr.typed(Prim.Bool).tpe
  }
  case class MonadComp (elem :TermTree, clauses :Seq[TermTree]) extends TermTree {
    protected def computeType (proto :Type)(implicit ctx :Context) = ???
  }

  case class DefExpr (df :DefTree) extends TermTree {
    protected def computeType (proto :Type)(implicit ctx :Context) = df.typed(proto).tpe
  }
  case class Block (exprs :Seq[TermTree]) extends TermTree {
    protected def computeType (proto :Type)(implicit ctx :Context) = {
      def typedInScope (implicit ctx :Context) = {
        // first index contents, then type contents, then use type of last expr as block type
        val stats = exprs.take(exprs.size-1)
        stats foreach index
        stats foreach { _.typed() }
        exprs.last.typed(proto).tpe
      }
      typedInScope(ctx.withNestedScope(NoName))
    }
  }

  // naughty
  case class Assign (ident :TermName, value :TermTree) extends RefTree {
    protected def computeType (proto :Type)(implicit ctx :Context) = {
      value.typed()
      val lhsSym = setSym(ctx.scope.lookup(ident))
      if (lhsSym.exists) Untyped else Error(s"Unknown lhs '${ident}'")
    }
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
    RecordDef(Seq(), tupleName(rank).toTypeName, paramNames map Param, fields)
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
      case FieldDef(docs, name, typ) => apply(t, typ)
      case RecordDef(docs, name, args, fields) => apply(apply(t, args), fields)
      case UnionDef(docs, name, args, cases) => apply(apply(t, args), cases)
      case FaceDef(docs, name, params, parents, meths) =>
        apply(apply(apply(t, params), parents), meths)
      case MethodBinding(meth, fun) => t
      case ImplDef(docs, name, params, csts, parent, binds) =>
        apply(apply(apply(apply(t, params), csts), parent), binds)

      // term trees
      case Binding (name, typ, value) => apply(apply(t, typ), value)
      case LetDef(binds) => apply(t, binds)
      case VarDef(binds) => apply(t, binds)
      case OmittedBody => t
      case Literal(const) => t
      case ArrayLiteral(values) => apply(t, values)
      case IdentRef(ident) => t
      case Select(expr, field) => apply(t, expr)
      case Index(expr, index) => apply(apply(t, expr), index)
      case Lambda(args, body) => apply(apply(t, args), body)
      case FunApply(kind, fun, params, args) => apply(apply(apply(t, fun), params), args)
      case If(cond, ifTrue, ifFalse) => apply(apply(apply(t, cond), ifTrue), ifFalse)
      case LetPat(ident) => t
      case IdentPat(ident) => t
      case LiteralPat(const) => t
      case DestructPat(ctor, binds) => apply(t, binds)
      case Case(pattern, guard, result) => apply(apply(apply(t, pattern), guard), result)
      case Condition(guard, result) => apply(apply(t, guard), result)
      case Match(cond, cases) => apply(apply(t, cond), cases)
      case Cond(conds, elseResult) => apply(apply(t, conds), elseResult)
      case Generator(name, expr) => apply(t, expr)
      case Filter(expr) => apply(t, expr)
      case MonadComp(elem, clauses) => apply(apply(t, elem), clauses)
      case DefExpr(df) => apply(t, df)
      case Block(exprs) => apply(t, exprs)
      case Assign(ident, value) => apply(t, value)
      case While(cond, body) => apply(apply(t, cond), body)
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

  def printType (typ :TypeTree)(implicit pr :Printer) :Unit = {
    pr.print(typ) // TODO
  }
  def printOptType (typ :TypeTree)(implicit pr :Printer) = typ match {
    case OmittedType => // nothing
    case _ => pr.print(" :") ; printType(typ)
  }

  def printMemberDef (df :DefTree)(implicit pr :Printer) = printDef(df, true)(pr.nest)
  def printDefList (defs :Seq[DefTree], brackets :Brackets)(implicit pr :Printer) =
    if (!defs.isEmpty) printSep(defs, printMemberDef, brackets, ", ")

  def printTree (tree :Tree)(implicit pr :Printer) = tree match {
    case tree :TypeTree => printType(tree)
    case tree :DefTree => printDef(tree)
    case tree :TermTree => printTerm(tree)
  }

  def printDef (df :DefTree, member :Boolean = false)(implicit pr :Printer) :Printer = {
    df match {
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
        printDefList(params, Square)
        printDefList(fields, Paren)
      case UnionDef(docs, name, args, cases) =>
        docs.foreach { doc => pr.println(s"/// $doc") }
        pr.print("data ", name)
        printDefList(args, Square)
        if (!cases.isEmpty) {
          pr.print(" = ")
          printSep(cases, printMemberDef, Blank, " | ")
        }
      case Param(name) => pr.print(name)
      case Constraint(name, params) => pr.print(name) ; printSep(params, printType, Square)
      case ArgDef(docs, name, typ) => pr.print(name) ; printOptType(typ)
      case FunDef(docs, name, params, csts, args, ret, body) =>
        docs.foreach { doc => pr.printIndent(s"/// $doc").println() }
        pr.printIndent("fun ", name)
        printSep(params ++ csts, printTree, Square, ", ")
        pr.print(" ") ; printDefList(args, Paren)
        printOptType(ret)
        if (body != OmittedBody) { pr.print(" = ") ; printTerm(body) }
      case FaceDef(docs, name, params, parents, meths) =>
        docs.foreach { doc => pr.println(s"/// $doc") }
        pr.print("interface ", name)
        printDefList(params, Square)
        if (!parents.isEmpty) { pr.print(" : ") ; printDefList(parents, Blank) }
        pr.print(" {")
        meths foreach { meth => printDef(meth)(pr.println().nest) }
        pr.println().printIndent("}")
      case ImplDef(docs, name, params, csts, parent, binds) =>
        docs.foreach { doc => pr.println(s"/// $doc") }
        pr.print("impl ", name)
        printSep(params ++ csts, printTree, Square, ", ")
        pr.print(" = ") ; printType(parent)
        printSep(binds, printTerm, Paren)
    }
    pr
  }

  def printTerm (expr :TermTree)(implicit pr :Printer) :Printer = {
    def print1 (expr :TermTree) = printTerm(expr)(pr.nest)
    def printTuple (exprs :Seq[TermTree]) = printSep(exprs, printTerm, Paren)

    expr match {
      case Binding(name, typ, value) =>
        pr.print(name) ; printOptType(typ) ; pr.print(" = ") ; printTerm(value)
      case LetDef(binds) => pr.print("let ") ; printSep(binds, printTerm, Blank)
      case VarDef(binds) => pr.print("var ") ; printSep(binds, printTerm, Blank)

      case OmittedBody => // nada
      case Literal(const) => pr.print(const)
      case ArrayLiteral(values) => printSep(values, printTerm, Square)
      case IdentRef (ident) => pr.print(ident)
      case Select(expr, field) => printTerm(expr).print(".", field)
      case Index(expr, index) => printTerm(expr).print("@") ; printTerm(index)

      case Lambda(args, body) =>
        if (args.size == 1) printDef(args(0))
        else printSep[DefTree](args, printDef(_, false), Paren)
        pr.print(" => ") ; printTerm(body)

      case FunApply(kind, ident, params, args) => kind match {
          case FunKind.UnOp =>
            printTerm(ident) ; printTerm(args(0))
          case FunKind.BinOp =>
            pr.print("(") ; printTerm(args(0)).print(" ")
            printTerm(ident).print(" ")
            printTerm(args(1)).print(")")
          case FunKind.Normal =>
            printTerm(ident)
            if (!params.isEmpty) printSep(params, printType, Square)
            printTuple(args)
          case FunKind.Receiver =>
            printTerm(args(0)) ; pr.print(".") ; printTerm(ident)
            printTuple(args.drop(1))
        }

      case If(cond, ifTrue, ifFalse) =>
        pr.print("if ") ; printTerm(cond) ; pr.print(" ") ; printTerm(ifTrue)
        pr.print(" else ") ; printTerm(ifFalse)

      case LetPat(ident) => pr.print(ident)
      case IdentPat(ident) => pr.print(ident)
      case LiteralPat(const) => pr.print(const)
      case DestructPat(ctor, binds) =>
        pr.print(ctor) ; printSep(binds, printTerm, Blank)
      case Case(pattern, guard, result) =>
        pr.printIndent("case ", pattern, " = ") ; printTerm(result)(pr.nest)
      case Match(cond, cases) =>
        pr.print("match ") ; printTerm(cond)
        cases foreach { printTerm(_)(pr.println().nest) }

      case Condition(guard, result) =>
        pr.printIndent("") ; printTerm(guard) ; pr.print(" = ") ; printTerm(result)(pr.nest)
      case Cond(conds, elseResult) => pr.print("cond")
        conds foreach { printTerm(_)(pr.println().nest) }
        pr.println().nest.printIndent("else = ") ; print1(elseResult)

      case Generator(name, expr) => pr.print(name, " <- ") ; printTerm(expr)
      case Filter(expr) => printTerm(expr)
      case MonadComp(elem, clauses) =>
        pr.print("[") ; printTerm(elem) ; pr.print(" where ")
        printSep(clauses, printTerm, Blank) ; pr.print("]")

      case MethodBinding(meth, fun) => pr.print(meth, " = ", fun)
      case DefExpr(df) => printDef(df)
      case Block(exprs) =>
        pr.print("{")
        def printSep () = pr.println().nest.printIndent()
        exprs foreach { expr => printSep() ; print1(expr) }
        pr.println().printIndent("}")
      case Assign(ident, value) => pr.print(ident, " = ") ; printTerm(value)

      case While(cond, body) =>
        pr.print("while ") ; printTerm(cond).print(" ") ; printTerm(body)
      case DoWhile(body, cond) =>
        pr.print("do ") ; printTerm(body)
        pr.print(" while ") ; printTerm(cond)
      case For(gens, body) =>
        pr.print("for ")
        printSep[Generator](gens, {
          case Generator(name, expr) => pr.print(name, " <- ") ; printTerm(expr)
        }, Blank)
        pr.print(" ") ; printTerm(body)
    }
    pr
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

  // TODO: we desperately need tree positions and better error reporting
  type Path = List[Tree]
  def errors (tree :Tree) :Seq[(Path, String)] = {
    case class ErrCtx (path :List[Tree], errs :ArrayBuffer[(Path, String)])
    val acc = new Accumulator[ErrCtx]() {
      override def apply (ec :ErrCtx, tree :Tree)(implicit ctx :Context) = {
        val treeType = (if (tree.isTyped) tree.tpe
                        else if (tree == OmittedType) Untyped
                        else Error("Missing"))
        if (treeType.isError) ec.errs += ((tree :: ec.path) -> treeType.toString)
        foldOver(ec.copy(path = tree :: ec.path), tree)
      }
    }
    implicit val ctx = moduleContext(termName("errors"))
    acc.apply(ErrCtx(Nil, new ArrayBuffer[(Path, String)]()), tree).errs
  }

  def debugTree (out :PrintWriter)(tree :Tree) :Unit = {
    val acc = new Accumulator[Printer]() {
      override def apply (pr :Printer, tree :Tree)(implicit ctx :Context) = {
        def treeType = (if (tree.isTyped) tree.tpe else Error("Missing")).debugString
        implicit val ipr = pr
        tree match {
          case DefExpr(dt) => apply(pr, dt)
          case Param(name) =>
            pr.printIndent("param ", name)
            pr.println(" ::", treeType)
          case Constraint(name, params) =>
            pr.printIndent("cst ", name)
            printSep(params, printTree, Square)
            pr.println(" ::", treeType)
          case ArgDef(docs, name, typ) =>
            pr.printIndent("arg ", name, " :", typ)
            pr.println(" ::", treeType)
          case FunDef(docs, name, params, csts, args, result, body) =>
            pr.printIndent("fun ", name)
            printSep(params ++ csts, printTree, Square, ", ")
            printDefList(args, Paren)
            pr.print(" :", result)
            pr.println(" ::", treeType)
            apply(pr.nest, params)
            apply(pr.nest, csts)
            apply(pr.nest, args)
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
          case tree @ FunApply(kind, fun, params, args) =>
            pr.printIndent("<apply>", " ::", treeType)
            if (tree.impl != NoImpl) pr.print(" (via ", tree.impl, ")")
            pr.println()
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
          case If(cond, ifTrue, ifFalse) =>
            pr.printIndent(s"<if> ::", treeType).println()
            apply(pr.nest, cond)
            apply(pr.nest, ifTrue)
            apply(pr.nest, ifFalse)
            pr
          case Block(exprs) =>
            pr.printIndent("block ::", treeType).println()
            apply(pr.nest, exprs)
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
            printDefList(params, Square)
            pr.println(" ::", treeType)
            apply(pr.nest, fields)
            pr
          case UnionDef(docs, name, params, cases) =>
            pr.printIndent("data ", name)
            printDefList(params, Square)
            pr.println(" ::", treeType)
            apply(pr.nest, cases)
            pr
          case FaceDef(docs, name, params, parents, meths) =>
            pr.printIndent("face ", name)
            printDefList(params, Square)
            if (!parents.isEmpty) { pr.print(" : ") ; printDefList(parents, Blank) }
            pr.println(" ::", treeType)
            apply(pr.nest, parents)
            apply(pr.nest, meths)
            pr
          case MethodBinding(meth, fun) =>
            pr.printIndent("bind ", meth, "=", fun).println(" ::", treeType)
          case ImplDef(docs, name, params, csts, face, binds) =>
            pr.printIndent("impl ", name)
            printSep(params ++ csts, printTree, Square, ", ")
            pr.println(" ::", treeType)
            apply(pr.nest, face)
            apply(pr.nest, binds)
            pr
          case Lambda (args, body) =>
            pr.printIndent("lambda ")
            printSep(args, printTree, Paren)
            pr.println(" ::", treeType)
            apply(pr.nest, args)
            apply(pr.nest, body)
            pr
          case Condition (guard, result) =>
            apply(pr, guard)
            pr.println(" ::", treeType)
            apply(pr.nest, result)
            pr
          case Cond (conds, elseResult) =>
            pr.printIndent("cond ::").println(treeType)
            apply(pr.nest, conds)
            apply(pr.nest, elseResult)
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
        //   case Binding (name, typ, value) =>
        //   case Literal (const) =>
        //   case ArrayLiteral (values) =>
        //   case Select (expr, field) =>
        //   case Index (expr, index) =>
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

  def show (tree :Tree) :String = {
    val str = new StringWriter()
    val out = new PrintWriter(str)
    printTree(tree)(new Printer(out))
    out.flush()
    str.toString
  }

  def print (tree :Tree) :Unit = {
    printTree(tree)(sysPrint)
    sysOut.println()
    sysOut.flush()
  }
}
