//
// Compose Mark 0 compiler
// https://github.com/samskivert/compose/c0/LICENSE

package compose

import java.io.PrintWriter

object Trees {
  import Constants._
  import Contexts._
  import Names._
  import Types._
  import Indexer._

  class UntypedTreeException (tree :Tree) extends RuntimeException {
    override def getMessage :String = s"Type of ${tree.id}#$tree is not assigned"
  }

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
      if (treeType == null) throw new UntypedTreeException(this)
      treeType
    }

    /** Ensures this tree (and its children) are typed, and returns it. */
    def typed (implicit ctx :Context) :this.type = {
      // TODO: save ctx and ensure we don't request the type later with an invalid context?
      if (!isTyped) {
        treeType = computeType
        computeBodyType()
      }
      this
    }

    protected def computeType (implicit ctx :Context) :Type
    protected def computeBodyType ()(implicit ctx :Context) :Unit = {}
  }

  sealed trait TypeTree extends Tree {
    type ThisTree <: TypeTree
    override def isType = true
  }
  case object OmittedType extends TypeTree {
    protected def computeType (implicit ctx :Context) = Prim.None
    override def toString = ""
  }
  case class TypeRef (name :TypeName) extends TypeTree {
    protected def computeType (implicit ctx :Context) = typeFor(name)
    override def toString = name.toString
  }
  case class TypeApply (ctor :TypeName, params :Seq[TypeTree]) extends TypeTree {
    protected def computeType (implicit ctx :Context) = {
      val paramTypes = params.map(_.typed.tpe)
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
    protected def computeType (implicit ctx :Context) =
      Arrow(Seq(), args.map(_.typed.tpe), ret.typed.tpe)
    override def toString = s"${args.mkString("(", ", ", ")")} => $ret"
  }

  //
  // Definitions

  sealed trait DefTree extends Tree {
    type ThisTree <: DefTree
    override def isDef = true
  }

  sealed trait ParamTree extends DefTree {
    type ThisTree <: ParamTree
  }
  case class Param (name :TypeName) extends ParamTree {
    type ThisTree <: Param
    protected def computeType (implicit ctx :Context) = Var(name, ctx.scope.id)
  }
  case class Constraint (name :TypeName, params :Seq[Param]) extends ParamTree {
    type ThisTree <: Constraint
    protected def computeType (implicit ctx :Context) =
      applyType(typeFor(name), params.map(_.typed.tpe))
  }

  // TODO: destructuring fun arg bindings
  case class ArgDef (docs :Seq[String], name :TermName, typ :TypeTree) extends DefTree {
    type ThisTree <: ArgDef
    protected def computeType (implicit ctx :Context) = typ.typed.tpe
  }
  case class FunDef (docs :Seq[String], name :TermName, params :Seq[ParamTree],
                     args :Seq[ArgDef], result :TypeTree, body :TermTree) extends DefTree {
    type ThisTree <: FunDef
    protected def computeType (implicit ctx :Context) = {
      val funSym = ctx.scope.lookup(name)
      assert(funSym.exists, s"Missing symbol for fun def $name")
      val ownerParams = ctx.owner.params
      def typedWith (implicit ctx :Context) =
        Arrow((ownerParams ++ params).map(_.typed.tpe), args.map(_.typed.tpe), result.typed.tpe)
      typedWith(ctx.withOwner(funSym))
    }
    override protected def computeBodyType ()(implicit ctx :Context) = {
      val funSym = ctx.scope.lookup(name)
      assert(funSym.exists, s"Missing symbol for fun def $name")
      body.typed(ctx.withOwner(funSym).withoutFlag(TypingDef))
    }
  }

  // TODO: destructuring let/var bindings
  case class Binding (name :TermName, typ :TypeTree, value :TermTree) extends DefTree {
    protected def computeType (implicit ctx :Context) = {
      typ.typed ; value.typed
      if (typ == OmittedType) value.tpe else typ.tpe
    }
  }
  case class LetDef (binds :Seq[Binding]) extends DefTree {
    protected def computeType (implicit ctx :Context) = {
      val bindCtx = ctx.withoutFlag(TypingDef)
      binds.foreach(_.typed(bindCtx))
      Prim.None
    }
  }
  case class VarDef (binds :Seq[Binding]) extends DefTree {
    protected def computeType (implicit ctx :Context) =  {
      val bindCtx = ctx.withoutFlag(TypingDef)
      binds.foreach(_.typed(bindCtx))
      Prim.None
    }
  }

  case class FieldDef (docs :Seq[String], name :TermName, typ :TypeTree) extends DefTree {
    type ThisTree <: FieldDef
    protected def computeType (implicit ctx :Context) = typ.typed.tpe
  }
  case class RecordDef (docs :Seq[String], name :TypeName, params :Seq[ParamTree],
                        fields :Seq[FieldDef]) extends DefTree {
    protected def computeType (implicit ctx :Context) = {
      val recSym = ctx.scope.lookup(name)
      assert(recSym.exists, s"Missing symbol for record def $name")
      def typedWith (implicit ctx :Context) =
        Record(name, params.map(_.typed.tpe), fields.map(f => Field(f.name, f.typed.tpe)))
      typedWith(ctx.withOwner(recSym))
    }
  }

  case class UnionDef (docs :Seq[String], name :TypeName, params :Seq[ParamTree],
                       cases :Seq[DefTree]) extends DefTree {
    protected def computeType (implicit ctx :Context) = {
      val unionSym = ctx.scope.lookup(name)
      assert(unionSym.exists, s"Missing symbol for union def $name")
      def typedWith (implicit ctx :Context) =
        Union(name, params.map(_.typed.tpe), cases.map(_.typed.tpe))
      typedWith(ctx.withOwner(unionSym))
    }
  }

  case class FaceDef (docs :Seq[String], name :TypeName, params :Seq[ParamTree],
                      parents :Seq[Constraint], meths :Seq[FunDef]) extends DefTree {
    protected def computeType (implicit ctx :Context) = {
      val faceSym = ctx.scope.lookup(name)
      assert(faceSym.exists, s"Missing symbol for interface def $name")
      def typedWith (implicit ctx :Context) = {
        params.foreach(_.typed)
        parents.foreach(_.typed)
        // TODO: parents should be in type?
        Interface(name, params.map(_.tpe), meths.map(m => Method(m.name, m.typed.tpe)))
      }
      typedWith(ctx.withOwner(faceSym))
    }
  }

  case class MethodBinding (meth :TermName, fun :TermName) extends DefTree {
    protected def computeType (implicit ctx :Context) = Prim.None // TODO
  }
  case class ImplDef (docs :Seq[String], name :TermName, params :Seq[ParamTree],
                      face :TypeTree, binds :Seq[MethodBinding]) extends DefTree {
    protected def computeType (implicit ctx :Context) = Prim.None // TODO
    // TEMP: impl is typed as function from constraints to record
    // implSym.initInfoLazy(() => Arrow(paramSyms.map(_.info), Seq(), typeFor(face)(implCtx)))
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
    protected def computeType (implicit ctx :Context) = typeFor(ident)
    override def toString = ident.toString
  }
  case class LiteralPat (const :Constant) extends PatTree {
    protected def computeType (implicit ctx :Context) = Const(const)
    override def toString = const.toString
  }
  case class DestructPat (ctor :TermName, binds :Seq[PatTree]) extends PatTree {
    // TODO: resolve constructor in symbol table; use that to type the bindings
    protected def computeType (implicit ctx :Context) = ???
    override def toString = s"$ctor(${binds.mkString(", ")})"
  }
  // TODO: named destructors? (i.e. Node[left @ Node[ll lr], right])
  case class Case (pattern :PatTree, guard :Option[TermTree], result :TermTree) extends PatTree {
    type ThisTree <: Case
    protected def computeType (implicit ctx :Context) = {
      pattern.typed
      guard.foreach(_.typed) // TODO: expect Bool
      // TODO: use ctx obtained by typing pattern as it has necessary name bindings
      result.typed.tpe
    }
  }
  case class Condition (guard :TermTree, result :TermTree) extends PatTree {
    type ThisTree <: Condition
    protected def computeType (implicit ctx :Context) = {
      guard.typed // TODO: expect Bool
      result.typed.tpe
    }
  }

  //
  // Comprehensions

  sealed trait CompTree extends Tree {
    type ThisTree <: CompTree
    override def isComprehension = true
  }

  case class Generator (name :TermName, expr :TermTree) extends CompTree {
    // TODO: any expected type? probably not because generators will presumable be driven by a
    // traversable type class
    protected def computeType (implicit ctx :Context) = expr.typed.tpe
  }
  case class Filter (expr :TermTree) extends CompTree {
    protected def computeType (implicit ctx :Context) = expr.typed.tpe // TODO: expect Bool
  }

  //
  // Terms / expressions

  sealed trait TermTree extends Tree {
    type ThisTree <: TermTree
    override def isTerm = true
  }
  case object OmittedBody extends TermTree {
    protected def computeType (implicit ctx :Context) = Prim.None
  }

  // pure
  case class Literal (const :Constant) extends TermTree {
    protected def computeType (implicit ctx :Context) = Const(const)
  }
  case class ArrayLiteral (values :Seq[TermTree]) extends TermTree {
    protected def computeType (implicit ctx :Context) =
      Array(unify(values.map(_.typed.tpe))) // TODO: disallow empty array literal?
  }

  case class IdentRef (ident :TermName) extends TermTree {
    protected def computeType (implicit ctx :Context) = typeFor(ident)
  }

  case class Select (expr :TermTree, field :TermName) extends TermTree {
    protected def computeType (implicit ctx :Context) = {
      // if the receiver is not a record, or does not define the selected field, attempt to resolve
      // that field as a function and pass the receiver as a single argument to it
      def asReceiverFun (errmsg :String) = ctx.scope.lookup(field).info match {
        case Arrow(_, _, resultType) => resultType
        case _ => Error(errmsg)
      }
      expr.typed.tpe match {
        case Record(name, _, fields) => fields.find(_.name == field) match {
          case Some(field) => field.tpe
          case None => asReceiverFun(
            s"'${name}' record does not define a '${field}' field, " +
              s"nor does a '${field}' function exist.")
        }
        case _ => asReceiverFun(s"No function named '${field}' could be found")
      }
    }
  }

  case class Index (expr :TermTree, index :TermTree) extends TermTree {
    protected def computeType (implicit ctx :Context) = {
      index.typed.tpe // compute type of index for side effect (TODO: expect integral?)
      expr.typed.tpe match {
        case Array(elem) => elem
        case tpe         => Error(s"Target of @ must be an array type (got $tpe)")
      }
    }
  }

  case class Tuple (exprs :Seq[TermTree]) extends TermTree {
    protected def computeType (implicit ctx :Context) =
      applyType(Prim.tuple(exprs.size), exprs.map(_.typed.tpe))
  }

  case class Lambda (args :Seq[ArgDef], body :TermTree) extends TermTree {
    protected def computeType (implicit ctx :Context) = {
      val lamSym = ctx.owner.createTerm(NoName, this) // TODO: synthesize lambda name
      val lamCtx = ctx.withOwner(lamSym)
      args foreach { arg => index(arg)(lamCtx) } // index args in lambda context
      args foreach { arg => arg.typed(lamCtx) } // and type them in same context (TODO: inference)
      body.typed(lamCtx).tpe
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
    protected def computeType (implicit ctx :Context) = {
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

      fun.typed.tpe match {
        case funType :Arrow =>
          val paramTypes = inferFunParams(funType, params.map(_.typed.tpe), args.map(_.typed.tpe))
          applyType(funType, paramTypes).asInstanceOf[Arrow].result
        case tpe => Error(s"Target of apply must be an arrow type (got: $tpe)")
      }
    }
  }

  def inferFunParams (funType :Arrow, applyParams :Seq[Type], applyArgs :Seq[Type]) :Seq[Type] =
    applyParams // TODO

  case class If (cond :TermTree, ifTrue :TermTree, ifFalse :TermTree) extends TermTree {
    protected def computeType (implicit ctx :Context) = {
      cond.typed // TODO: expect bool
      unify(Seq(ifTrue.typed.tpe, ifFalse.typed.tpe))
    }
  }
  // TODO: if+let? or maybe "let Ctor(arg) = expr" is a LetTermTree which evaluates to true/false?
  // latter might be fiddly due to it being an expr that introduces defs in a scope up the AST

  case class Match (cond :TermTree, cases :Seq[Case]) extends TermTree {
    protected def computeType (implicit ctx :Context) = {
      cond.typed // compute type of condition
      unify(cases.map(_.typed.result.tpe))
    }
  }
  case class Cond (conds :Seq[Condition], elseResult :TermTree) extends TermTree {
    protected def computeType (implicit ctx :Context) =
      unify(conds.map(_.typed.result.tpe) :+ elseResult.typed.tpe)
  }
  case class MonadComp (elem :TermTree, clauses :Seq[CompTree]) extends TermTree {
    protected def computeType (implicit ctx :Context) = ???
  }

  case class DefExpr (df :DefTree) extends TermTree {
    protected def computeType (implicit ctx :Context) = df.typed(ctx.withFlag(TypingDef)).tpe
  }
  case class Block (exprs :Seq[TermTree]) extends TermTree {
    protected def computeType (implicit ctx :Context) = {
      // first index contents, then type contents, then use type of last expr as block type
      exprs foreach index
      exprs.map(_.typed).last.tpe
    }
  }

  // naughty
  case class Assign (ident :TermName, value :TermTree) extends TermTree {
    protected def computeType (implicit ctx :Context) = { value.typed ; Prim.None }
  }

  case class While (cond :TermTree, body :TermTree) extends TermTree {
    protected def computeType (implicit ctx :Context) = { cond.typed ; body.typed ; Prim.None }
  }
  case class DoWhile (body :TermTree, cond :TermTree) extends TermTree {
    protected def computeType (implicit ctx :Context) = { body.typed ; cond.typed ; Prim.None }
  }
  case class For (gens :Seq[Generator], body :TermTree) extends TermTree {
    protected def computeType (implicit ctx :Context) = ???
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
      case FunDef(docs, name, params, args, result, body) =>
        apply(apply(apply(apply(t, params), args), result), body)
      case Binding (name, typ, value) => apply(apply(t, typ), value)
      case LetDef(binds) => apply(t, binds)
      case VarDef(binds) => apply(t, binds)
      case FieldDef(docs, name, typ) => apply(t, typ)
      case RecordDef(docs, name, args, fields) => apply(apply(t, args), fields)
      case UnionDef(docs, name, args, cases) => apply(apply(t, args), cases)
      case FaceDef(docs, name, params, parents, meths) =>
        apply(apply(apply(t, params), parents), meths)
      case MethodBinding(meth, fun) => t
      case ImplDef(docs, name, params, parent, binds) =>
        apply(apply(apply(t, params), parent), binds)

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
      case Constraint(name, params) => pr.print(name) ; printDefList(params, "[", "]")
      case ArgDef(docs, name, typ) => pr.print(name) ; printOptType(typ)
      case FunDef(docs, name, params, args, ret, body) =>
        docs.foreach { doc => pr.printIndent(s"/// $doc").println() }
        pr.printIndent("fun ", name)
        printDefList(params, "[", "]")
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
      case ImplDef(docs, name, params, parent, binds) =>
        docs.foreach { doc => pr.println(s"/// $doc") }
        pr.print("impl ", name)
        printDefList(params, "[", "]")
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
        def treeType = if (tree.isTyped) tree.tpe else Prim.Missing
        tree match {
          case DefExpr(dt) => apply(pr, dt)
          case FunDef(docs, name, params, args, result, body) =>
            pr.printIndent("fun ", name)
            printDefList(params, "[", "]")(pr)
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

  def print (expr :TermTree) :Unit = {
    val out = new PrintWriter(System.out)
    printExpr(expr)(new Printer(out))
    out.println()
    out.flush()
  }
}
