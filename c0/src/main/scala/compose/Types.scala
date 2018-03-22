//
// Compose Mark 0 compiler
// https://github.com/samskivert/compose/c0/LICENSE

package compose

object Types {
  import Constants._
  import Contexts._
  import Indexer._
  import Names._
  import Symbols._
  import Trees._

  sealed abstract class Type {

    /** Applies `f` to `this` and returns the result, with caveats.
      * If `this` is an error type, `f` is not applied and `this` is returned directly.
      * If `this` is the `Lazy` type, the type is forced and then `f` is applied. */
    def map (f :Type => Type) :Type = f(this)

    def canUnify = true

    def debugString :String = toString
  }

  // used for things that have no type or for which type is yet unassigned
  // (TODO: use two different sentinels?)
  case object Untyped extends Type {
    override def canUnify = false
    override def toString = "<none>"
  }

  // literal/constant type: 1, 2e3, 'c', "pants"
  case class Const (const :Constant) extends Type {
    override def toString = const.toString
  }
  // we defer giving literal expressions "real" types until we know what coercions are desired

  // ground/opaque type: I32, Bool, etc.
  case class Data (sym :TypeSymbol, kind :Int, bitWidth :Int) extends Type {
    def name :TypeName = sym.name
    override def toString = name.toString
  }

  // TODO: can we model array as something else? data carries no parameters, but record has
  // unnecessary fields... will we need "name + params" for any other kind of type?
  case class Array (elem :Type) extends Type {
    override def toString = s"Array[$elem]"
  }

  // record type: "data Foo (bar :Int, baz :String)"
  case class Record (sym :TypeSymbol, params :Seq[Type], fields :Seq[Field]) extends Type {
    def name :TypeName = sym.name
    override def toString = name + mkString(params, "[]") + mkString(fields, "()")
  }
  case class Field (sym :TermSymbol, tpe :Type) extends Type {
    def name :TermName = sym.name
    override def toString = s"$name :$tpe"
  }

  // interface type: "interface Eq[A] { fun eq (a :A, b :A) :Bool }"
  case class Interface (sym :TypeSymbol, params :Seq[Type], methods :Seq[Method]) extends Type {
    override def toString = sym.name + mkString(params, "[]")
  }
  case class Method (sym :TermSymbol, tpe :Type) {
    def name :TermName = sym.name
    override def toString = s"$name :$tpe"
  }

  // tagged union: "data List[a] = Nil | Cons[a]"
  case class Union (sym :TypeSymbol, params :Seq[Type], cases :Seq[Type]) extends Type {
    def name :TypeName = sym.name
    override def toString = name + mkString(params, "[]")
  }

  // type application: "List[a]"
  case class Apply (ctor :Type, params :Seq[Type]) extends Type {
    override def toString = ctor + mkString(params, "[]")
  }

  // function types: I32 => I32, (String, String) => I32, [A:Ord] (a1 :A, a2 :A) => Bool
  case class Arrow (
    sym :Symbol, params :Seq[Type], csts :Seq[Type], args :Seq[Type], result :Type
  ) extends Type {
    def name = sym.name
    override def toString = mkString(params ++ csts, "[]") +
      args.mkString("(", ", ", ")") + " => " + result
    override def debugString = name + toString
  }

  // type alias: type Foo = Bar
  case class Alias (source :Type) extends Type {
    override def toString = s"alias $source"
  }

  // type variable: the a in List[a]
  case class Var (sym :TypeSymbol, scopeId :Int) extends Type {
    override def toString = s"${sym.name}$$$scopeId"
  }

  // lazy type reference, used to handle recursive declarations
  case class Lazy (tree :DefTree) extends Type {
    override def map (f :Type => Type) = f(tree.sig)
    override def toString = if (tree.hasSig) s"Lazy(${tree.sig})" else s"Lazy($tree)"
  }

  // an error type used to record name resolution failure
  case class Unknown (name :Name) extends Type {
    override def canUnify = false
    override def map (f :Type => Type) = this
    override def toString = s"?$name"
  }

  // an error type used to record other kinds of errors
  case class Error (msg :String) extends Type {
    override def canUnify = false
    override def map (f :Type => Type) = this
  }

  // TEMP: some built-in primitive types
  object Prim {
    val sym = Symbols.rootSymbol
    implicit val ctx = Context(sym)

    val Void   = primitive("Void", VoidTag,   0)
    val Unit   = primitive("Unit", UnitTag,   0)
    val Bool   = primitive("Bool", BoolTag,   1)
    val I8     = primitive("I8",   IntTag,    8)
    val I16    = primitive("I16",  IntTag,   16)
    val I32    = primitive("I32",  IntTag,   32)
    val I64    = primitive("I64",  IntTag,   64)
    val F32    = primitive("F32",  FloatTag, 32)
    val F64    = primitive("F64",  FloatTag, 64)
    val Char   = primitive("Char", CharTag,  16)

    val MaxTuple = 16 // yes yes, hack hack, will fix later
    val Tuples = 2 until MaxTuple map { r =>
      val tupTree = tupleTree(r)
      index(tupTree)
      tupTree.typed().tpe
    }

    /** Returns the primitive for the specified constant `kind` and minimum bit width. */
    def forKind (kind :Int, minWidth :Int) = kind match {
      case VoidTag   => Void
      case UnitTag   => Unit
      case BoolTag   => Bool
      case CharTag   => Char
      case IntTag    => if (minWidth > 32) I64
                        else if (minWidth > 16) I32
                        else if (minWidth > 8) I16
                        else I8
      case FloatTag  => if (minWidth > 32) F64
                        else F32
      case StringTag => ??? // TODO: strings?
    }

    /** Returns the tuple type with the specified `rank`. */
    def tuple (rank :Int) = Tuples(rank-2)

    /** Defines a primitive type and enters it into the root/primitives scope. */
    private def primitive (name :String, kind :Int, bitWidth :Int) = {
      val tname = typeName(name)
      sym.scope.enter(new TypeSymbol(tname) {
        val owner = sym
        val scope = sym.scope.nestedScope(tname)
        val info = Data(this, kind, bitWidth)
      }).info
    }
  }

  /** Creates the type of the ctor function for the record type defined by `tpe`.
   * @return the ctor `Arrow` type, or an `Error` type if `tpe` is not a `Record`. */
  def ctorType (tpe :Type) = tpe match {
    case Record(sym, params, fields) =>
      if (fields.isEmpty) tpe
      else Arrow(sym, params, Seq(), fields.map(_.tpe), tpe)
    case _ => Error(s"Cannot make constructor function for non-record type: $tpe")
  }

  /** Forces `tpe` if it is lazy, otherwise returns as is. */
  def force (tpe :Type) = tpe match {
    case Lazy(tree) => tree.sig
    case _ => tpe
  }

  /** Unfolds a recursive (lazy) type by one step. For types of the form
    * `Apply(Lazy(tpe), params))` we force the lazy type and apply the params. */
  def unfold (tpe :Type) :Type = tpe match {
    case Apply(ctor, params) => applyType(force(ctor), params)
    case _ => tpe
  }

  /** Joins two types into their upper bound if possible; yields an error type if not. */
  def join (typeA :Type, typeB :Type) :Type = {
    def fail = Error(s"Cannot join $typeA to $typeB")
    if (typeA == typeB) typeA
    else if (typeB == Untyped) typeA
    else typeA match {
      case Untyped => typeB
      case Const(constA) => typeB match {
        case Const(constB) =>
          if (constA.kind != constB.kind) fail
          // TODO: if we don't auto-widen numeric primitive types then we need some sort of
          // "constant of min width" type which we will auto-widen and then eventually convert to
          // some actual numeric primitive type...
          else Prim.forKind(constA.kind, math.max(constA.minWidth, constB.minWidth))
        case _ => join(typeB, typeA)
      }
      case Array(elemA) => typeB match {
        case Array(elemB) => join(elemA, elemB).map(Array)
        case _ => fail
      }
      case Data(sym, kindA, bitWidthA) => typeB match {
        case Const(const) if (const.kind == kindA) =>
          Prim.forKind(kindA, math.max(bitWidthA, const.minWidth))
        case Data(symB, kindB, bitWidthB) if (kindA == kindB) =>
          Prim.forKind(kindA, math.max(bitWidthA, bitWidthB))
        case _ => fail
      }
      case Union(sym, params, cases) => typeB match {
        case recType :Record if (recType.sym.owner == sym) => typeA
        case _ => fail // TODO
      }
      case Record(sym, params, fields) => typeB match {
        case unionType :Union => join(typeB, typeA)
        case _ => fail // TODO
      }
      case Field(sym, tpe) => fail
      // TODO: does this ever happen or should all applications have been processed by the time we
      // get to the point of joining a type with another?
      case Apply(ctor, params) => join(applyType(ctor, params), typeB) // TODO: is this valid?
      case Alias(source) => join(source, typeB) // TODO: is this valid?
      case Arrow(sym, params, csts, args, result) => fail // TODO: can we join function types?
      case Lazy(tree) => join(tree.sig, typeB)
      case Interface(sym, params, methods) => fail // TODO
      case Var(sym, _) => fail
      case Unknown(name) => fail
      case Error(msg) => typeA // TODO: should we combine messages if two errors are joined?
    }
  }

  /** Joins a set of types into their upper bound if possible; yields an error type if not. */
  def join (types :Seq[Type]) :Type = types reduce join

  def applyType (ctor :Type, params :Seq[Type]) :Type = if (params.isEmpty) ctor else {
    // println(s"applyType($ctor, $params)")
    def apply (tvars :Seq[Var]) =
      if (params.size != tvars.size) Error(s"Cannot match $params to vars of $ctor ($tvars)")
      else subst((tvars zip params).toMap)(ctor)
    def vars (ps :Seq[Type]) = ps.collect { case tp :Var => tp }
    force(ctor) match {
      case Apply(_, tparams) => apply(vars(tparams))
      case Arrow(_, tparams, _, _, _) => apply(vars(tparams))
      case Union(_, tparams, _) => apply(vars(tparams))
      case Record(_, tparams, _) => apply(vars(tparams))
      case Interface(_, tparams, _) => apply(vars(tparams))
      case Alias(source) => applyType(source, params) // TODO: aliases, lol
      case _ => Error(s"Cannot apply $params to non-constructor $ctor")
    }
  }

  private def subst (map :Map[Var, Type])(source :Type) :Type = source match {
    case vr@Var(_, _) => map.getOrElse(vr, vr)
    case Apply(ctor, params) => Apply(ctor, params.map(subst(map)))
    case Array(elem) => Array(subst(map)(elem))
    case Arrow(sym, params, csts, args, result) => Arrow(
      sym, params.map(subst(map)), csts.map(subst(map)), args.map(subst(map)), subst(map)(result))
    case Union(sym, params, cases) =>
      Union(sym, params.map(subst(map)), cases.map(subst(map)))
    case Record(sym, params, fields) =>
      Record(sym, params.map(subst(map)), fields.map(f => f.copy(tpe=subst(map)(f.tpe))))
    case Interface(sym, params, meths) =>
      Interface(sym, params.map(subst(map)), meths.map(m => m.copy(tpe=subst(map)(m.tpe))))
    case Lazy(tree) => subst(map)(tree.sig)
    case _ => source
  }

  def unify (csts :Seq[(Type, Type)]) :Either[Error, Map[Var, Type]] = {
    // println(s"unify($csts)")
    def bind (map :Map[Var, Type], v :Var, t :Type) = map.get(v) match {
      case None => map + (v -> t)
      case Some(ot) => map + (v -> join(t, ot))
    }
    def loop (cs :Seq[(Type, Type)], accum :Map[Var, Type]) :Either[Error, Map[Var, Type]] =
      if (cs.isEmpty) Right(accum)
      else {
        val (ta, tb) = cs.head
        def fail (msg :String) = Left(Error(s"Can't unify '$ta' and '$tb': $msg"))
        if (ta == tb) loop(cs.tail, accum)
        else (ta, tb) match {
          case (av @ Var(a, aid), _) =>
            loop(cs.tail, if (tb.canUnify) bind(accum, av, tb) else accum)
          case (Array(paramA), Array(paramB)) =>
            loop(cs.tail :+ (paramA -> paramB), accum)
          case (Apply(ctorA, paramsA), Apply(ctorB, paramsB)) =>
            if (paramsA.length != paramsB.length) fail("param lists differ in length")
            else loop((cs.tail :+ (ctorA -> ctorB)) ++ (paramsA zip paramsB), accum)
          case (Arrow(_, paramsA, cstsA, argsA, resultA),
                Arrow(_, paramsB, cstsB, argsB, resultB)) =>
            if (!paramsA.isEmpty || !paramsB.isEmpty) fail("TODO: higher order unification?")
            else if (!cstsA.isEmpty || !cstsB.isEmpty) fail("TODO: constraint unification?")
            else if (argsA.size != argsB.size) fail("arg lists differ in length")
            else loop(cs.tail ++ (argsA zip argsB) :+ (resultA -> resultB), accum)
          case _ => join(ta, tb) match {
            case err :Error => Left(err)
            case _ => loop(cs.tail, accum)
          }
        }
      }
    loop(csts, Map()).flatMap { subs =>
      val errs = subs.collect { case (v, t :Error) => s"could not unify $v: ${t.msg}" }
      if (errs.isEmpty) Right(subs)
      else Left(Error(s"Unify failure(s): ${errs.mkString(", ")}"))
    }
  }

  def unifyApply (csts :Seq[(Type, Type)], funType :Arrow) :Type =
    unify(csts).
    map(sols => applyType(funType, funType.params map subst(sols))).merge

  def unifyApplyMap (csts :Seq[(Type, Type)], funType :Arrow)(xf :Arrow => Type) :Type =
    unify(csts).
    map(sols => applyType(funType, funType.params map subst(sols)) match {
      case arr :Arrow => xf(arr)
      case tpe => tpe
    }).merge

  def boxedString[Type] (types :Seq[Type]) = mkString(types, "[]")

  private def mkString[A] (types :Seq[A], wrap :String) =
    if (types.isEmpty) "" else types.mkString(wrap.substring(0, 1), ", ", wrap.substring(1))
}
