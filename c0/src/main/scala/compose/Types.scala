//
// Compose Mark 0 compiler
// https://github.com/samskivert/compose/c0/LICENSE

package compose

object Types {
  import Names._
  import Constants._
  import Trees._

  sealed abstract class Type {

    /** Applies `f` to `this` and returns the result, with caveats.
      * If `this` is an error type, `f` is not applied and `this` is returned directly.
      * If `this` is the `Lazy` type, the type is forced and then `f` is applied. */
    def map (f :Type => Type) :Type = f(this)
  }

  // literal/constant type: 1, 2e3, 'c', "pants"
  case class Const (const :Constant) extends Type {
    override def toString = const.toString
  }
  // we defer giving literal expressions "real" types until we know what coercions are desired

  // ground/opaque type: I32, Bool, etc.
  case class Data (name :TypeName, kind :Int, bitWidth :Int) extends Type {
    override def toString = name.toString
  }

  // TODO: can we model array as something else? data carries no parameters, but record has
  // unnecessary fields... will we need "name + params" for any other kind of type?
  case class Array (elem :Type) extends Type {
    override def toString = s"Array[$elem]"
  }

  // record type: "data Foo (bar :Int, baz :String)"
  case class Record (name :TypeName, params :Seq[Type], fields :Seq[Field]) extends Type {
    override def toString = name + mkString(params, "[]") + mkString(fields, "()")
  }
  case class Field (name :TermName, tpe :Type) {
    override def toString = s"$name :$tpe"
  }

  // interface type: "interface Eq[A] { fun eq (a :A, b :A) :Bool }"
  case class Interface (name :TypeName, params :Seq[Type], methods :Seq[Method]) extends Type {
    override def toString = name + mkString(params, "[]")
  }
  case class Method (name :TermName, tpe :Type) {
    override def toString = s"$name :$tpe"
  }

  // tagged union: "data List[a] = Nil | Cons[a]"
  case class Union (name :TypeName, params :Seq[Type], cases :Seq[Type]) extends Type {
    override def toString = name + mkString(params, "[]")
  }

  // type application: "List[a]"
  case class Apply (ctor :Type, params :Seq[Type]) extends Type {
    override def toString = ctor + mkString(params, "[]")
  }

  // function types: I32 => I32, (String, String) => I32, [A:Ord] (a1 :A, a2 :A) => Bool
  case class Arrow (params :Seq[Type], args :Seq[Type], result :Type) extends Type {
    override def toString = mkString(params, "[]") + args.mkString("(", ", ", ")") + " => " + result
  }

  // type alias: type Foo = Bar
  case class Alias (source :Type) extends Type {
    override def toString = s"alias $source"
  }

  // type variable: the a in List[a]
  case class Var (name :TypeName, scopeId :Int) extends Type {
    override def toString = s"$name$$$scopeId"
  }

  // lazy type reference, used to handle recursive declarations
  case class Lazy (tree :Tree) extends Type {
    override def map (f :Type => Type) = f(tree.tpe)
    override def toString = if (tree.isTyped) s"Lazy(${tree.tpe})" else s"Lazy($tree)"
  }

  // an error type used to record name resolution failure
  case class Unknown (name :Name) extends Type {
    override def map (f :Type => Type) = this
    override def toString = s"?$name"
  }

  // an error type used to record other kinds of errors
  case class Error (msg :String) extends Type {
    override def map (f :Type => Type) = this
  }

  // TEMP: some built-in primitive types
  object Prim {
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

    val Types = Seq(Void, Unit, Bool, I8, I16, I32, I64, F32, F64, Char)

    val MaxTuple = 20 // yes yes, hack hack, will fix later
    val Tuples = 2 until MaxTuple map { r =>
      val params = 1 to r map { n => Var(typeName(s"T$n"), 0) }
      val fields = 1 to r map { n => Field(termName(s"_$n"), params(n-1)) }
      Record(typeName(s"Tuple$r"), params, fields)
    }
    def tuple (rank :Int) = Tuples(rank-2)

    // TEMP: the type of trees that have no type (e.g. defs)
    val None = primitive("None", VoidTag, 0)

    // TEMP: for debugging trees
    val Missing = Error("Missing")

    // TEMP: the type of type trees
    val Star = primitive("Star", VoidTag, 0)

    def primitive (name :String, kind :Int, bitWidth :Int) =
      Data(termName(name).toTypeName, kind, bitWidth)
  }

  /** Forces `tpe` if it is lazy, otherwise returns as is. */
  def force (tpe :Type) = tpe match {
    case Lazy(tree) => tree.tpe
    case _ => tpe
  }

  /** Joins two types into their upper bound if possible; yields an error type if not. */
  def join (typeA :Type, typeB :Type) :Type = {
    def fail = Error(s"Cannot join $typeA to $typeB")
    if (typeA == typeB) typeA
    else if (typeA == Prim.None) typeB
    else if (typeB == Prim.None) typeA
    else typeA match {
      case Const(constA) => typeB match {
        case Const(constB) =>
          if (constA.kind != constB.kind) fail
          // TODO: if we don't auto-widen numeric primitive types then we need some sort of
          // "constant of min width" type which we will auto-widen and then eventually convert to
          // some actual numeric primitive type...
          else Prim.forKind(constA.kind, math.max(constA.minWidth, constB.minWidth))
        case _ => join(typeB, typeA)
      }
      case Array(elem) => fail // TODO
      case Data(name, kind, bitWidth) => typeB match {
        case Const(const) if (const.kind == kind) =>
          Prim.forKind(kind, math.max(bitWidth, const.minWidth))
        case Data(nameB, kindB, bitWidthB) if (kind == kindB) =>
          Prim.forKind(kind, math.max(bitWidth, bitWidthB))
        case _ => fail
      }
      case Record(name, params, fields) => fail // TODO
      case Interface(name, params, methods) => fail // TODO
      case Union(name, params, cases) => fail // TODO
      case Apply(ctor, params) => fail // TODO
      case Arrow(params, args, result) => fail // TODO: can we join function types?
      case Alias(source) => join(source, typeB) // TODO: is this valid?
      case Var(name, _) => fail
      case Lazy(tree) => join(tree.tpe, typeB)
      case Unknown(name) => fail
      case Error(msg) => typeA // TODO: should we combine messages if two errors are joined?
    }
  }

  /** Joins a set of types into their upper bound if possible; yields an error type if not. */
  def join (types :Seq[Type]) :Type = types reduce join

  // TODO: do we need a tree walker for type trees?

  def applyType (ctor :Type, params :Seq[Type]) :Type = if (params.isEmpty) ctor else {
    // println(s"applyType($ctor, $params)")
    def apply (tvars :Seq[Var]) =
      if (params.size != tvars.size) Error(s"Cannot match $params to vars of $ctor")
      else subst((tvars zip params).toMap)(ctor)
    def vars (ps :Seq[Type]) = ps.collect { case tp :Var => tp }
    force(ctor) match {
      case Apply(_, tparams) => apply(vars(tparams))
      case Arrow(tparams, _, _) => apply(vars(tparams))
      case Union(_, tparams, _) => apply(vars(tparams))
      case Record(_, tparams, _) => apply(vars(tparams))
      case Interface(_, tparams, _) => apply(vars(tparams))
      case Alias(source) => applyType(source, params) // TODO: aliases, lol
      case _ => Error(s"Cannot apply $params to non-constructor $ctor")
    }
  }

  def subst (map :Map[Var, Type])(source :Type) :Type = source match {
    case vr@Var(_, _) => map.getOrElse(vr, vr)
    case Apply(ctor, params) => Apply(ctor, params.map(subst(map)))
    case Array(elem) => Array(subst(map)(elem))
    case Arrow(params, args, result) => Arrow(
      params.map(subst(map)), args.map(subst(map)), subst(map)(result))
    case Union(name, params, cases) =>
      Union(name, params.map(subst(map)), cases.map(subst(map)))
    case Record(name, params, fields) =>
      Record(name, params.map(subst(map)), fields.map(f => f.copy(tpe=subst(map)(f.tpe))))
    case Interface(name, params, meths) =>
      Interface(name, params.map(subst(map)), meths.map(m => m.copy(tpe=subst(map)(m.tpe))))
    case _ => source
  }

  def unify (constraints :Seq[(Type, Type)]) :Either[Error, Map[Var, Type]] = {
    // println(s"unify($constraints)")
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
            loop(cs.tail, bind(accum, av, tb))
          case (Apply(ctorA, paramsA), Apply(ctorB, paramsB)) =>
            if (paramsA.length != paramsB.length) fail("param lists differ in length")
            else loop((cs.tail :+ (ctorA -> ctorB)) ++ (paramsA zip paramsB), accum)
          case (Arrow(paramsA, argsA, resultA), Arrow(paramsB, argsB, resultB)) =>
            if (!paramsA.isEmpty || !paramsB.isEmpty) fail("TODO: higher order unification?")
            else if (argsA.size != argsB.size) fail("arg lists differ in length")
            else loop(cs.tail ++ (argsA zip argsB) :+ (resultA -> resultB), accum)
          case _ => join(ta, tb) match {
            case err :Error => Left(err)
            case _ => loop(cs.tail, accum)
          }
        }
      }
    loop(constraints, Map())
  }

  private def mkString[A] (types :Seq[A], wrap :String) =
    if (types.isEmpty) "" else types.mkString(wrap.substring(0, 1), ", ", wrap.substring(1))
}
