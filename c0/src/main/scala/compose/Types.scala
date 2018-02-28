//
// Compose Mark 0 compiler
// https://github.com/samskivert/compose/c0/LICENSE

package compose

object Types {
  import Names._
  import Constants._

  abstract class Type {
    // TODO!
  }

  // literal/constant type: 1, 2e3, 'c', "pants"
  case class Const (const :Constant) extends Type {
    override def toString = const.toString
  }
  // we defer giving literal expressions "real" types until we know what coercions are desired

  // ground/opaque type: I32, Bool, etc.
  case class Data (name :TypeName, bitWidth :Int) extends Type {
    override def toString = name.toString
  }

  // TODO: can we model array as something else? data carries no parameters, but record has
  // unnecessary fields... will we need "name + params" for any other kind of type?
  case class Array (elem :Type) extends Type {
    override def toString = s"Array[$elem]"
  }

  // record type: "data Foo (bar :Int, baz :String)"
  case class Record (name :TypeName, params :Seq[Type], fields :Seq[Field]) extends Type {
    override def toString = name + params.mkString("[", ", ", "]") + fields.mkString("(", ", ", ")")
  }
  case class Field (name :TermName, tpe :Type) {
    override def toString = s"$name :$tpe"
  }

  // interface type: "interface Eq[A] { fun eq (a :A, b :A) :Bool }"
  case class Interface (name :TypeName, params :Seq[Type], methods :Seq[Method]) extends Type {
    override def toString = name + params.mkString("[", ", ", "]")
  }
  case class Method (name :TermName, tpe :Type) {
    override def toString = s"$name :$tpe"
  }

  // tagged union: "data List[a] = Nil | Cons[a]"
  case class Union (name :TypeName, params :Seq[Type], cases :Seq[Type]) extends Type {
    override def toString = name + params.mkString("[", ", ", "]")
  }

  // type application: "List[a]"
  case class Apply (ctor :Type, params :Seq[Type]) extends Type {
    override def toString = ctor + params.mkString("[", ", ", "]")
  }

  // function types: I32 => I32, (String, String) => I32, [A:Ord] (a1 :A, a2 :A) => Bool
  case class Arrow (params :Seq[Type], args :Seq[Type], result :Type) extends Type {
    override def toString = params.mkString("[", ", ", "]") + args.mkString("(", ", ", ")") +
      " => " + result
  }

  // type alias: type Foo = Bar
  case class Alias (source :Type) extends Type {
    override def toString = s"alias $source"
  }

  // type variable: the a in List[a]
  case class Var (name :TypeName) extends Type {
    override def toString = name.toString
  }
  // TODO: bounded type variables

  // an error type used to record name resolution failure
  case class Unknown (name :Name) extends Type {
    override def toString = s"?$name"
  }

  // an error type used to record other kinds of errors
  case class Error (msg :String) extends Type

  // TEMP: some built-in primitive types
  object Prim {
    val Void   = primitive("Void",   0)
    val Unit   = primitive("Unit",   0)
    val Bool   = primitive("Bool",   1)
    val I8     = primitive("I8",     8)
    val I16    = primitive("I16",   16)
    val I32    = primitive("I32",   32)
    val I64    = primitive("I64",   64)
    val F32    = primitive("F32",   32)
    val F64    = primitive("F64",   64)
    val Char   = primitive("Char",  16)

    val Types = Seq(Void, Unit, Bool, I8, I16, I32, I64, F32, F64, Char)

    val MaxTuple = 20 // yes yes, hack hack, will fix later
    val Tuples = 2 until MaxTuple map { r =>
      val params = 1 to r map { n => Var(typeName(s"T$n")) }
      val fields = 1 to r map { n => Field(termName(s"_$n"), params(n-1)) }
      Record(typeName(s"Tuple$r"), params, fields)
    }
    def tuple (rank :Int) = Tuples(rank-2)

    // TEMP: the type of trees that have no type (e.g. defs)
    val None = primitive("None",   0)

    // TEMP: for debugging trees
    val Missing = Error("Missing")

    // TEMP: the type of type trees
    val Star = primitive("Star",   0)

    def primitive (name :String, bitWidth :Int) = Data(termName(name).toTypeName, bitWidth)
  }

  // TODO: should this be called 'meet'? maybe unify has a different technical meaning...
  def unify (types :Seq[Type]) :Type =
    // TODO:
    //
    // - unify constant types with their associated primitives (widening integral and float types
    // as necessary)
    //
    // - unify integral types via widening? (and F32 to F64?)
    //
    // - unify union cases to the union type
    if (types.isEmpty) ??? else types.head

  // TODO: implement type application!
  def applyType (ctor :Type, args :Seq[Type]) :Type =
    if (args.isEmpty) ctor else ???
}
