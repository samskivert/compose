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
  case class Const (const :Constant) extends Type
  // we defer giving literal expressions "real" types until we know what coercions are desired

  // ground/opaque type: I32, Bool, etc.
  case class Data (name :TypeName, bitWidth :Int) extends Type

  // record type: "data Foo (bar :Int, baz :String)"
  case class Record (name :TypeName, params :Seq[Type], fields :Seq[Field]) extends Type
  case class Field (name :TermName, typ :Type)

  // tagged union: "data List[a] = Nil | Cons[a]"
  case class Union (name :TypeName, params :Seq[Type], cases :Seq[Type]) extends Type

  // type application: "List[a]"
  case class Apply (ctor :Type, params :Seq[Type]) extends Type

  // function types: I32 => I32, (String, String) => I32, [A:Ord] (a1 :A, a2 :A) => Bool
  case class Arrow (params :Seq[Type], args :Seq[Type], result :Type) extends Type

  // type alias: type Foo = Bar
  case class Alias (source :Type) extends Type

  // type variable: the a in List[a]
  case class Var (name :TypeName) extends Type
  // TODO: bounded type variables

  // an unknown type: used to record an error when name resolution fails
  case class Unknown (name :TypeName) extends Type

  // TEMP: some built-in primitive types
  object Prims {
    val Void   = primitive("Void",   0)
    val Unit   = primitive("Unit",   0)
    val Bool   = primitive("Bool",   1)
    val Int    = primitive("Int",   32)
    val Float  = primitive("Float", 32)
    val Char   = primitive("Char",  16)

    // TEMP: the type of trees that have no type (e.g. defs)
    val None   = primitive("None",   0)

    // TEMP: the type of type trees
    val Star   = primitive("Star",   0)

    def primitive (name :String, bitWidth :Int) = Data(termName(name).toTypeName, bitWidth)
  }

  // TODO: unify a set of types into a single type
  def unify (types :Seq[Type]) :Type = ???
}
