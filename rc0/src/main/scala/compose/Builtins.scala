package compose

object Builtins {
  import Constants._
  import Names._
  import Symbols._
  import Types._
  import Scopes._

  val voidType   = Scalar("Void",   VoidTag,    0)
  val unitType   = Scalar("Unit",   UnitTag,    0)
  val boolType   = Scalar("Bool",   BoolTag,    1)
  val intType    = Scalar("Int",    IntTag,    32)
  val floatType  = Scalar("Float",  FloatTag,  64)
  val stringType = Scalar("String", StringTag, 64)

  class BuiltinTypeSym (val tpe :Scalar) extends Sym(termName(tpe.name)) {
    def sort = Sort.Type
    def flavor = Flavor.None
  }

  val typeSyms = Seq(
    new BuiltinTypeSym(voidType),
    new BuiltinTypeSym(unitType),
    new BuiltinTypeSym(boolType),
    new BuiltinTypeSym(intType),
    new BuiltinTypeSym(floatType),
    new BuiltinTypeSym(stringType))

  val scope :Scope = new Scope {
    val terms = Map[Name,BuiltinTypeSym]()
    val types = typeSyms.map(sym => (sym.name -> sym)).toMap
    def lookupTerm (name :Name) :Sym = terms.getOrElse(name, new MissingSym(Sort.Term, name))
    def lookupType (name :Name) :Sym = types.getOrElse(name, new MissingSym(Sort.Type, name))
    // def _addCompletions (pred :(sym :Sym) => Boolean, prefix :String,
    //                      syms :Builder[Sym,Seq[Sym]]) = {}
    override def toString = "<builtins>"
  }
}
