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

  class BuiltinSym (name :Name, val sort :Sort, val tpe :Type) extends Sym(name) {
    def flavor = Flavor.None
  }
  def scalarTypeSym (tpe :Scalar) = new BuiltinSym(typeName(tpe.name), Sort.Type, tpe)

  val termSyms = Seq(
    new BuiltinSym(termName("foreign"), Sort.Term, {
      val aSym = new Sym(typeName("A")) {
        def sort = Sort.Type
        def flavor = Flavor.None
        def tpe = UVar(this)
      }
      Abs(UVar(aSym), Arrow(stringType, UVar(aSym)))
    }))

  val typeSyms = Seq(
    scalarTypeSym(voidType),
    scalarTypeSym(unitType),
    scalarTypeSym(boolType),
    scalarTypeSym(intType),
    scalarTypeSym(floatType),
    scalarTypeSym(stringType))

  val scope :Scope = new Scope {
    val terms = termSyms.map(sym => (sym.name -> sym)).toMap
    val types = typeSyms.map(sym => (sym.name -> sym)).toMap
    def lookupTerm (name :Name) :Sym = terms.getOrElse(name, new MissingSym(Sort.Term, name))
    def lookupType (name :Name) :Sym = types.getOrElse(name, new MissingSym(Sort.Type, name))
    // def _addCompletions (pred :(sym :Sym) => Boolean, prefix :String,
    //                      syms :Builder[Sym,Seq[Sym]]) = {}
    override def toString = "<builtins>"
  }
}
