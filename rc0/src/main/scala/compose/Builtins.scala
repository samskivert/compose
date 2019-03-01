package compose

object Builtins {
  import Constants._
  import Names._
  import Symbols._
  import Types._

  val voidType   = Scalar("Void",   VoidTag,    0)
  val unitType   = Scalar("Unit",   UnitTag,    0)
  val boolType   = Scalar("Bool",   BoolTag,    1)
  val intType    = Scalar("Int",    IntTag,    32)
  val floatType  = Scalar("Float",  FloatTag,  64)
  val stringType = Scalar("String", StringTag, 64)

  class BuiltinTermSym (name :TermName, val tpe :Type) extends TermSym(name) {
    def flavor = Flavor.None
  }
  class BuiltinTypeSym (name :TypeName, val sig :Type) extends TypeSym(name)
  def scalarTypeSym (sig :Scalar) :TypeSym = new BuiltinTypeSym(typeName(sig.name), sig)

  val termSyms = Seq(
    new BuiltinTermSym(termName("foreign"), {
      val aSym = new TypeSym(typeName("A")) {
        def sig = UVar(this)
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
    def lookupTerm (name :TermName) = terms.getOrElse(name, missingTerm(name))
    def lookupType (name :TypeName) = types.getOrElse(name, missingType(name))
    override def toString = "<builtins>"
  }
}
