package compose

object Symbols {
  import Constants._
  import Names._
  import Types._

  //--------
  // Symbols

  abstract class Sym {
    /** The name of this symbol. */
    def name :Name

    override def toString = s"#$name"
    override def hashCode = System.identityHashCode(this)
    override def equals (that: Any) = this eq that.asInstanceOf[AnyRef]
  }

  enum Flavor { case None, Func, Ctor }

  abstract class TermSym (val name :TermName) extends Sym {
    /** Ad-hoc refinement of term symbols. */
    def flavor :Flavor
    /** The type of the term referenced by this symbol. */
    def tpe :Type
  }

  abstract class TypeSym (val name :TypeName) extends Sym {
    /** The signature of the type defined by this symbol. */
    def sig :Type
  }

  trait TermSymTree {
    def sym :TermSym
    def symType :Type
  }
  class LexicalTermSym (name :TermName, val flavor :Flavor = Flavor.None) extends TermSym(name) {
    private var _tree :TermSymTree = _
    def tree :TermSymTree =
      if (_tree != null) _tree
      else throw new IllegalStateException(s"Requested tree of uninitialized lexical sym `$name`")
    def setTree (tree :TermSymTree) :tree.type =
      if (_tree == null) { _tree = tree ; tree }
      else throw new IllegalStateException(s"Attempt to reinit lexical sym `$name`")
    def tpe = tree.symType
    def scope (parent :Scope) :Scope = new LexicalTermScope(parent, this)
    override def toString = {
      val pre = if (_tree == null) "$!" else "$"
      s"$pre$name"
    }
  }
  def termSym (name :TermName) = new LexicalTermSym(name)

  trait TypeSymTree {
    def sym :TypeSym
    def symSig :Type
  }
  class LexicalTypeSym (name :TypeName) extends TypeSym(name) {
    private var _tree :TypeSymTree = _
    def tree :TypeSymTree =
      if (_tree != null) _tree
      else throw new IllegalStateException(s"Requested tree of uninitialized lexical sym `$name`")
    def setTree (tree :TypeSymTree) :tree.type =
      if (_tree == null) { _tree = tree ; tree }
      else throw new IllegalStateException(s"Attempt to reinit lexical sym `$name`")
    def sig = tree.symSig
    def scope (parent :Scope) :Scope = new LexicalTypeScope(parent, this)
    override def toString = {
      val pre = if (_tree == null) "$!" else "$"
      s"$pre$name"
    }
  }
  def typeSym (name :TypeName) = new LexicalTypeSym(name)

  class MissingTermSym (name :TermName) extends TermSym(name) {
    def flavor = Flavor.None
    def tpe = Hole0
    override def toString = s"!$name"
  }
  def missingTerm (name :TermName) = new MissingTermSym(name)

  class MissingTypeSym (name :TypeName) extends TypeSym(name) {
    def sig = Hole0
    override def toString = s"!$name"
  }
  def missingType (name :TypeName) = new MissingTypeSym(name)

  object TermHoleSym extends TermSym(NoName) {
    def tpe = Hole0
    def flavor = Flavor.None
  }
  object TypeHoleSym extends TypeSym(NoName.toTypeName) {
    def sig = Hole0
  }

  //-------
  // Scopes

  abstract class Scope {
    def lookupTerm (name :TermName) :TermSym
    def lookupType (name :TypeName) :TypeSym
  }

  class NestedScope (parent :Scope) extends Scope {
    def lookupTerm (name :TermName) = parent.lookupTerm(name)
    def lookupType (name :TypeName) = parent.lookupType(name)
  }
  class LexicalTermScope (parent :Scope, sym :TermSym) extends NestedScope(parent) {
    override def lookupTerm (name :TermName) =
      if (name == sym.name) sym else super.lookupTerm(name)
  }
  class LexicalTypeScope (parent :Scope, sym :TypeSym) extends NestedScope(parent) {
    override def lookupType (name :TypeName) =
      if (name == sym.name) sym else super.lookupType(name)
  }

  val emptyScope :Scope = new Scope {
    def lookupTerm (name :TermName) = missingTerm(name)
    def lookupType (name :TypeName) = missingType(name)
    override def toString = "<empty>"
  }
}
