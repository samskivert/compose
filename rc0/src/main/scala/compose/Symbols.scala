package compose

object Symbols {
  import Constants._
  import Names._
  import Scopes._
  import Types._

  enum Sort { case Term, Type, Module }
  enum Flavor { case None, Func, Ctor }

  abstract class Symbol (val name :Name) {
    /** Whether this symbol represents a term, type or module. */
    def sort :Sort
    /** Ad-hoc further refinement of this symbol's sort. */
    def flavor :Flavor
    /** The type of this symbol. */
    def tpe :Type

    override def toString = s"#$name"
    override def hashCode = System.identityHashCode(this)
    override def equals (that: Any) = this eq that.asInstanceOf[AnyRef]
  }

  trait SymTree {
    def sym :Symbol
    def symType :Type
  }
  class LexicalSym (name :Name, val sort :Sort) extends Symbol(name) {
    private var _tree :SymTree = _
    def tree :SymTree =
      if (_tree != null) _tree
      else throw new IllegalStateException(s"Requested tree of uninitialized lexical sym `$name`")
    def setTree (tree :SymTree) :tree.type =
      if (_tree == null) { _tree = tree ; tree }
      else throw new IllegalStateException(s"Attempt to reinit lexical sym `$name`")
    def flavor = Flavor.None
    def tpe = tree.symType
    def scope (parent :Scope) :Scope = sort match {
      case Sort.Term => new LexicalTermScope(parent, this)
      case Sort.Type => new LexicalTypeScope(parent, this)
      case Sort.Module => throw new IllegalStateException(
        s"Cannot make scope for module symbol `$name`")
    }
    override def toString = {
      val pre = if (_tree == null) "$!" else "$"
      s"$pre$name"
    }
  }
  def termSym (name :Name) = new LexicalSym(name, Sort.Term)
  def typeSym (name :Name) = new LexicalSym(name, Sort.Type)

  // TODO: DefSym?

  abstract class DetachedSym (name :Name) extends Symbol(name) {
    def tpe = Hole0
    override def toString = s"?$name"
  }

  class MissingSym (val sort :Sort, name :Name) extends DetachedSym(name) {
    def flavor = Flavor.None
    override def toString = s"!$name"
  }

  abstract class HoleSym extends DetachedSym(NoName) {
    def flavor = Flavor.None
  }
  class TermHoleSym extends HoleSym {
    def sort = Sort.Term
  }
  class TypeHoleSym extends HoleSym {
    def sort = Sort.Type
  }

  /** A placeholder symbol used during parsing. It is replaced with a real symbol in the name
    * resolution pass. */
  class ParsedSym (name :Name) extends DetachedSym(name) {
    def sort = Sort.Term
    def flavor = Flavor.None
  }
}
