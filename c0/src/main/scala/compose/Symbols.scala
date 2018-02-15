//
// Compose Mark 0 compiler
// https://github.com/samskivert/compose/c0/LICENSE

package compose

object Symbols {
  import Names._
  import Types._

  abstract class Symbol (val owner :Symbol) {
    def name :Name
    def info :Type
    def isType :Boolean = false
    def isTerm :Boolean = false
    def asType :TypeSymbol = throw new ClassCastException(s"Not a type symbol: $this")
    def asTerm :TermSymbol = throw new ClassCastException(s"Not a term symbol: $this")
  }

  abstract class TypeSymbol (owner :Symbol, val name :TypeName) extends Symbol(owner) {
    override def isType :Boolean = true
    override def asType = this
    override def toString = s"type $name" // TODO: more debug info
  }

  abstract class TermSymbol (owner :Symbol, val name :TermName) extends Symbol(owner) {
    override def isTerm :Boolean = true
    override def asTerm = this
    override def toString = s"term $name" // TODO: more debug info
  }

  def newModuleSymbol (name :TermName) :Symbol = new TermSymbol(NoTerm, name) {
    override def info = Prims.Unit // TODO: module types
  }

  val NoType = new TypeSymbol(null, NoName.toTypeName) {
    override def info = Prims.Void
  }
  val NoTerm = new TermSymbol(null, NoName) {
    override def info = Prims.Void
    override def asType = NoType
  }
}
