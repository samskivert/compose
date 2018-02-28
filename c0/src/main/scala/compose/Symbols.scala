//
// Compose Mark 0 compiler
// https://github.com/samskivert/compose/c0/LICENSE

package compose

object Symbols {
  import Names._
  import Types._
  import Scopes._

  abstract class Symbol (val owner :Symbol, val scope :Scope) {
    def name :Name

    def exists :Boolean = true
    def isType :Boolean = false
    def isTerm :Boolean = false
    def asType :TypeSymbol = throw new ClassCastException(s"Not a type symbol: $this")
    def asTerm :TermSymbol = throw new ClassCastException(s"Not a term symbol: $this")

    def map[T] (f :Symbol => T) :Option[T] = if (exists) Some(f(this)) else None

    def info :Type = {
      if (_type == null) {
        if (typeCompleter == null) _type = Error(s"Missing $what completer: $name")
        else {
          _type = typeCompleter()
          typeCompleter = null
        }
      }
      _type
    }

    def initInfo (info :Type) :this.type = { _type = info ; this }
    def initInfoLazy (completer :() => Type) :this.type = {
      if (typeCompleter != null) throw new AssertionError("Symbol already initialized: $this")
      typeCompleter = completer
      this
    }

    /** Creates a type symbol owned by this symbol, with a newly nested scope and enters it into
      * this symbol's scope. */
    def defineType (name :TypeName) :TypeSymbol =
      scope.enter(new TypeSymbol(this, scope.nestedScope(name), name))

    /** Creates a term symbol owned by this symbol, with a newly nested scope and enters it into
      * this symbol's scope. */
    def defineTerm (name :TermName) :TermSymbol = scope.enter(createTerm(name))

    /** Creates a term symbol owned by this symbol, with a newly nested scope, but does not enter
      * it into this symbol's scope. */
    def createTerm (name :TermName) :TermSymbol =
      new TermSymbol(this, scope.nestedScope(name), name)

    override def toString = {
      val tpe = if (_type == null) "<uncompleted>" else _type.toString
      s"$what $name :$tpe"
    }

    private def what = if (isTerm) "term" else "type"
    private[this] var _type :Type = _
    private[this] var typeCompleter :() => Type = _
  }

  class TypeSymbol (owner :Symbol, scope :Scope, val name :TypeName) extends Symbol(owner, scope) {
    override def isType :Boolean = true
    override def asType = this
  }

  class TermSymbol (owner :Symbol, scope :Scope, val name :TermName) extends Symbol(owner, scope) {
    override def isTerm :Boolean = true
    override def asTerm = this
  }

  val NoType = new TypeSymbol(null, newReadOnlyScope(NoName), NoName.toTypeName) {
    override def exists :Boolean = false
  }
  val NoTerm = new TermSymbol(null, newReadOnlyScope(NoName), NoName) {
    override def exists :Boolean = false
    override def asType = NoType
  }

  val rootSymbol = {
    val rootName = termName("<root>")
    val root = new TermSymbol(NoTerm, newScope(rootName), rootName)
    Prim.Types foreach { tpe =>
      val sym = new TypeSymbol(root, root.scope.nestedScope(tpe.name), tpe.name)
      sym.initInfo(tpe)
      root.scope.enter(sym)
    }
    root
  }

  def newModuleSymbol (name :TermName) :Symbol = rootSymbol.createTerm(name)
}