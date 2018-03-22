//
// Compose Mark 0 compiler
// https://github.com/samskivert/compose/c0/LICENSE

package compose

object Symbols {
  import Names._
  import Scopes._
  import Trees._
  import Types._

  abstract class Symbol {
    val name :Name
    val owner :Symbol
    val scope :Scope

    def exists :Boolean = true
    def isType :Boolean = false
    def isTerm :Boolean = false
    def asType :TypeSymbol = throw new ClassCastException(s"Not a type symbol: $this")
    def asTerm :TermSymbol = throw new ClassCastException(s"Not a term symbol: $this")

    def isFace :Boolean = false
    def isFun :Boolean = false
    def isMethod :Boolean = isFun && owner.isFace

    def funArgs :Seq[ArgDef] = throw new UnsupportedOperationException(
      s"Not a fundef symbol: $this")

    def map[T] (f :Symbol => T) :Option[T] = if (exists) Some(f(this)) else None

    /** Any "ambient" type parameters introduced by this symbol into its lexical scope. */
    def params :Seq[Param] = Seq()

    /** Any constraints associated with this symbol. */
    def csts :Seq[Constraint] = Seq()

    /** The type of the symbol, usually lazily computed. */
    def info :Type

    /** Creates a type symbol owned by this symbol, with a newly nested scope and enters it into
      * this symbol's scope. */
    def defineType (name :TypeName, tree :DefTree) :TypeSymbol =
      scope.enter(new TreeTypeSymbol(this, scope.nestedScope(name), name, tree))

    /** Creates a term symbol owned by this symbol, with a newly nested scope and enters it into
      * this symbol's scope. */
    def defineTerm (name :TermName, tree :DefTree) :TermSymbol =
      scope.enter(createTerm(name, tree, _ => tree.sig))

    /** Creates a term symbol owned by this symbol, with a newly nested scope and enters it into
      * this symbol's scope. */
    def defineTerm (name :TermName, tree :Tree, sigFn :Tree => Type) :TermSymbol =
      scope.enter(createTerm(name, tree, sigFn))

    /** Creates a term symbol owned by this symbol, with a newly nested scope, but does not enter
      * it into this symbol's scope. */
    def createTerm (name :TermName, tree :Tree, sigFn :Tree => Type) :TermSymbol =
      new TreeTermSymbol(this, scope.nestedScope(name), name, tree, sigFn)

    override def toString = s"$what $name :$info"
    override def hashCode = System.identityHashCode(this)
    override def equals (that: Any) = this eq that.asInstanceOf[AnyRef]

    protected def what = if (isTerm) "term" else "type"
  }

  abstract class TypeSymbol (val name :TypeName) extends Symbol {
    override def isType :Boolean = true
    override def asType = this
  }

  abstract class TermSymbol (val name :TermName) extends Symbol {
    override def isTerm :Boolean = true
    override def asTerm = this
  }

  class TreeTypeSymbol (
    val owner :Symbol, val scope :Scope, name :TypeName, tree :DefTree
  ) extends TypeSymbol(name) {
    override def isFace = tree.isInstanceOf[FaceDef]
    override def params = tree.defParams
    override def csts = tree.defCsts
    override def info = tree.sig
    override def toString = s"$what $name ($tree)"
  }

  class TreeTermSymbol (
    val owner :Symbol, val scope :Scope, name :TermName, tree :Tree, sigFn :Tree => Type
  ) extends TermSymbol(name) {
    override def info = sigFn(tree)
    override def isFun = tree.isInstanceOf[FunDef]
    override def funArgs = tree match {
      case tree :FunDef => tree.args
      case _ => super.funArgs
    }
    override def toString = s"$what $name ($tree)"
  }

  private val NoSymbol :Symbol = new Symbol {
    val name = NoName
    val scope = newReadOnlyScope(NoName)
    val owner = null
    override def info = Untyped
  }

  val NoType :TypeSymbol = new TypeSymbol(NoName.toTypeName) {
    val owner = NoSymbol
    val scope = newReadOnlyScope(NoName)
    override def exists :Boolean = false
    override def info = Untyped
  }

  val NoTerm :TermSymbol = new TermSymbol(NoName) {
    val owner = NoSymbol
    val scope = newReadOnlyScope(NoName)
    override def exists :Boolean = false
    override def asType = NoType
    override def info = Untyped
  }

  def rootSymbol :TermSymbol = new TermSymbol(termName("<root>")) {
    val owner = NoTerm
    val scope = newScope(this.name)
    override def info = Untyped
  }

  def errorTerm (msg :String) :TermSymbol = new TermSymbol(termName("<error>")) {
    val owner = NoTerm
    val scope = newScope(this.name)
    override def exists :Boolean = false
    override def info = Error(msg)
  }

  def errorType (msg :String) :TypeSymbol = new TypeSymbol(typeName("<error>")) {
    val owner = NoTerm
    val scope = newScope(this.name)
    override def exists :Boolean = false
    override def info = Error(msg)
  }

  // TODO: require a tree to create our module symbol? modules should eventually have a type...
  def newModuleSymbol (name :TermName) :Symbol = new TermSymbol(name) {
    val owner = Prim.sym
    val scope = Prim.sym.scope.nestedScope(name)
    override def info = Untyped
  }
}
