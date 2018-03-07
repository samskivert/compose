//
// Compose Mark 0 compiler
// https://github.com/samskivert/compose/c0/LICENSE

package compose

object Symbols {
  import Names._
  import Types._
  import Scopes._
  import Trees._

  abstract class Symbol (val owner :Symbol, val scope :Scope) {
    def name :Name

    def exists :Boolean = true
    def isType :Boolean = false
    def isTerm :Boolean = false
    def asType :TypeSymbol = throw new ClassCastException(s"Not a type symbol: $this")
    def asTerm :TermSymbol = throw new ClassCastException(s"Not a term symbol: $this")

    def map[T] (f :Symbol => T) :Option[T] = if (exists) Some(f(this)) else None

    def info :Type

    /** Any "ambient" type parameters introduced by this symbol into its lexical scope. */
    def params :Seq[Param]

    /** Creates a type symbol owned by this symbol, with a newly nested scope and enters it into
      * this symbol's scope. */
    def defineType (name :TypeName, tree :DefTree, params :Seq[Param]) :TypeSymbol =
      tree.index(scope.enter(new TypeSymbol(this, scope.nestedScope(name), name, params) {
        override def info = treeType(tree)
        override def toString = s"$what $name ($tree)"
      }))

    /** Creates a term symbol owned by this symbol, with a newly nested scope and enters it into
      * this symbol's scope. */
    def defineTerm (name :TermName, tree :DefTree) :TermSymbol =
      tree.index(scope.enter(createTerm(name, tree)))

    /** Creates a term symbol owned by this symbol, with a newly nested scope, but does not enter
      * it into this symbol's scope. */
    def createTerm (name :TermName, tree :Tree) :TermSymbol =
      new TermSymbol(this, scope.nestedScope(name), name) {
        override def info = treeType(tree)
        override def toString = s"$what $name ($tree)"
      }

    override def toString = s"$what $name :$info"

    protected def what = if (isTerm) "term" else "type"
  }

  abstract class TypeSymbol (
    owner :Symbol, scope :Scope, val name :TypeName, val params :Seq[Param]
  ) extends Symbol(owner, scope) {
    override def isType :Boolean = true
    override def asType = this
  }

  abstract class TermSymbol (
    owner :Symbol, scope :Scope, val name :TermName
  ) extends Symbol(owner, scope) {
    override def isTerm :Boolean = true
    override def asTerm = this
    override def params = Seq()
  }

  val NoType = new TypeSymbol(null, newReadOnlyScope(NoName), NoName.toTypeName, Seq()) {
    override def exists :Boolean = false
    override def info = Untyped
  }
  val NoTerm = new TermSymbol(null, newReadOnlyScope(NoName), NoName) {
    override def exists :Boolean = false
    override def asType = NoType
    override def info = Untyped
  }

  def rootSymbol = {
    val rootName = termName("<root>")
    val root = new TermSymbol(NoTerm, newScope(rootName), rootName) {
      override def info = Untyped
    }
    root
  }

  // TODO: require a tree to create our module symbol? modules should eventually have a type...
  def newModuleSymbol (name :TermName) :Symbol = Prim.sym.createTerm(name, OmittedBody)

  private def treeType (tree :Tree) = if (tree.isTyped) tree.tpe else Lazy(tree)
}
