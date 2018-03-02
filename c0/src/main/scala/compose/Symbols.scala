//
// Compose Mark 0 compiler
// https://github.com/samskivert/compose/c0/LICENSE

package compose

object Symbols {
  import Names._
  import Types._
  import Scopes._
  import Trees._

  type InfoFn = () => Type

  abstract class Symbol (val owner :Symbol, val scope :Scope, infoFn :InfoFn) {
    def name :Name

    def exists :Boolean = true
    def isType :Boolean = false
    def isTerm :Boolean = false
    def asType :TypeSymbol = throw new ClassCastException(s"Not a type symbol: $this")
    def asTerm :TermSymbol = throw new ClassCastException(s"Not a term symbol: $this")

    def map[T] (f :Symbol => T) :Option[T] = if (exists) Some(f(this)) else None

    def info :Type = infoFn()

    /** Any "ambient" type parameters (or class constraints, TBD) introduced by this symbol into
      * its lexical scope. */
    def params :Seq[ParamTree]

    /** Creates a type symbol owned by this symbol, with a newly nested scope and enters it into
      * this symbol's scope. */
    def defineType (name :TypeName, tree :Tree, params :Seq[ParamTree]) :TypeSymbol =
      scope.enter(new TypeSymbol(this, scope.nestedScope(name), treeType(tree), name, params) {
        override def toString = s"$what $name ($tree)"
      })

    /** Creates a term symbol owned by this symbol, with a newly nested scope and enters it into
      * this symbol's scope. */
    def defineTerm (name :TermName, tree :Tree) :TermSymbol = scope.enter(createTerm(name, tree))

    /** Creates a term symbol owned by this symbol, with a newly nested scope, but does not enter
      * it into this symbol's scope. */
    def createTerm (name :TermName, tree :Tree) :TermSymbol =
      new TermSymbol(this, scope.nestedScope(name), treeType(tree), name) {
        override def toString = s"$what $name ($tree)"
      }

    override def toString = s"$what $name :$info"

    private def what = if (isTerm) "term" else "type"
  }

  class TypeSymbol (
    owner :Symbol, scope :Scope, infoFn :InfoFn, val name :TypeName, val params :Seq[ParamTree]
  ) extends Symbol(owner, scope, infoFn) {
    override def isType :Boolean = true
    override def asType = this
  }

  class TermSymbol (owner :Symbol, scope :Scope, infoFn :InfoFn, val name :TermName)
      extends Symbol(owner, scope, infoFn) {
    override def isTerm :Boolean = true
    override def asTerm = this
    override def params = Seq()
  }

  val NoType = new TypeSymbol(null, newReadOnlyScope(NoName), noneType, NoName.toTypeName, Seq()) {
    override def exists :Boolean = false
  }
  val NoTerm = new TermSymbol(null, newReadOnlyScope(NoName), noneType, NoName) {
    override def exists :Boolean = false
    override def asType = NoType
  }

  val rootSymbol = {
    val rootName = termName("<root>")
    val root = new TermSymbol(NoTerm, newScope(rootName), noneType, rootName)
    Prim.Types foreach { tpe =>
      val sym = new TypeSymbol(root, root.scope.nestedScope(tpe.name), () => tpe, tpe.name, Seq())
      root.scope.enter(sym)
    }
    root
  }

  // TODO: require a tree to create our module symbol? modules should eventually have a type...
  def newModuleSymbol (name :TermName) :Symbol = rootSymbol.createTerm(name, OmittedBody)

  private val noneType :InfoFn = () => Prim.None
  private def treeType (tree :Tree) :InfoFn = () => if (tree.isTyped) tree.tpe else Lazy(tree)
}
