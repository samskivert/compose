package compose

import java.util.HashMap

object Modules {
  import Names._
  import Symbols._
  import Trees._

  trait Module {
    def scope :Scope

    def termFor (name :TermName) :TermSym
    def typeFor (name :TypeName) :TypeSym
    def treeFor (sym :Sym) :Tree

    def terms :Iterable[TermSym]
    def types :Iterable[TypeSym]
    def trees :Iterable[Tree]

    def enter (tree :TermDefTree, trace :Boolean = false) :TermDefTree
    def enter (tree :TypeDefTree) :TypeDefTree
  }

  def tempModule :Module = new EphemeralModule

  private class EphemeralModule extends Module {
    val _terms = new HashMap[TermName,TermSym]()
    val _types = new HashMap[TypeName,TypeSym]()
    val _trees = new HashMap[Sym, Tree]()

    val scope = new Scope {
      val parent = Builtins.scope
      def lookupTerm (name :TermName) = _terms.get(name) match {
        case null => parent.lookupTerm(name)
        case sym  => sym
      }
      def lookupType (name :TypeName) = _types.get(name) match {
        case null => parent.lookupType(name)
        case sym  => sym
      }
    }

    def termFor (name :TermName) = _terms.get(name) match {
      case null => missingTerm(name)
      case sym  => sym
    }
    def typeFor (name :TypeName) = _types.get(name) match {
      case null => missingType(name)
      case sym  => sym
    }
    def treeFor (sym :Sym) = _trees.get(sym) match {
      case null => sym match {
        case _ :TermSym => EmptyTermTree
        case _ :TypeSym => EmptyTypeTree
      }
      case tree => tree
    }

    import scala.collection.JavaConverters._
    def terms = mapAsScalaMap(_terms).values
    def types = mapAsScalaMap(_types).values
    def trees = mapAsScalaMap(_trees).values

    def enter (tree :TermDefTree, trace :Boolean = false) :TermDefTree = {
      val rtree = tree.resolve(scope)
      _terms.put(rtree.sym.name, rtree.sym)
      _trees.put(rtree.sym, rtree)
      // if this function has a synonym (i.e. plus => `+`) enter it under the synonym as well
      val syn = TermSyns.get(rtree.sym.name)
      syn.map(sname => _terms.put(sname, rtree.sym))
      val itype = rtree.assignType(trace)
      rtree
    }

    def enter (tree :TypeDefTree) :TypeDefTree = {
      val rtree = tree.resolve(scope)
      _types.put(rtree.sym.name, rtree.sym)
      rtree
    }
  }
}
