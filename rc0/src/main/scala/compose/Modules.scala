package compose

import java.util.HashMap

object Modules {
  import Names._
  import Symbols._
  import Trees._

  class Module {
    val terms = new HashMap[TermName,TermSym]()
    val types = new HashMap[TypeName,TypeSym]()

    val scope = new Scope {
      val parent = Builtins.scope
      def lookupTerm (name :TermName) = terms.get(name) match {
        case null => parent.lookupTerm(name)
        case sym  => sym
      }
      def lookupType (name :TypeName) = types.get(name) match {
        case null => parent.lookupType(name)
        case sym  => sym
      }
    }

    def enter (tree :TermDefTree) :TermDefTree = {
      val rtree = tree.resolve(scope)
      terms.put(rtree.sym.name, rtree.sym)
      // if this function has a synonym (i.e. plus => `+`) enter it under the synonym as well
      val syn = TermSyns.get(rtree.sym.name)
      syn.map(sname => terms.put(sname, rtree.sym))
      // println(s"Entered ${rtree.sym.name} (syn: $syn): ${rtree.sym}")
      val itype = rtree.assignType(false)
      rtree
    }

    def enter (tree :TypeDefTree) :TypeDefTree = {
      val rtree = tree.resolve(scope)
      types.put(rtree.sym.name, rtree.sym)
      rtree
    }
  }
}
