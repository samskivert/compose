//
// Compose Mark 0 compiler
// https://github.com/samskivert/compose/c0/LICENSE

package compose

object Resolve {
  import Contexts._
  import Symbols._
  import Types._

  // a simple AST for resolved implementations
  sealed trait ImplTree
  case object NoImpl extends ImplTree
  case class ErrorImpl (msg :String) extends ImplTree
  case class ImplRef (impl :TermSymbol) extends ImplTree
  case class ImplApply (impl :TermSymbol, args :Seq[ImplTree]) extends ImplTree

  /** Resolves the implementations for `csts` in the implied `ctx`.
    * Any unresolvable implementations will yield an error tree. */
  def resolveCsts (csts :Seq[Type])(implicit ctx :Context) :Seq[ImplTree] = csts map {
    case cst @ Interface(faceSym, appParams, _) => resolveImpl(faceSym, appParams)
    case tpe => ErrorImpl(s"Expected constraint to have interface type, got '$tpe'")
  }

  /** Resolves a matching interface implementation in the implied `ctx`.
    * @param faceSym the type symbol of the interface for which we're resolving an impl.
    * @param appParams the type parameters being applied to the method which will be provided by
    * the implementation. The prefix of these that corresponds to the interface type parameters
    * will be used to find a matching implementation. */
  def resolveImpl (faceSym :TypeSymbol, appParams :Seq[Type])(implicit ctx :Context) :ImplTree = {
    // the apply params (possibly a prefix thereof) must match the params of the impl
    val faceParams = force(faceSym.info).asInstanceOf[Interface].params
    val implParams = appParams.take(faceParams.size)
    ctx.scope.impls(faceSym) flatMap tryApply(implParams) match {
      case Seq() => ErrorImpl(s"Unable to find ${faceSym.name} for ${boxedString(appParams)}")
      case Seq(impl) => impl
      case impls => ErrorImpl(s"Multiple matching impls: $impls")
    }
  }

  def tryApply (tgtParams :Seq[Type])(impl :TermSymbol)(implicit ctx :Context) :Option[ImplTree] =
    // an impl symbol will either be an impldef signature type (fun from possibly constrained
    // params to interface), or interface type directly (when we're just passing dicts through)
    force(impl.info) match {
      case arrow :Arrow =>
        // an impl is applicable if the params of its interface type match our target params
        def faceParams (arrow :Arrow) = arrow.result.asInstanceOf[Interface].params
        def checkApply (arrow :Arrow) = if (tgtParams != faceParams(arrow)) None
                                        else if (arrow.csts.isEmpty) Some(ImplRef(impl))
                                        else Some(ImplApply(impl, resolveCsts(arrow.csts)))
        if (arrow.params.isEmpty) checkApply(arrow)
        // if the impl is constrained, it is effectively a function from params/constraints to an
        // impl; so we do the same unifyApply that we do for a funapply to match up our target
        // params to the impl fun params and then apply the fun type using the matched params;
        // finally we can compare our target params to those of the "instantiated" interface type
        else unifyApply(faceParams(arrow) zip tgtParams, arrow) match {
          case applied :Arrow => checkApply(applied)
          case _              => None
        }
      case face :Interface =>
        if (tgtParams == face.params) Some(ImplRef(impl)) else None
      case tpe => println(s"Zoiks! $tpe") ; ??? // unreachable but scalac don't know it
    }
}
