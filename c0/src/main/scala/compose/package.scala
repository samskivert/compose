//
// Compose Mark 0 compiler
// https://github.com/samskivert/compose/c0/LICENSE

package object compose {

  def fail (msg :String) :Nothing = throw new AssertionError(msg)

  def unreachable (obj :AnyRef) = fail(s"unreachable: $obj")
}
