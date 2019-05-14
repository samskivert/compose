package compose

import org.junit.Test
import org.junit.Assert._
import java.io.PrintWriter

class LowerTest {
  import Constants._
  import Modules._
  import Names._
  import Symbols._
  import Trees._
  import Types._
  import Lower._
  import TestUtils._

  @Test def testUnifyIf = {
    val p = Parsers.parser
    val mod = testModule
    val src = """def foo :: x :Int -> y :Int -> Int =
                   let bar = if x == (if y == 0 then 1 else 2) then 0
                             else if x == 1 then 1
                             else foo (x-1) y
                   in bar+1"""
    val foo = mod.enter(p.parseDef(src))
    foo.debugPrint(new PrintWriter(System.out, true), "")
    assertNoErrors(foo)
    val lfoo = lower(foo)
    lfoo foreach Lower.print
  }
}
