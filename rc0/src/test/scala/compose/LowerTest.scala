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
    val src = """def foo :: x :Int -> Int =
                   let bar = if x == 0 then 0 else if x == 1 then 1 else 2
                   in bar"""
    val foo = mod.enter(p.parseDef(src))
    foo.debugPrint(new PrintWriter(System.out, true), "")
    assertNoErrors(foo)
    val lfoo = lower(foo)
    lfoo foreach Lower.print
  }
}
