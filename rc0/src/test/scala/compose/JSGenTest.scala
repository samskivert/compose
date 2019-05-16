package compose

import org.junit.Test
import org.junit.Assert._
import java.io.PrintWriter
import java.lang.StringBuilder

class JSGenTest {
  import Constants._
  import Modules._
  import Names._
  import Symbols._
  import Trees._
  import Types._
  import Lower._
  import TestUtils._

  def testGen (src :String) :Unit = {
    val p = Parsers.parser
    val mod = testModule
    val foo = mod.enter(p.parseDef(src))
    // foo.debugPrint(new PrintWriter(System.out, true), "")
    assertNoErrors(foo)

    import scala.collection.JavaConverters._
    for (tree <- mod.trees) {
      val out = new StringBuilder
      JSGen.gen(lower(tree), Printing.printInto(out))
      println(out.toString)
    }
  }

  @Test def testGenFib = {
    val src = """def fib :: x:Int -> Int =
                   if x == 0 then 0
                   else if x == 1 then 1
                   else fib (x-2) + fib (x-1)"""
    testGen(src)
  }
}
