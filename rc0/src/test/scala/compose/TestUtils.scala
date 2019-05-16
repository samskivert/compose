package compose

import org.junit.Assert._

object TestUtils {
  import Modules._
  import Trees._
  import Types._

  val TestDefs = Seq(
    """def eq  :: x:Int -> y:Int -> Bool = foreign "x === y" """,
    """def add :: x:Int -> y:Int -> Int  = foreign "x + y" """,
    """def sub :: x:Int -> y:Int -> Int  = foreign "x - y" """,
    """def mul :: x:Int -> y:Int -> Int  = foreign "x * y" """,
    """def div :: x:Int -> y:Int -> Int  = foreign "x / y" """,
    """def mod :: x:Int -> y:Int -> Int  = foreign "x % y" """,
  )

  def treeErrors (tree :Tree) :List[Error] = tree.fold(List[Error]())(
    (errs, tree) => tree.treeType match {
      case err :Error => err :: errs
      case _ => errs
    })

  def assertNoErrors (tree :Tree) = {
    val errs = treeErrors(tree)
    errs foreach { err =>  println(s"Error: $err") }
    assertTrue(s"Tree has ${errs.size} error(s)", errs.isEmpty)
  }

  def testModule = {
    val p = Parsers.parser
    val mod = tempModule
    for (dsrc <- TestDefs) {
      val dtree = mod.enter(p.parseDef(dsrc))
      assertNoErrors(dtree)
    }
    mod
  }

  def typeExpr (src :String, trace :Boolean) :(Tree, Either[Error, Type]) = {
    val p = Parsers.parser
    val mod = testModule
    val tree = p.parseExpr(src).resolve(mod.scope)
    val ctx = Analysis.newCtx(trace)
    val tres = tree.inferSave(ctx)
    if (trace) ctx.tracer.result foreach println
    (tree, tres.map(_._1))
  }
}
