//
// Compose Mark 0 compiler
// https://github.com/samskivert/compose/c0/LICENSE

package compose

import java.io.PrintWriter
import java.nio.file.{Paths, Files}
import fastparse.all.P
import fastparse.core.Parsed

object TestCode {
  import Contexts._
  import Indexer._
  import Names._
  import Parser._
  import Trees._

  val out = new PrintWriter(System.out)

  val StdlibFiles = Seq(
    "std/prelude.cz", "std/logic.cz", "std/rings.cz", "std/eq.cz", "std/semigroup.cz"
  )

  def parseCode[T] (path :String, parser :P[T] = program) :Parsed[T, _, _] = {
    val cwd = Paths.get(System.getProperty("user.dir"))
    val fullPath = cwd.resolve(path)
    val sb = new java.lang.StringBuilder
    Files.lines(fullPath).forEach(sb.append(_).append("\n"))
    parser.parse(sb.toString)
  }

  def extract[T] (result :Parsed[T, _, _]) :T = result.fold(
    (p, pos, extra) => {
      extra.traced.trace.split(" / " ).foreach(f => println(s"- $f"))
      fail(result.toString)
    },
    (res, pos) => res
  )

  def testContext (name :String) = moduleContext(termName(s"${name}.cz"))

  def typeTrees (modname :String, trees :Seq[Tree]) :Seq[Tree] = {
    implicit val ctx = testContext(modname)
    trees foreach index
    trees map { _.typed() }
  }

  def typeCode (module :String, code :String) :Seq[Tree] =
    typeTrees(module, extract(program.parse(code)))

  def typeFile (file :String) :Seq[Tree] =
    typeTrees(file, extract(parseCode(file)))

  def typeFiles (files :Seq[String]) :Seq[Tree] =
    typeTrees(files.last, files.map(file => extract(parseCode(file))).flatten)

  def checkTrees (trees :Seq[Tree]) :Seq[Tree] = {
    val errs = trees.flatMap(Trees.errors)
    if (errs.isEmpty) trees
    else {
      errs foreach { case (path, err) =>
        println(s"Error: $err @ ${Trees.show(path.head)}")
        path foreach { t => println(s"- ${Trees.show(t)} : ${t.productPrefix}") }
      }
      fail("Trees had errors")
    }
  }

  val CondFib = """
    fun eq (a :I32, b :I32) :Bool = foreign("return a === b")
    fun add (a :I32, b :I32) :I32 = foreign("return a + b")
    fun sub (a :I32, b :I32) :I32 = foreign("return a - b")
    fun fib (n :I32) :I32 = cond
      eq(n, 0) = 0
      eq(n, 1) = 1
      else     = fib(n - 2) + fib(n - 1)
  """

  val MatchFib = """
    fun add (a :I32, b :I32) :I32 = foreign("return a + b")
    fun sub (a :I32, b :I32) :I32 = foreign("return a - b")
    fun fib (n :I32) :I32 = match n
      case 0 = 0
      case 1 = 1
      case _ = fib(n - 2) + fib(n - 1)
  """

  val OrdData = """
    data Ordering = LT | EQ | GT
  """

  val ListData = """
    data List[A] = Nil | Cons(head :A, tail :List[A])
  """

  val ListApply = """
    data List[A] = Nil | Cons(head :A, tail :List[A])
    fun id[A] (a :A) :A = a
    let a :List[I32] = Nil
    let b = id(5)
    let c :List[List[I32]] = Nil
    let d :List[I32] = Cons(5, Nil)
    let e = Cons(5, Nil), f = e.head, g = e.tail
  """

  val ApplyImpl = """
    interface Num[A] {
      fun add (a0 :A, a1 :A) :A
      fun add2 (a0 :A, a1 :A) :A
    }
    fun defAdd2[A:Num] (a0 :A, a1 :A) :A = add(a0, add(a1, a1))
    // fun i32Add (a :I32, b :I32) :I32 = 0
    // impl i32Num = Num[I32](add=i32Add, add2=defAdd2)
    fun i8Add (a :I8, b :I8) :I8 = 0
    impl i8Num = Num[I8](add=i8Add, add2=defAdd2)
    let a = 1, b = 2
    let c = a + b
    fun foo[A:Num] (a :A, b :A) = defAdd2(a, b)
    let d = foo(a, b)
  """

  val ParenBlock = """
    data Foo(value :I32)
    let f = ({
      let a = Foo(1), b = Foo(2)
      if false a else b
    }).value
  """

  val SimpleMatch = """
    fun add (a :I32, b :I32) :I32 = a
    fun sub (a :I32, b :I32) :I32 = a
    fun less (a :I32, b :I32) :Bool = false
    let a :I32 = 5
    let b = match a
      case 0 = 0
      case 1 = 1
      case n = n+1
  """

  val TupleMatch = """
    let bar = match (0, 0)
      case (0, 0) = 0
      case (1, 0) = 1
      case (_, _) = 2
  """

  val DestructMatch = """
    fun greater (a :I32, b :I32) :Bool = false
    data JSValue = JSInt(n :I32) | JSBool(b :Bool) | JSNone
    let foo :JSValue = JSNone
    let bar = match foo
      case JSInt(n) = n > 2
      case JSBool(b) = b
      case JSNone = false
  """

  val ParamDestructMatch = """
    fun add (a :I32, b :I32) :I32 = a
    data List[A] = Nil | Cons(head :A, tail :List[A])
    let foo :List[I32] = Cons(1, Nil)
    let bar = match foo
      case Nil = 1
      case Cons(h, t) = h + 1
  """

  val GuardedMatch = """
    fun add (a :I32, b :I32) :I32 = a
    fun eq (a :I32, b :I32) :Bool = true
    data List[A] = Nil | Cons(head :A, tail :List[A])
    let foo :List[I32] = Cons(1, Nil)
    let bar = match foo
      case Nil = 1
      case l if (false) = 2
      case Cons(h, t) if (h == 1) = 3
      case Cons(h, t) = 4
      case l = 5
  """

  val ForeignOps = """
    fun not (a :Bool) :Bool = foreign("return !a")
    fun length[A] (as :Array[A]) :I32 = foreign("return as.length")
  """
}
