package hdl

@main def elaborateTest(): Unit =
  import ConnectOps.*
  import Operations.*
  import scala.concurrent.{Await, Future}
  import scala.concurrent.duration.*
  import scala.concurrent.ExecutionContext.Implicits.global

  case class AdderIO(a: UInt, b: UInt, sum: UInt) extends Bundle

  class Adder(width: Int)(using ElabContext) extends Module:
    val my_io = IO(AdderIO(
      a = Input(UInt(Width(width))),
      b = Input(UInt(Width(width))),
      sum = Output(UInt(Width(width + 1)))
    ))
    def body(): Unit =
      val temp = Wire(UInt(Width(width + 1)))
      temp := my_io.a
      my_io.sum := temp

  val elaborator = Elaborator()
  def makeAdder(using ElabContext): Adder = new Adder(8)
  val ir = elaborator.elaborate(makeAdder)

  println("=" * 50)
  println("Adder IR")
  println("=" * 50)
  println(s"Module: ${ir.name}")
  println("Ports:")
  ir.ports.foreach(p => println(s"  ${p.dir} ${p.name}: ${p.tpe}"))
  println("Body:")
  ir.body.foreach(s => println(s"  $s"))

  println("\n" + "=" * 50)
  println("Submodule Instantiation Test")
  println("=" * 50)

  class NestedModule(width: Int)(using ElabContext) extends Module:
    val io = IO(AdderIO(
      a = Input(UInt(Width(width))),
      b = Input(UInt(Width(width))),
      sum = Output(UInt(Width(width + 1)))
    ))
    def body(): Unit =
      def makeA(using ElabContext): Adder = new Adder(width + 1)
      def makeB(using ElabContext): Adder = new Adder(width)
      val ma = Instantiate(makeA)
      val mb = Instantiate(makeB)
      val ra = Reg(UInt(Width(width)))
      ra := io.a
      ma.my_io.a := io.a
      ma.my_io.b := io.b
      mb.my_io.a := io.b
      mb.my_io.b := ra
      io.sum := ma.my_io.sum + mb.my_io.sum

  val modulesBefore = elaborator.modules.toSet
  def makeNested(using ElabContext): NestedModule = new NestedModule(4)
  val nestedIR = elaborator.elaborate(makeNested)

  println(s"Module: ${nestedIR.name}")
  println("Ports:")
  nestedIR.ports.foreach(p => println(s"  ${p.dir} ${p.name}: ${p.tpe}"))
  println("Body:")
  nestedIR.body.foreach(s => println(s"  $s"))
  val childModules = elaborator.modules.filter(m => !modulesBefore.contains(m)).filter(_.name != nestedIR.name).distinct
  println("Child Modules:")
  childModules.foreach(m => println(s"  ${m.name}"))

  println("\n" + "=" * 50)
  println("Parallel Elaborate Test")
  println("=" * 50)

  case class LinkIO(in: UInt, out: UInt) extends Bundle

  class Leaf(id: Int)(using ElabContext) extends Module:
    val io = IO(LinkIO(
      in = Input(UInt(Width(4))),
      out = Output(UInt(Width(4)))
    ))
    def body(): Unit =
      io.out := io.in

  class Fanout(level: Int, fanout: Int)(using ElabContext) extends Module:
    val io = IO(LinkIO(
      in = Input(UInt(Width(4))),
      out = Output(UInt(Width(4)))
    ))
    def body(): Unit =
      val children = (0 until fanout).map { i =>
        def makeLeaf(using ElabContext): Leaf = new Leaf(level * 10 + i)
        Instantiate(makeLeaf)
      }
      children.foreach(c => c.io.in := io.in)
      val sum = children.map(c => c.io.out).reduce(_ + _)
      io.out := sum

  class Deep(levels: Int, fanout: Int)(using ElabContext) extends Module:
    val io = IO(LinkIO(
      in = Input(UInt(Width(4))),
      out = Output(UInt(Width(4)))
    ))
    def body(): Unit =
      if levels == 0 then
        io.out := io.in
      else
        val branches = (0 until fanout).map { _ =>
          def makeDeep(using ElabContext): Deep = new Deep(levels - 1, fanout)
          Instantiate(makeDeep)
        }
        branches.foreach(b => b.io.in := io.in)
        io.out := io.in

  val parallelElab = Elaborator()
  def makeRootDeep(using ElabContext): Deep = new Deep(3, 2)
  def makeRootFanout(using ElabContext): Fanout = new Fanout(2, 3)
  val tasks = Seq(
    Future(parallelElab.elaborate(makeRootDeep)),
    Future(parallelElab.elaborate(makeRootFanout))
  )
  Await.result(Future.sequence(tasks), 60.seconds)
