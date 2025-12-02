package hdl

@main def elaborateTest(): Unit =
  import ConnectOps.*
  import builder.*
  import Operations.*
  import scala.concurrent.{Await, Future}
  import scala.concurrent.duration.*
  import scala.concurrent.ExecutionContext.Implicits.global

  case class AdderIO(a: UInt, b: UInt, sum: UInt) extends Bundle

  class Adder(width: Int) extends Module:
    val my_io = IO(AdderIO(
      a = Input(UInt(Width(width))),
      b = Input(UInt(Width(width))),
      sum = Output(UInt(Width(width + 1)))
    ))
    def body(using ctx: ElabContext): Unit =
      val temp = Wire(UInt(Width(width + 1)))
      temp := my_io.a
      my_io.sum := temp

  val elaborator = Elaborator()
  val adder = Adder(8)
  val ir = elaborator.elaborate(adder)

  println("=" * 50)
  println("Adder IR")
  println("=" * 50)
  println(s"Module: ${ir.name}")
  println("Ports:")
  ir.ports.foreach(p => println(s"  ${p.dir} ${p.name}: ${p.tpe}"))
  println("Body:")
  ir.body.foreach(s => println(s"  $s"))

  println("\n" + "=" * 50)
  println("Bundle Wire and Reg Test")
  println("=" * 50)

  case class DataBundle(valid: Bool, data: UInt) extends Bundle
  case class NestedBundle(ctrl: DataBundle, status: UInt) extends Bundle

  class BundleTest extends Module:
    val io = IO(AdderIO(
      a = Input(UInt(Width(8))),
      b = Input(UInt(Width(8))),
      sum = Output(UInt(Width(9)))
    ))

    def body(using ctx: ElabContext): Unit =
      val bundleType = DataBundle(
        valid = Bool(),
        data = UInt(Width(8))
      )
      val dataWire = Wire(bundleType)
      val dataWire2 = Wire(bundleType)

      val nestedType = NestedBundle(
        ctrl = DataBundle(
          valid = Bool(),
          data = UInt(Width(16))
        ),
        status = UInt(Width(4))
      )
      val nestedReg = Reg(nestedType)

      dataWire.valid := Lit(Bool())(true)
      dataWire.data := io.a
      nestedReg.ctrl.valid := dataWire.valid
      nestedReg.ctrl.data := dataWire.data
      nestedReg.status := io.b
      dataWire := dataWire2
      dataWire := Lit(bundleType)((valid = false, data = BigInt(3)))
      io.sum := nestedReg.status

  val bundleTest = BundleTest()
  val bundleIR = elaborator.elaborate(bundleTest)

  println(s"Module: ${bundleIR.name}")
  println("Ports:")
  bundleIR.ports.foreach(p => println(s"  ${p.dir} ${p.name}: ${p.tpe}"))
  println("Body:")
  bundleIR.body.foreach(s => println(s"  $s"))

  println("\n" + "=" * 50)
  println("Bundle Port Structure Test")
  println("=" * 50)

  case class InnerBundle2(a: UInt, b: UInt) extends Bundle
  case class ComplexBundle(foo: UInt, bar: InnerBundle2) extends Bundle

  class BundlePortTop extends Module:
    val io = IO(ComplexBundle(
      foo = Input(UInt(Width(3))),
      bar = InnerBundle2(
        a = Output(UInt(Width(2))),
        b = Output(UInt(Width(4)))
      )
    ))
    def body(using ctx: ElabContext): Unit =
      io.bar.a := io.foo
      io.bar.b := io.foo

  val bundlePortTop = BundlePortTop()
  val bundlePortIR = elaborator.elaborate(bundlePortTop)
  bundlePortIR.ports.foreach(p => println(s"  ${p.dir} ${p.name}: ${p.tpe}"))

  println("\n" + "=" * 50)
  println("Submodule Instantiation Test")
  println("=" * 50)

  class NestedModule(width: Int) extends Module:
    val io = IO(AdderIO(
      a = Input(UInt(Width(width))),
      b = Input(UInt(Width(width))),
      sum = Output(UInt(Width(width + 1)))
    ))

    def body(using ctx: ElabContext): Unit =
      val ma = Module(new Adder(width + 1))
      val mb = Module(new Adder(width))
      val ra = Reg(UInt(Width(width)))
      ra := io.a
      ma.my_io.a := io.a
      ma.my_io.b := io.b
      mb.my_io.a := io.b
      mb.my_io.b := ra
      io.sum := ma.my_io.sum + mb.my_io.sum

  val modulesBefore = elaborator.modules.toSet
  val nested = NestedModule(4)
  val nestedIR = elaborator.elaborate(nested)

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

  class Leaf(id: Int) extends Module:
    val io = IO(LinkIO(
      in = Input(UInt(Width(4))),
      out = Output(UInt(Width(4)))
    ))
    def body(using ctx: ElabContext): Unit =
      io.out := io.in

  class Fanout(level: Int, fanout: Int) extends Module:
    val io = IO(LinkIO(
      in = Input(UInt(Width(4))),
      out = Output(UInt(Width(4)))
    ))
    def body(using ctx: ElabContext): Unit =
      val children = (0 until fanout).map(i => Module(new Leaf(level * 10 + i)))
      children.foreach(c => c.io.in := io.in)
      val sum = children.map(c => c.io.out).reduce(_ + _)
      io.out := sum

  class Deep(levels: Int, fanout: Int) extends Module:
    val io = IO(LinkIO(
      in = Input(UInt(Width(4))),
      out = Output(UInt(Width(4)))
    ))
    def body(using ctx: ElabContext): Unit =
      if levels == 0 then
        io.out := io.in
      else
        val branches = (0 until fanout).map(_ => Module(new Deep(levels - 1, fanout)))
        branches.foreach(b => b.io.in := io.in)
        io.out := io.in

  val parallelElab = Elaborator()
  val roots = Seq(Deep(3, 2), Fanout(2, 3))
  val tasks = roots.map(r => Future(parallelElab.elaborate(r)))
  Await.result(Future.sequence(tasks), 60.seconds)
