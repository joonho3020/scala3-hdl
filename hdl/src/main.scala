package hdl

@main def demo(): Unit =
  import scala.compiletime.testing.{typeCheckErrors, typeChecks}

  final case class InnerBundle(a: UInt, b: UInt) extends Bundle
  final case class MyBundle(x: UInt, y: UInt, i: InnerBundle) extends Bundle
  val mb = MyBundle(UInt(Width(2)), UInt(Width(3)), InnerBundle(UInt(Width(4)), UInt(Width(5))))

  val reg = Reg(mb)
  val reg_x: Reg[UInt] = reg.x
  val reg_y: Reg[UInt] = reg.y
  val reg_i: Reg[InnerBundle] = reg.i
  val reg_i_a: Reg[UInt] = reg_i.a
  val reg_i_b: Reg[UInt] = reg.i.b
  println(s"reg_x: ${reg_x} reg_y: ${reg_y} reg_i: ${reg_i} reg_i_a ${reg_i_a} reg_i_b ${reg_i_b}")

  inline def invalidRegType1 = "val reg_y: hdl.Reg[hdl.UInt] = null.asInstanceOf[hdl.Reg[hdl.Bool]]"
  inline def invalidRegType2 = "val reg_i: hdl.Reg[hdl.UInt] = null.asInstanceOf[hdl.Reg[hdl.Bool]]"
  assert(typeCheckErrors(invalidRegType1).nonEmpty, "Reg type mismatch should fail")
  assert(typeCheckErrors(invalidRegType2).nonEmpty, "Reg type mismatch should fail")

  val ulit = Lit[UInt](3)
  println(s"ulit.get: ${ulit.get}")

  val inner_bundle_host_type: HostTypeOf[InnerBundle] = (
    a = 3,
    b = 2,
  )
  println(s"inner_bundle_host_type ${inner_bundle_host_type}")

  inline def invalidHostType = """
    import hdl.*
    final case class InnerBundle(a: UInt, b: UInt) extends Bundle
    val inner_bundle_host_type: HostTypeOf[InnerBundle] = (a = 3, b = 2, c = 4)
  """
  assert(typeCheckErrors(invalidHostType).nonEmpty, "Extra field in HostTypeOf should fail")

  val ilit = Lit[InnerBundle]((a = 3, b = 4))
  val ilit_a: Lit[UInt] = ilit.a
  println(s"ilit.a ${ilit.a.get} ilit_a.get ${ilit_a.get}")

  inline def invalidLitFieldType = """
    import hdl.*
    final case class InnerBundle(a: UInt, b: UInt) extends Bundle
    val ilit = Lit[InnerBundle]((a = 3, b = 4))
    val ilit_a: Lit[Bool] = ilit.a
  """
  assert(typeCheckErrors(invalidLitFieldType).nonEmpty, "Lit field type mismatch should fail")

  val mylit = Lit[MyBundle]((
    x = 2,
    y = 3,
    i = (a = 4, b = 5)))

  val mylit_x: Lit[UInt] = mylit.x
  println(s"mylit_x.get ${mylit_x.get} mylit.x.get ${mylit.x.get}")

  val mylit_i: Lit[InnerBundle] = mylit.i
  println(s"mylit_i.get ${mylit_i.get} mylit.i.get ${mylit.i.get}")

  inline def invalidNestedLitType1 = """
    import hdl.*
    final case class InnerBundle(a: UInt, b: UInt) extends Bundle
    final case class MyBundle(x: UInt, y: UInt, i: InnerBundle) extends Bundle
    val mylit = Lit[MyBundle]((x = 2, y = 3, i = (a = 4, b = 5)))
    val mylit_i: Lit[MyBundle] = mylit.i
  """
  inline def invalidNestedLitType2 = """
    import hdl.*
    final case class InnerBundle(a: UInt, b: UInt) extends Bundle
    final case class MyBundle(x: UInt, y: UInt, i: InnerBundle) extends Bundle
    val mylit = Lit[MyBundle]((x = 2, y = 3, i = (a = 4, b = 5)))
    val mylit_i: Lit[UInt] = mylit.i
  """
  assert(typeCheckErrors(invalidNestedLitType1).nonEmpty, "Nested Lit type mismatch should fail")
  assert(typeCheckErrors(invalidNestedLitType2).nonEmpty, "Nested Lit type mismatch should fail")

  val mylit_i_a: Lit[UInt] = mylit.i.a
  println(s"mylit_i_a.get ${mylit_i_a.get} ${mylit.i.a.get} ${mylit_i.a.get}")

  inline def invalidTupleOrder = """
    import hdl.*
    final case class InnerBundle(a: UInt, b: UInt) extends Bundle
    final case class MyBundle(x: UInt, y: UInt, i: InnerBundle) extends Bundle
    val mylit_2 = Lit[MyBundle]((y = 3, x = 2, i = (a = 4, b = 5)))
  """
  assert(typeCheckErrors(invalidTupleOrder).nonEmpty, "Wrong named tuple order should fail")

  import RegVecOps.*
  import LitVecOps.*

  final case class InnerVecBundle(a: UInt, b: Vec[UInt]) extends Bundle
  final case class MyVecBundle(i: Vec[InnerVecBundle], c: Bool) extends Bundle
  val mvb = MyVecBundle(
    Vec(
      InnerVecBundle(
        UInt(Width(2)),
        Vec(UInt(Width(3)), 2)
      ),
      3
    ),
    Bool()
  )
  val reg_mvb: Reg[MyVecBundle] = Reg(mvb)
  println(s"reg_mvg ${reg_mvb}")

  val reg_mvb_0: Reg[InnerVecBundle] = reg_mvb.i(0)
  println(s"reg_mvg_0 ${reg_mvb_0}")

  val reg_mvb_12: Reg[Vec[InnerVecBundle]] = reg_mvb.i(1, 2)
  println(s"reg_mvg_12 ${reg_mvb_12}")

  val reg_uintbool = Reg(Vec(Bool(), 10))
  val reg_uintbool_2: Reg[Bool] = reg_uintbool(2)
  println(s"reg_uintbool_2 ${reg_uintbool_2}")

  val mvb_lit = Lit[MyVecBundle]((
    i = Seq.fill(3)((a = 3, b = Seq(1, 2))),
    c = true
  ))
  val mvb_lit_i0_a: Lit[UInt] = mvb_lit.i(0).a
  println(s"mvb_lit_i0_a ${mvb_lit_i0_a.get}")

  val mvb_lit_i1_b: Lit[Vec[UInt]] = mvb_lit.i(1).b
  println(s"mvb_lit_i1_b ${mvb_lit_i1_b.get}")

  val mvb_lit_i1_b0: Lit[UInt] = mvb_lit.i(1).b(0)
  println(s"mvb_lit_i1_b0 ${mvb_lit_i1_b0.get}")

  // Directionality

  final case class A(a: UInt, b: UInt) extends Bundle
  final case class B(x: A, y: Bool) extends Bundle

  val bundle_a = A(
    a = Input(UInt(Width(3))),
    b = UInt(Width(4)) // defaults to Output
  )

  println(s"bundle_a ${bundle_a}")

  val bundle_b = B(
    x = Flipped(bundle_a),
    y = Output(Bool(()))
  )
  println(s"bundle_b ${bundle_b}")

  val bundle_b_reg = Reg(bundle_b)
  val bundle_b_reg_x_a: Reg[UInt] = bundle_b_reg.x.a
  val bundle_b_reg_x_b: Reg[UInt] = bundle_b_reg.x.b
  val bundle_b_reg_y: Reg[Bool] = bundle_b_reg.y
  // val bundle_b_reg_x_a: Reg[Bool] = bundle_b_reg.x.a // Compile error, type mismatch
  println(s"bundle_b_reg_x_a ${bundle_b_reg_x_a} bundle_b_reg_x_b ${bundle_b_reg_x_b} bundle_b_reg_y ${bundle_b_reg_y}")

@main def elaborateTest(): Unit =
  import ConnectOps.*
  import builder.*
  import Operations.*
  import scala.concurrent.{Await, Future}
  import scala.concurrent.duration.*
  import scala.concurrent.ExecutionContext.Implicits.global

  println("=" * 50)
  println("New API Test")
  println("=" * 50)

  case class AdderIO(a: UInt, b: UInt, sum: UInt) extends Bundle

  class Adder(width: Int) extends Module:
    val my_io = IO(AdderIO(
      a = Input(UInt(Width(width))),
      b = Input(UInt(Width(width))),
      sum = Output(UInt(Width(width + 1)))
    ))
    def body(using ctx: ElabContext): Unit =
      val temp = Wire(UInt(Width(width + 1)))
      val a = my_io.a
      temp := a
      my_io.sum := temp

  val elaborator = Elaborator()
  val adder = Adder(8)
  val ir = elaborator.elaborate(adder)

  println(s"Module: ${ir.name}")
  println(s"Ports:")
  ir.ports.foreach(p => println(s"  ${p.dir} ${p.name}: ${p.tpe}"))
  println(s"Body:")
  ir.body.foreach(s => println(s"  $s"))

  println("\n" + "=" * 50)
  println("Bundle Wire and Reg Test")
  println("=" * 50)

  // Test creating wires and registers with bundle types
  case class DataBundle(valid: Bool, data: UInt) extends Bundle
  case class NestedBundle(ctrl: DataBundle, status: UInt) extends Bundle

  class BundleTest extends Module:
    val io = IO(AdderIO(
      a = Input(UInt(Width(8))),
      b = Input(UInt(Width(8))),
      sum = Output(UInt(Width(9)))
    ))

    def body(using ctx: ElabContext): Unit =
      val dataWire = Wire(DataBundle(
        valid = Bool(),
        data = UInt(Width(8))
      ))

      val dataWire_2 = Wire(DataBundle(
        valid = Bool(),
        data = UInt(Width(8))
      ))

      val validWire: Wire[Bool] = dataWire.valid
      val dataFieldWire: Wire[UInt] = dataWire.data

      // Create a register with a nested bundle type
      val nestedReg = Reg(NestedBundle(
        ctrl = DataBundle(
          valid = Bool(),
          data = UInt(Width(16))
        ),
        status = UInt(Width(4))
      ))

      val ctrlReg: Reg[DataBundle] = nestedReg.ctrl
      val validReg: Reg[Bool] = nestedReg.ctrl.valid
      val dataReg: Reg[UInt] = nestedReg.ctrl.data
      val statusReg: Reg[UInt] = nestedReg.status

      dataWire.valid := Lit[Bool](true)
      dataWire.data := io.a
      nestedReg.ctrl.valid := dataWire.valid
      nestedReg.ctrl.data := dataWire.data
      nestedReg.status := io.b
      io.sum := nestedReg.status

      dataWire := dataWire_2

      dataWire := Lit[DataBundle]((valid = false, data = 3))

      println(s"Created wire with bundle: $dataWire")
      println(s"  - valid field: $validWire")
      println(s"  - data field: $dataFieldWire")
      println(s"Created register with nested bundle: $nestedReg")
      println(s"  - ctrl field: $ctrlReg")
      println(s"  - ctrl.valid field: $validReg")
      println(s"  - ctrl.data field: $dataReg")
      println(s"  - status field: $statusReg")

  val bundleTest = BundleTest()
  val bundleIR = elaborator.elaborate(bundleTest)

  println(s"\nModule: ${bundleIR.name}")
  println(s"Ports:")
  bundleIR.ports.foreach(p => println(s"  ${p.dir} ${p.name}: ${p.tpe}"))
  println(s"Body:")
  bundleIR.body.foreach(s => println(s"  $s"))

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

  println(s"\nModule: ${nestedIR.name}")
  println(s"Ports:")
  nestedIR.ports.foreach(p => println(s"  ${p.dir} ${p.name}: ${p.tpe}"))
  println(s"Body:")
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
