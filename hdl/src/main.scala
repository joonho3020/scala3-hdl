package hdl

import scala.compiletime.testing.*


final case class InnerBundle(a: UInt, b: UInt) extends Bundle
final case class MyBundle(x: UInt, y: UInt, i: InnerBundle) extends Bundle


def instantiation_check(): Unit =
  val mb = MyBundle(UInt(Width(2)), UInt(Width(3)), InnerBundle(UInt(Width(4)), UInt(Width(5))))
  val reg = Reg(mb)
  println(s"$reg")

  val reg_x: HW[UInt] = reg.x
  val reg_y: HW[UInt] = reg.y
  val reg_i: HW[InnerBundle] = reg.i
  val reg_i_a: HW[UInt] = reg_i.a

  val ulit = Lit(UInt(Width(3)))(3)
  println(s"ulit.getValue: ${ulit.getValue}")


  inline val tc1 = """
  val rg: HW[MyBundle] = Reg(mb)
  val reg_x: HW[UInt] = rg.x
  val reg_y: HW[UInt] = rg.y
  val reg_i: HW[InnerBundle] = rg.i
  val reg_i_a: HW[UInt] = rg.i.a
  val reg_i_b: HW[UInt] = rg.i.b
  """

  assert(typeCheckErrors(tc1).isEmpty)


def hosttype_check(): Unit =
  val inner_bundle_host_type: HostTypeOf[InnerBundle] = (
    a = 3,
    b = 2,
  )
  println(s"inner_bundle_host_type ${inner_bundle_host_type}")

  inline val invalidHostType = """
  val inner_bundle_host_type: hdl.HostTypeOf[hdl.InnerBundle] = (
    a = 3, b = 2, c = 4
  )
  """
  assert(typeCheckErrors(invalidHostType).nonEmpty)

def literal_check(): Unit =
  val ilit = Lit(InnerBundle(UInt(Width(4)), UInt(Width(5))))((a = 3, b = 4))

  val ilit_a: Lit[UInt] = ilit.a

  inline val invalidLitType = """
  val ilit = hdl.Lit(hdl.InnerBundle(hdl.UInt(hdl.Width(4)), hdl.UInt(hdl.Width(5))))((a = 3, b = 4))
  val ilit_a: hdl.Lit[hdl.Bool] = ilit.a
  """
  assert(typeCheckErrors(invalidLitType).nonEmpty)

  assert(ilit.a.getValue == 3)
  assert(ilit_a.getValue == 3)
  assert(ilit.getValue == (3, 4))

  val mylit = Lit(MyBundle(
    x = UInt(Width(2)),
    y = UInt(Width(3)),
    i = InnerBundle(UInt(Width(4)), UInt(Width(5)))
  ))((
    x = 2,
    y = 3,
    i = (a = 4, b = 5)))

  val mylit_x: Lit[UInt] = mylit.x
  assert(mylit.x.getValue == 2)
  assert(mylit_x.getValue == 2)

  val mylit_i: Lit[InnerBundle] = mylit.i
  inline val invalidLitType2 = """
  val mylit = hdl.Lit(hdl.MyBundle(
    x = hdl.UInt(hdl.Width(2)),
    y = hdl.UInt(hdl.Width(3)),
    i = hdl.InnerBundle(hdl.UInt(hdl.Width(4)), hdl.UInt(hdl.Width(5)))
  ))((
    x = 2,
    y = 3,
    i = (a = 4, b = 5)))
  val mylit_i1: hdl.Lit[hdl.MyBundle] = mylit.i
  val mylit_i2: hdl.Lit[hdl.UInt] = mylit.i
  """
  assert(typeCheckErrors(invalidLitType2).nonEmpty)

  assert(mylit_i.getValue == (4, 5))
  assert(mylit.i.getValue == (4, 5))
  assert(mylit.i.a.getValue == 4)
  assert(mylit.i.b.getValue == 5)

  inline val invalidLitType3 = """
  val mylit_2 = hdl.Lit(hdl.MyBundle(
    x = hdl.UInt(hdl.Width(2)),
    y = hdl.UInt(hdl.Width(3)),
    i = hdl.InnerBundle(hdl.UInt(hdl.Width(4)), hdl.UInt(hdl.Width(5)))
  ))((
    y = 3,
    x = 2,
    i = (a = 4, b = 5)))
  """
  assert(typeCheckErrors(invalidLitType3).nonEmpty)

def directionality_check(): Unit =
  final case class A(a: UInt, b: UInt) extends Bundle
  final case class B(x: A, y: Bool) extends Bundle

  val bundle_a = A(
    a = Input(UInt(Width(3))),
    b = UInt(Width(4))
  )

  assert(bundle_a.a.dir == Direction.In)
  assert(bundle_a.b.dir == Direction.Out)

  val bundle_b = B(
    x = Flipped(bundle_a),
    y = Output(Bool(()))
  )
  assert(bundle_b.x.a.dir == Direction.Out)
  assert(bundle_b.x.b.dir == Direction.In)
  assert(bundle_b.y.dir == Direction.Out)

def list_operation_check(): Unit =
  final case class MultBySumIO(a: UInt, b: UInt, sum: UInt) extends Bundle
  object MultBySumIO:
    def apply(w: Int): MultBySumIO =
      MultBySumIO(
        a = Input(UInt(Width(w))),
        b = Input(UInt(Width(w))),
        sum = Output(UInt(Width(w)))
      )

  class MultBySum(width: Int, maxMult: Int) extends Module:
    val io = IO(MultBySumIO(width))
    val wires = Seq.fill(maxMult)(Wire(UInt(Width(width))))
    wires.foreach(_ := io.a)
    io.sum := wires.reduce(_ + _)

  val top = MultBySum(4, 3)
  val elaborator = new Elaborator
  val design = elaborator.elaborate(top)
  val rendered = elaborator.emit(design)
  println("=" * 50)
  println(rendered)
  println("=" * 50)

def nested_module_check(): Unit =
  final case class SimpleIO(in: UInt, out: UInt) extends Bundle
  class A extends Module:
    val io = IO(SimpleIO(Input(UInt(Width(4))), Output(UInt(Width(4)))), Some("io"))
    io.out := io.in

  class C extends Module:
    val io = IO(SimpleIO(Input(UInt(Width(5))), Output(UInt(Width(5)))), Some("io"))
    io.out := io.in

  class B extends Module:
    val io = IO(SimpleIO(Input(UInt(Width(6))), Output(UInt(Width(6)))), Some("io"))
    val c = Module(new C)
    c.io.in := io.in
    io.out := c.io.out

  class Top extends Module:
    val io = IO(SimpleIO(Input(UInt(Width(7))), Output(UInt(Width(7)))), Some("io"))
    val a0 = Module(new A)
    val a1 = Module(new A)
    val b = Module(new B)
    a0.io.in := io.in
    a1.io.in := io.in
    b.io.in := io.in

    inline val connectionTypeCheck = """
    b.io.in := Lit[Bool](false)
    """
    assert(typeCheckErrors(connectionTypeCheck).nonEmpty)

    io.out := (a0.io.out + a1.io.out) + b.io.out

  val elaborator = new Elaborator
  val top = new Top
  val design = elaborator.elaborate(top)
  println("=" * 50)
  println(elaborator.emit(design))
  println("=" * 50)

def inheritance_check(): Unit =
  final case class SimpleIO(in: UInt, out: UInt) extends Bundle
  class Abstract extends Module:
    val io = IO(SimpleIO(Input(UInt(Width(4))), Output(UInt(Width(4)))), Some("io"))
    io.out := io.in

  class Concrete extends Abstract:
    io.out := io.in + io.in

  val elaborator = new Elaborator
  val concrete = new Concrete
  val design = elaborator.elaborate(concrete)
  println("=" * 50)
  println(elaborator.emit(design))
  println("=" * 50)

def type_parameterization_check(): Unit =
  final case class SimpleIO[T <: ValueType](in: T, out: T) extends Bundle
  class TypeParamModule[T <: ValueType](val t: T)(using DirLike[T]) extends Module:
    val io = IO(SimpleIO[T](
      in = Input(t),
      out = Output(t)
    ), Some("io"))
    io.out := io.in

  val elaborator = new Elaborator
  val tp = new TypeParamModule(t = UInt(Width(3)))
  val design = elaborator.elaborate(tp)
  println("=" * 50)
  println(elaborator.emit(design))
  println("=" * 50)

@main def demo(): Unit =
  instantiation_check()
  hosttype_check()
  literal_check()
  directionality_check()
  list_operation_check()
  nested_module_check()
  inheritance_check()
  type_parameterization_check()
