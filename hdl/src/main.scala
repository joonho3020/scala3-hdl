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

  val ulit = Lit[UInt](3)
  println(s"ulit.getValue: ${ulit.getValue}")


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
  val ilit = Lit[InnerBundle]((a = 3, b = 4))

  val ilit_a: Lit[UInt] = ilit.a

  inline val invalidLitType = """
  val ilit_a: Lit[Bool] = ilit.a // Type mismatch doesn't compile
  """
  assert(typeCheckErrors(invalidLitType).nonEmpty)

  assert(ilit.a.getValue == 3)
  assert(ilit_a.getValue == 3)
  assert(ilit.getValue == (3, 4))

  val mylit = Lit[MyBundle]((
    x = 2,
    y = 3,
    i = (a = 4, b = 5)))

  val mylit_x: Lit[UInt] = mylit.x
  assert(mylit.x.getValue == 2)
  assert(mylit_x.getValue == 2)

  val mylit_i: Lit[InnerBundle] = mylit.i
  inline val invalidLitType2 = """
  val mylit_i: Lit[MyBundle] = mylit.i // Type mismatch doesn't compile
  val mylit_i: Lit[UInt] = mylit.i // Type mismatch doesn't compile
  """
  assert(typeCheckErrors(invalidLitType2).nonEmpty)

  assert(mylit_i.getValue == (4, 5))
  assert(mylit.i.getValue == (4, 5))
  assert(mylit.i.a.getValue == 4)
  assert(mylit.i.b.getValue == 5)

  inline val invalidLitType3 = """
  val mylit_2 = Lit[MyBundle]((
    y = 3,
    x = 2,
    i = (a = 4, b = 5))) // Doesn't compile because we mixed up the order of named tuples
  """
  assert(typeCheckErrors(invalidLitType3).nonEmpty)

def directionality_check(): Unit =
  final case class A(a: UInt, b: UInt) extends Bundle
  final case class B(x: A, y: Bool) extends Bundle

  val bundle_a = A(
    a = Input(UInt(Width(3))),
    b = UInt(Width(4)) // defaults to Output
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

@main def demo(): Unit =
  instantiation_check()
  hosttype_check()
  literal_check()
  directionality_check()

