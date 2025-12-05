package hdl

final case class InnerBundle(a: UInt, b: UInt) extends Bundle
final case class MyBundle(x: UInt, y: UInt, i: InnerBundle) extends Bundle

def instantiation_check(): Unit =
  val mb = MyBundle(UInt(Width(2)), UInt(Width(3)), InnerBundle(UInt(Width(4)), UInt(Width(5))))
  val reg = Reg(mb)
  val reg_x: UInt = reg.x
  val reg_y: UInt = reg.y
  val reg_i: InnerBundle = reg.i
  val reg_i_a: UInt = reg_i.a
  val ulit = Lit(UInt(Width(3)))(3)
  assert(reg_x.kind == LeafKinds.Reg)
  assert(reg_y.kind == LeafKinds.Reg)
  assert(reg_i_a.kind == LeafKinds.Reg)
  assert(ulit.getValue == 3)

def literal_check(): Unit =
  val ilit = Lit(InnerBundle(UInt(Width(4)), UInt(Width(5))))((a = 3, b = 4))
  val ilit_a: UInt = ilit.a
  val mylit = Lit(MyBundle(
    x = UInt(Width(2)),
    y = UInt(Width(3)),
    i = InnerBundle(UInt(Width(4)), UInt(Width(5)))
  ))((
    x = 2,
    y = 3,
    i = (a = 4, b = 5)))
  val mylit_x: UInt = mylit.x
  val mylit_i: InnerBundle = mylit.i
  assert(ilit_a.getValue == 3)
  assert(ilit.getValue == (3, 4))
  assert(mylit_x.getValue == 2)
  assert(mylit_i.getValue == (4, 5))
  assert(mylit.i.a.getValue == 4)
  assert(mylit.i.b.getValue == 5)

@main def run(): Unit =
  instantiation_check()
  literal_check()
