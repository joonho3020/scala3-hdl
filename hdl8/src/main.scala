package hdl8

@main def demo(): Unit =
  // // What I want
  // class InnerBundle(wa: Int, wb: Int) extends Bundle {
  //   val a = UInt(Width(wa))
  //   val b = UInt(Width(wb))
  // }
  // class MyBundle(wa: Int, wb: Int, wx: Int, wy: Int) extends Bundle {
  //   val x = UInt(Width(wx))
  //   val y = UInt(Width(wy))
  //   val i = new InnerBundle(wa, wb)
  // }
  // val mybundle_reg = Reg[MyBundle](new MyBundle(2, 3, 4, 5))
  // val x: Reg[UInt] = mybundle_reg.x
  // val i: Reg[InnerBundle] = mybundle_reg.i
  // val a: Reg[UInt] = mybundle_reg.i.a

  // val mybundle_lit = Lit[MyBundle]((
  //     x = UIntLit(3),
  //     y = UIntLit(2),
  //     i = (
  //       a = UIntLit(4),
  //       b = UIntLit(5)
  //     )
  //   ))
  // val xl: UIntLit = mybundle_lit.x
  // val yl: UIntLit = mybundle_lit.y
  // val al: UIntLit = mybundle_lit.i.a

  println("Hello World")

  final case class InnerBundle(a: UInt, b: UInt) extends Bundle
  final case class MyBundle(x: UInt, y: UInt, i: InnerBundle) extends Bundle
  val mb = MyBundle(UInt(Width(2)), UInt(Width(3)), InnerBundle(UInt(Width(4)), UInt(Width(5))))

  val reg = Reg(mb)
  val reg_x: Reg[UInt] = reg.x
  val reg_y: Reg[UInt] = reg.y
  // val reg_y: Reg[UIntLit] = reg.y // Type mismatch, doesn't compile
  val reg_i: Reg[InnerBundle] = reg.i
  // val reg_i: Reg[UIntLit] = reg.i // Type mismatch doesn't compile
  val reg_i_a: Reg[UInt] = reg_i.a
  // val reg_i_a: Reg[UIntLit] = reg_i.a // Type mismatch doesn't compile

  val reg_i_b: Reg[UInt] = reg.i.b
  // val reg_i_b: Reg[UIntLit] = reg.i.b // Type mismatch doesn't compile
  println(s"reg_x: ${reg_x} reg_y: ${reg_y} reg_i: ${reg_i} reg_i_a ${reg_i_a} reg_i_b ${reg_i_b}")

  val ulit = Lit[UInt](3)
  println(s"ulit.get: ${ulit.get}")

  val inner_bundle_host_type: HostTypeOf[InnerBundle] = (
    a = 3,
    b = 2,
  )
  // val inner_bundle_host_type: HostTypeOf[InnerBundle] = (
  //   a = 3,
  //   b = 2,
  //   c = 4
  // ) // compile fails, type mismatch

  println(s"inner_bundle_host_type ${inner_bundle_host_type}")


  val ilit = Lit[InnerBundle]((a = 3, b = 4))

  val ilit_a: Lit[UInt] = ilit.a

  // val ilit_a: Lit[Bool] = ilit.a // Type mismatch doesn't compile

  println(s"ilit.a ${ilit.a.get} ilit_a.get ${ilit_a.get}")

  val mylit = Lit[MyBundle]((
    x = 2,
    y = 3,
    i = (a = 4, b = 5)))

  val mylit_x: Lit[UInt] = mylit.x
  println(s"mylit_x.get ${mylit_x.get} mylit.x.get ${mylit.x.get}")

  val mylit_i: Lit[InnerBundle] = mylit.i
// val mylit_i: Lit[MyBundle] = mylit.i // Type mismatch doesn't compile
// val mylit_i: Lit[UInt] = mylit.i // Type mismatch doesn't compile
  println(s"mylit_i.get ${mylit_i.get} mylit.i.get ${mylit.i.get}")

  val mylit_i_a: Lit[UInt] = mylit.i.a
  println(s"mylit_i_a.get ${mylit_i_a.get} ${mylit.i.a.get} ${mylit_i.a.get}")

  // val mylit_2 = Lit[MyBundle]((
  //   y = 3,
  //   x = 2,
  //   i = (a = 4, b = 5))) // Doesn't compile because we mixed up the order of named tuples




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

  // A "Bundle" as a case class
  final case class InnerDir(
    a: Dir[UInt, Input],
    b: Dir[Bool, Output]
  )

  final case class OuterDir(
    x: Dir[UInt, Output],
    i: InnerDir,
    v: Vec[Dir[UInt, Input]]
  )

  type InnerFlipped = FlipAll[Inner] // a:-1, b:+1
  type OuterFlipped = FlipAll[Outer] // x:+1, i flipped, v’s element flipped

  val in  = DirT(UInt(8),  1)
  val out = DirT(UInt(8), -1)

  val o = Outer(
    x = out,
    i = Inner(a = in, b = DirT(Bool(), -1)),
    v = Vec(DirT(UInt(4), 1), len = 3)
  )

  val f = flip(o) // type: Oute
