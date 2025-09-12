package hdl9

@main def demo(): Unit =
  println("Hello World")

  case class InnerBundleParam(wa: Int, wb: Int)

  class InnerBundle(p: InnerBundleParam) extends Bundle:
    val a = UInt(Width(p.wa))
    val b = UInt(Width(p.wb))

  class MyBundle(p: InnerBundleParam) extends Bundle:
    val wx = p.wa + p.wb
    val x = UInt(Width(wx))
    val y = Bool()
    val i = new InnerBundle(p)

  val reg = Reg(new MyBundle(InnerBundleParam(5, 6)))
  val reg_x: Reg[UInt] = reg.x
  val reg_y: Reg[Bool] = reg.y
  val reg_i: Reg[InnerBundle] = reg.i
  val reg_i_a: Reg[UInt] = reg_i.a
  val reg_i_b: Reg[UInt] = reg.i.b

// val reg_i_b: Reg[Bool] = reg.i.b // Type mismatch doesn't compile

  println(s"reg_x: ${reg_x} reg_y: ${reg_y} reg_i: ${reg_i} reg_i_a ${reg_i_a} reg_i_b ${reg_i_b}")



  BundleMacros.printBundleFields[MyBundle]
  BundleMacros.printBundleFields[InnerBundle]

  println(s"${BundleMacros.fieldNamesOf[MyBundle]}")

  // Inspect type-level computed tuple of names for bundles

// val innerNames = BundleMacros.fieldNamesOf[InnerBundle]
// val myNames = BundleMacros.fieldNamesOf[MyBundle]
// println(s"innerNames type = ${innerNames}")
// println(s"myNames type = ${myNames}")

  val ulit = Lit[UInt](3)
  println(s"ulit.get: ${ulit.get}")

  // val inner_bundle_host_type: HostTypeOf[InnerBundle] = (
  //   a = 3,
  //   b = 2,
  // )
  // // val inner_bundle_host_type: HostTypeOf[InnerBundle] = (
  // //   a = 3,
  // //   b = 2,
  // //   c = 4
  // // ) // compile fails, type mismatch

  // println(s"inner_bundle_host_type ${inner_bundle_host_type}")


//   val ilit = Lit[InnerBundle]((a = 3, b = 4))
// 
//   val ilit_a: Lit[UInt] = ilit.a
// 
//   // val ilit_a: Lit[Bool] = ilit.a // Type mismatch doesn't compile
// 
//   println(s"ilit.a ${ilit.a.get} ilit_a.get ${ilit_a.get}")
// 
//   val mylit = Lit[MyBundle]((
//     x = 2,
//     y = 3,
//     i = (a = 4, b = 5)))
// 
//   val mylit_x: Lit[UInt] = mylit.x
//   println(s"mylit_x.get ${mylit_x.get} mylit.x.get ${mylit.x.get}")
// 
//   val mylit_i: Lit[InnerBundle] = mylit.i
// // val mylit_i: Lit[MyBundle] = mylit.i // Type mismatch doesn't compile
// // val mylit_i: Lit[UInt] = mylit.i // Type mismatch doesn't compile
//   println(s"mylit_i.get ${mylit_i.get} mylit.i.get ${mylit.i.get}")
// 
//   val mylit_i_a: Lit[UInt] = mylit.i.a
//   println(s"mylit_i_a.get ${mylit_i_a.get} ${mylit.i.a.get} ${mylit_i.a.get}")
// 
//   // val mylit_2 = Lit[MyBundle]((
//   //   y = 3,
//   //   x = 2,
//   //   i = (a = 4, b = 5))) // Doesn't compile because we mixed up the order of named tuples
// 
// 
