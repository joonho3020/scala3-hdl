package hdl

final case class SimpleIO(in: UInt, out: UInt) extends Bundle[SimpleIO]

def nested_module_check(): Unit =
  class A(w: Int) extends Module:
    val io = IO(SimpleIO(Input(UInt(w.W)), Output(UInt(w.W))))
    body:
      val wire = Wire(UInt(Width(w)))

  class B(w: Int) extends Module:
    val io = IO(SimpleIO(Input(UInt(Width(w))), Output(UInt(Width(w)))))
    body:
      val wire = Wire(UInt(Width(w)))

  class C(w1: Int, w2: Int) extends Module:
    val io = IO(SimpleIO(Input(UInt(Width(w1))), Output(UInt(Width(w1)))))
    body:
      val b1 = Module(new B(w1))
      val b2 = Module(new B(w2))
      val b3 = Module(new B(w1))
      val wire = Wire(UInt(Width(w2)))

  class Top extends Module:
    val io = IO(SimpleIO(Input(UInt(Width(7))), Output(UInt(Width(7)))))
    body:
      val a0 = Module(new A(2))
      val a1 = Module(new A(3))
      val c = Module(new C(1, 2))
      val w = Wire(UInt(10.W))

  println("-------------- Before elaboration --------------")
  val top = new Top

  println("-------------- Start elaboration --------------")
  val elaborator = new Elaborator
  val designs = elaborator.elaborate(top)
  println(elaborator.emitAll(designs))

@main def demo(): Unit =
  nested_module_check()
