package hdl

import scala.compiletime.testing.*

final case class SimpleIO(in: UInt, out: UInt) extends Bundle[SimpleIO]

def nested_module_check(): Unit =
  class A(w: Int) extends Module:
    given Module = this
    val io = IO(SimpleIO(Input(UInt(Width(w))), Output(UInt(Width(w)))))
    val wire = Wire(UInt(Width(w)))

  class C(w: Int) extends Module:
    given Module = this
    val io = IO(SimpleIO(Input(UInt(Width(w))), Output(UInt(Width(w)))))
    val wire = Wire(UInt(Width(w)))

  class B(w1: Int, w2: Int) extends Module:
    given Module = this
    val io = IO(SimpleIO(Input(UInt(Width(w1))), Output(UInt(Width(w1)))))
    val c1 = Module(new C(w1))
    val c2 = Module(new C(w2))
    val wire = Wire(UInt(Width(w2)))

  class Top extends Module:
    given Module = this
    val io = IO(SimpleIO(Input(UInt(Width(7))), Output(UInt(Width(7)))))
    val a0 = Module(new A(2))
    val a1 = Module(new A(3))
    val a2 = Module(new A(2))
    val b = Module(new B(4, 5))

  val top = new Top

@main def demo(): Unit =
  nested_module_check()
