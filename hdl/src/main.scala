package hdl

import scala.compiletime.testing.*

final case class SimpleIO(in: UInt, out: UInt) extends Bundle[SimpleIO]

def nested_module_check(): Unit =
  class A(w: Int) extends Module { body { given Module = this
    val io = IO(SimpleIO(Input(UInt(w.W)), Output(UInt(w.W))))
    val wire = Wire(UInt(Width(w)))
  }}

  class B(w: Int) extends Module { body { given Module = this
    val io = IO(SimpleIO(Input(UInt(Width(w))), Output(UInt(Width(w)))))
    val wire = Wire(UInt(Width(w)))
  }}

  class C(w1: Int, w2: Int) extends Module { body { given Module = this
    val io = IO(SimpleIO(Input(UInt(Width(w1))), Output(UInt(Width(w1)))))
    val b1 = Module(new B(w1))
    val b2 = Module(new B(w2))
    val b3 = Module(new B(w1))
    val wire = Wire(UInt(Width(w2)))
  }}

// class Top extends Module:
// val io = IO(SimpleIO(Input(UInt(Width(7))), Output(UInt(Width(7)))))
// val a0 = Module(new A(2))
// val a1 = Module(new A(3))
// val a2 = Module(new A(2))
// val b = Module(new B(4, 5))

// val top = new Top
  val a = new A(1)
  val b = new B(2)
  val c = new C(1, 2)

  println("Befor runBody")

  given ElabContext = new ElabContext
  println("a")
  a.runBody

  println("b")
  b.runBody

  println("c")
  c.runBody


@main def demo(): Unit =
  nested_module_check()
