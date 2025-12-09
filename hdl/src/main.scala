package hdl

final case class SimpleIO(in: UInt, out: UInt) extends Bundle[SimpleIO]

def nested_module_check(): Unit =
  final case class BParams(w: Int) derives StableHash

  class A(w: Int) extends Module:
    val io = IO(SimpleIO(Input(UInt(w.W)), Output(UInt(w.W))))
    body:
      println(s"Module A ${w}")
      val wire = Wire(UInt(Width(w)))

  class B(p: BParams) extends Module with CacheableModule:
    type ElabParams = BParams
    given stableHashElabParams: StableHash[BParams] = summon[StableHash[BParams]]
    def elabParams: BParams = p

    val io = IO(SimpleIO(Input(UInt(Width(p.w))), Output(UInt(Width(p.w)))))
    body:
      println(s"Module B ${p}")
      val wire = Wire(UInt(Width(p.w)))

  class C(w1: Int, w2: Int) extends Module:
    val io = IO(SimpleIO(Input(UInt(Width(w1))), Output(UInt(Width(w1)))))
    body:
      println(s"Module C ${w1} ${w2}")
      val b1 = Module(new B(BParams(w1)))
      val b2 = Module(new B(BParams(w2)))
      val b3 = Module(new B(BParams(w1)))
      val wire = Wire(UInt(Width(w2)))

  class Top extends Module:
    val io = IO(SimpleIO(Input(UInt(Width(7))), Output(UInt(Width(7)))))
    body:
      println(s"Module Top")
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
