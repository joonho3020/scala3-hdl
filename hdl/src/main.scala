package hdl

import scala.compiletime.testing.*

final case class SimpleIO(in: UInt, out: UInt) extends Bundle[SimpleIO]

def simple_module_test(): Unit =
  class A extends Module:
    given Module = this
    val io = IO(SimpleIO(Input(UInt(Width(4))), Output(UInt(Width(4)))))
    io.out := io.in

  inline val tc1 = """
  io.out := Lit(Bool)(false)
  """
  assert(typeCheckErrors(tc1).nonEmpty)

  val elaborator = new Elaborator
  val a = new A
  val designs = elaborator.elaborate(a)
  println("=" * 50)
  println("Simple Module Test:")
  println(elaborator.emitAll(designs))

def list_operation_check(): Unit =
  final case class MultBySumIO(a: UInt, b: UInt, sum: Seq[UInt]) extends Bundle[MultBySumIO]
  object MultBySumIO:
    def apply(w: Int): MultBySumIO =
      MultBySumIO(
        a = Input(UInt(Width(w))),
        b = Input(UInt(Width(w))),
        sum = Seq.fill(3)(Output(UInt(Width(w)))),
      )

  class MultBySum(width: Int, maxMult: Int) extends Module:
    given Module = this
    val io = IO(MultBySumIO(width))
    val wires = Seq.fill(maxMult)(Wire(UInt(Width(width))))
    wires.foreach(_ := io.a)
    wires(0) := wires(1) + Lit(UInt(Width(width)))(3)
    io.sum(0) := wires.reduce(_ + _)
    io.sum(1) := wires.reduce(_ + _)

  val top = new MultBySum(4, 3)
  val elaborator = new Elaborator
  val designs = elaborator.elaborate(top)
  val rendered = elaborator.emitAll(designs)
  println("=" * 50)
  println("List Operation Check:")
  println(rendered)
  println("=" * 50)

def nested_module_check(): Unit =
  class A(w: Int) extends Module:
    given Module = this
    val io = IO(SimpleIO(Input(UInt(Width(w))), Output(UInt(Width(w)))))
    io.out := io.in

  class C(w: Int) extends Module:
    given Module = this
    val io = IO(SimpleIO(Input(UInt(Width(w))), Output(UInt(Width(w)))))
    io.out := io.in

  class B(w1: Int, w2: Int) extends Module:
    given Module = this
    val io = IO(SimpleIO(Input(UInt(Width(w1))), Output(UInt(Width(w1)))))
    val c = Module(new C(w2))
    c.io.in := io.in
    io.out := c.io.out

  class Top extends Module:
    given Module = this
    val io = IO(SimpleIO(Input(UInt(Width(7))), Output(UInt(Width(7)))))
    val a0 = Module(new A(2))
    val a1 = Module(new A(3))
    val b = Module(new B(4, 5))
    a0.io.in := io.in
    a1.io.in := io.in
    b.io.in := io.in

    io.out := (a0.io.out + a1.io.out) + b.io.out

  val elaborator = new Elaborator
  val top = new Top
  val designs = elaborator.elaborate(top)
  println("=" * 50)
  println("Nested Module Check:")
  println(elaborator.emitAll(designs))
  println("=" * 50)

def nested_bundle_check(): Unit =
  case class ABundle(x: UInt, y: UInt) extends Bundle[ABundle]
  case class BBundle(z: ABundle, w: ABundle) extends Bundle[BBundle]

  class NestedBundleModule extends Module:
    given Module = this
    val io = IO(BBundle(
      z = Input(ABundle(x = UInt(Width(1)), y = UInt(Width(2)))),
      w = Output(ABundle(x = UInt(Width(1)), y = UInt(Width(2))))))

    io.w := Reg(io.z)

  val elaborator = new Elaborator
  val top = new NestedBundleModule
  val designs = elaborator.elaborate(top)
  println("=" * 50)
  println("Nested Bundle Check:")
  println(elaborator.emitAll(designs))
  println("=" * 50)

def inheritance_check(): Unit =
  class Abstract extends Module:
    given Module = this
    val io = IO(
      SimpleIO(Input(UInt(Width(4))), Output(UInt(Width(4))))
    )
    io.out := io.in

  class Concrete extends Abstract:
    val add_result = io.in + io.in + io.in
    io.out := add_result

  val elaborator = new Elaborator
  val concrete = new Concrete
  val designs = elaborator.elaborate(concrete)
  println("=" * 50)
  println("Inheritance Check:")
  println(elaborator.emitAll(designs))
  println("=" * 50)

def type_parameterization_check(): Unit =
  final case class SimpleIOT[T <: HWData](in: T, out: T)
    extends Bundle[SimpleIOT[T]]

  class TypeParamModule[T <: HWData](
    val t: T
  ) extends Module:
    given Module = this
    val io = IO(SimpleIOT[T](
      in = Input(t),
      out = Output(t)
    ))
    io.out := io.in

  val elaborator = new Elaborator
  val tp = new TypeParamModule(t = UInt(Width(3)))
  val design = elaborator.elaborate(tp)
  println("=" * 50)
  println("Type Parameterization Check: UInt(Width(3))")
  println(elaborator.emitAll(design))

  val elaborator2 = new Elaborator
  val tp2 = new TypeParamModule(t = Bool())
  val design2 = elaborator2.elaborate(tp2)
  println("Type Parameterization Check: Bool")
  println(elaborator2.emitAll(design2))
  println("=" * 50)

def conditional_generation_check(): Unit =
  class A(add: Boolean) extends Module:
    given Module = this
    val io = IO(SimpleIO(Input(UInt(Width(4))), Output(UInt(Width(4)))))
    if (add)
      io.out := io.in + io.in
    else
      io.out := io.in

  val elaborator = new Elaborator
  val add_true = new A(add = true)
  val design = elaborator.elaborate(add_true)

  println("=" * 50)
  println("Conditional generational check: add = true")
  println(elaborator.emitAll(design))

  val elaborator2 = new Elaborator
  val add_false = new A(add = false)
  val design_false = elaborator2.elaborate(add_false)

  println("Conditional generational check: add = false")
  println(elaborator.emitAll(design_false))
  println("=" * 50)

def when_behavior_check(): Unit =
  final case class WhenIO(
    sel: Bool, sel2: Bool, sel3: Bool, in: UInt, alt: UInt, out: UInt) extends Bundle[WhenIO]

  class WhenModule extends Module:
    given Module = this
    val io = IO(WhenIO(
      sel = Input(Bool()),
      sel2 = Input(Bool()),
      sel3 = Input(Bool()),
      in = Input(UInt(Width(4))),
      alt = Input(UInt(Width(4))),
      out = Output(UInt(Width(4)))
    ))
    when(io.sel) {
      io.out := io.in
    }.otherwise {
      io.out := io.alt
    }

  class WhenElseModule extends Module:
    given Module = this
    val io = IO(WhenIO(
      sel = Input(Bool()),
      sel2 = Input(Bool()),
      sel3 = Input(Bool()),
      in = Input(UInt(Width(4))),
      alt = Input(UInt(Width(4))),
      out = Output(UInt(Width(4)))
    ))
    when(io.sel) {
      io.out := io.in
    }.elsewhen(io.sel2) {
      when (io.sel3) {
        io.out := io.alt
      } .otherwise {
        io.out := io.in
      }
    }.otherwise {
      io.out := io.in + io.alt
    }

  val elaborator = new Elaborator
  val m1 = new WhenModule
  val d1 = elaborator.elaborate(m1)
  println("=" * 50)
  println("When/Otherwise Check:")
  println(elaborator.emitAll(d1))

  val elaborator2 = new Elaborator
  val m2 = new WhenElseModule
  val d2 = elaborator2.elaborate(m2)
  println("When/Elsewhen/Otherwise Check:")
  println(elaborator2.emitAll(d2))
  println("=" * 50)

def comparison_operator_check(): Unit =
  final case class CmpIO(in: UInt, thresh: UInt, flag: Bool) extends Bundle[CmpIO]
  class Cmp extends Module:
    given Module = this
    val io = IO(CmpIO(
      in = Input(UInt(Width(4))),
      thresh = Input(UInt(Width(4))),
      flag = Output(Bool())
    ))
    io.flag := io.in === io.thresh

  final case class CmpBoolIO(a: Bool, b: Bool, out: Bool) extends Bundle[CmpBoolIO]
  class CmpBool extends Module:
    given Module = this
    val io = IO(CmpBoolIO(
      a = Input(Bool()),
      b = Input(Bool()),
      out = Output(Bool())
    ))
    io.out := io.a =/= io.b

  val elaborator = new Elaborator
  val m1 = new Cmp
  val d1 = elaborator.elaborate(m1)
  println("=" * 50)
  println("Comparison Operator Check (UInt):")
  println(elaborator.emitAll(d1))

  val elaborator2 = new Elaborator
  val m2 = new CmpBool
  val d2 = elaborator2.elaborate(m2)
  println("Comparison Operator Check (Bool):")
  println(elaborator2.emitAll(d2))
  println("=" * 50)

def optional_io_check(): Unit =
   class A(debug: Boolean, w: Int) extends Module:
     given Module = this
     case class MyBundleIf(
       a: Option[UInt],
       b: UInt,
       c: UInt) extends Bundle[MyBundleIf]

     val io = IO(MyBundleIf(
       a = if (debug) Some(Input(UInt(Width(w)))) else None,
       b = Input(UInt(Width(w))),
       c = Output(UInt(Width(w + 1)))
     ))
     io.c := io.b
     io.a.map(x => {
       io.c := x + io.b
     })

   val elaborator = new Elaborator
   val add_true = new A(debug = true, w = 2)
   val design = elaborator.elaborate(add_true)
   println(elaborator.emitAll(design))

   val elaborator2 = new Elaborator
   val add_false = new A(debug = false, w = 2)
   val design_2 = elaborator2.elaborate(add_false)
   println(elaborator2.emitAll(design_2))

def nested_seq_generation_check(): Unit =
  final case class AccIO(in: UInt, out: UInt) extends Bundle[AccIO]
  class MatrixAcc(rows: Int, cols: Int) extends Module:
    given Module = this
    val io = IO(AccIO(Input(UInt(Width(8))), Output(UInt(Width(16)))))
    val mats = Seq.tabulate(rows, cols)((r, c) => Wire(UInt(Width(8))))
    mats.flatten.zipWithIndex.foreach { case (w, idx) =>
      w := io.in + Lit(UInt(Width(8)))(idx)
    }
    val rowSums = mats.map(_.reduce(_ + _))
    io.out := rowSums.reduce(_ + _)
  val elaborator = new Elaborator
  val top = new MatrixAcc(2, 3)
  val designs = elaborator.elaborate(top)
  println("=" * 50)
  println("Nested Seq Generation Check:")
  println(elaborator.emitAll(designs))
  println("=" * 50)

def optional_and_map_check(): Unit =
  final case class OptIO(a: Option[UInt], b: UInt, out: UInt) extends Bundle[OptIO]
  class OptModule(useA: Boolean) extends Module:
    given Module = this
    val io = IO(OptIO(
      a = if useA then Some(Input(UInt(Width(4)))) else None,
      b = Input(UInt(Width(4))),
      out = Output(UInt(Width(5)))
    ))
    io.out := io.b
    io.a.map(x => io.out := x + io.b)
  val elaborator = new Elaborator
  val m1 = new OptModule(true)
  val d1 = elaborator.elaborate(m1)
  println("=" * 50)
  println("Optional Map Check (with a):")
  println(elaborator.emitAll(d1))
  println("=" * 50)
  val elaborator2 = new Elaborator
  val m2 = new OptModule(false)
  val d2 = elaborator2.elaborate(m2)
  println("=" * 50)
  println("Optional Map Check (without a):")
  println(elaborator.emitAll(d2))
  println("=" * 50)

def module_array_generation_check(): Unit =
  final case class FanIO(in: UInt, out: UInt) extends Bundle[FanIO]
  class Leaf(offset: Int) extends Module:
    given Module = this
    val io = IO(FanIO(Input(UInt(Width(4))), Output(UInt(Width(4)))))
    io.out := io.in + Lit(UInt(Width(4)))(offset)
  class Fanout(count: Int) extends Module:
    given Module = this
    val io = IO(FanIO(Input(UInt(Width(4))), Output(UInt(Width(4)))))
    val leaves = Seq.tabulate(count)(i => Module(new Leaf(i)))
    leaves.foreach(_.io.in := io.in)
    io.out := leaves.map(_.io.out).reduce(_ + _)
  val elaborator = new Elaborator
  val top = new Fanout(3)
  val designs = elaborator.elaborate(top)
  println("=" * 50)
  println("Module Array Generation Check:")
  println(elaborator.emitAll(designs))
  println("=" * 50)

def parameter_sweep_check(): Unit =
  final case class PassthroughIO[T <: HWData](in: T, out: T) extends Bundle[PassthroughIO[T]]
  class Passthrough[T <: HWData](gen: => T) extends Module:
    given Module = this
    val io = IO(PassthroughIO(Input(gen), Output(gen)))
    io.out := io.in
  val elaborator = new Elaborator
  val mods = Seq(
    new Passthrough(UInt(Width(8))),
    new Passthrough(UInt(Width(12))),
    new Passthrough(Bool())
  )
  val designs = mods.flatMap(m => elaborator.elaborate(m))
  println("=" * 50)
  println("Parameter Sweep Check:")
  println(elaborator.emitAll(designs))
  println("=" * 50)

def vec_check(): Unit =
  case class Entry(a: UInt, b: UInt) extends Bundle[Entry]
  case class TableIO(entries: Vec[Vec[Entry]]) extends Bundle[TableIO]
  case class ModuleIO(in: TableIO, reduce_idx: UInt, out: UInt) extends Bundle[ModuleIO]

  case class VectorParams(w_in: Int, rows: Int, cols: Int)

  object ModuleIO:
    def apply(p: VectorParams): ModuleIO =
      val log2_rows = p.rows
      ModuleIO(
        in = Input(TableIO(
          entries = Vec(Seq.fill(p.rows)(Vec(Seq.fill(p.cols)(Entry(
            a = UInt(Width(p.w_in)),
            b = UInt(Width(p.w_in))
            )))))
          )),
        reduce_idx = Input(UInt(Width(log2_rows))),
        out = Output(UInt(Width(p.w_in))))

  class VectorTest(p: VectorParams) extends Module:
    given Module = this
    val io = IO(ModuleIO(p))
// io.out := io.in.entries(io.reduce_idx).reduce(_ + _)

  val elaborator = new Elaborator
  val m = new VectorTest(VectorParams(2, 3, 4))
  val d = elaborator.elaborate(m)
  println("=" * 50)
  println("Vec Check:")
  println(elaborator.emitAll(d))
  println("=" * 50)


@main def demo(): Unit =
  simple_module_test()
  list_operation_check()
  nested_module_check()
  nested_bundle_check()
  inheritance_check()
  type_parameterization_check()
  conditional_generation_check()
  optional_io_check()
  nested_seq_generation_check()
  optional_and_map_check()
  module_array_generation_check()
  parameter_sweep_check()
  when_behavior_check()
  comparison_operator_check()
  vec_check()
