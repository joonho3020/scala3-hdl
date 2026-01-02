package hdl

import hdl.core._
import hdl.util._
import hdl.elaboration._

import utest.*
import scala.compiletime.testing.*
import scala.reflect.ClassTag

final case class SimpleIO(in: UInt, out: UInt) extends Bundle[SimpleIO]

private object TestHelpers:
  def id(name: String): IR.Identifier = IR.Identifier(name)
  def portIn(name: String, tpe: IR.Type): IR.Port = IR.Port(id(name), Direction.In, tpe)
  def portOut(name: String, tpe: IR.Type): IR.Port = IR.Port(id(name), Direction.Out, tpe)
  val clockPort: IR.Port = portIn("clock", IR.ClockType)
  val resetPort: IR.Port = portIn("reset", IR.ResetType)
  def u(width: Int): IR.Type = IR.UIntType(Width(width))
  def s(width: Int): IR.Type = IR.SIntType(Width(width))
  def bundle(fields: (String, Boolean, IR.Type)*): IR.Type =
    IR.BundleType(fields.map { case (n, f, t) => IR.BundleField(id(n), f, t) })
  def module(name: String, ports: Seq[IR.Port], body: Seq[IR.Stmt]): IR.Module =
    IR.Module(id(name), ports, body)
  def wire(name: String, tpe: IR.Type): IR.Stmt = IR.Wire(id(name), tpe)
  def reg(name: String, tpe: IR.Type, clock: IR.Expr): IR.Stmt = IR.Reg(id(name), tpe, clock)
  def regReset(name: String, tpe: IR.Type, clock: IR.Expr, reset: IR.Expr, init: IR.Expr): IR.Stmt =
    IR.RegReset(id(name), tpe, clock, reset, init)
  def ref(name: String): IR.Expr = IR.Ref(id(name))
  def inst(name: String, module: String): IR.Stmt = IR.Inst(id(name), id(module))
  def sf(e: IR.Expr, field: String): IR.Expr = e match
    case IR.Ref(name) => IR.Ref(id(s"${name.value}.$field"))
    case other => IR.SubField(other, id(field))
  def si(e: IR.Expr, idx: Int): IR.Expr = IR.SubIndex(e, idx)
  def sa(e: IR.Expr, idx: IR.Expr): IR.Expr = IR.SubAccess(e, idx)
  def lit(value: String): IR.Expr = IR.Literal(id(value))
  def addPrim(a: IR.Expr, b: IR.Expr): IR.Expr = IR.DoPrim(IR.PrimOp.Add, Seq(a, b))
  def add(a: IR.Expr, b: IR.Expr): IR.Expr = tailExpr(addPrim(a, b), 1)
  def and(a: IR.Expr, b: IR.Expr): IR.Expr = IR.DoPrim(IR.PrimOp.And, Seq(a, b))
  def or(a: IR.Expr, b: IR.Expr): IR.Expr = IR.DoPrim(IR.PrimOp.Or, Seq(a, b))
  def eqv(a: IR.Expr, b: IR.Expr): IR.Expr = IR.DoPrim(IR.PrimOp.Eq, Seq(a, b))
  def neqv(a: IR.Expr, b: IR.Expr): IR.Expr = IR.DoPrim(IR.PrimOp.Neq, Seq(a, b))
  def not(e: IR.Expr): IR.Expr = IR.DoPrim(IR.PrimOp.Not, Seq(e))
  def bits(a: IR.Expr, hi: Int, lo: Int): IR.Expr = IR.DoPrim(IR.PrimOp.Bits, Seq(a), Seq(hi, lo))
  def asBoolPrim(e: IR.Expr): IR.Expr = IR.DoPrim(IR.PrimOp.AsBool, Seq(e))
  def tailExpr(a: IR.Expr, n: Int): IR.Expr = IR.DoPrim(IR.PrimOp.Tail, Seq(a), Seq(n))
  def dshr(a: IR.Expr, b: IR.Expr): IR.Expr = IR.DoPrim(IR.PrimOp.DShr, Seq(a, b))
  def reverseBits(e: IR.Expr): IR.Expr = IR.DoPrim(IR.PrimOp.Not, Seq(e))
  def rem(a: IR.Expr, b: IR.Expr): IR.Expr = IR.DoPrim(IR.PrimOp.Rem, Seq(a, b))
  def cat(a: IR.Expr, b: IR.Expr): IR.Expr = IR.DoPrim(IR.PrimOp.Cat, Seq(a, b))
  def asUIntPrim(e: IR.Expr): IR.Expr = IR.DoPrim(IR.PrimOp.AsUInt, Seq(e))
  def mem(name: String, tpe: IR.Type, depth: Int, readers: Seq[String], writers: Seq[String], readwriters: Seq[String]): IR.Stmt =
    IR.Mem(
      id(name),
      tpe,
      depth,
      1,
      1,
      readers.map(id),
      writers.map(id),
      readwriters.map(id),
      IR.ReadUnderWrite.Undefined
    )
  def assertDesigns(label: String, actual: Seq[ElaboratedDesign], expected: Seq[IR.Module]): Unit =
    val renderer = new Elaborator
    val actualRendered = renderer.emitAll(actual)
    val expectedRendered = renderer.emitAll(expected.map(m => ElaboratedDesign(m.name, m)))
    assert(actualRendered == expectedRendered)

import TestHelpers.*

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
  val expected = Seq(
    module(
      "A",
      Seq(
        clockPort,
        resetPort,
        portOut("io", bundle(
          ("in", true, u(4)),
          ("out", false, u(4))
        ))
      ),
      Seq(
        IR.Connect(
          sf(ref("io"), "out"),
          sf(ref("io"), "in")
        )
      )
    )
  )
  println("=" * 50)
  println("Simple Module Test:")
  println(elaborator.emitAll(designs))
  assertDesigns("Simple Module Test", designs, expected)

def regnext_check(): Unit =
  class RegNextModule extends Module:
    given Module = this
    val io = IO(SimpleIO(Input(UInt(Width(4))), Output(UInt(Width(4)))))
    val pc_1 = RegNext(io.in)
    io.out := pc_1

  val elaborator = new Elaborator
  val m = new RegNextModule
  val designs = elaborator.elaborate(m)
  val rendered = elaborator.emitAll(designs)
  val expected = Seq(
    module(
      "RegNextModule",
      Seq(
        clockPort,
        resetPort,
        portOut("io", bundle(
          ("in", true, u(4)),
          ("out", false, u(4))
        ))
      ),
      Seq(
        reg("pc_1", u(4), ref("clock")),
        IR.Connect(ref("pc_1"), sf(ref("io"), "in")),
        IR.Connect(sf(ref("io"), "out"), ref("pc_1"))
      )
    )
  )
  println("=" * 50)
  println("RegNext Check:")
  println(rendered)
  assertDesigns("RegNext Check", designs, expected)
  println("=" * 50)

def dontcare_check(): Unit =
  class DontCareModule extends Module:
    given Module = this
    val io = IO(SimpleIO(Input(UInt(Width(4))), Output(UInt(Width(4)))))
    io.out := DontCare

  val elaborator = new Elaborator
  val m = new DontCareModule
  val designs = elaborator.elaborate(m)
  val rendered = elaborator.emitAll(designs)
  val expected = Seq(
    module(
      "DontCareModule",
      Seq(
        clockPort,
        resetPort,
        portOut("io", bundle(
          ("in", true, u(4)),
          ("out", false, u(4))
        ))
      ),
      Seq(
        IR.Invalid(sf(ref("io"), "out"))
      )
    )
  )
  println("=" * 50)
  println("DontCare Check:")
  println(rendered)
  assertDesigns("DontCare Check", designs, expected)
  println("=" * 50)

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
  val expected = Seq(
    module(
      "MultBySum",
      Seq(
        clockPort,
        resetPort,
        portOut("io", bundle(
          ("a", true, u(4)),
          ("b", true, u(4)),
          ("sum", false, IR.VecType(3, u(4)))
        ))
      ),
      Seq(
        wire("wires", u(4)),
        wire("wires_0", u(4)),
        wire("wires_1", u(4)),
        IR.Connect(ref("wires"), sf(ref("io"), "a")),
        IR.Connect(ref("wires_0"), sf(ref("io"), "a")),
        IR.Connect(ref("wires_1"), sf(ref("io"), "a")),
        IR.Connect(ref("wires"), add(ref("wires_0"), lit("UInt<4>(3)"))),
        IR.Connect(
          si(sf(ref("io"), "sum"), 0),
          add(add(ref("wires"), ref("wires_0")), ref("wires_1"))
        ),
        IR.Connect(
          si(sf(ref("io"), "sum"), 1),
          add(add(ref("wires"), ref("wires_0")), ref("wires_1"))
        )
      )
    )
  )
  println("=" * 50)
  println("List Operation Check:")
  println(rendered)
  assertDesigns("List Operation Check", designs, expected)
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
  val expected = Seq(
    module(
      "A",
      Seq(
        clockPort,
        resetPort,
        portOut("io", bundle(
          ("in", true, u(2)),
          ("out", false, u(2))
        ))
      ),
      Seq(
        IR.Connect(sf(ref("io"), "out"), sf(ref("io"), "in"))
      )
    ),
    module(
      "A_2",
      Seq(
        clockPort,
        resetPort,
        portOut("io", bundle(
          ("in", true, u(3)),
          ("out", false, u(3))
        ))
      ),
      Seq(
        IR.Connect(sf(ref("io"), "out"), sf(ref("io"), "in"))
      )
    ),
    module(
      "C",
      Seq(
        clockPort,
        resetPort,
        portOut("io", bundle(
          ("in", true, u(5)),
          ("out", false, u(5))
        ))
      ),
      Seq(
        IR.Connect(sf(ref("io"), "out"), sf(ref("io"), "in"))
      )
    ),
    module(
      "B",
      Seq(
        clockPort,
        resetPort,
        portOut("io", bundle(
          ("in", true, u(4)),
          ("out", false, u(4))
        ))
      ),
      Seq(
        inst("c", "C"),
        IR.Connect(sf(sf(ref("c"), "io"), "in"), sf(ref("io"), "in")),
        IR.Connect(sf(ref("io"), "out"), sf(sf(ref("c"), "io"), "out"))
      )
    ),
    module(
      "Top",
      Seq(
        clockPort,
        resetPort,
        portOut("io", bundle(
          ("in", true, u(7)),
          ("out", false, u(7))
        ))
      ),
      Seq(
        inst("a0", "A"),
        inst("a1", "A_2"),
        inst("b", "B"),
        IR.Connect(sf(sf(ref("a0"), "io"), "in"), sf(ref("io"), "in")),
        IR.Connect(sf(sf(ref("a1"), "io"), "in"), sf(ref("io"), "in")),
        IR.Connect(sf(sf(ref("b"), "io"), "in"), sf(ref("io"), "in")),
        IR.Connect(
          sf(ref("io"), "out"),
          add(add(sf(sf(ref("a0"), "io"), "out"), sf(sf(ref("a1"), "io"), "out")), sf(sf(ref("b"), "io"), "out"))
        )
      )
    )
  )
  println("=" * 50)
  println("Nested Module Check:")
  println(elaborator.emitAll(designs))
  assertDesigns("Nested Module Check", designs, expected)
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
  val expected = Seq(
    module(
      "NestedBundleModule",
      Seq(
        clockPort,
        resetPort,
        portOut("io", bundle(
          ("z", true, bundle(
            ("x", false, u(1)),
            ("y", false, u(2))
          )),
          ("w", false, bundle(
            ("x", false, u(1)),
            ("y", false, u(2))
          ))
        ))
      ),
      Seq(
        reg("reg_1", bundle(
          ("x", false, u(1)),
          ("y", false, u(2))
        ), ref("clock")),
        IR.Connect(sf(ref("io"), "w"), ref("reg_1"))
      )
    )
  )
  println("=" * 50)
  println("Nested Bundle Check:")
  println(elaborator.emitAll(designs))
  assertDesigns("Nested Bundle Check", designs, expected)
  println("=" * 50)

def inheritance_check(): Unit =
  class Abstract extends Module:
    given Module = this
    val io = IO(
      SimpleIO(Input(UInt(Width(4))), Output(UInt(Width(4))))
    )
    // Common logic that should be shared for child classes of the base class
    // cann't be in the 'body' block. Hence, it is always eagerly elaborated
    io.out := io.in

  class Concrete extends Abstract:
    val add_result = io.in + io.in + io.in
    body:
      io.out := add_result

  val elaborator = new Elaborator
  val concrete = new Concrete
  val designs = elaborator.elaborate(concrete)
  val expected = Seq(
    module(
      "Concrete",
      Seq(
        clockPort,
        resetPort,
        portOut("io", bundle(
          ("in", true, u(4)),
          ("out", false, u(4))
        ))
      ),
      Seq(
        IR.Connect(sf(ref("io"), "out"), sf(ref("io"), "in")),
        IR.DefNode(id("add_result"), addPrim(sf(ref("io"), "in"), sf(ref("io"), "in"))),
        IR.DefNode(id("add_result_0"), tailExpr(ref("add_result"), 1)),
        IR.DefNode(id("add_result_1"), addPrim(ref("add_result_0"), sf(ref("io"), "in"))),
        IR.DefNode(id("add_result_2"), tailExpr(ref("add_result_1"), 1)),
        IR.Connect(sf(ref("io"), "out"), ref("add_result_2"))
      )
    )
  )
  println("=" * 50)
  println("Inheritance Check:")
  println(elaborator.emitAll(designs))
  assertDesigns("Inheritance Check", designs, expected)
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
  val rendered = elaborator.emitAll(design)
  val expectedUInt = Seq(
    module(
      "TypeParamModule",
      Seq(
        clockPort,
        resetPort,
        portOut("io", bundle(
          ("in", true, u(3)),
          ("out", true, u(3))
        ))
      ),
      Seq(
        IR.Connect(sf(ref("io"), "out"), sf(ref("io"), "in"))
      )
    )
  )
  println("=" * 50)
  println("Type Parameterization Check: UInt(Width(3))")
  println(rendered)
  assertDesigns("Type Parameterization Check UInt", design, expectedUInt)

  val elaborator2 = new Elaborator
  val tp2 = new TypeParamModule(t = Bool())
  val design2 = elaborator2.elaborate(tp2)
  val rendered2 = elaborator2.emitAll(design2)
  val expectedBool = Seq(
    module(
      "TypeParamModule",
      Seq(
        clockPort,
        resetPort,
        portOut("io", bundle(
          ("in", true, IR.BoolType),
          ("out", true, IR.BoolType)
        ))
      ),
      Seq(
        IR.Connect(sf(ref("io"), "out"), sf(ref("io"), "in"))
      )
    )
  )
  println("Type Parameterization Check: Bool")
  println(rendered2)
  assertDesigns("Type Parameterization Check Bool", design2, expectedBool)
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
  val rendered = elaborator.emitAll(design)
  val expectedTrue = Seq(
    module(
      "A",
      Seq(
        clockPort,
        resetPort,
        portOut("io", bundle(
          ("in", true, u(4)),
          ("out", false, u(4))
        ))
      ),
      Seq(
        IR.Connect(sf(ref("io"), "out"), add(sf(ref("io"), "in"), sf(ref("io"), "in")))
      )
    )
  )

  println("=" * 50)
  println("Conditional generational check: add = true")
  println(rendered)
  assertDesigns("Conditional Generation add=true", design, expectedTrue)

  val elaborator2 = new Elaborator
  val add_false = new A(add = false)
  val design_false = elaborator2.elaborate(add_false)
  val renderedFalse = elaborator2.emitAll(design_false)
  val expectedFalse = Seq(
    module(
      "A",
      Seq(
        clockPort,
        resetPort,
        portOut("io", bundle(
          ("in", true, u(4)),
          ("out", false, u(4))
        ))
      ),
      Seq(
        IR.Connect(sf(ref("io"), "out"), sf(ref("io"), "in"))
      )
    )
  )

  println("Conditional generational check: add = false")
  println(renderedFalse)
  assertDesigns("Conditional Generation add=false", design_false, expectedFalse)
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
  val rendered = elaborator.emitAll(d1)
  val expectedWhen = Seq(
    module(
      "WhenModule",
      Seq(
        clockPort,
        resetPort,
        portOut("io", bundle(
          ("sel", true, IR.BoolType),
          ("sel2", true, IR.BoolType),
          ("sel3", true, IR.BoolType),
          ("in", true, u(4)),
          ("alt", true, u(4)),
          ("out", false, u(4))
        ))
      ),
      Seq(
        IR.When(
          sf(ref("io"), "sel"),
          Seq(IR.Connect(sf(ref("io"), "out"), sf(ref("io"), "in"))),
          Seq(IR.Connect(sf(ref("io"), "out"), sf(ref("io"), "alt")))
        )
      )
    )
  )
  println("=" * 50)
  println("When/Otherwise Check:")
  println(rendered)
  assertDesigns("When/Otherwise Check", d1, expectedWhen)

  val elaborator2 = new Elaborator
  val m2 = new WhenElseModule
  val d2 = elaborator2.elaborate(m2)
  val rendered2 = elaborator2.emitAll(d2)
  val expectedElse = Seq(
    module(
      "WhenElseModule",
      Seq(
        clockPort,
        resetPort,
        portOut("io", bundle(
          ("sel", true, IR.BoolType),
          ("sel2", true, IR.BoolType),
          ("sel3", true, IR.BoolType),
          ("in", true, u(4)),
          ("alt", true, u(4)),
          ("out", false, u(4))
        ))
      ),
      Seq(
        IR.When(
          sf(ref("io"), "sel"),
          Seq(IR.Connect(sf(ref("io"), "out"), sf(ref("io"), "in"))),
          Seq(
            IR.When(
              sf(ref("io"), "sel2"),
              Seq(
                IR.When(
                  sf(ref("io"), "sel3"),
                  Seq(IR.Connect(sf(ref("io"), "out"), sf(ref("io"), "alt"))),
                  Seq(IR.Connect(sf(ref("io"), "out"), sf(ref("io"), "in")))
                )
              ),
              Seq(
                IR.Connect(sf(ref("io"), "out"), add(sf(ref("io"), "in"), sf(ref("io"), "alt")))
              )
            )
          )
        )
      )
    )
  )
  println("When/Elsewhen/Otherwise Check:")
  println(rendered2)
  assertDesigns("When/Elsewhen/Otherwise Check", d2, expectedElse)
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
  val rendered = elaborator.emitAll(d1)
  val expectedUInt = Seq(
    module(
      "Cmp",
      Seq(
        clockPort,
        resetPort,
        portOut("io", bundle(
          ("in", true, u(4)),
          ("thresh", true, u(4)),
          ("flag", false, IR.BoolType)
        ))
      ),
      Seq(
        IR.Connect(sf(ref("io"), "flag"), eqv(sf(ref("io"), "in"), sf(ref("io"), "thresh")))
      )
    )
  )
  println("=" * 50)
  println("Comparison Operator Check (UInt):")
  println(rendered)
  assertDesigns("Comparison Operator Check UInt", d1, expectedUInt)

  val elaborator2 = new Elaborator
  val m2 = new CmpBool
  val d2 = elaborator2.elaborate(m2)
  val rendered2 = elaborator2.emitAll(d2)
  val expectedBool = Seq(
    module(
      "CmpBool",
      Seq(
        clockPort,
        resetPort,
        portOut("io", bundle(
          ("a", true, IR.BoolType),
          ("b", true, IR.BoolType),
          ("out", false, IR.BoolType)
        ))
      ),
      Seq(
        IR.Connect(sf(ref("io"), "out"), neqv(sf(ref("io"), "a"), sf(ref("io"), "b")))
      )
    )
  )
  println("Comparison Operator Check (Bool):")
  println(rendered2)
  assertDesigns("Comparison Operator Check Bool", d2, expectedBool)
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
   val rendered = elaborator.emitAll(design)
   val expectedTrue = Seq(
     module(
       "A",
       Seq(
         clockPort,
         resetPort,
         portOut("io", bundle(
           ("a", true, u(2)),
           ("b", true, u(2)),
           ("c", false, u(3))
         ))
       ),
       Seq(
         IR.Connect(sf(ref("io"), "c"), sf(ref("io"), "b")),
         IR.Connect(sf(ref("io"), "c"), add(sf(ref("io"), "a"), sf(ref("io"), "b")))
       )
     )
   )
   println("=" * 50)
   println("Optional IO Check (debug = true):")
   println(rendered)
   assertDesigns("Optional IO Check debug=true", design, expectedTrue)

   val elaborator2 = new Elaborator
   val add_false = new A(debug = false, w = 2)
   val design_2 = elaborator2.elaborate(add_false)
   val rendered2 = elaborator2.emitAll(design_2)
   val expectedFalse = Seq(
     module(
       "A",
       Seq(
         clockPort,
         resetPort,
         portOut("io", bundle(
           ("b", true, u(2)),
           ("c", false, u(3))
         ))
       ),
       Seq(
         IR.Connect(sf(ref("io"), "c"), sf(ref("io"), "b"))
       )
     )
   )
   println("Optional IO Check (debug = false):")
   println(rendered2)
   assertDesigns("Optional IO Check debug=false", design_2, expectedFalse)
   println("=" * 50)

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
  val rendered = elaborator.emitAll(designs)
  val expected = Seq(
    module(
      "MatrixAcc",
      Seq(
        clockPort,
        resetPort,
        portOut("io", bundle(
          ("in", true, u(8)),
          ("out", false, u(16))
        ))
      ),
      Seq(
        wire("mats", u(8)),
        wire("mats_0", u(8)),
        wire("mats_1", u(8)),
        wire("mats_2", u(8)),
        wire("mats_3", u(8)),
        wire("mats_4", u(8)),
        IR.Connect(ref("mats"), add(sf(ref("io"), "in"), lit("UInt<8>(0)"))),
        IR.Connect(ref("mats_0"), add(sf(ref("io"), "in"), lit("UInt<8>(1)"))),
        IR.Connect(ref("mats_1"), add(sf(ref("io"), "in"), lit("UInt<8>(2)"))),
        IR.Connect(ref("mats_2"), add(sf(ref("io"), "in"), lit("UInt<8>(3)"))),
        IR.Connect(ref("mats_3"), add(sf(ref("io"), "in"), lit("UInt<8>(4)"))),
        IR.Connect(ref("mats_4"), add(sf(ref("io"), "in"), lit("UInt<8>(5)"))),
        IR.DefNode(id("rowSums"), addPrim(ref("mats"), ref("mats_0"))),
        IR.DefNode(id("rowSums_0"), tailExpr(ref("rowSums"), 1)),
        IR.DefNode(id("rowSums_1"), addPrim(ref("rowSums_0"), ref("mats_1"))),
        IR.DefNode(id("rowSums_2"), tailExpr(ref("rowSums_1"), 1)),
        IR.DefNode(id("rowSums_3"), addPrim(ref("mats_2"), ref("mats_3"))),
        IR.DefNode(id("rowSums_4"), tailExpr(ref("rowSums_3"), 1)),
        IR.DefNode(id("rowSums_5"), addPrim(ref("rowSums_4"), ref("mats_4"))),
        IR.DefNode(id("rowSums_6"), tailExpr(ref("rowSums_5"), 1)),
        IR.Connect(sf(ref("io"), "out"), add(ref("rowSums_2"), ref("rowSums_6")))
      )
    )
  )
  println("=" * 50)
  println("Nested Seq Generation Check:")
  println(rendered)
  assertDesigns("Nested Seq Generation Check", designs, expected)
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
  val rendered = elaborator.emitAll(d1)
  val expectedTrue = Seq(
    module(
      "OptModule",
      Seq(
        clockPort,
        resetPort,
        portOut("io", bundle(
          ("a", true, u(4)),
          ("b", true, u(4)),
          ("out", false, u(5))
        ))
      ),
      Seq(
        IR.Connect(sf(ref("io"), "out"), sf(ref("io"), "b")),
        IR.Connect(sf(ref("io"), "out"), add(sf(ref("io"), "a"), sf(ref("io"), "b")))
      )
    )
  )
  println("=" * 50)
  println("Optional Map Check (with a):")
  println(rendered)
  assertDesigns("Optional Map Check with a", d1, expectedTrue)
  println("=" * 50)
  val elaborator2 = new Elaborator
  val m2 = new OptModule(false)
  val d2 = elaborator2.elaborate(m2)
  val rendered2 = elaborator2.emitAll(d2)
  val expectedFalse = Seq(
    module(
      "OptModule",
      Seq(
        clockPort,
        resetPort,
        portOut("io", bundle(
          ("b", true, u(4)),
          ("out", false, u(5))
        ))
      ),
      Seq(
        IR.Connect(sf(ref("io"), "out"), sf(ref("io"), "b"))
      )
    )
  )
  println("=" * 50)
  println("Optional Map Check (without a):")
  println(rendered2)
  assertDesigns("Optional Map Check without a", d2, expectedFalse)
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
  val rendered = elaborator.emitAll(designs)
  val expected = Seq(
    module(
      "Leaf",
      Seq(
        clockPort,
        resetPort,
        portOut("io", bundle(
          ("in", true, u(4)),
          ("out", false, u(4))
        ))
      ),
      Seq(
        IR.Connect(sf(ref("io"), "out"), add(sf(ref("io"), "in"), lit("UInt<4>(0)")))
      )
    ),
    module(
      "Leaf_2",
      Seq(
        clockPort,
        resetPort,
        portOut("io", bundle(
          ("in", true, u(4)),
          ("out", false, u(4))
        ))
      ),
      Seq(
        IR.Connect(sf(ref("io"), "out"), add(sf(ref("io"), "in"), lit("UInt<4>(1)")))
      )
    ),
    module(
      "Leaf_3",
      Seq(
        clockPort,
        resetPort,
        portOut("io", bundle(
          ("in", true, u(4)),
          ("out", false, u(4))
        ))
      ),
      Seq(
        IR.Connect(sf(ref("io"), "out"), add(sf(ref("io"), "in"), lit("UInt<4>(2)")))
      )
    ),
    module(
      "Fanout",
      Seq(
        clockPort,
        resetPort,
        portOut("io", bundle(
          ("in", true, u(4)),
          ("out", false, u(4))
        ))
      ),
      Seq(
        inst("leaves", "Leaf"),
        inst("leaves_0", "Leaf_2"),
        inst("leaves_1", "Leaf_3"),
        IR.Connect(sf(sf(ref("leaves"), "io"), "in"), sf(ref("io"), "in")),
        IR.Connect(sf(sf(ref("leaves_0"), "io"), "in"), sf(ref("io"), "in")),
        IR.Connect(sf(sf(ref("leaves_1"), "io"), "in"), sf(ref("io"), "in")),
        IR.Connect(
          sf(ref("io"), "out"),
          add(add(sf(sf(ref("leaves"), "io"), "out"), sf(sf(ref("leaves_0"), "io"), "out")), sf(sf(ref("leaves_1"), "io"), "out"))
        )
      )
    )
  )
  println("=" * 50)
  println("Module Array Generation Check:")
  println(rendered)
  assertDesigns("Module Array Generation Check", designs, expected)
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
  val rendered = elaborator.emitAll(designs)
  val expected = Seq(
    module(
      "Passthrough",
      Seq(
        clockPort,
        resetPort,
        portOut("io", bundle(
          ("in", true, u(8)),
          ("out", false, u(8))
        ))
      ),
      Seq(
        IR.Connect(sf(ref("io"), "out"), sf(ref("io"), "in"))
      )
    ),
    module(
      "Passthrough_2",
      Seq(
        clockPort,
        resetPort,
        portOut("io", bundle(
          ("in", true, u(12)),
          ("out", false, u(12))
        ))
      ),
      Seq(
        IR.Connect(sf(ref("io"), "out"), sf(ref("io"), "in"))
      )
    ),
    module(
      "Passthrough_3",
      Seq(
        clockPort,
        resetPort,
        portOut("io", bundle(
          ("in", true, IR.BoolType),
          ("out", false, IR.BoolType)
        ))
      ),
      Seq(
        IR.Connect(sf(ref("io"), "out"), sf(ref("io"), "in"))
      )
    )
  )
  println("=" * 50)
  println("Parameter Sweep Check:")
  println(rendered)
  assertDesigns("Parameter Sweep Check", designs, expected)
  println("=" * 50)

def vec_check(): Unit =
  case class Entry(a: UInt, b: UInt) extends Bundle[Entry]
  case class TableIO(entries: Vec[Vec[Entry]]) extends Bundle[TableIO]
  case class ModuleIO(in: TableIO, reduce_idx: UInt, out: UInt) extends Bundle[ModuleIO]

  case class VectorParams(w_in: Int, rows: Int, cols: Int)

  object ModuleIO:
    def apply(p: VectorParams): ModuleIO =
      val log2_rows = log2Ceil(p.rows)
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

    body:
      io.out := io.in.entries(io.reduce_idx).elems.map(e => e.a + e.b).reduce(_ + _)

  val m = new VectorTest(VectorParams(2, 3, 4))

  val elaborator = new Elaborator
  val d = elaborator.elaborate(m)
  val rendered = elaborator.emitAll(d)
  val baseEntries = sf(sf(ref("io"), "in"), "entries")
  val rowSel = sa(baseEntries, sf(ref("io"), "reduce_idx"))
  val sum0 = add(sf(si(rowSel, 0), "a"), sf(si(rowSel, 0), "b"))
  val sum1 = add(sf(si(rowSel, 1), "a"), sf(si(rowSel, 1), "b"))
  val sum2 = add(sf(si(rowSel, 2), "a"), sf(si(rowSel, 2), "b"))
  val sum3 = add(sf(si(rowSel, 3), "a"), sf(si(rowSel, 3), "b"))
  val expected = Seq(
    module(
      "VectorTest",
      Seq(
        clockPort,
        resetPort,
        portOut("io", bundle(
          ("in", true, bundle(
            ("entries", false, IR.VecType(3, IR.VecType(4, bundle(
              ("a", false, u(2)),
              ("b", false, u(2))
            ))))
          )),
          ("reduce_idx", true, u(2)),
          ("out", false, u(2))
        ))
      ),
      Seq(
        IR.Connect(
          sf(ref("io"), "out"),
          add(add(add(sum0, sum1), sum2), sum3)
        )
      )
    )
  )
  println("=" * 50)
  println("Vec Check:")
  println(rendered)
  assertDesigns("Vec Check", d, expected)
  println("=" * 50)

def bit_select_check(): Unit =
  final case class BitSelectIO(in: UInt, idx: UInt, constSel: UInt, dynSel: UInt) extends Bundle[BitSelectIO]
  class BitSelect extends Module:
    given Module = this
    val io = IO(BitSelectIO(
      in = Input(UInt(Width(4))),
      idx = Input(UInt(Width(2))),
      constSel = Output(UInt(Width(1))),
      dynSel = Output(UInt(Width(1)))
    ))
    io.constSel := io.in(2)
    io.dynSel := io.in(io.idx)

  val elaborator = new Elaborator
  val m = new BitSelect
  val d = elaborator.elaborate(m)
  val rendered = elaborator.emitAll(d)
  val expected = Seq(
    module(
      "BitSelect",
      Seq(
        clockPort,
        resetPort,
        portOut("io", bundle(
          ("in", true, u(4)),
          ("idx", true, u(2)),
          ("constSel", false, u(1)),
          ("dynSel", false, u(1))
        ))
      ),
      Seq(
        IR.Connect(
          sf(ref("io"), "constSel"),
          bits(sf(ref("io"), "in"), 2, 2)
        ),
        IR.Connect(
          sf(ref("io"), "dynSel"),
          bits(dshr(sf(ref("io"), "in"), sf(ref("io"), "idx")), 0, 0)
        )
      )
    )
  )
  println("=" * 50)
  println("Bit Select Check:")
  println(rendered)
  assertDesigns("Bit Select Check", d, expected)
  println("=" * 50)

def bitwise_reverse_check(): Unit =
  final case class ReverseIO(in: UInt, out: UInt) extends Bundle[ReverseIO]
  class ReverseModule extends Module:
    given Module = this
    val io = IO(ReverseIO(
      in = Input(UInt(Width(4))),
      out = Output(UInt(Width(4)))
    ))
    io.out := ~io.in

  val elaborator = new Elaborator
  val m = new ReverseModule
  val d = elaborator.elaborate(m)
  val rendered = elaborator.emitAll(d)
  val expected = Seq(
    module(
      "ReverseModule",
      Seq(
        clockPort,
        resetPort,
        portOut("io", bundle(
          ("in", true, u(4)),
          ("out", false, u(4))
        ))
      ),
      Seq(
        IR.Connect(
          sf(ref("io"), "out"),
          reverseBits(sf(ref("io"), "in"))
        )
      )
    )
  )
  println("=" * 50)
  println("Bitwise Reverse Check:")
  println(rendered)
  assertDesigns("Bitwise Reverse Check", d, expected)
  println("=" * 50)

def bitwise_reduce_check(): Unit =
  final case class ReduceIO(in: UInt, orOut: Bool, andOut: Bool) extends Bundle[ReduceIO]
  class ReduceModule extends Module:
    given Module = this
    val io = IO(ReduceIO(
      in = Input(UInt(Width(4))),
      orOut = Output(Bool()),
      andOut = Output(Bool())
    ))
    io.orOut := io.in.orR
    io.andOut := io.in.andR

  val elaborator = new Elaborator
  val m = new ReduceModule
  val d = elaborator.elaborate(m)
  val rendered = elaborator.emitAll(d)
  val inRef = sf(ref("io"), "in")
  val b0 = asBoolPrim(bits(inRef, 0, 0))
  val b1 = asBoolPrim(bits(inRef, 1, 1))
  val b2 = asBoolPrim(bits(inRef, 2, 2))
  val b3 = asBoolPrim(bits(inRef, 3, 3))
  val expected = Seq(
    module(
      "ReduceModule",
      Seq(
        clockPort,
        resetPort,
        portOut("io", bundle(
          ("in", true, u(4)),
          ("orOut", false, IR.BoolType),
          ("andOut", false, IR.BoolType)
        ))
      ),
      Seq(
        IR.Connect(
          sf(ref("io"), "orOut"),
          or(or(or(b0, b1), b2), b3)
        ),
        IR.Connect(
          sf(ref("io"), "andOut"),
          and(and(and(b0, b1), b2), b3)
        )
      )
    )
  )
  println("=" * 50)
  println("Bitwise Reduce Check:")
  println(rendered)
  assertDesigns("Bitwise Reduce Check", d, expected)
  println("=" * 50)

def first_set_indices_check(): Unit =
  ()

def mux_and_concat_check(): Unit =
  final case class MuxConcatIO(a: UInt, b: UInt, sel: Bool, out: UInt, cat: UInt) extends Bundle[MuxConcatIO]
  class MuxConcat extends Module:
    given Module = this
    val io = IO(MuxConcatIO(
      a = Input(UInt(Width(4))),
      b = Input(UInt(Width(4))),
      sel = Input(Bool()),
      out = Output(UInt(Width(4))),
      cat = Output(UInt(Width(8)))
    ))
    io.out := Mux(io.sel, io.a, io.b)
    io.cat := Cat(Seq(io.a, io.b))

    io.out := io.asUInt

  val elaborator = new Elaborator
  val m = new MuxConcat
  val d = elaborator.elaborate(m)
  val rendered = elaborator.emitAll(d)
  val expected = Seq(
    module(
      "MuxConcat",
      Seq(
        clockPort,
        resetPort,
        portOut("io", bundle(
          ("a", true, u(4)),
          ("b", true, u(4)),
          ("sel", true, IR.BoolType),
          ("out", false, u(4)),
          ("cat", false, u(8))
        ))
      ),
      Seq(
        IR.Connect(
          sf(ref("io"), "out"),
          IR.DoPrim(IR.PrimOp.Mux, Seq(sf(ref("io"), "sel"), sf(ref("io"), "a"), sf(ref("io"), "b")))
        ),
        IR.Connect(
          sf(ref("io"), "cat"),
          IR.DoPrim(IR.PrimOp.Cat, Seq(sf(ref("io"), "a"), sf(ref("io"), "b")))
        ),
        IR.Connect(
          sf(ref("io"), "out"),
          IR.DoPrim(IR.PrimOp.AsUInt, Seq(ref("io")))
        ),
      )
    )
  )
  println("=" * 50)
  println("Mux and Concat Check:")
  println(rendered)
  assertDesigns("Mux and Concat Check", d, expected)
  println("=" * 50)

/**
 * Example demonstrating SRAM API usage.
 *
 * SRAMs are explicitly instantiated (not inferred) but provide behavioral
 * read/write APIs. This example shows:
 * - Creating an SRAM with read, write, and read-write ports
 * - Using readPorts for read-only access
 * - Using writePorts for write-only access
 * - Using readwritePorts for combined read/write access
 *
 * @return A Module instance demonstrating SRAM operations
 */
def sramExample(): Module =
  final case class SramIO(rData: UInt, rwData: UInt) extends Bundle[SramIO]
  class SramModule extends Module:
    given Module = this
    val io = IO(SramIO(
      rData = Output(UInt(Width(8))),
      rwData = Output(UInt(Width(8)))
    ))
    val mem = SRAM(UInt(Width(8)), 4)(1, 1, 1)
    val r = mem.readPorts(0).read(1.U(Width(2)))
    io.rData := r
    mem.writePorts(0).write(2.U(Width(2)), 5.U(Width(8)))
    val rw = mem.readwritePorts(0).read(3.U(Width(2)))
    mem.readwritePorts(0).writeData := 0.U(Width(8))
    io.rwData := rw
  new SramModule

def sram_check(): Unit =
  final case class SramIO(rData: UInt, rwData: UInt) extends Bundle[SramIO]
  val elaborator = new Elaborator
  val m = sramExample()
  val d = elaborator.elaborate(m)
  val rendered = elaborator.emitAll(d)
  val expected = Seq(
    module(
      "SramModule",
      Seq(
        clockPort,
        resetPort,
        portOut("io", bundle(
          ("rData", false, u(8)),
          ("rwData", false, u(8))
        ))
      ),
      Seq(
        mem("mem", u(8), 4, Seq("r0"), Seq("w0"), Seq("rw0")),
        wire("mem_r_en", IR.VecType(1, IR.BoolType)),
        wire("mem_r_addr", IR.VecType(1, IR.UIntType(Width(2)))),
        wire("mem_r_data", IR.VecType(1, u(8))),
        wire("mem_w_en", IR.VecType(1, IR.BoolType)),
        wire("mem_w_addr", IR.VecType(1, IR.UIntType(Width(2)))),
        wire("mem_w_data", IR.VecType(1, u(8))),
        wire("mem_rw_en", IR.VecType(1, IR.BoolType)),
        wire("mem_rw_addr", IR.VecType(1, IR.UIntType(Width(2)))),
        wire("mem_rw_wmode", IR.VecType(1, IR.BoolType)),
        wire("mem_rw_wdata", IR.VecType(1, u(8))),
        wire("mem_rw_rdata", IR.VecType(1, u(8))),
        IR.Connect(sf(sf(ref("mem"), "r0"), "en"), si(ref("mem_r_en"), 0)),
        IR.Connect(sf(sf(ref("mem"), "r0"), "addr"), si(ref("mem_r_addr"), 0)),
        IR.Connect(si(ref("mem_r_data"), 0), sf(sf(ref("mem"), "r0"), "data")),
        IR.Connect(sf(sf(ref("mem"), "w0"), "en"), si(ref("mem_w_en"), 0)),
        IR.Connect(sf(sf(ref("mem"), "w0"), "addr"), si(ref("mem_w_addr"), 0)),
        IR.Connect(sf(sf(ref("mem"), "w0"), "data"), si(ref("mem_w_data"), 0)),
        IR.Connect(sf(sf(ref("mem"), "w0"), "mask"), lit("UInt<1>(1)")),
        IR.Connect(sf(sf(ref("mem"), "rw0"), "en"), si(ref("mem_rw_en"), 0)),
        IR.Connect(sf(sf(ref("mem"), "rw0"), "addr"), si(ref("mem_rw_addr"), 0)),
        IR.Connect(sf(sf(ref("mem"), "rw0"), "wmode"), si(ref("mem_rw_wmode"), 0)),
        IR.Connect(sf(sf(ref("mem"), "rw0"), "wdata"), si(ref("mem_rw_wdata"), 0)),
        IR.Connect(si(ref("mem_rw_rdata"), 0), sf(sf(ref("mem"), "rw0"), "rdata")),
        IR.Connect(sf(sf(ref("mem"), "rw0"), "wmask"), lit("UInt<1>(1)")),
        IR.Connect(si(ref("mem_r_en"), 0), lit("Bool(true)")),
        IR.Connect(si(ref("mem_r_addr"), 0), lit("UInt<2>(1)")),
        IR.Connect(sf(ref("io"), "rData"), si(ref("mem_r_data"), 0)),
        IR.Connect(si(ref("mem_w_en"), 0), lit("Bool(true)")),
        IR.Connect(si(ref("mem_w_addr"), 0), lit("UInt<2>(2)")),
        IR.Connect(si(ref("mem_w_data"), 0), lit("UInt<8>(5)")),
        IR.Connect(si(ref("mem_rw_en"), 0), lit("Bool(true)")),
        IR.Connect(si(ref("mem_rw_wmode"), 0), lit("Bool(false)")),
        IR.Connect(si(ref("mem_rw_addr"), 0), lit("UInt<2>(3)")),
        IR.Connect(si(ref("mem_rw_wdata"), 0), lit("UInt<8>(0)")),
        IR.Connect(sf(ref("io"), "rwData"), si(ref("mem_rw_rdata"), 0))
      )
    )
  )
  println("=" * 50)
  println("SRAM Check:")
  println(rendered)
  assertDesigns("SRAM Check", d, expected)
  println("=" * 50)

enum TestEnumOpcode:
  case Idle, Run, Wait

/**
 * Example demonstrating basic hardware enum usage.
 *
 * Shows how to:
 * - Define a Scala enum (TestEnumOpcode)
 * - Use HWEnum[T] to create hardware enum types
 * - Use .EN to get the hardware enum value from a Scala enum case
 * - Compare and assign enum values
 * - Use enums in when() conditional blocks
 *
 * @return A Module instance demonstrating basic enum operations
 */
def enumBasicExample(): Module =
  final case class EnumIO(in: HWEnum[TestEnumOpcode], out: HWEnum[TestEnumOpcode]) extends Bundle[EnumIO]

  class EnumModule extends Module:
    given Module = this
    val io = IO(EnumIO(Input(HWEnum(TestEnumOpcode)), Output(HWEnum(TestEnumOpcode))))
    val reg = RegInit(TestEnumOpcode.Idle.EN)
    when(io.in.asUInt === TestEnumOpcode.Run.EN.asUInt) {
      reg := TestEnumOpcode.Wait.EN
    }
    io.out := reg

  new EnumModule

def enum_basic_check(): Unit =
  final case class EnumIO(in: HWEnum[TestEnumOpcode], out: HWEnum[TestEnumOpcode]) extends Bundle[EnumIO]

  TestEnumOpcode.values.foreach(x => println(x))

  val elaborator = new Elaborator
  val mod = enumBasicExample()
  val designs = elaborator.elaborate(mod)
  val expected = Seq(
    module(
      "EnumModule",
      Seq(
        clockPort,
        resetPort,
        portOut("io", bundle(
          ("in", true, u(2)),
          ("out", false, u(2))
        ))
      ),
      Seq(
        regReset("reg", u(2), ref("clock"), ref("reset"), lit("UInt<2>(0)")),
        IR.When(
          IR.DoPrim(
            IR.PrimOp.Eq,
            Seq(
              IR.DoPrim(IR.PrimOp.AsUInt, Seq(sf(ref("io"), "in"))),
              IR.DoPrim(IR.PrimOp.AsUInt, Seq(lit("UInt<2>(1)")))
            )
          ),
          Seq(IR.Connect(ref("reg"), lit("UInt<2>(2)"))),
          Seq.empty
        ),
        IR.Connect(sf(ref("io"), "out"), ref("reg"))
      )
    )
  )
  val rendered = elaborator.emitAll(designs)
  println(rendered)
  assertDesigns("Enum Check", designs, expected)

/**
 * Example demonstrating switch statement usage with hardware enums.
 *
 * Shows how enums work seamlessly with switch/is statements,
 * similar to Scala's pattern matching. The switch statement provides
 * cleaner syntax compared to nested when/elsewhen blocks for
 * multi-way branches.
 *
 * @return A Module instance demonstrating enum switch statements
 */
def enumSwitchExample(): Module =
  final case class SwitchEnumIO(in: HWEnum[TestEnumOpcode], out: HWEnum[TestEnumOpcode]) extends Bundle[SwitchEnumIO]

  class SwitchEnum extends Module:
    given Module = this
    val io = IO(SwitchEnumIO(Input(HWEnum(TestEnumOpcode)), Output(HWEnum(TestEnumOpcode))))
    val reg = RegInit(TestEnumOpcode.Idle.EN)
    reg switch {
      is(TestEnumOpcode.Idle.EN) { reg := TestEnumOpcode.Run.EN }
      is(TestEnumOpcode.Run.EN) { reg := TestEnumOpcode.Wait.EN }
      default { reg := TestEnumOpcode.Idle.EN }
    }
    io.out := reg

  new SwitchEnum

def switch_check(): Unit =
  final case class SwitchIO(sel: UInt, out: UInt) extends Bundle[SwitchIO]
  final case class SwitchEnumIO(in: HWEnum[TestEnumOpcode], out: HWEnum[TestEnumOpcode]) extends Bundle[SwitchEnumIO]

  class SwitchUInt extends Module:
    given Module = this
    val io = IO(SwitchIO(Input(UInt(Width(2))), Output(UInt(Width(4)))))
    val out = Wire(UInt(Width(4)))
    out := 0.U(Width(4))
    io.out := out
    io.sel switch {
      is(0.U(Width(2))) { out := 1.U(Width(4)) }
      is(1.U(Width(2))) { out := 2.U(Width(4)) }
      default { out := 3.U(Width(4)) }
    }

  val elaborator = new Elaborator
  val modUInt = new SwitchUInt
  val designsUInt = elaborator.elaborate(modUInt)
  val expectedUInt = Seq(
    module(
      "SwitchUInt",
      Seq(
        clockPort,
        resetPort,
        portOut("io", bundle(
          ("sel", true, u(2)),
          ("out", false, u(4))
        ))
      ),
      Seq(
        wire("out", u(4)),
        IR.Connect(ref("out"), lit("UInt<4>(0)")),
        IR.Connect(sf(ref("io"), "out"), ref("out")),
        IR.When(
          eqv(sf(ref("io"), "sel"), lit("UInt<2>(0)")),
          Seq(
            IR.Connect(ref("out"), lit("UInt<4>(1)"))
          ),
          Seq(
            IR.When(
              eqv(sf(ref("io"), "sel"), lit("UInt<2>(1)")),
              Seq(
                IR.Connect(ref("out"), lit("UInt<4>(2)"))
              ),
              Seq(
                IR.Connect(ref("out"), lit("UInt<4>(3)"))
              )
            )
          )
        )
      )
    )
  )
  val renderedUInt = elaborator.emitAll(designsUInt)
  println(renderedUInt)
  assertDesigns("Switch UInt Check", designsUInt, expectedUInt)

  val elaborator2 = new Elaborator
  val modEnum = enumSwitchExample()
  val designsEnum = elaborator2.elaborate(modEnum)
  val expectedEnum = Seq(
    module(
      "SwitchEnum",
      Seq(
        clockPort,
        resetPort,
        portOut("io", bundle(
          ("in", true, u(2)),
          ("out", false, u(2))
        ))
      ),
      Seq(
        regReset("reg", u(2), ref("clock"), ref("reset"), lit("UInt<2>(0)")),
        IR.When(
          eqv(ref("reg"), lit("UInt<2>(0)")),
          Seq(
            IR.Connect(ref("reg"), lit("UInt<2>(1)"))
          ),
          Seq(
            IR.When(
              eqv(ref("reg"), lit("UInt<2>(1)")),
              Seq(
                IR.Connect(ref("reg"), lit("UInt<2>(2)"))
              ),
              Seq(
                IR.Connect(ref("reg"), lit("UInt<2>(0)"))
              )
            )
          )
        ),
        IR.Connect(sf(ref("io"), "out"), ref("reg"))
      )
    )
  )
  val renderedEnum = elaborator2.emitAll(designsEnum)
  println(renderedEnum)
  assertDesigns("Switch Enum Check", designsEnum, expectedEnum)

def fill_check(): Unit =
  final case class FillIO(in: Bool, out4: UInt, out8: UInt) extends Bundle[FillIO]
  class FillModule extends Module:
    given Module = this
    val io = IO(FillIO(
      in = Input(Bool()),
      out4 = Output(UInt(Width(4))),
      out8 = Output(UInt(Width(8)))
    ))
    body:
      io.out4 := Fill(4, io.in)
      io.out8 := Fill(8, io.in)

  val elaborator = new Elaborator
  val m = new FillModule
  val designs = elaborator.elaborate(m)
  val rendered = elaborator.emitAll(designs)
  val inRef = sf(ref("io"), "in")
  val fill4 = cat(cat(cat(asUIntPrim(inRef), asUIntPrim(inRef)), asUIntPrim(inRef)), asUIntPrim(inRef))
  val fill8 = cat(cat(cat(cat(cat(cat(cat(
    asUIntPrim(inRef), asUIntPrim(inRef)), asUIntPrim(inRef)), asUIntPrim(inRef)),
    asUIntPrim(inRef)), asUIntPrim(inRef)), asUIntPrim(inRef)), asUIntPrim(inRef))
  val expected = Seq(
    module(
      "FillModule",
      Seq(
        clockPort,
        resetPort,
        portOut("io", bundle(
          ("in", true, IR.BoolType),
          ("out4", false, u(4)),
          ("out8", false, u(8))
        ))
      ),
      Seq(
        IR.Connect(sf(ref("io"), "out4"), fill4),
        IR.Connect(sf(ref("io"), "out8"), fill8)
      )
    )
  )
  println("=" * 50)
  println("Fill Check:")
  println(rendered)
  assertDesigns("Fill Check", designs, expected)
  println("=" * 50)

def reverse_check(): Unit =
  final case class ReverseIO(in: UInt, out: UInt) extends Bundle[ReverseIO]
  class ReverseModule extends Module:
    given Module = this
    val io = IO(ReverseIO(
      in = Input(UInt(Width(8))),
      out = Output(UInt(Width(8)))
    ))
    body:
      io.out := Reverse(io.in)

  val elaborator = new Elaborator
  val m = new ReverseModule
  val designs = elaborator.elaborate(m)
  val rendered = elaborator.emitAll(designs)
  val inRef = sf(ref("io"), "in")
  val reversedBits = cat(cat(cat(cat(cat(cat(cat(
    ref("bits_6"), ref("bits_5")), ref("bits_4")), ref("bits_3")),
    ref("bits_2")), ref("bits_1")), ref("bits_0")), ref("bits"))
  val expected = Seq(
    module(
      "ReverseModule",
      Seq(
        clockPort,
        resetPort,
        portOut("io", bundle(
          ("in", true, u(8)),
          ("out", false, u(8))
        ))
      ),
      Seq(
        IR.DefNode(id("bits"), bits(inRef, 0, 0)),
        IR.DefNode(id("bits_0"), bits(inRef, 1, 1)),
        IR.DefNode(id("bits_1"), bits(inRef, 2, 2)),
        IR.DefNode(id("bits_2"), bits(inRef, 3, 3)),
        IR.DefNode(id("bits_3"), bits(inRef, 4, 4)),
        IR.DefNode(id("bits_4"), bits(inRef, 5, 5)),
        IR.DefNode(id("bits_5"), bits(inRef, 6, 6)),
        IR.DefNode(id("bits_6"), bits(inRef, 7, 7)),
        IR.Connect(sf(ref("io"), "out"), reversedBits)
      )
    )
  )
  println("=" * 50)
  println("Reverse Check:")
  println(rendered)
  assertDesigns("Reverse Check", designs, expected)
  println("=" * 50)

def bitpat_check(): Unit =
  val pat1 = BitPat("b1010")
  assert(pat1.width == 4)
  assert(pat1.value == BigInt(10))
  assert(pat1.mask == BigInt(15))

  val pat2 = BitPat("b10?0")
  assert(pat2.width == 4)
  assert(pat2.value == BigInt(8))
  assert(pat2.mask == BigInt(13))

  val pat3 = BitPat("b????")
  assert(pat3.width == 4)
  assert(pat3.value == BigInt(0))
  assert(pat3.mask == BigInt(0))

  println("=" * 50)
  println("BitPat Check:")
  println(s"pat1: $pat1")
  println(s"pat2: $pat2")
  println(s"pat3: $pat3")

  final case class BitPatIO(inst: UInt, match1: Bool, match2: Bool) extends Bundle[BitPatIO]
  class BitPatModule extends Module:
    given Module = this
    val io = IO(BitPatIO(
      inst = Input(UInt(Width(32))),
      match1 = Output(Bool()),
      match2 = Output(Bool())
    ))
    body:
      val ADD  = BitPat("b0000000??????????000?????0110011")
      val ADDI = BitPat("b?????????????????000?????0010011")
      io.match1 := ADD === io.inst
      io.match2 := ADDI === io.inst

  val elaborator = new Elaborator
  val m = new BitPatModule
  val designs = elaborator.elaborate(m)
  val rendered = elaborator.emitAll(designs)
  val instRef = sf(ref("io"), "inst")
  val expected = Seq(
    module(
      "BitPatModule",
      Seq(
        clockPort,
        resetPort,
        portOut("io", bundle(
          ("inst", true, u(32)),
          ("match1", false, IR.BoolType),
          ("match2", false, IR.BoolType)
        ))
      ),
      Seq(
        IR.Connect(
          sf(ref("io"), "match1"),
          eqv(and(instRef, lit("UInt<32>(4261441663)")), lit("UInt<32>(51)"))
        ),
        IR.Connect(
          sf(ref("io"), "match2"),
          eqv(and(instRef, lit("UInt<32>(28799)")), lit("UInt<32>(19)"))
        )
      )
    )
  )
  println("BitPat Module Check:")
  println(rendered)
  assertDesigns("BitPat Module Check", designs, expected)

def bitpat_switch_check(): Unit =
  final case class BitPatSwitchIO(inst: UInt, aluOp: UInt) extends Bundle[BitPatSwitchIO]
  class BitPatSwitchModule extends Module:
    given Module = this
    val io = IO(BitPatSwitchIO(
      inst = Input(UInt(Width(32))),
      aluOp = Output(UInt(Width(4)))
    ))
    body:
      val ADD = BitPat("b0000000??????????000?????0110011")
      val SUB = BitPat("b0100000??????????000?????0110011")
      val AND = BitPat("b0000000??????????111?????0110011")

      io.aluOp := 0.U
      switch(io.inst) {
        is(ADD) { io.aluOp := 1.U }
        is(SUB) { io.aluOp := 2.U }
        is(AND) { io.aluOp := 3.U }
        default { io.aluOp := DontCare }
      }

  val elaborator = new Elaborator
  val m = new BitPatSwitchModule
  val designs = elaborator.elaborate(m)
  val rendered = elaborator.emitAll(designs)
  val instRef = sf(ref("io"), "inst")
  val aluOpRef = sf(ref("io"), "aluOp")
  val expected = Seq(
    module(
      "BitPatSwitchModule",
      Seq(
        clockPort,
        resetPort,
        portOut("io", bundle(
          ("inst", true, u(32)),
          ("aluOp", false, u(4))
        ))
      ),
      Seq(
        IR.Connect(aluOpRef, lit("UInt(0)")),
        IR.When(
          eqv(and(instRef, lit("UInt<32>(4261441663)")), lit("UInt<32>(51)")),
          Seq(IR.Connect(aluOpRef, lit("UInt(1)"))),
          Seq(
            IR.When(
              eqv(and(instRef, lit("UInt<32>(4261441663)")), lit("UInt<32>(1073741875)")),
              Seq(IR.Connect(aluOpRef, lit("UInt(2)"))),
              Seq(
                IR.When(
                  eqv(and(instRef, lit("UInt<32>(4261441663)")), lit("UInt<32>(28723)")),
                  Seq(IR.Connect(aluOpRef, lit("UInt(3)"))),
                  Seq(IR.Invalid(aluOpRef))
                )
              )
            )
          )
        )
      )
    )
  )
  println("=" * 50)
  println("BitPat Switch Check:")
  println(rendered)
  assertDesigns("BitPat Switch Check", designs, expected)
  println("=" * 50)

def sint_basic_check(): Unit =
  final case class SIntIO(a: SInt, b: SInt, sum: SInt, diff: SInt, neg: SInt, cmp: Bool) extends Bundle[SIntIO]
  class SIntModule extends Module:
    given Module = this
    val io = IO(SIntIO(
      a = Input(SInt(Width(8))),
      b = Input(SInt(Width(8))),
      sum = Output(SInt(Width(8))),
      diff = Output(SInt(Width(8))),
      neg = Output(SInt(Width(9))),
      cmp = Output(Bool())
    ))
    io.sum := io.a + io.b
    io.diff := io.a - io.b
    io.neg := -io.a
    io.cmp := io.a < io.b

  val elaborator = new Elaborator
  val m = new SIntModule
  val designs = elaborator.elaborate(m)
  val rendered = elaborator.emitAll(designs)
  val expected = Seq(
    module(
      "SIntModule",
      Seq(
        clockPort,
        resetPort,
        portOut("io", bundle(
          ("a", true, s(8)),
          ("b", true, s(8)),
          ("sum", false, s(8)),
          ("diff", false, s(8)),
          ("neg", false, s(9)),
          ("cmp", false, IR.BoolType)
        ))
      ),
      Seq(
        IR.Connect(
          sf(ref("io"), "sum"),
          IR.DoPrim(IR.PrimOp.AsSInt, Seq(tailExpr(IR.DoPrim(IR.PrimOp.Add, Seq(sf(ref("io"), "a"), sf(ref("io"), "b"))), 1)))
        ),
        IR.Connect(
          sf(ref("io"), "diff"),
          IR.DoPrim(IR.PrimOp.AsSInt, Seq(tailExpr(IR.DoPrim(IR.PrimOp.Sub, Seq(sf(ref("io"), "a"), sf(ref("io"), "b"))), 1)))
        ),
        IR.Connect(
          sf(ref("io"), "neg"),
          IR.DoPrim(IR.PrimOp.Neg, Seq(sf(ref("io"), "a")))
        ),
        IR.Connect(
          sf(ref("io"), "cmp"),
          IR.DoPrim(IR.PrimOp.Lt, Seq(sf(ref("io"), "a"), sf(ref("io"), "b")))
        )
      )
    )
  )
  println("=" * 50)
  println("SInt Basic Check:")
  println(rendered)
  assertDesigns("SInt Basic Check", designs, expected)
  println("=" * 50)

def sint_conversion_check(): Unit =
  final case class ConvIO(uIn: UInt, sIn: SInt, uOut: UInt, sOut: SInt) extends Bundle[ConvIO]
  class ConvModule extends Module:
    given Module = this
    val io = IO(ConvIO(
      uIn = Input(UInt(Width(8))),
      sIn = Input(SInt(Width(8))),
      uOut = Output(UInt(Width(8))),
      sOut = Output(SInt(Width(8)))
    ))
    io.uOut := io.sIn.asUInt
    io.sOut := io.uIn.asSInt

  val elaborator = new Elaborator
  val m = new ConvModule
  val designs = elaborator.elaborate(m)
  val rendered = elaborator.emitAll(designs)
  val expected = Seq(
    module(
      "ConvModule",
      Seq(
        clockPort,
        resetPort,
        portOut("io", bundle(
          ("uIn", true, u(8)),
          ("sIn", true, s(8)),
          ("uOut", false, u(8)),
          ("sOut", false, s(8))
        ))
      ),
      Seq(
        IR.Connect(
          sf(ref("io"), "uOut"),
          IR.DoPrim(IR.PrimOp.AsUInt, Seq(sf(ref("io"), "sIn")))
        ),
        IR.Connect(
          sf(ref("io"), "sOut"),
          IR.DoPrim(IR.PrimOp.AsSInt, Seq(sf(ref("io"), "uIn")))
        )
      )
    )
  )
  println("=" * 50)
  println("SInt Conversion Check:")
  println(rendered)
  assertDesigns("SInt Conversion Check", designs, expected)
  println("=" * 50)

def connection_type_check(): Unit =
  final case class MixedBundle(
    in_1: UInt,
    in_2: SInt,
    in_3: Bool,
    out_1: UInt,
    out_2: SInt) extends Bundle[MixedBundle]

  object MixedBundle:
    def apply(): MixedBundle = MixedBundle(
      Input(UInt(Width(8))),
      Input(SInt(Width(8))),
      Input(Bool()),
      Output(UInt(Width(8))),
      Output(SInt(Width(8)))
    )

  case class ConnectionTestIO(in: MixedBundle, out: MixedBundle) extends Bundle[ConnectionTestIO]

  object ConnectionTestIO:
    def apply(): ConnectionTestIO =
      ConnectionTestIO(
        in = MixedBundle(),
        out = MixedBundle())

  class ConnectionTest extends Module:
    given Module = this
    val io = IO(ConnectionTestIO())

    inline val tc1 = """
    io.out.out_1 := io.in.in_2
    """
    assert(typeCheckErrors(tc1).nonEmpty)

    inline val tc2 = """
    io.out := io.in
    """
    assert(typeCheckErrors(tc2).isEmpty)

    inline val tc3 = """
    io.out := DontCare
    """
    assert(typeCheckErrors(tc3).isEmpty)

    inline val tc4 = """
    io.out <> io.in
    """
    assert(typeCheckErrors(tc4).isEmpty)

    inline val tc5 = """
    io.out <> DontCare
    """
    assert(typeCheckErrors(tc5).isEmpty)

    inline val tc6 = """
    io.out.out_2 <> DontCare
    """
    assert(typeCheckErrors(tc6).nonEmpty)

    inline val tc7 = """
    io.out.out_2 <> io.in
    """
    assert(typeCheckErrors(tc7).nonEmpty)

  val elaborator = new Elaborator
  val a = new ConnectionTest
  val designs = elaborator.elaborate(a)

def tuple_bundle_check(): Unit =
  class TupleBundleModule extends Module:
    given Module = this
    val io = IO(Bundle((
      a = Input(UInt(Width(8))),
      b = Input(UInt(Width(8))),
      sum = Output(UInt(Width(9)))
    )))
    body:
      io.sum := io.a +& io.b

  val elaborator = new Elaborator
  val a = new TupleBundleModule
  val designs = elaborator.elaborate(a)
  val expected = Seq(
    module(
      "TupleBundleModule",
      Seq(
        clockPort,
        resetPort,
        portOut("io", bundle(
          ("a", true, u(8)),
          ("b", true, u(8)),
          ("sum", false, u(9))
        ))
      ),
      Seq(
        IR.Connect(
          sf(ref("io"), "sum"),
          addPrim(sf(ref("io"), "a"), sf(ref("io"), "b"))
        )
      )
    )
  )
  assertDesigns("tuple_bundle_check", designs, expected)

def nested_tuple_bundle_check(): Unit =
  case class InnerBundle(x: UInt, y: UInt) extends Bundle[InnerBundle]

  class NestedTupleBundleModule extends Module:
    given Module = this
    val io = IO(Bundle((
      inner = Input(InnerBundle(UInt(Width(4)), UInt(Width(4)))),
      out = Output(UInt(Width(4)))
    )))
    body:
      io.out := io.inner.x + io.inner.y

  val elaborator = new Elaborator
  val a = new NestedTupleBundleModule
  val designs = elaborator.elaborate(a)
  val expected = Seq(
    module(
      "NestedTupleBundleModule",
      Seq(
        clockPort,
        resetPort,
        portOut("io", bundle(
          ("inner", true, bundle(
            ("x", false, u(4)),
            ("y", false, u(4))
          )),
          ("out", false, u(4))
        ))
      ),
      Seq(
        IR.Connect(
          sf(ref("io"), "out"),
          tailExpr(addPrim(sf(sf(ref("io"), "inner"), "x"), sf(sf(ref("io"), "inner"), "y")), 1)
        )
      )
    )
  )
  assertDesigns("nested_tuple_bundle_check", designs, expected)

def tuple_bundle_wire_check(): Unit =
  class TupleBundleWireModule extends Module:
    given Module = this
    val io = IO(Bundle((
      in = Input(UInt(Width(8))),
      out = Output(UInt(Width(8)))
    )))
    body:
      val temp = Wire(Bundle((
        x = UInt(Width(8)),
        y = UInt(Width(8))
      )))
      temp.x := io.in
      temp.y := io.in + 1.U
      io.out := temp.x + temp.y

  val elaborator = new Elaborator
  val a = new TupleBundleWireModule
  val designs = elaborator.elaborate(a)
  assert(designs.nonEmpty)

object ModuleChecksSpec extends TestSuite:
  val tests = Tests {
    test("simple_module_test") { simple_module_test() }
    test("regnext_check") { regnext_check() }
    test("dontcare_check") { dontcare_check() }
    test("list_operation_check") { list_operation_check() }
    test("nested_module_check") { nested_module_check() }
    test("nested_bundle_check") { nested_bundle_check() }
    test("inheritance_check") { inheritance_check() }
    test("type_parameterization_check") { type_parameterization_check() }
    test("conditional_generation_check") { conditional_generation_check() }
    test("optional_io_check") { optional_io_check() }
    test("nested_seq_generation_check") { nested_seq_generation_check() }
    test("optional_and_map_check") { optional_and_map_check() }
    test("module_array_generation_check") { module_array_generation_check() }
    test("parameter_sweep_check") { parameter_sweep_check() }
    test("when_behavior_check") { when_behavior_check() }
    test("comparison_operator_check") { comparison_operator_check() }
    test("vec_check") { vec_check() }
    test("bit_select_check") { bit_select_check() }
    test("bitwise_reverse_check") { bitwise_reverse_check() }
    test("bitwise_reduce_check") { bitwise_reduce_check() }
    test("first_set_indices_check") { first_set_indices_check() }
    test("mux_and_concat_check") { mux_and_concat_check() }
    test("enum_basic_check") { enum_basic_check() }
    test("switch_check") { switch_check() }
    test("fill_check") { fill_check() }
    test("reverse_check") { reverse_check() }
    test("bitpat_check") { bitpat_check() }
    test("bitpat_switch_check") { bitpat_switch_check() }
    test("sint_basic_check") { sint_basic_check() }
    test("sint_conversion_check") { sint_conversion_check() }
    test("connection_type_check") { connection_type_check() }
    test("tuple_bundle_check") { tuple_bundle_check() }
    test("nested_tuple_bundle_check") { nested_tuple_bundle_check() }
    test("tuple_bundle_wire_check") { tuple_bundle_wire_check() }
  }
