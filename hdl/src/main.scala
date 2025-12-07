package hdl

import scala.compiletime.testing.*

// Need to declare fields as `private val` so that when accessing subfields, selectDynamic in Bundle gets called
// This is only required for Literal types where you want to propagate the top level literal payload to subfields
final case class InnerBundleIf(private val a: UInt, private val b: UInt) extends Bundle[InnerBundleIf]
final case class MyBundleIf(private val x: UInt, private val y: UInt, private val i: InnerBundleIf) extends Bundle[MyBundleIf]

def instantiation_check(): Unit =
  val mb = MyBundleIf(
    x = UInt(Width(2)),
    y = UInt(Width(3)),
    i = InnerBundleIf(
      a = UInt(Width(4)),
      b = UInt(Width(5))
    ))

  val reg = Reg(mb)
  println(s"$reg")

  val reg_x: UInt = reg.x
  val reg_y: UInt = reg.y
  val x: InnerBundleIf = reg.i
// val reg_i: Bundle[InnerBundleIf] = reg.i
// val reg_i_a: UInt = reg_i.a

  val ulit = Lit(UInt(Width(3)))(3)
  assert(ulit.getLitVal == BigInt(3))

  inline val tc1 = """
  val rg: MyBundleIf = Reg(mb)
  val reg_x: UInt = rg.x
  val reg_y: UInt = rg.y
  val reg_i: InnerBundleIf = rg.i
  val reg_i_a: UInt = rg.i.a
  val reg_i_b: UInt = rg.i.b
  """
  assert(typeCheckErrors(tc1).isEmpty)

def hosttype_check(): Unit =
  val inner_bundle_host_type: HostTypeOf[InnerBundleIf] = (
    a = 3,
    b = 2,
  )
  println(s"inner_bundle_host_type ${inner_bundle_host_type}")

  inline val invalidHostType = """
  val inner_bundle_host_type: hdl.HostTypeOf[hdl.InnerBundleIf] = (
    a = 3, b = 2, c = 4
  )
  """
  assert(typeCheckErrors(invalidHostType).nonEmpty)

def literal_check(): Unit =
  val ilit = Lit(
    InnerBundleIf(UInt(Width(4)), UInt(Width(5)))
  )((a = BigInt(3), b = BigInt(4)))

  println(s"ilit.literal: ${ilit.literal}")
  assert(ilit.literal == Some((3, 4)))

  val ilit_a: UInt = ilit.a

  inline val invalidLitType = """
  val ilit = hdl.Lit(hdl.InnerBundleIf(hdl.UInt(hdl.Width(4)), hdl.UInt(hdl.Width(5))))((a = 3, b = 4))
  val ilit_a: hdl.Lit[hdl.Bool] = ilit.a
  """
  assert(typeCheckErrors(invalidLitType).nonEmpty)

  println(ilit.a)
  assert(ilit.a.literal.isDefined)

  assert(ilit.a.getLitVal == BigInt(3))
  assert(ilit_a.getLitVal == BigInt(3))
  assert(ilit.getLitVal == (BigInt(3), BigInt(4)))

  val x: HostTypeOf[MyBundleIf] = (
    x = 2,
    y = 3,
    i = (a = 2, b = 3)
  )

  val mylit = Lit(MyBundleIf(
    x = UInt(Width(2)),
    y = UInt(Width(3)),
    i = InnerBundleIf(UInt(Width(4)), UInt(Width(5)))
  ))((
    x = BigInt(2),
    y = BigInt(3),
    i = (a = BigInt(4), b = BigInt(5))))

  val mylit_x: UInt = mylit.x
  assert(mylit.x.getLitVal == BigInt(2))
  assert(mylit_x.getLitVal == BigInt(2))

  inline val invalidLitType2 = """
  val mylit = hdl.Lit(hdl.MyBundleIf(
    x = hdl.UInt(hdl.Width(2)),
    y = hdl.UInt(hdl.Width(3)),
    i = hdl.InnerBundleIf(hdl.UInt(hdl.Width(4)), hdl.UInt(hdl.Width(5)))
    ))((
      x = 2,
      y = 3,
      i = (a = 4, b = 5)))
    val mylit_i1: hdl.Lit[hdl.MyBundleIf] = mylit.i
    val mylit_i2: hdl.Lit[hdl.UInt] = mylit.i
    """
  assert(typeCheckErrors(invalidLitType2).nonEmpty)

  val mylit_i: Bundle[InnerBundleIf] = mylit.i
  assert(mylit_i.getLitVal == (BigInt(4), BigInt(5)))
  assert(mylit.i.getLitVal == (BigInt(4), BigInt(5)))
  assert(mylit.i.a.getLitVal == BigInt(4))
  assert(mylit.i.b.getLitVal == BigInt(5))

  inline val invalidLitType3 = """
  val mylit_2 = hdl.Lit(hdl.MyBundleIf(
    x = hdl.UInt(hdl.Width(2)),
    y = hdl.UInt(hdl.Width(3)),
    i = hdl.InnerBundleIf(hdl.UInt(hdl.Width(4)), hdl.UInt(hdl.Width(5)))
    ))((
      y = 3,
      x = 2,
      i = (a = 4, b = 5)))
    """
  assert(typeCheckErrors(invalidLitType3).nonEmpty)

def directionality_check(): Unit =
  final case class A(private val a: UInt, private val b: UInt) extends Bundle[A]
  final case class B(private val x: A,    private val y: Bool) extends Bundle[B]

  val bundle_a = A(
    a = Input(UInt(Width(3))),
    b = UInt(Width(4))
  )

  assert(bundle_a.a.dir == Direction.In)
  assert(bundle_a.b.dir == Direction.Out)

  val bundle_b = B(
    x = Flipped(bundle_a),
    y = Output(Bool(()))
  )
  assert(bundle_b.x.a.dir == Direction.In)
  assert(bundle_b.x.b.dir == Direction.Out)
  assert(bundle_b.y.dir   == Direction.Out)

  val bundle_b_lit = Lit(bundle_b)((
    x = (
      a = BigInt(1),
      b = BigInt(2),
    ),
    y = false))

def optional_io_directionality_check(): Unit =
  case class OptBundle(
    a: Option[UInt],
    b: UInt,
    c: UInt) extends Bundle[OptBundle]

  case class MyBundleIf(
    opt: OptBundle,
    lux: UInt) extends Bundle[MyBundleIf]

  case class SimpleBundle(a: UInt, b: UInt) extends Bundle[SimpleBundle]
  case class OptBundle2(
    a: Option[SimpleBundle],
    b: UInt,
    c: UInt,
    d: UInt,
  ) extends Bundle[OptBundle2]

  val io_2 = IO(
    Flipped(OptBundle2(
      a = Some(Flipped(SimpleBundle(
        a = Input(UInt(Width(2))),
        b = Output(UInt(Width(3)))
      ))),
      b = UInt(Width(4)),
      c = Input(UInt(Width(5))),
      d = Flipped(Input(UInt(Width(6))))
  )))
  io_2.a.get.a
  io_2.a.map(x => println(s"io.a = ${x}"))

// println(s"io_2 ${io_2}")

  assert(io_2.dir == Direction.Flipped)
  assert(io_2.b.dir == Direction.Default)
  assert(io_2.c.dir == Direction.Flipped)
  assert(io_2.d.dir == Direction.Default)
  assert(io_2.a.get.dir == Direction.Flipped)
  assert(io_2.a.get.a.dir == Direction.Flipped)
  assert(io_2.a.get.b.dir == Direction.Default)


def parameterized_bundle_check(): Unit =
  case class Security(
    pixelstealing: UInt,
    bpred: UInt,
    prefetcher: UInt
  ) extends Bundle[Security]

  case class SecurityParams(w: Int)

  object Security:
    def apply(p: SecurityParams): Security =
      Security(
        pixelstealing = UInt(Width(p.w)),
        bpred = UInt(Width(p.w)),
        prefetcher = UInt(Width(p.w)))

  case class Student(
    age: UInt,
    female: Bool
  ) extends Bundle[Student]

  object Student:
    def apply(): Student =
      Student(
        age = UInt(Width(3)),
        female = Bool())

  case class Child(
    age: UInt,
  ) extends Bundle[Child]

  object Child:
    def apply(): Child =
      Child(age = Input(UInt(Width(4))))

  case class Fletcher(
    private val security: Security,
    private val students: Seq[Student],
    private val childs: Option[Seq[Child]]
  ) extends Bundle[Fletcher]

  case class FletcherParams(
    num_students: Int,
    has_child: Boolean,
    num_childs: Int,
    security: SecurityParams
  )

  object Fletcher:
    def apply(p: FletcherParams): Fletcher =
      Fletcher(
        security = Input(Security(p.security)),
        students = Seq.fill(p.num_students)(
          Flipped(Student())
        ),
        childs = if (p.has_child) Some(Seq.fill(p.num_childs)(Output(Child())))
                 else None)

  val fp = FletcherParams(
    num_students = 3,
    num_childs = 2,
    has_child = true,
    SecurityParams(w = 5))

  val fletcher = Reg(Fletcher(fp))

  val students: Seq[Student] = fletcher.students
  val age: UInt = fletcher.students(0).age

  assert(fletcher.security.dir == Direction.Flipped)

  fletcher.students.foreach(x => {
    assert(x.dir        == Direction.Flipped)
    assert(x.age.dir    == Direction.Default)
    assert(x.female.dir == Direction.Default)
  })

  fletcher.childs.map(child => {
    child.foreach(c => {
      assert(c.age.dir == Direction.Flipped)
    })
  })

  // Note: Literals with Scala types doesn't work. In theary, we can add support
  // later by extending `HostTypeOf` and `FieldToNode` for `Bundle`.
  //
  // val fletcher_lit = Lit(Fletcher(fp))((
  //   security = (
  //     pixelstealing = BigInt(1),
  //     bpred = BigInt(2),
  //     prefetcher = BigInt(3)
  //   ),
  //   students = Seq.fill(fp.num_students)((
  //     age = BigInt(4),
  //     female = true
  //   )),
  //   childs = if (fp.has_child) Some(
  //     Seq.fill(fp.num_childs)((
  //       age = BigInt(5)
  //     )))
  //     else None
  // ))

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

@main def demo(): Unit =
  instantiation_check()
  hosttype_check()
  literal_check()
  directionality_check()
  optional_io_directionality_check()
  parameterized_bundle_check()
  simple_module_test()
  list_operation_check()
  nested_module_check()
  inheritance_check()
  type_parameterization_check()
  conditional_generation_check()
  optional_io_check()
  nested_seq_generation_check()
  optional_and_map_check()
  module_array_generation_check()
  parameter_sweep_check()
