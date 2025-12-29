package hdl

import hdl.core._
import hdl.util._
import hdl.elaboration._

import utest.*
import scala.compiletime.testing.*

object Reg:
  def apply[T <: HWData](t: T, name: Option[String] = None): T =
    t.setNodeKind(NodeKind.Reg)
    t

object Lit:
  def apply[T <: HWData](t: T)(payload: HostTypeOf[T]): T =
    t.setNodeKind(NodeKind.Lit)
    t.setLitVal(payload)
    t

object IO:
  def apply[T <: HWData](t: T, name: Option[String] = None): T =
    t.setNodeKind(NodeKind.IO)
    t

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

  case class Turing(
    private val security: Security,
    private val students: Seq[Student],
    private val childs: Option[Seq[Child]]
  ) extends Bundle[Turing]

  case class TuringParams(
    num_students: Int,
    has_child: Boolean,
    num_childs: Int,
    security: SecurityParams
  )

  object Turing:
    def apply(p: TuringParams): Turing =
      Turing(
        security = Input(Security(p.security)),
        students = Seq.fill(p.num_students)(
          Flipped(Student())
        ),
        childs = if (p.has_child) Some(Seq.fill(p.num_childs)(Output(Child())))
                 else None)

  val tp = TuringParams(
    num_students = 3,
    num_childs = 2,
    has_child = true,
    SecurityParams(w = 5))

  val turing = Reg(Turing(tp))

  val students: Seq[Student] = turing.students
  val age: UInt = turing.students(0).age

  assert(turing.security.dir == Direction.Flipped)

  turing.students.foreach(x => {
    assert(x.dir        == Direction.Flipped)
    assert(x.age.dir    == Direction.Default)
    assert(x.female.dir == Direction.Default)
  })

  turing.childs.map(child => {
    child.foreach(c => {
      assert(c.age.dir == Direction.Flipped)
    })
  })

  // Note: Literals with Scala types doesn't work. In theary, we can add support
  // later by extending `HostTypeOf` and `FieldToNode` for `Bundle`.
  //
  // val turing_lit = Lit(Turing(tp))((
  //   security = (
  //     pixelstealing = BigInt(1),
  //     bpred = BigInt(2),
  //     prefetcher = BigInt(3)
  //   ),
  //   students = Seq.fill(tp.num_students)(
  //     (
  //       age = BigInt(4),
  //       female = true
  //     )
  //   ),
  //   childs = if (tp.has_child) Some(
  //     Seq.fill(tp.num_childs)(
  //       (
  //         age = BigInt(5)
  //       )
  //     ))
  //     else None
  // ))

  // val turing_lit = Lit(Turing(tp))((
  //   security = Lit(Security(tp.security))((
  //     pixelstealing = BigInt(1),
  //     bpred = BigInt(2),
  //     prefetcher = BigInt(3)
  //   )),
  //   students = Seq.fill(tp.num_students)(
  //     Lit(Student())((
  //       age = BigInt(4),
  //       female = true
  //     ))
  //   ),
  //   childs = if (tp.has_child) Some(
  //     Seq.fill(tp.num_childs)(
  //       Lit(Child())((
  //         age = BigInt(5)
  //       ))
  //     ))
  //     else None
  // ))

  // assert(turing_lit.security.pixelstealing.getLitVal == BigInt(1))

  case class Godel(
    private val security: Security,
    private val students: Vec[Vec[Student]],
  ) extends Bundle[Godel]

  case class GodelParams(
    num_students: Int,
    security: SecurityParams
  )

  object Godel:
    def apply(p: GodelParams): Godel =
      Godel(
        security = Input(Security(p.security)),
        students = Vec(Seq.fill(p.num_students)(
          Vec(Seq.fill(p.num_students)(
            Flipped(Student())
          ))
        ))
      )

  val godel_lit = Lit(
    Godel(GodelParams(
      num_students = 2,
      security = SecurityParams(3)
    ))
  )(
    (
      security = (
        pixelstealing = BigInt(1),
        bpred = BigInt(2),
        prefetcher = BigInt(3)
      ),
      students = Seq(
        Seq(
          (
            age = BigInt(4),
            female = true
          ),
          (
            age = BigInt(5),
            female = false
          ),
        ),
        Seq(
          (
            age = BigInt(6),
            female = true
          ),
          (
            age = BigInt(7),
            female = false
          ),
        ),
      ),
    )
  )

  assert(godel_lit.students(0)(0).age.getLitVal == BigInt(4))
  assert(godel_lit.students(0)(0).female.getLitVal == true)

  assert(godel_lit.students(0)(1).age.getLitVal == BigInt(5))
  assert(godel_lit.students(0)(1).female.getLitVal == false)

  assert(godel_lit.students(1)(0).age.getLitVal == BigInt(6))
  assert(godel_lit.students(1)(0).female.getLitVal == true)

  assert(godel_lit.students(1)(1).age.getLitVal == BigInt(7))
  assert(godel_lit.students(1)(1).female.getLitVal == false)

  assert(godel_lit.security.pixelstealing.getLitVal == BigInt(1))
  assert(godel_lit.security.bpred.getLitVal == BigInt(2))
  assert(godel_lit.security.prefetcher.getLitVal == BigInt(3))

object BundleChecksSpec extends TestSuite:
  val tests = Tests {
    test("instantiation_check") { instantiation_check() }
    test("hosttype_check") { hosttype_check() }
    test("literal_check") { literal_check() }
    test("directionality_check") { directionality_check() }
    test("optional_io_directionality_check") { optional_io_directionality_check() }
    test("parameterized_bundle_check") { parameterized_bundle_check() }
  }
