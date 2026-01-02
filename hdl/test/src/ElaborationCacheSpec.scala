package hdl

import hdl.core._
import hdl.util._
import hdl.elaboration._

import utest.*
import java.io.{ByteArrayOutputStream, PrintStream}
import java.nio.file.{Files, Path}

final case class LeafParams(width: Int, work: Int) derives StableHash

final class CacheableLeaf(p: LeafParams) extends Module with CacheableModule:
  type ElabParams = LeafParams
  given stableHashElabParams: StableHash[LeafParams] = summon[StableHash[LeafParams]]
  def elabParams: LeafParams = p

  val io = IO(SimpleIO(Input(UInt(p.width.W)), Output(UInt(p.width.W))))
  body:
    val regs = Vector.tabulate(p.work)(i => Reg(UInt(p.width.W)))
    var idx = 0
    while idx < regs.length do
      regs(idx) := Lit(UInt(p.width.W))(idx)
      idx += 1
    io.out := io.in

final class NonCacheableLeaf(width: Int, work: Int) extends Module:
  val io = IO(SimpleIO(Input(UInt(width.W)), Output(UInt(width.W))))
  body:
    val wires = Vector.tabulate(work)(i => Wire(UInt(width.W)))
    var idx = 0
    while idx < wires.length do
      wires(idx) := io.in
      idx += 1
    io.out := io.in

final class CacheBranch(nonWidth: Int, id: Int) extends Module:
  val io = IO(SimpleIO(Input(UInt(Width(4))), Output(UInt(Width(4)))))
  body:
    val cacheA  = Module(new CacheableLeaf(LeafParams(8  + id, 64)))
    val cacheB  = Module(new CacheableLeaf(LeafParams(10 + id, 64)))
    val cacheA2 = Module(new CacheableLeaf(LeafParams(8  + id, 64)))
    val non     = Module(new NonCacheableLeaf(nonWidth + id, 48))
    io.out := io.in

object CacheTop:
  val cacheablePerBranch = 3
  val cacheableInstanceCount = cacheablePerBranch * 2

final class CacheTop(nonWidth: Int) extends Module:
  val io = IO(SimpleIO(Input(UInt(Width(4))), Output(UInt(Width(4)))))
  body:
    val left  = Module(new CacheBranch(nonWidth, 0))
    // cacheA: 8, 64
    // cacheB: 10, 64
    // cacheA2: 8, 64
    // non:    9, 48
    val right = Module(new CacheBranch(nonWidth + 1, 1))
    // cacheA: 9,  64
    // cacheB: 11, 64
    // cacheA2: 9, 64
    // non:    11, 48
    io.out := io.in

final case class SimpleParams(width: Int) derives StableHash

final class CacheableOne(p: SimpleParams) extends Module with CacheableModule:
  override def moduleName: String = "MutableLeaf"
  type ElabParams = SimpleParams
  given stableHashElabParams: StableHash[SimpleParams] = summon[StableHash[SimpleParams]]
  def elabParams: SimpleParams = p
  val io = IO(SimpleIO(Input(UInt(p.width.W)), Output(UInt(p.width.W))))
  body:
    val w = Wire(UInt(p.width.W))
    w := io.in
    io.out := w

final class CacheableTwo(p: SimpleParams) extends Module with CacheableModule:
  override def moduleName: String = "MutableLeaf"
  type ElabParams = SimpleParams
  given stableHashElabParams: StableHash[SimpleParams] = summon[StableHash[SimpleParams]]
  def elabParams: SimpleParams = p
  val io = IO(SimpleIO(Input(UInt(p.width.W)), Output(UInt(p.width.W))))
  body:
    val r = Reg(UInt(p.width.W))
    r := io.in
    io.out := r

final class CacheableOneTop(p: SimpleParams) extends Module:
  val io = IO(SimpleIO(Input(UInt(p.width.W)), Output(UInt(p.width.W))))
  body:
    val leaf = Module(new CacheableOne(p))
    io.out := io.in

final class CacheableTwoTop(p: SimpleParams) extends Module:
  val io = IO(SimpleIO(Input(UInt(p.width.W)), Output(UInt(p.width.W))))
  body:
    val leaf = Module(new CacheableTwo(p))
    io.out := io.in

final class CacheBranchV1(nonWidth: Int, id: Int) extends Module:
  override def moduleName: String = "CacheBranch"
  val io = IO(SimpleIO(Input(UInt(Width(4))), Output(UInt(Width(4)))))
  body:
    val cacheA  = Module(new CacheableLeaf(LeafParams(8  + id, 64)))
    val cacheB  = Module(new CacheableLeaf(LeafParams(10 + id, 64)))
    val cacheA2 = Module(new CacheableLeaf(LeafParams(8  + id, 64)))
    val non     = Module(new NonCacheableLeaf(nonWidth + id, 48))
    io.out := io.in

final class CacheBranchV2(nonWidth: Int, id: Int) extends Module:
  override def moduleName: String = "CacheBranch"
  val io = IO(SimpleIO(Input(UInt(Width(4))), Output(UInt(Width(4)))))
  body:
    val cacheA  = Module(new CacheableLeaf(LeafParams(8  + id + 1, 64))) // miss
    val cacheB  = Module(new CacheableLeaf(LeafParams(10 + id, 64)))     // hit
    val cacheB2 = Module(new CacheableLeaf(LeafParams(10 + id + 1, 64))) // miss
    val cacheA2 = Module(new CacheableLeaf(LeafParams(8  + id, 64)))     // hit
    val non     = Module(new NonCacheableLeaf(nonWidth + id, 48))        // miss (non-cacheable)
    io.out := io.in + io.in

final class CacheBranchV1Top(nonWidth: Int) extends Module:
  val io = IO(SimpleIO(Input(UInt(Width(4))), Output(UInt(Width(4)))))
  body:
    val branch = Module(new CacheBranchV1(nonWidth, 0))
    io.out := io.in

final class CacheBranchV2Top(nonWidth: Int) extends Module:
  val io = IO(SimpleIO(Input(UInt(Width(4))), Output(UInt(Width(4)))))
  body:
    val branch = Module(new CacheBranchV2(nonWidth, 0))
    io.out := io.in

case class TestBundle(a: UInt, b: Bool) extends Bundle[TestBundle]
case class NestedBundle(inner: TestBundle, data: Vec[UInt]) extends Bundle[NestedBundle]
case class OptionalBundle(data: UInt, debug: Option[UInt]) extends Bundle[OptionalBundle]

case class ParameterizedBundle(dataWidth: Int, count: Int)(
  val data: Vec[UInt],
  val valid: Bool
) extends Bundle[ParameterizedBundle]
object ParameterizedBundle:
  def apply(dataWidth: Int, count: Int): ParameterizedBundle =
    new ParameterizedBundle(dataWidth, count)(Vec.fill(count)(UInt(Width(dataWidth))), Bool())

case class MixedScalaBundle(
  hwData: UInt,
  scalaInt: Int,
  scalaString: String,
  optionalHw: Option[UInt],
  seqHw: Seq[UInt]
) extends Bundle[MixedScalaBundle]

case class DeepNestedBundle(
  level1: NestedBundle,
  extra: Vec[TestBundle]
) extends Bundle[DeepNestedBundle]

case class SeqOfBundlesBundle(
  items: Seq[TestBundle],
  count: Int
) extends Bundle[SeqOfBundlesBundle]

object StableHashHWDataSpec extends TestSuite:
  def hash(hw: HWData): String =
    StableHash.hex(StableHash.digest(hw)(using summon[StableHash[HWData]]))

  def tests = Tests {
    test("same types produce same hash") {
      val u1 = UInt(Width(8))
      val u2 = UInt(Width(8))
      assert(hash(u1) == hash(u2))
    }

    test("different widths produce different hashes") {
      val u8 = UInt(Width(8))
      val u16 = UInt(Width(16))
      assert(hash(u8) != hash(u16))
    }

    test("different types produce different hashes") {
      val u = UInt(Width(8))
      val b = Bool()
      assert(hash(u) != hash(b))
    }

    test("Vec types with same elements produce same hash") {
      val v1 = Vec(Seq(UInt(Width(8)), UInt(Width(8))))
      val v2 = Vec(Seq(UInt(Width(8)), UInt(Width(8))))
      assert(hash(v1) == hash(v2))
    }

    test("Vec types with different lengths produce different hashes") {
      val v2 = Vec(Seq(UInt(Width(8)), UInt(Width(8))))
      val v3 = Vec(Seq(UInt(Width(8)), UInt(Width(8)), UInt(Width(8))))
      assert(hash(v2) != hash(v3))
    }

    test("Vec types with different element widths produce different hashes") {
      val v1 = Vec(Seq(UInt(Width(8)), UInt(Width(8))))
      val v2 = Vec(Seq(UInt(Width(16)), UInt(Width(16))))
      assert(hash(v1) != hash(v2))
    }

    test("Bundle types with same structure produce same hash") {
      val b1 = TestBundle(UInt(Width(8)), Bool())
      val b2 = TestBundle(UInt(Width(8)), Bool())
      assert(hash(b1) == hash(b2))
    }

    test("Bundle types with different structure produce different hashes") {
      val b1 = TestBundle(UInt(Width(8)), Bool())
      val b2 = TestBundle(UInt(Width(16)), Bool())
      assert(hash(b1) != hash(b2))
    }

    test("nested Bundle types produce consistent hashes") {
      val inner1 = TestBundle(UInt(Width(8)), Bool())
      val inner2 = TestBundle(UInt(Width(8)), Bool())
      val n1 = NestedBundle(inner1, Vec(Seq(UInt(Width(4)), UInt(Width(4)))))
      val n2 = NestedBundle(inner2, Vec(Seq(UInt(Width(4)), UInt(Width(4)))))
      assert(hash(n1) == hash(n2))
    }

    test("Bundle with Option[HWData] fields") {
      val withDebug = OptionalBundle(UInt(Width(8)), Some(UInt(Width(4))))
      val withoutDebug = OptionalBundle(UInt(Width(8)), None)
      assert(hash(withDebug) != hash(withoutDebug))

      val withDebug2 = OptionalBundle(UInt(Width(8)), Some(UInt(Width(4))))
      assert(hash(withDebug) == hash(withDebug2))

      val withDifferentDebugWidth = OptionalBundle(UInt(Width(8)), Some(UInt(Width(8))))
      assert(hash(withDebug) != hash(withDifferentDebugWidth))
    }

    test("parameterized Bundle with same parameters produce same hash") {
      val p1 = ParameterizedBundle(8, 4)
      val p2 = ParameterizedBundle(8, 4)
      assert(hash(p1) == hash(p2))
    }

    test("parameterized Bundle with different dataWidth produces different hash") {
      val p1 = ParameterizedBundle(8, 4)
      val p2 = ParameterizedBundle(16, 4)
      assert(hash(p1) != hash(p2))
    }

    test("parameterized Bundle with different count produces different hash") {
      val p1 = ParameterizedBundle(8, 4)
      val p2 = ParameterizedBundle(8, 8)
      assert(hash(p1) != hash(p2))
    }

    test("Bundle mixing Scala and HWData types with same values") {
      val m1 = MixedScalaBundle(
        hwData = UInt(Width(8)),
        scalaInt = 42,
        scalaString = "test",
        optionalHw = Some(UInt(Width(4))),
        seqHw = Seq(UInt(Width(2)), UInt(Width(2)))
      )
      val m2 = MixedScalaBundle(
        hwData = UInt(Width(8)),
        scalaInt = 42,
        scalaString = "test",
        optionalHw = Some(UInt(Width(4))),
        seqHw = Seq(UInt(Width(2)), UInt(Width(2)))
      )
      assert(hash(m1) == hash(m2))
    }

    test("Bundle mixing Scala and HWData types - different HW width") {
      val m1 = MixedScalaBundle(
        hwData = UInt(Width(8)),
        scalaInt = 42,
        scalaString = "test",
        optionalHw = None,
        seqHw = Seq()
      )
      val m2 = MixedScalaBundle(
        hwData = UInt(Width(16)),
        scalaInt = 42,
        scalaString = "test",
        optionalHw = None,
        seqHw = Seq()
      )
      assert(hash(m1) != hash(m2))
    }

    test("Bundle mixing Scala and HWData types - different Seq contents") {
      val m1 = MixedScalaBundle(
        hwData = UInt(Width(8)),
        scalaInt = 42,
        scalaString = "test",
        optionalHw = None,
        seqHw = Seq(UInt(Width(4)))
      )
      val m2 = MixedScalaBundle(
        hwData = UInt(Width(8)),
        scalaInt = 42,
        scalaString = "test",
        optionalHw = None,
        seqHw = Seq(UInt(Width(4)), UInt(Width(4)))
      )
      assert(hash(m1) != hash(m2))
    }

    test("deep nested Bundle with same structure") {
      val inner1 = TestBundle(UInt(Width(4)), Bool())
      val nested1 = NestedBundle(inner1, Vec.fill(2)(UInt(Width(8))))
      val deep1 = DeepNestedBundle(nested1, Vec.fill(3)(TestBundle(UInt(Width(2)), Bool())))

      val inner2 = TestBundle(UInt(Width(4)), Bool())
      val nested2 = NestedBundle(inner2, Vec.fill(2)(UInt(Width(8))))
      val deep2 = DeepNestedBundle(nested2, Vec.fill(3)(TestBundle(UInt(Width(2)), Bool())))

      assert(hash(deep1) == hash(deep2))
    }

    test("deep nested Bundle with different inner structure") {
      val inner1 = TestBundle(UInt(Width(4)), Bool())
      val nested1 = NestedBundle(inner1, Vec.fill(2)(UInt(Width(8))))
      val deep1 = DeepNestedBundle(nested1, Vec.fill(3)(TestBundle(UInt(Width(2)), Bool())))

      val inner2 = TestBundle(UInt(Width(8)), Bool())
      val nested2 = NestedBundle(inner2, Vec.fill(2)(UInt(Width(8))))
      val deep2 = DeepNestedBundle(nested2, Vec.fill(3)(TestBundle(UInt(Width(2)), Bool())))

      assert(hash(deep1) != hash(deep2))
    }

    test("Bundle with Seq[Bundle] field - same contents") {
      val s1 = SeqOfBundlesBundle(
        items = Seq(TestBundle(UInt(Width(4)), Bool()), TestBundle(UInt(Width(8)), Bool())),
        count = 2
      )
      val s2 = SeqOfBundlesBundle(
        items = Seq(TestBundle(UInt(Width(4)), Bool()), TestBundle(UInt(Width(8)), Bool())),
        count = 2
      )
      assert(hash(s1) == hash(s2))
    }

    test("Bundle with Seq[Bundle] field - different contents") {
      val s1 = SeqOfBundlesBundle(
        items = Seq(TestBundle(UInt(Width(4)), Bool())),
        count = 1
      )
      val s2 = SeqOfBundlesBundle(
        items = Seq(TestBundle(UInt(Width(4)), Bool()), TestBundle(UInt(Width(8)), Bool())),
        count = 2
      )
      assert(hash(s1) != hash(s2))
    }

    test("TupleBundle with same structure produces same hash") {
      val tb1 = Bundle((
        a = UInt(Width(8)),
        b = Bool(),
        c = Vec.fill(2)(UInt(Width(4)))
      ))
      val tb2 = Bundle((
        a = UInt(Width(8)),
        b = Bool(),
        c = Vec.fill(2)(UInt(Width(4)))
      ))
      assert(hash(tb1) == hash(tb2))
    }

    test("TupleBundle with different field widths produces different hash") {
      val tb1 = Bundle((
        a = UInt(Width(8)),
        b = Bool()
      ))
      val tb2 = Bundle((
        a = UInt(Width(16)),
        b = Bool()
      ))
      assert(hash(tb1) != hash(tb2))
    }

    test("TupleBundle with different field names produces different hash") {
      val tb1 = Bundle((
        foo = UInt(Width(8)),
        bar = Bool()
      ))
      val tb2 = Bundle((
        baz = UInt(Width(8)),
        qux = Bool()
      ))
      assert(hash(tb1) != hash(tb2))
    }

    test("TupleBundle nested in case class Bundle") {
      case class OuterBundle(
        tuple: TupleBundle[("x", "y"), (UInt, Bool)],
        extra: UInt
      ) extends Bundle[OuterBundle]

      val tb = Bundle((x = UInt(Width(4)), y = Bool()))
      val outer1 = OuterBundle(tb, UInt(Width(8)))
      val outer2 = OuterBundle(Bundle((x = UInt(Width(4)), y = Bool())), UInt(Width(8)))
      assert(hash(outer1) == hash(outer2))
    }

    test("TupleBundle with nested case class Bundle") {
      val tb1 = Bundle((
        inner = TestBundle(UInt(Width(8)), Bool()),
        data = UInt(Width(4))
      ))
      val tb2 = Bundle((
        inner = TestBundle(UInt(Width(8)), Bool()),
        data = UInt(Width(4))
      ))
      assert(hash(tb1) == hash(tb2))

      val tb3 = Bundle((
        inner = TestBundle(UInt(Width(16)), Bool()),
        data = UInt(Width(4))
      ))
      assert(hash(tb1) != hash(tb3))
    }

    test("TupleBundle with Vec of Bundles") {
      val tb1 = Bundle((
        items = Vec.fill(3)(TestBundle(UInt(Width(4)), Bool())),
        count = UInt(Width(8))
      ))
      val tb2 = Bundle((
        items = Vec.fill(3)(TestBundle(UInt(Width(4)), Bool())),
        count = UInt(Width(8))
      ))
      assert(hash(tb1) == hash(tb2))

      val tb3 = Bundle((
        items = Vec.fill(4)(TestBundle(UInt(Width(4)), Bool())),
        count = UInt(Width(8))
      ))
      assert(hash(tb1) != hash(tb3))
    }

    test("TupleBundle with Option field") {
      val tb1 = Bundle((
        data = UInt(Width(8)),
        debug = Some(UInt(Width(4)))
      ))
      val tb2 = Bundle((
        data = UInt(Width(8)),
        debug = Some(UInt(Width(4)))
      ))
      assert(hash(tb1) == hash(tb2))

      val tb3 = Bundle((
        data = UInt(Width(8)),
        debug = None: Option[UInt]
      ))
      assert(hash(tb1) != hash(tb3))
    }
  }

object ElaborationCacheSpec extends TestSuite:
  private def newCachePath(prefix: String): Path =
    Files.createTempDirectory(prefix).resolve("elab-cache.bin")

  private def capture(top: Module, cachePath: Path): (String, Long) =
    val out = new ByteArrayOutputStream()
    val ps = new PrintStream(out)
    val start = System.nanoTime()
    try
      val elaborator = new Elaborator(BuildCache.at(cachePath), s => ps.println(s))
      elaborator.elaborate(top)
    finally
      ps.close()
    val elapsed = System.nanoTime() - start
    (out.toString, elapsed)

  private def count(log: String, token: String): Int =
    log.linesIterator.count(_.contains(token))

  private def runTop(cachePath: Path, nonWidth: Int): (String, Long) =
    capture(new CacheTop(nonWidth), cachePath)

  def printTime(times: Seq[Long]): Unit =
      val ns_to_us = 1000
      times.zipWithIndex.foreach( (t, i) =>
        println(s"$i th run ${t.floatValue / ns_to_us.floatValue} us")
      )

  def tests = Tests {
    test("cold run then warm run uses cache hits and warm is faster") {
      val cacheableInsts = CacheTop.cacheableInstanceCount
      val cachePath = newCachePath("hdl-cache-cold-warm")

      val cold = runTop(cachePath, nonWidth = 9)

// println(s"cold ${cold._1}")
      assert(count(cold._1, "Cache Hit")  == 0)

      val cold2 = runTop(cachePath, nonWidth = 9)
      val cold3 = runTop(cachePath, nonWidth = 9)
      val cold4 = runTop(cachePath, nonWidth = 9)
      val cold5 = runTop(cachePath, nonWidth = 9)
      val cold6 = runTop(cachePath, nonWidth = 9)
      val cold7 = runTop(cachePath, nonWidth = 9)

      val warm = runTop(cachePath, nonWidth = 9)
// println(s"warm ${warm._1}")

      assert(count(warm._1, "Cache Miss") == 0)

      printTime(Seq(cold._2, cold2._2, 
        cold3._2, cold4._2, cold5._2, cold6._2, cold7._2,
        warm._2))
    }

    test("unchanged module hits cache on warm run") {
      val cachePath = newCachePath("hdl-cache-unchanged")
      val p = SimpleParams(4)

      val cold = capture(new CacheableOneTop(p), cachePath)
      assert(count(cold._1, "Cache Hit") == 0)
      assert(count(cold._1, "Cache Miss") == 1)

      val warm = capture(new CacheableOneTop(p), cachePath)
      assert(count(warm._1, "Cache Hit") == 1)
      assert(count(warm._1, "Cache Miss") == 0)

      printTime(Seq(cold._2, warm._2))
    }

    test("same module with different parameters misses the cache") {
      val cachePath = newCachePath("hdl-cache-changed-body")
      val p = SimpleParams(5)

      val first = capture(new CacheableOneTop(p), cachePath)
      assert(count(first._1, "Cache Hit") == 0)
      assert(count(first._1, "Cache Miss") == 1)

      val changed = capture(new CacheableTwoTop(p), cachePath)
      assert(count(changed._1, "Cache Hit") == 0)
      assert(count(changed._1, "Cache Miss") == 1)

      val changedWarm = capture(new CacheableTwoTop(p), cachePath)
      assert(count(changedWarm._1, "Cache Hit") == 1)
      assert(count(changedWarm._1, "Cache Miss") == 0)

      printTime(Seq(first._2, changed._2, changedWarm._2))
    }

    test("structural changes within branch give mixed hits and misses") {
      val cachePath = newCachePath("hdl-cache-branch-change")
      val cold = capture(new CacheBranchV1Top(9), cachePath)
      assert(count(cold._1, "Cache Hit") == 0)
// assert(count(cold._1, "Cache Miss") == 3)

      val warm = capture(new CacheBranchV2Top(9), cachePath)
      assert(count(warm._1, "Cache Hit") == 2)
      assert(count(warm._1, "Cache Miss") == 2)

      printTime(Seq(cold._2, warm._2))
    }
  }
