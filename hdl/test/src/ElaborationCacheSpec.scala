package hdl

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
    val right = Module(new CacheBranch(nonWidth + 1, 1))
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

      assert(count(cold._1, "Cache Hit")  == 0)
      assert(count(cold._1, "Cache Miss") == cacheableInsts)

      val cold2 = runTop(cachePath, nonWidth = 9)
      val cold3 = runTop(cachePath, nonWidth = 9)
      val cold4 = runTop(cachePath, nonWidth = 9)
      val cold5 = runTop(cachePath, nonWidth = 9)
      val cold6 = runTop(cachePath, nonWidth = 9)
      val cold7 = runTop(cachePath, nonWidth = 9)

      val warm = runTop(cachePath, nonWidth = 9)
      assert(count(warm._1, "Cache Hit")  == cacheableInsts)
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
      assert(count(cold._1, "Cache Miss") == 3)

      val warm = capture(new CacheBranchV2Top(9), cachePath)
      assert(count(warm._1, "Cache Hit") == 2)
      assert(count(warm._1, "Cache Miss") == 2)

      printTime(Seq(cold._2, warm._2))
    }
  }
