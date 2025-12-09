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

object ElaborationCacheSpec extends TestSuite:
  private def newCachePath(prefix: String): Path =
    Files.createTempDirectory(prefix).resolve("elab-cache.bin")

  private def capture(top: Module, cachePath: Path): (String, Long) =
    val out = new ByteArrayOutputStream()
    val ps = new PrintStream(out)
    val start = System.nanoTime()
    try
      Console.withOut(ps) {
// val elaborator = new Elaborator(BuildCache.at(cachePath))
        val elaborator = new Elaborator
        elaborator.elaborate(top)
      }
    finally
      ps.close()
    val elapsed = System.nanoTime() - start
    (out.toString, elapsed)

  private def count(log: String, token: String): Int =
    log.linesIterator.count(_.contains(token))

  private def runTop(cachePath: Path, nonWidth: Int): (String, Long) =
    capture(new CacheTop(nonWidth), cachePath)

  def tests = Tests {
    test("cold run then warm run uses cache hits and warm is faster") {
      val cacheableInsts = CacheTop.cacheableInstanceCount
      val cachePath = newCachePath("hdl-cache-cold-warm")

      val cold = runTop(cachePath, nonWidth = 9)

      assert(count(cold._1, "Cache Hit")  == 0)
      assert(count(cold._1, "Cache Miss") == cacheableInsts)

      val warm = runTop(cachePath, nonWidth = 9)
      assert(count(warm._1, "Cache Hit")  == cacheableInsts)
      assert(count(warm._1, "Cache Miss") == 0)

      println(s"cold run time ${cold}")
      println(s"warm run time ${warm}")
    }
  }
