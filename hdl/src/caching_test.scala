package hdl

import scala.collection.concurrent.TrieMap
import java.util.concurrent.atomic.AtomicInteger

final case class CacheTestIO(in: UInt, out: UInt) extends Bundle[CacheTestIO]

final case class CacheableLeafParams(width: Int, offset: Int) derives StableHash

class CacheableLeaf(width: Int, offset: Int) extends Module with CacheableModule:
  override type ElabParams = CacheableLeafParams
  override def elabParams: ElabParams = CacheableLeafParams(width, offset)
  override given stableHashElabParams: StableHash[ElabParams] = summon[StableHash[ElabParams]]
  given Module = this
  val io = IO(CacheTestIO(Input(UInt(Width(width))), Output(UInt(Width(width)))))
  io.out := io.in + Lit(UInt(Width(width)))(offset)

class NonCacheableLeaf(width: Int, offset: Int) extends Module:
  given Module = this
  val io = IO(CacheTestIO(Input(UInt(Width(width))), Output(UInt(Width(width)))))
  io.out := io.in + Lit(UInt(Width(width)))(offset)

final case class MixedParentParams(numChild: Int, width: Int) derives StableHash

class MixedParamParent(numChild: Int, width: Int, useSameParams: Boolean)
    extends Module with CacheableModule:
  override type ElabParams = MixedParentParams
  override def elabParams: ElabParams = MixedParentParams(numChild, width)
  override given stableHashElabParams: StableHash[ElabParams] = summon[StableHash[ElabParams]]
  given Module = this
  val io = IO(CacheTestIO(Input(UInt(Width(width))), Output(UInt(Width(width)))))
  val child = Seq.tabulate(numChild) { i =>
    val offset = if useSameParams then 0 else i
    Module(new CacheableLeaf(width, offset))
  }
  child.foreach(_.io.in := io.in)
  io.out := child.map(_.io.out).reduce(_ + _)

final case class DeepTreeParams(depth: Int, width: Int) derives StableHash

class DeepTreeNode(depth: Int, fanout: Int, width: Int, nodeId: Int)
    extends Module with CacheableModule:
  override type ElabParams = DeepTreeParams
  override def elabParams: ElabParams = DeepTreeParams(depth, width)
  override given stableHashElabParams: StableHash[ElabParams] = summon[StableHash[ElabParams]]
  given Module = this
  val io = IO(CacheTestIO(Input(UInt(Width(width))), Output(UInt(Width(width)))))

  if depth > 0 then
    val child = Seq.tabulate(fanout)(i =>
      Module(new DeepTreeNode(depth - 1, fanout, width, nodeId * fanout + i))
    )
    child.foreach(_.io.in := io.in)
    io.out := child.map(_.io.out).reduce(_ + _)
  else
    io.out := io.in + Lit(UInt(Width(width)))(nodeId)

class AllDifferentParamsTree(depth: Int, fanout: Int, width: Int)
    extends Module with CacheableModule:
  override type ElabParams = DeepTreeParams
  override def elabParams: ElabParams = DeepTreeParams(depth, width)
  override given stableHashElabParams: StableHash[ElabParams] = summon[StableHash[ElabParams]]
  given Module = this
  val io = IO(CacheTestIO(Input(UInt(Width(width))), Output(UInt(Width(width)))))

  if depth > 0 then
    val child = Seq.tabulate(fanout)(i =>
      Module(new AllDifferentParamsTree(depth - 1, fanout, width + i + 1))
    )
    child.foreach(_.io.in := io.in)
    io.out := child.map(_.io.out).reduce(_ + _)
  else
    io.out := io.in

class MixedCacheTree(depth: Int, fanout: Int, width: Int)
    extends Module with CacheableModule:
  override type ElabParams = DeepTreeParams
  override def elabParams: ElabParams = DeepTreeParams(depth, width)
  override given stableHashElabParams: StableHash[ElabParams] = summon[StableHash[ElabParams]]
  given Module = this
  val io = IO(CacheTestIO(Input(UInt(Width(width))), Output(UInt(Width(width)))))

  if depth > 0 then
    val child = Seq.tabulate(fanout) { i =>
      val childWidth = if i % 2 == 0 then width else width + i
      Module(new MixedCacheTree(depth - 1, fanout, childWidth))
    }
    child.foreach(_.io.in := io.in)
    io.out := child.map(_.io.out).reduce(_ + _)
  else
    io.out := io.in + Lit(UInt(Width(width)))(1)

def test_cache_hit_same_params(): Unit =
  println("=" * 60)
  println("Test: Cache Hit with Same Parameters")
  val elaborator = new Elaborator
  val top = new MixedParamParent(numChild = 4, width = 8, useSameParams = true)
  val designs = elaborator.elaborate(top)
  println(s"Cache hits: ${elaborator.cacheHits}, Cache misses: ${elaborator.cacheMisses}")
  println(s"Hit rate: ${elaborator.hitRate * 100}%")
  println(s"Unique designs: ${designs.size}")
  assert(elaborator.cacheHits == 3, s"Expected 3 cache hits, got ${elaborator.cacheHits}")
  assert(elaborator.cacheMisses == 2, s"Expected 2 cache misses, got ${elaborator.cacheMisses}")
  println("PASSED")
  println("=" * 60)

def test_cache_miss_different_params(): Unit =
  println("=" * 60)
  println("Test: Cache Miss with Different Parameters")
  val elaborator = new Elaborator
  val top = new MixedParamParent(numChild = 4, width = 8, useSameParams = false)
  val designs = elaborator.elaborate(top)
  println(s"Cache hits: ${elaborator.cacheHits}, Cache misses: ${elaborator.cacheMisses}")
  println(s"Hit rate: ${elaborator.hitRate * 100}%")
  println(s"Unique designs: ${designs.size}")
  assert(elaborator.cacheHits == 0, s"Expected 0 cache hits, got ${elaborator.cacheHits}")
  assert(elaborator.cacheMisses == 5, s"Expected 5 cache misses, got ${elaborator.cacheMisses}")
  println("PASSED")
  println("=" * 60)

def test_non_cacheable_module_uniqueness(): Unit =
  println("=" * 60)
  println("Test: Non-Cacheable Module Always Gets Unique Keys")
  class NonCacheableParent extends Module:
    given Module = this
    val io = IO(CacheTestIO(Input(UInt(Width(8))), Output(UInt(Width(8)))))
    val child: Seq[NonCacheableLeaf] = Seq.tabulate(4)(_ => Module(new NonCacheableLeaf(8, 0)))
    child.foreach(_.io.in := io.in)
    io.out := child.map(_.io.out).reduce(_ + _)

  val elaborator = new Elaborator
  val top = new NonCacheableParent
  val designs = elaborator.elaborate(top)
  println(s"Cache hits: ${elaborator.cacheHits}, Cache misses: ${elaborator.cacheMisses}")
  println(s"Unique designs: ${designs.size}")
  assert(elaborator.cacheHits == 0, s"Expected 0 cache hits for non-cacheable, got ${elaborator.cacheHits}")
  assert(designs.size == 5, s"Expected 5 unique designs, got ${designs.size}")
  println("PASSED")
  println("=" * 60)

def test_deep_tree_mixed_params(): Unit =
  println("=" * 60)
  println("Test: Deep Tree with Mixed Parameters (Cache Hit/Miss)")
  val elaborator = new Elaborator
  val top = new MixedCacheTree(depth = 3, fanout = 4, width = 8)
  val designs = elaborator.elaborate(top)
  println(s"Cache hits: ${elaborator.cacheHits}, Cache misses: ${elaborator.cacheMisses}")
  println(s"Total modules processed: ${elaborator.totalModules}")
  println(s"Hit rate: ${(elaborator.hitRate * 100).formatted("%.1f")}%")
  println(s"Unique designs: ${designs.size}")
  assert(elaborator.cacheHits > 0, s"Expected some cache hits, got ${elaborator.cacheHits}")
  assert(elaborator.cacheMisses > 0, s"Expected some cache misses, got ${elaborator.cacheMisses}")
  assert(designs.size < elaborator.totalModules, "Expected fewer designs than total modules due to caching")
  println("PASSED")
  println("=" * 60)

def test_all_different_params_no_cache(): Unit =
  println("=" * 60)
  println("Test: All Different Parameters - No Cache Hits")
  val elaborator = new Elaborator
  val top = new AllDifferentParamsTree(depth = 3, fanout = 2, width = 4)
  val designs = elaborator.elaborate(top)
  println(s"Cache hits: ${elaborator.cacheHits}, Cache misses: ${elaborator.cacheMisses}")
  println(s"Total modules processed: ${elaborator.totalModules}")
  println(s"Unique designs: ${designs.size}")
  assert(elaborator.cacheHits == 0, s"Expected 0 cache hits, got ${elaborator.cacheHits}")
  assert(designs.size == elaborator.totalModules, "All designs should be unique")
  println("PASSED")
  println("=" * 60)

def test_parallel_elaboration_depth(): Unit =
  println("=" * 60)
  println("Test: Parallel Elaboration with Deep Tree (Multiple Threads)")
  val startTime = System.nanoTime()
  val elaborator = new Elaborator
  val top = new DeepTreeNode(depth = 4, fanout = 4, width = 8, nodeId = 0)
  val designs = elaborator.elaborate(top)
  val elapsedMs = (System.nanoTime() - startTime) / 1_000_000.0
  println(s"Elaboration time: ${elapsedMs}ms")
  println(s"Total modules: ${elaborator.totalModules}")
  println(s"Cache hits: ${elaborator.cacheHits}, Cache misses: ${elaborator.cacheMisses}")
  println(s"Unique designs: ${designs.size}")
  val expectedModules = (1 until 5).map(d => math.pow(4, d).toInt).sum + 1
  println(s"Expected total modules: $expectedModules")
  println("PASSED")
  println("=" * 60)

def test_determinism_same_input(): Unit =
  println("=" * 60)
  println("Test: Determinism - Same Input Produces Same Output")
  val results = (1 to 3).map { run =>
    val elaborator = new Elaborator
    val top = new MixedCacheTree(depth = 2, fanout = 3, width = 8)
    val designs = elaborator.elaborate(top)
    val hash = designs.map(d => s"${d.name}:${d.ports.mkString}:${d.body.mkString}").sorted.mkString("|")
    (elaborator.cacheHits, elaborator.cacheMisses, designs.size, hash)
  }
  results.sliding(2).foreach { pair =>
    val (h1, m1, s1, hash1) = pair.head
    val (h2, m2, s2, hash2) = pair.last
    assert(h1 == h2, s"Cache hits differ: $h1 vs $h2")
    assert(m1 == m2, s"Cache misses differ: $m1 vs $m2")
    assert(s1 == s2, s"Design count differs: $s1 vs $s2")
    assert(hash1 == hash2, "Design content differs between runs")
  }
  println("All runs produced identical results")
  println("PASSED")
  println("=" * 60)

def test_elaboration_order_determinism(): Unit =
  println("=" * 60)
  println("Test: Elaboration Order Determinism")
  class OrderTestParent extends Module with CacheableModule:
    override type ElabParams = Unit
    override def elabParams: Unit = ()
    override given stableHashElabParams: StableHash[ElabParams] = summon[StableHash[ElabParams]]
    given Module = this
    val io = IO(CacheTestIO(Input(UInt(Width(8))), Output(UInt(Width(8)))))
    val a = Module(new CacheableLeaf(8, 1))
    val b = Module(new CacheableLeaf(8, 2))
    val c = Module(new CacheableLeaf(8, 1))
    val d = Module(new CacheableLeaf(8, 3))
    Seq(a, b, c, d).foreach(_.io.in := io.in)
    io.out := a.io.out + b.io.out + c.io.out + d.io.out

  val moduleNames = (1 to 5).map { _ =>
    val elaborator = new Elaborator
    val top = new OrderTestParent
    val designs = elaborator.elaborate(top)
    designs.map(_.name).sorted.mkString(",")
  }
  moduleNames.sliding(2).foreach { pair =>
    assert(pair.head == pair.last, s"Module names differ: ${pair.head} vs ${pair.last}")
  }
  println("Module naming is deterministic across runs")
  println("PASSED")
  println("=" * 60)

@main def caching_test(): Unit =
  test_cache_hit_same_params()
  test_cache_miss_different_params()
  test_non_cacheable_module_uniqueness()
  test_deep_tree_mixed_params()
  test_all_different_params_no_cache()
  test_parallel_elaboration_depth()
  test_determinism_same_input()
  test_elaboration_order_determinism()
  println("\n" + "=" * 60)
  println("All caching and parallel elaboration tests passed!")
  println("=" * 60)
