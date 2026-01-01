package riscv_inorder

import hdl.core._
import hdl.util._
import hdl.elaboration._


case class ICacheParams(
  nSets: Int = 64,
  nWays: Int = 4,
  cacheLineBytes: Int = 64
) derives StableHash

case class DCacheParams(
  nSets: Int = 64,
  nWays: Int = 4,
  cacheLineBytes: Int = 64,
  mshrs: Int = 2
) derives StableHash

case class BPUParams(
  bhtEntries: Int = 128,
  btbEntries: Int = 32,
  rasEntries: Int = 8
) derives StableHash

case class LSUParams(
  lqEntries: Int = 8,
  sqEntries: Int = 8
) derives StableHash

case class CoreParams(
  magic_mem_outstanding: Int,
  pcBits: Int,
  xlenBits: Int,
  paddrBits: Int,
  coreWidth: Int,
  icacheFetchBytes: Int,
  instBytes: Int = 4,
  ic: ICacheParams,
  bpu: BPUParams = BPUParams(),
  aluPipes: Int,
  dc: DCacheParams,
  lsu: LSUParams = LSUParams(),
) derives StableHash:
  def xlenBytes: Int = xlenBits / 8

  def numInstsPerICacheLine = ic.cacheLineBytes / instBytes
  def icacheFetchInstCount: Int = icacheFetchBytes / instBytes
  def instBits: Int = instBytes * 8

  def fetchWidth: Int = coreWidth
  def fetchBytes: Int = coreWidth * instBytes
  def coreInstBytes: Int = instBytes
  def memLineBytes: Int = ic.cacheLineBytes
  def memLineWords: Int = memLineBytes / 4

  def fetchOffset(addr: UInt)(using m: Module) = addr & (fetchBytes-1).U
  def fetchAlign(addr: UInt)(using m: Module) = ~(~addr | (fetchBytes-1).U)
  def blockAlign(addr: UInt)(using m: Module) = ~(~addr | (ic.cacheLineBytes-1).U)
  def nextFetch(addr: UInt)(using m: Module) = fetchAlign(addr) + fetchBytes.U
  def fetchMask(addr: UInt)(using m: Module): UInt = {
    val idx = addr(
      log2Ceil(fetchWidth)+log2Ceil(coreInstBytes)-1,
      log2Ceil(coreInstBytes))
    ((1 << fetchWidth)-1).U << idx
  }

  assert((coreWidth & (coreWidth-1)) == 0)
  assert(icacheFetchInstCount == coreWidth)


trait CoreCacheable(p: CoreParams) extends CacheableModule:
  this: Module =>
  type ElabParams = CoreParams
  given stableHashElabParams: StableHash[CoreParams] = summon[StableHash[CoreParams]]
  def elabParams: CoreParams = p
