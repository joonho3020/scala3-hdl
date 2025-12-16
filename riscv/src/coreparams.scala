package riscv

import hdl._

case class ICacheParams(
  nSets: Int = 64,
  nWays: Int = 4,
  cacheLineBytes: Int = 64
) derives StableHash

case class CoreParams(
  pcBits: Int,
  xlenBits: Int,
  coreWidth: Int,
  icacheFetchBytes: Int,
  instBytes: Int = 4,
  ic: ICacheParams,
) derives StableHash:
  def xlenBytes: Int = xlenBits / 8

  def numInstsPerICacheLine = ic.cacheLineBytes / instBytes
  def icacheFetchInstCount: Int = icacheFetchBytes / instBytes
  def instBits: Int = instBytes * 8

  def fetchWidth: Int = coreWidth
  def fetchBytes: Int = coreWidth * instBytes
  def coreInstBytes: Int = instBytes

  def fetchAlign(addr: UInt)(using m: Module) = ~(~addr | (fetchBytes-1).U)
  def blockAlign(addr: UInt)(using m: Module) = ~(~addr | (ic.cacheLineBytes-1).U)
  def nextFetch(addr: UInt)(using m: Module) = fetchAlign(addr) + fetchBytes.U
  def fetchMask(addr: UInt)(using m: Module) = {
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
