package riscv

import hdl._

case class CoreParams(
  pcBits: Int,
  xlenBits: Int,
  coreWidth: Int,
  cacheLineBytes: Int,
  icacheFetchBytes: Int,
  instBytes: Int = 4
) derives StableHash:
  assert((coreWidth & (coreWidth-1)) == 0)

  def xlenBytes: Int = xlenBits / 8

  def icacheFetchInstCount: Int = icacheFetchBytes / instBytes
  def instBits: Int = instBytes * 8

  def fetchWidth: Int = coreWidth
  def fetchBytes: Int = coreWidth * instBytes
  def coreInstBytes: Int = instBytes

  def fetchAlign(addr: UInt)(using m: Module) = ~(~addr | (fetchBytes-1).U)
  def blockAlign(addr: UInt)(using m: Module) = ~(~addr | (cacheLineBytes-1).U)
  def nextFetch(addr: UInt)(using m: Module) = fetchAlign(addr) + fetchBytes.U
  def fetchMask(addr: UInt)(using m: Module) = {
    val idx = addr(
      log2Ceil(fetchWidth)+log2Ceil(coreInstBytes)-1,
      log2Ceil(coreInstBytes))
    ((1 << fetchWidth)-1).U << idx
  }

trait CoreCacheable(p: CoreParams) extends CacheableModule:
  this: Module =>
  type ElabParams = CoreParams
  given stableHashElabParams: StableHash[CoreParams] = summon[StableHash[CoreParams]]
  def elabParams: CoreParams = p
