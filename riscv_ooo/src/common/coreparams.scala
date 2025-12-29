package riscv_ooo

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

case class ROBParams(
  numEntries: Int = 32
) derives StableHash

case class PRFParams(
  numEntries: Int = 64,
) derives StableHash

case class IssueQueueParams(
  numEntries: Int = 12,
) derives StableHash

case class BranchParams(
  inFlightBranches: Int = 4,
  ftqEntries: Int = 8,
) derives StableHash

case class CoreParams(
  debug: Boolean,
  pcBits: Int,
  xlenBits: Int,
  paddrBits: Int,

  fetchWidth: Int,
  issueWidth: Int,

  icacheFetchBytes: Int,
  instBytes: Int = 4,
  ic: ICacheParams,
  bpu: BPUParams = BPUParams(),
  aluPipes: Int,
  dc: DCacheParams,
  lsu: LSUParams,
  rob: ROBParams = ROBParams(),
  prf: PRFParams = PRFParams(),
  isq: IssueQueueParams = IssueQueueParams(),
  br: BranchParams = BranchParams(),
) derives StableHash:
  def xlenBytes: Int = xlenBits / 8

  def numInstsPerICacheLine = ic.cacheLineBytes / instBytes
  def icacheFetchInstCount: Int = icacheFetchBytes / instBytes
  def instBits: Int = instBytes * 8

  def coreWidth: Int = fetchWidth
  def fetchBytes: Int = fetchWidth * instBytes
  def coreInstBytes: Int = instBytes
  def memLineBytes: Int = ic.cacheLineBytes
  def memLineWords: Int = memLineBytes / 4
  def retireWidth: Int = issueWidth

  require(retireWidth == issueWidth)

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

  def pRegIdxBits: Int = log2Ceil(nPhysicalRegs + 1)
  def coreWidthBits: Int = log2Ceil(coreWidth + 1)

  def robEntries: Int = rob.numEntries
  def robRows: Int = rob.numEntries / coreWidth
  def robIdxBits: Int = log2Ceil(rob.numEntries + 1)
  def robRowIdxBits: Int = log2Ceil(robRows)

  def nPhysicalRegs: Int = prf.numEntries

  def numBranchTags: Int = br.inFlightBranches
  def branchTagBits: Int = numBranchTags
  def ftqIdxBits: Int = log2Ceil(br.ftqEntries + 1)

  // TODO: need better uarching to prevent prfReadPort count from
  // increasing linearly w.r.t the core width.
  // Probably there is a way of banking it & multiplexing the physical ports
  // across logical ports????
  def prfReadPorts: Int = issueWidth * 2
  def prfWritePorts: Int = issueWidth

trait CoreCacheable(p: CoreParams) extends CacheableModule:
  this: Module =>
  type ElabParams = CoreParams
  given stableHashElabParams: StableHash[CoreParams] = summon[StableHash[CoreParams]]
  def elabParams: CoreParams = p
