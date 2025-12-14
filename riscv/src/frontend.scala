package riscv

import hdl._

case class CoreParams(
  pcBits: Int,
  xlenBits: Int,
  coreWidth: Int,
  cacheLineBytes: Int,
  icacheFetchBytes: Int,
  instBytes: Int
) derives StableHash:

  def xlenBytes: Int = xlenBits / 8

  def icacheFetchInstCount: Int = icacheFetchBytes / xlenBytes

  def fetchWidth: Int = coreWidth
  def fetchBytes: Int = coreWidth * xlenBytes
  def coreInstBytes: Int = xlenBytes

  def fetchAlign(addr: UInt)(using m: Module) = ~(~addr | (fetchBytes-1).U)
  def blockAlign(addr: UInt)(using m: Module) = ~(~addr | (cacheLineBytes-1).U)
  def nextFetch(addr: UInt)(using m: Module) = fetchAlign(addr) + fetchBytes.U
  def fetchMask(addr: UInt)(using m: Module) = {
    val idx = addr.bits(log2Ceil(fetchWidth)+log2Ceil(coreInstBytes)-1, log2Ceil(coreInstBytes))
    ((1 << fetchWidth)-1).U << idx
  }

case class ICacheReqIf(addr: UInt) extends Bundle[ICacheReqIf]
case class ICacheRespIf(insts: Vec[Valid[UInt]]) extends Bundle[ICacheRespIf]

case class ICacheIf(
  req: Valid[ICacheReqIf],
  resp: ICacheRespIf) extends Bundle[ICacheIf]

object ICacheIf:
  def apply(p: CoreParams): ICacheIf =
    ICacheIf(
      req = Output(Valid(ICacheReqIf(
        addr = UInt(p.pcBits.W)
      ))),
      resp = Flipped(ICacheRespIf(
        insts = Vec(Seq.fill(p.icacheFetchInstCount)(Valid(UInt(p.xlenBits.W))))
      ))
    )

case class UOp(
  pc: UInt,
  inst: UInt) extends Bundle[UOp]

object UOp:
  def apply(p: CoreParams): UOp =
    UOp(
      pc = UInt(p.pcBits.W),
      inst = UInt(p.xlenBits.W))

case class CoreIf(insts: Vec[Valid[UOp]]) extends Bundle[CoreIf]

object CoreIf:
  def apply(p: CoreParams): CoreIf =
    CoreIf(
      insts = Vec(Seq.fill(p.coreWidth)(Valid(UOp(p))))
    )

case class FrontendIf(icache: ICacheIf, core: CoreIf) extends Bundle[FrontendIf]

object FrontendIf:
  def apply(p: CoreParams): FrontendIf =
    FrontendIf(
      icache = ICacheIf(p),
      core = CoreIf(p)
    )

trait CoreCacheable(p: CoreParams) extends CacheableModule:
  this: Module =>
  type ElabParams = CoreParams
  given stableHashElabParams: StableHash[CoreParams] = summon[StableHash[CoreParams]]
  def elabParams: CoreParams = p

class Frontend(p: CoreParams) extends Module with CoreCacheable(p):
  val io = IO(FrontendIf(p))
  body {
    val next_pc = Wire(UInt(p.pcBits.W))

    // Stage 0
    // - i$ tag lookup
    next_pc := p.nextFetch(next_pc)

    io.icache.req.valid := true.B
    io.icache.req.bits.addr := next_pc

    // Stage 1 - todo
    // - branch prediction & next_pc redirects
    // - address translation
    // - i$ tag matching
    val s1_pc = RegNext(next_pc)

    // Stage 2 - icache resp
    val s2_pc = RegNext(s1_pc)
    val s2_fetch_mask = p.fetchMask(s2_pc)

    io.core.insts.zipWithIndex.foreach((uop, idx) => {
// uop.valid := io.icache.resp.insts(idx).valid && s2_fetch_mask(idx)
      uop.valid := io.icache.resp.insts(idx).valid
      uop.bits  := io.icache.resp.insts(idx).bits
    })

  }
