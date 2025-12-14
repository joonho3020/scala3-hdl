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

  def icacheFetchInstCount: Int = icacheFetchBytes / instBytes

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

case class FetchIf(insts: Vec[Valid[UOp]]) extends Bundle[FetchIf]

object FetchIf:
  def apply(p: CoreParams): FetchIf =
    FetchIf(
      insts = Vec(Seq.fill(p.coreWidth)(Valid(UOp(p))))
    )

case class FrontendIf(icache: ICacheIf, core: FetchIf) extends Bundle[FrontendIf]

object FrontendIf:
  def apply(p: CoreParams): FrontendIf =
    FrontendIf(
      icache = ICacheIf(p),
      core = FetchIf(p)
    )

trait CoreCacheable(p: CoreParams) extends CacheableModule:
  this: Module =>
  type ElabParams = CoreParams
  given stableHashElabParams: StableHash[CoreParams] = summon[StableHash[CoreParams]]
  def elabParams: CoreParams = p

class Frontend(p: CoreParams) extends Module with CoreCacheable(p):
  val io = IO(FrontendIf(p))
  body {
    val s0_vpc = WireInit(0.U(p.pcBits.W))

    // Stage 0
    // - i$ tag lookup

    io.icache.req.valid := true.B
    io.icache.req.bits.addr := s0_vpc

    // Stage 1 - todo
    // - branch prediction & s0_vpc redirects
    // - address translation
    // - i$ tag matching
    val s1_vpc = RegNext(s0_vpc)
    val s1_valid = Reg(Bool())
    s1_valid := DontCare

    when (s1_valid) {
      s0_vpc := p.nextFetch(s1_vpc)
    }

    // Stage 2 - icache resp
    val s2_vpc = RegNext(s1_vpc)
    val s2_valid = Reg(Bool())
    val s2_fetch_mask = p.fetchMask(s2_vpc)
    s2_valid := DontCare

    io.core.insts.zipWithIndex.foreach((uop, idx) => {
// uop.valid := io.icache.resp.insts(idx).valid && s2_fetch_mask(idx)
      uop.valid := s2_valid && io.icache.resp.insts(idx).valid
      uop.bits.inst := io.icache.resp.insts(idx).bits
      uop.bits.pc   := s2_vpc + (p.instBytes * idx).U
    })
  }
