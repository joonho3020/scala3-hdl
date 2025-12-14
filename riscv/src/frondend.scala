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

  def xlenBytes: Int =
    xlenBits / 8

  def icacheFetchInstCount: Int =
    icacheFetchBytes / xlenBytes

case class ICacheReqIf(addr: UInt) extends Bundle[ICacheReqIf]
case class ICacheRespIf(insts: Vec[UInt]) extends Bundle[ICacheRespIf]

case class ICacheIf(
  req: Valid[ICacheReqIf],
  resp: Valid[ICacheRespIf]) extends Bundle[ICacheIf]

object ICacheIf:
  def apply(p: CoreParams): ICacheIf =
    ICacheIf(
      req = Output(Valid(ICacheReqIf(
        addr = UInt(p.pcBits.W)
      ))),
      resp = Flipped(Valid(ICacheRespIf(
        insts = Vec(Seq.fill(p.icacheFetchInstCount)(UInt(p.xlenBits.W)))
      )))
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
    next_pc := next_pc + 4.U

    io.icache.req.valid := true.B
    io.icache.req.bits.addr := next_pc

    // Stage 1 - todo: branch prediction & next_pc redirects
// val pc_1 = RegNext(next_pc)

    io.core.insts.zipWithIndex.foreach((uop, idx) => {
      uop.valid := io.icache.resp.valid
      // uop.bits.inst := io.icache.resp.bits.insts(idx)
    })

  }
