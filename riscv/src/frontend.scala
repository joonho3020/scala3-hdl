package riscv

import hdl._

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
      resp = Input(Valid(ICacheRespIf(
        insts = Vec.fill(p.icacheFetchInstCount)(UInt(p.instBits.W))
      )))
    )

case class FetchBundle(
  pc: UInt,
  insts: Vec[Valid[UInt]],
) extends Bundle[FetchBundle]

object FetchBundle:
  def apply(p: CoreParams): FetchBundle =
    FetchBundle(
      pc = UInt(p.xlenBits.W),
      insts = Vec(Seq.fill(p.coreWidth)(Valid(UInt(32.W))))
    )

case class FrontendIf(icache: ICacheIf, uops: Vec[Decoupled[UOp]]) extends Bundle[FrontendIf]

object FrontendIf:
  def apply(p: CoreParams): FrontendIf =
    FrontendIf(
      icache = ICacheIf(p),
      uops = Vec.fill(p.coreWidth)(Decoupled(UOp(p)))
    )

class Frontend(p: CoreParams) extends Module with CoreCacheable(p):
  val io = IO(FrontendIf(p))
  body {
    val f3_ready = Wire(Bool())

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

    val fetch_bundle = Wire(FetchBundle(p))
    fetch_bundle := DontCare
    fetch_bundle.pc := s2_vpc
    fetch_bundle.insts.zipWithIndex.foreach((inst_val, idx) => {
      inst_val.valid := s2_valid && io.icache.resp.valid && s2_fetch_mask(idx).asBool
      inst_val.bits  := io.icache.resp.bits.insts(idx)
    })

    val fb = Module(new FetchBuffer(p, depth = 4))
    fb.io.clear := DontCare

    fb.io.enq.valid := s2_valid && io.icache.resp.valid
    fb.io.enq.bits  := fetch_bundle
    f3_ready := fb.io.enq.ready

    when (s2_valid && io.icache.resp.valid && !f3_ready) {
      // Fetch buffer full, redirect
      s0_vpc := s2_vpc
    }

    io.uops.zip(fb.io.deq).foreach((io_uop, fb_uop) => {
      io_uop.valid := fb_uop.valid
      io_uop.bits  := fb_uop.bits
      fb_uop.ready := io_uop.ready
    })
  }
