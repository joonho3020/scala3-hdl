package riscv

import hdl._

case class ICacheIf(
  s0_vaddr: Valid[UInt],

  s1_kill:  Bool,
  s1_paddr: Valid[UInt],

  s2_kill:  Bool,
  s2_valid: Bool,
  s2_insts: Vec[UInt],
) extends Bundle[ICacheIf]

object ICacheIf:
  def apply(p: CoreParams): ICacheIf =
    ICacheIf(
      s0_vaddr = Output(Valid(UInt(p.pcBits.W))),

      s1_kill  = Output(Bool()),
      s1_paddr = Output(Valid(UInt(p.pcBits.W))),

      s2_kill  = Output(Bool()),
      s2_valid = Input(Bool()),
      s2_insts = Flipped(Vec.fill(p.icacheFetchInstCount)(UInt(p.instBits.W)))
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

case class FrontendIf(mem: MagicMemIf, uops: Vec[Decoupled[UOp]]) extends Bundle[FrontendIf]

object FrontendIf:
  def apply(p: CoreParams): FrontendIf =
    FrontendIf(
      mem = MagicMemIf(p),
      uops = Vec.fill(p.coreWidth)(Decoupled(UOp(p)))
    )

class Frontend(p: CoreParams) extends Module:
  val io = IO(FrontendIf(p))
  body {

    val f3_ready = Wire(Bool())

    val s0_vpc = WireInit(0.U(p.pcBits.W))

    // Stage 0
    // - i$ tag lookup
    val icache = Module(new ICache(p)).io
    val ic = icache.core

    io.mem := icache.mem

    ic.s0_vaddr.valid := true.B
    ic.s0_vaddr.bits  := s0_vpc

    // Stage 1 - todo
    // - branch prediction & s0_vpc redirects
    // - address translation
    // - i$ tag matching
    val s1_vpc = RegNext(s0_vpc)
    val s1_valid = Reg(Bool())
    s1_valid := DontCare

    ic.s1_paddr.valid := s1_valid
    ic.s1_paddr.bits  := s1_vpc // no virtual memory for now
    ic.s1_kill := false.B

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
      inst_val.valid := s2_valid && ic.s2_valid && s2_fetch_mask(idx).asBool
      inst_val.bits  := ic.s2_insts(idx)
    })

    ic.s2_kill := false.B

    val fb = Module(new FetchBuffer(p, depth = 4))
    fb.io.clear := DontCare

    fb.io.enq.valid := s2_valid && ic.s2_valid
    fb.io.enq.bits  := fetch_bundle
    f3_ready := fb.io.enq.ready

    io.uops.zip(fb.io.deq).foreach((io_uop, fb_uop) => {
      io_uop.valid := fb_uop.valid
      io_uop.bits  := fb_uop.bits
      fb_uop.ready := io_uop.ready
    })

    when (s2_valid && ic.s2_valid && !f3_ready) {
      // Fetch buffer full, redirect
      s0_vpc := s2_vpc
    }
  }
