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
  next_pc: Valid[UInt],
) extends Bundle[FetchBundle]

object FetchBundle:
  def apply(p: CoreParams): FetchBundle =
    FetchBundle(
      pc = UInt(p.xlenBits.W),
      insts = Vec(Seq.fill(p.coreWidth)(Valid(UInt(32.W)))),
      next_pc = Valid(UInt(p.pcBits.W))
    )

case class FrontendIf(
  mem: MagicMemIf,
  redirect: RedirectIf,
  uops: Vec[Decoupled[UOp]]
) extends Bundle[FrontendIf]

object FrontendIf:
  def apply(p: CoreParams): FrontendIf =
    FrontendIf(
      mem = MagicMemIf(p),
      redirect = Flipped(RedirectIf(p)),
      uops = Vec.fill(p.coreWidth)(Decoupled(UOp(p)))
    )

class Frontend(p: CoreParams) extends Module with CoreCacheable(p):
  val io = IO(FrontendIf(p))
  body {
    dontTouch(io)

    val f3_ready = Wire(Bool())

    val s0_vpc   = WireInit(0.U(p.pcBits.W))
    val s0_valid = WireInit(false.B)

    dontTouch(s0_vpc)
    dontTouch(s0_valid)

    // -----------------------------------------------------------------------
    // Stage 0
    // - i$ tag lookup
    // -----------------------------------------------------------------------
    val icache = Module(new ICache(p)).io
    io.mem := icache.mem

    val ic = icache.core
    ic.s0_vaddr.valid := s0_valid
    ic.s0_vaddr.bits  := s0_vpc

    // -----------------------------------------------------------------------
    // Stage 1 - todo
    // - branch prediction & s0_vpc redirects
    // - address translation
    // - i$ tag matching
    // -----------------------------------------------------------------------
    val s1_vpc = RegNext(s0_vpc)
    val s1_valid = RegInit(false.B)
    val f1_clear = WireInit(false.B)
    s1_valid := s0_valid

    dontTouch(s1_vpc)
    dontTouch(s1_valid)
    dontTouch(f1_clear)

    ic.s1_paddr.valid := s1_valid
    ic.s1_paddr.bits  := s1_vpc // no virtual memory for now
    ic.s1_kill := f1_clear

    when (s1_valid) {
      s0_vpc := p.nextFetch(s1_vpc) // Always not-taken
      s0_valid := true.B
    }

    // -----------------------------------------------------------------------
    // Stage 2 - icache resp
    // -----------------------------------------------------------------------
    val s2_vpc = RegNext(s1_vpc)
    val s2_valid = RegInit(false.B)
    val s2_fetch_mask = Wire(UInt())
    val f2_clear = WireInit(false.B)

    s2_valid := s1_valid && !f1_clear
    s2_fetch_mask := p.fetchMask(s2_vpc)

    dontTouch(s2_vpc)
    dontTouch(s2_valid)
    dontTouch(s2_fetch_mask)
    dontTouch(f2_clear)

    val fetch_bundle = Wire(FetchBundle(p))
    fetch_bundle := DontCare
    fetch_bundle.pc := s2_vpc
    fetch_bundle.insts.zipWithIndex.foreach((inst_val, idx) => {
      inst_val.valid := s2_valid && ic.s2_valid && s2_fetch_mask(idx).asBool
      inst_val.bits  := ic.s2_insts(idx)
    })

    // TODO: use this to add predicted branch target on a predicted-taken branch
    fetch_bundle.next_pc.valid := false.B
    fetch_bundle.next_pc.bits  := DontCare

    ic.s2_kill := f2_clear

    val fb = Module(new FetchBuffer(p, depth = 4))
    fb.io.clear := f2_clear

    fb.io.enq.valid := s2_valid && ic.s2_valid
    fb.io.enq.bits  := fetch_bundle
    f3_ready := fb.io.enq.ready

    io.uops.zip(fb.io.deq).foreach((io_uop, fb_uop) => {
      io_uop.valid := fb_uop.valid
      io_uop.bits  := fb_uop.bits
      fb_uop.ready := io_uop.ready
    })

    when (s2_valid && !ic.s2_valid) {
      s0_vpc := s2_vpc
      s0_valid := true.B
      f1_clear := true.B
    } .elsewhen (s2_valid && ic.s2_valid && !f3_ready) {
      s0_vpc := s2_vpc
      s0_valid := true.B
      f1_clear := true.B
    } .elsewhen (s1_valid && ic.s2_valid) {
      // Cache hit
    }

    when (io.redirect.valid) {
      s0_vpc := io.redirect.target
      s0_valid := true.B
      f1_clear := true.B
      f2_clear := true.B
      fb.io.clear := true.B
    }

    val jump_to_reset = RegInit(true.B)

    when (jump_to_reset) {
      s0_valid := true.B
      s0_vpc   := 0x80000000.U
      fb.io.clear := true.B
      f1_clear := true.B
      f2_clear := true.B
      jump_to_reset := false.B
    }
  }
