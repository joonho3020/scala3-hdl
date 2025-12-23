package riscv_ooo

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
  uops: Vec[Decoupled[UOp]],
  bpu_update: Valid[BPUUpdate]
) extends Bundle[FrontendIf]

object FrontendIf:
  def apply(p: CoreParams): FrontendIf =
    FrontendIf(
      mem = MagicMemIf(p),
      redirect = Flipped(RedirectIf(p)),
      uops = Vec.fill(p.coreWidth)(Decoupled(UOp(p))),
      bpu_update = Flipped(Valid(BPUUpdate(p)))
    )

class Frontend(p: CoreParams) extends Module with CoreCacheable(p):
  val io = IO(FrontendIf(p))
  body {
    dontTouch(io)

    val bpu = Module(new BranchPredictor(p))

// bpu.io.flush := io.redirect.valid
    bpu.io.flush := false.B
    bpu.io.update := io.bpu_update
    bpu.io.req.valid := false.B
    bpu.io.req.bits.pc := 0.U(p.pcBits.W)

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
    io.mem.req.valid := icache.mem.req.valid
    io.mem.req.bits := icache.mem.req.bits
    icache.mem.req.ready := io.mem.req.ready

    icache.mem.resp := io.mem.resp

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

    bpu.io.req.valid := s1_valid && !f1_clear
    bpu.io.req.bits.pc := p.fetchAlign(s1_vpc)
    dontTouch(bpu.io.resp)

    val f1_fetch_mask  = p.fetchMask(s1_vpc)
    val f1_next_fetch  = p.nextFetch(s1_vpc)
    val f1_taken_hits = bpu.io.resp.zipWithIndex.map((x, i) => {
        x.valid && x.bits.hit && x.bits.taken && f1_fetch_mask(i).asBool
    })
    val f1_taken_hit  = f1_taken_hits.reduce(_ || _)
    val f1_taken_hit_idx = PriorityEncoder(Cat(f1_taken_hits.reverse))
    val f1_pred_target = Mux(f1_taken_hit, bpu.io.resp(f1_taken_hit_idx).bits.target, f1_next_fetch)

    when (s1_valid && !f1_clear) {
      s0_vpc := f1_pred_target
      s0_valid := true.B
    }

    // -----------------------------------------------------------------------
    // Stage 2 - icache resp
    // -----------------------------------------------------------------------
    val s2_vpc = RegNext(s1_vpc)
    val s2_valid = RegInit(false.B)
    val s2_fetch_mask = Wire(UInt())
    val f2_clear = WireInit(false.B)

    val s2_taken_hit_idx = RegNext(f1_taken_hit_idx)
    val s2_taken_hit  = RegNext(f1_taken_hit)
    val s2_pred_target = RegNext(f1_pred_target)

    s2_valid := s1_valid && !f1_clear
    s2_fetch_mask := p.fetchMask(s2_vpc)

    dontTouch(s2_vpc)
    dontTouch(s2_valid)
    dontTouch(s2_fetch_mask)
    dontTouch(f2_clear)

    val fetch_bundle = Wire(FetchBundle(p))
    fetch_bundle := DontCare
    fetch_bundle.pc := s2_vpc

    val brjmp = Wire(Vec.fill(p.coreWidth)(BrJmpSignal()))
    val s2_is_taken = Wire(Vec.fill(p.coreWidth)(Bool()))
    dontTouch(brjmp)
    dontTouch(s2_is_taken)
    dontTouch(fetch_bundle)

    val s2_prev_brjmp_taken = Wire(Vec.fill(p.coreWidth)(Bool()))
    for (i <- 0 until p.coreWidth) {
      val inst = fetch_bundle.insts(i)
      inst.bits  := ic.s2_insts(i)

      BrJmpSignal.predecode(brjmp(i), inst.bits)
      val brjmp_taken = s2_taken_hit &&
                        (brjmp(i).is_jal  ||
                         brjmp(i).is_jalr ||
                         (brjmp(i).is_br && (i.U === s2_taken_hit_idx)))
      s2_is_taken(i) := brjmp_taken && s2_fetch_mask(i).asBool

      if i == 0 then
        s2_prev_brjmp_taken(i) := s2_is_taken(i)
        inst.valid := s2_valid && s2_fetch_mask(i).asBool
      else
        s2_prev_brjmp_taken(i) := s2_prev_brjmp_taken(i-1) || s2_is_taken(i)
        inst.valid := s2_valid && s2_fetch_mask(i).asBool && !s2_prev_brjmp_taken(i-1)
    }

    fetch_bundle.next_pc.valid := s2_valid && s2_is_taken.reduce(_ || _)
    fetch_bundle.next_pc.bits  := s2_pred_target

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
    } .elsewhen (s2_valid && ic.s2_valid) {
      // Cache hit
      when (fetch_bundle.next_pc.valid) {
        s0_vpc := s2_pred_target
        s0_valid := true.B
        f1_clear := true.B
      }
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
