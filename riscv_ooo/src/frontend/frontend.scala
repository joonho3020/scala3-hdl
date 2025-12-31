package riscv_ooo

import hdl.core._
import hdl.util._
import hdl.elaboration._
import riscv_inorder.Instructions._

case class FetchBundle(
  pc: UInt,
  insts: Vec[Valid[UInt]],

  ftq_idx: UInt,

  brjmp: BrJmpSignal,
  is_call: Bool,
  is_ret: Bool,

  ras_top: UInt,
  ras_ptr: UInt,
  ghist: UInt,
  lhist: UInt,

  /* Control flow instructions */
  cfi_mask: UInt,

  /* When set, cfi_idx is taken */
  cfi_taken: Bool,

  /* First taken instruction index within this fetch bundle */
  cfi_idx: UInt,

  /* Predicted next PC */
  target_pc: UInt,

) extends Bundle[FetchBundle]

object FetchBundle:
  def apply(p: CoreParams): FetchBundle =
    FetchBundle(
      pc = UInt(p.xlenBits.W),
      insts = Vec(Seq.fill(p.coreWidth)(Valid(UInt(32.W)))),
      ftq_idx = UInt(p.ftqIdxBits.W),

      brjmp = BrJmpSignal(),
      is_call = Bool(),
      is_ret = Bool(),

      ras_ptr = UInt(log2Ceil(p.bpu.rasEntries + 1).W),
      ras_top = UInt(p.pcBits.W),
      ghist = UInt(p.bpu.ghistBits.W),
      lhist = UInt(p.bpu.lhistBits.W),

      cfi_mask = UInt(p.coreWidth.W),
      cfi_taken    = Bool(),
      cfi_idx  = UInt(log2Ceil(p.coreWidth + 1).W),
      target_pc = UInt(p.pcBits.W),
    )

case class RedirectIf(
  valid:  Bool,
  target: UInt,
  ftq_idx: UInt,
  cfi_idx: UInt,
  taken: Bool,
) extends Bundle[RedirectIf]

object RedirectIf:
  def apply(p: CoreParams): RedirectIf =
    RedirectIf(
      valid  = Input(Bool()),
      target = Input(UInt(p.pcBits.W)),
      ftq_idx = Input(UInt(p.ftqIdxBits.W)),
      cfi_idx = Input(UInt(log2Ceil(p.coreWidth+1).W)),
      taken   = Input(Bool()),
    )

case class BranchCommitIf(
  valid: Bool,
  ftq_idx: UInt
) extends Bundle[BranchCommitIf]

object BranchCommitIf:
  def apply(p: CoreParams): BranchCommitIf =
    BranchCommitIf(
      valid = Input(Bool()),
      ftq_idx = Input(UInt(p.ftqIdxBits.W)),
    )

case class FrontendCoreIf(
  fetch_uops: Vec[Decoupled[UOp]],
  redirect: RedirectIf,
  commit:   BranchCommitIf
) extends Bundle[FrontendCoreIf]

object FrontendCoreIf:
  def apply(p: CoreParams): FrontendCoreIf =
    FrontendCoreIf(
      fetch_uops = Vec.fill(p.coreWidth)(Decoupled(UOp(p))),
      redirect = RedirectIf(p),
      commit = BranchCommitIf(p)
    )

case class FrontendIO(
  mem: MagicMemIf,
  core: FrontendCoreIf,
) extends Bundle[FrontendIO]

object FrontendIO:
  def apply(p: CoreParams): FrontendIO =
    FrontendIO(
      mem = MagicMemIf(p),
      core = FrontendCoreIf(p)
    )

class Frontend(p: CoreParams) extends Module with CoreCacheable(p):
  val io = IO(FrontendIO(p))
  body {
    dontTouch(io)

    val bpu = Module(new BranchPredictor(p))
    val ftq = Module(new FetchTargetQueue(p))

// bpu.io.flush := io.redirect.valid
    bpu.io.flush := false.B
    bpu.io.req.valid := false.B
    bpu.io.req.bits.pc := 0.U(p.pcBits.W)
    bpu.io.req.bits.cfi_mask := 0.U(p.coreWidth.W)
    bpu.io.req.bits.cfi_idx := 0.U
    bpu.io.req.bits.cfi_taken := false.B

    ftq.io.redirect.valid := io.core.redirect.valid
    ftq.io.redirect.target := io.core.redirect.target
    ftq.io.redirect.ftq_idx := io.core.redirect.ftq_idx
    ftq.io.redirect.cfi_idx := io.core.redirect.cfi_idx
    ftq.io.redirect.taken := io.core.redirect.taken

    ftq.io.commit.valid := io.core.commit.valid
    ftq.io.commit.ftq_idx := io.core.commit.ftq_idx


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
    val f1_taken_hits = Wire(Vec.fill(p.coreWidth)(Bool()))
    bpu.io.resp.zipWithIndex.map((x, i) => {
        f1_taken_hits(i) := x.valid && x.bits.hit && x.bits.taken && f1_fetch_mask(i).asBool
    })
    dontTouch(f1_taken_hits)
    val f1_taken_hit  = f1_taken_hits.reduce(_ || _)
    val f1_taken_hit_idx = PriorityEncoder(Cat(f1_taken_hits.reverse)).asWire
    dontTouch(f1_taken_hit_idx)
    val f1_pred_target = Mux(f1_taken_hit, bpu.io.resp(f1_taken_hit_idx).bits.target, f1_next_fetch)
    val f1_ghist = bpu.io.ghist
    val f1_lhist = bpu.io.lhist

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
    val s2_taken_hits = RegNext(Cat(f1_taken_hits.reverse))
    val s2_ghist = RegNext(f1_ghist)
    val s2_lhist = RegNext(f1_lhist)

    val fetch_bundle = Wire(FetchBundle(p))

    val brjmp = Wire(Vec.fill(p.coreWidth)(BrJmpSignal()))
    val s2_is_taken = Wire(Vec.fill(p.coreWidth)(Bool()))
    val s2_prev_brjmp_taken = Wire(Vec.fill(p.coreWidth)(Bool()))

    s2_valid := s1_valid && !f1_clear
    s2_fetch_mask := p.fetchMask(s2_vpc)

    fetch_bundle := DontCare
    fetch_bundle.pc := s2_vpc
    fetch_bundle.brjmp.is_br := false.B
    fetch_bundle.brjmp.is_jal := false.B
    fetch_bundle.brjmp.is_jalr := false.B
    fetch_bundle.cfi_taken := false.B

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

      when (s2_is_taken(i) && inst.valid) {
        fetch_bundle.brjmp.is_br := brjmp(i).is_br
        fetch_bundle.brjmp.is_jal := brjmp(i).is_jal
        fetch_bundle.brjmp.is_jalr := brjmp(i).is_jalr
        fetch_bundle.cfi_idx := i.U
        fetch_bundle.cfi_taken := true.B
      }
    }

    fetch_bundle.target_pc := s2_pred_target

    fetch_bundle.cfi_mask := Cat(fetch_bundle.insts.zip(brjmp).map((i, b) => {
      i.valid && b.is_br || b.is_jal || b.is_jalr
    }).reverse)

    val s2_cfi_idx = fetch_bundle.cfi_idx
    val s2_ti = fetch_bundle.insts(s2_cfi_idx)
    val s2_taken_is_call = s2_ti.valid &&
                          (lrd(s2_ti.bits) === 1.U) &&
                          (fetch_bundle.brjmp.is_jal || fetch_bundle.brjmp.is_jalr)
    val s2_taken_is_ret = s2_ti.valid &&
                          (lrd(s2_ti.bits) === 0.U && lrs1(s2_ti.bits) === 1.U) &&
                          fetch_bundle.brjmp.is_jalr
    val s2_call_pc = p.fetchAlign(s2_vpc) + (s2_cfi_idx << 2)

    fetch_bundle.ghist := s2_ghist
    fetch_bundle.lhist := s2_lhist
    fetch_bundle.ras_ptr := bpu.io.ras_snapshot.ptr
    fetch_bundle.ras_top := bpu.io.ras_snapshot.top
    fetch_bundle.is_call := s2_taken_is_call
    fetch_bundle.is_ret := s2_taken_is_ret

    // Speculatively update RAS
    bpu.io.ras_update.valid := s2_valid && ic.s2_valid && (s2_taken_is_ret || s2_taken_is_call)
    bpu.io.ras_update.bits := DontCare
    bpu.io.ras_update.bits.pc := s2_call_pc
    bpu.io.ras_update.bits.is_call := s2_taken_is_call
    bpu.io.ras_update.bits.is_ret := s2_taken_is_ret

    // BHT & BTB Updates & Restores
    bpu.io.bpu_update <> ftq.io.bpu_update
    bpu.io.restore <> ftq.io.bpu_restore



    val fb = Module(new FetchBuffer(p, depth = 4))
    val s2_valid_insts = fetch_bundle.insts.map(_.valid).reduce(_ || _)
    val fb_fire = DecoupledHelper(
      fb.io.enq.ready,
      ftq.io.enq.ready,
      s2_valid,
      s2_valid_insts,
      ic.s2_valid)

    ftq.io.enq.valid := fb_fire.fire(ftq.io.enq.ready)
    ftq.io.enq.bits := fetch_bundle
    fetch_bundle.ftq_idx := ftq.io.enq_idx

    fb.io.enq.valid := fb_fire.fire(fb.io.enq.ready)
    fb.io.enq.bits  := fetch_bundle

    ic.s2_kill := f2_clear
    fb.io.clear := f2_clear
    f3_ready := fb.io.enq.ready && ftq.io.enq.ready


    io.core.fetch_uops.zip(fb.io.deq).foreach((io_uop, fb_uop) => {
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
      when (fetch_bundle.cfi_taken) {
        s0_vpc := s2_pred_target
        s0_valid := true.B
        f1_clear := true.B
      }
    }

    when (io.core.redirect.valid) {
      s0_vpc := io.core.redirect.target
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

    dontTouch(s2_vpc)
    dontTouch(s2_valid)
    dontTouch(s2_fetch_mask)
    dontTouch(f2_clear)
    dontTouch(brjmp)
    dontTouch(s2_is_taken)
    dontTouch(fetch_bundle)
    dontTouch(s2_taken_hit_idx)
    dontTouch(f3_ready)
  }
