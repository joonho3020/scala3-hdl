package riscv_ooo

import hdl.core._
import hdl.util._
import hdl.elaboration._

case class FTQEntry(
  pc: UInt,
  target: UInt,
  taken: Bool,

  ras_ptr: UInt,
  ras_top: UInt,
  ghist: UInt,
  lhist: UInt,

  is_call: Bool,
  is_ret: Bool,

  cfi_mask: UInt,
  cfi_idx: Valid[UInt],
) extends Bundle[FTQEntry]

object FTQEntry:
  def apply(p: CoreParams): FTQEntry =
    FTQEntry(
      pc = UInt(p.pcBits.W),
      target = UInt(p.pcBits.W),
      taken  = Bool(),

      ras_ptr = UInt(log2Ceil(p.bpu.rasEntries + 1).W),
      ras_top = UInt(p.pcBits.W),
      ghist = UInt(p.bpu.ghistBits.W),
      lhist = UInt(p.bpu.lhistBits.W),

      is_call = Bool(),
      is_ret = Bool(),

      cfi_mask = UInt(p.coreWidth.W),
      cfi_idx  = Valid(UInt(log2Ceil(p.coreWidth + 1).W)),
    )

case class FTQIO(
  enq: Decoupled[FetchBundle],
  enq_idx: Valid[UInt],
  redirect_resp: Valid[FTQEntry],
  redirect: RedirectIf,
  bht_btb_update: Valid[BPUUpdate],
  commit: FTQCommitIf
) extends Bundle[FTQIO]

object FTQIO:
  def apply(p: CoreParams): FTQIO =
    FTQIO(
      enq = Flipped(Decoupled(FetchBundle(p))),
      enq_idx = Output(Valid(UInt(p.ftqIdxBits.W))),
      redirect_resp = Output(Valid(FTQEntry(p))),
      redirect = RedirectIf(p),
      bht_btb_update = Valid(BPUUpdate(p)),
      commit = FTQCommitIf(p)
    )

class FetchTargetQueue(p: CoreParams) extends Module with CoreCacheable(p):
  given Module = this
  val io = IO(FTQIO(p))

  body {
    val numEntries = p.br.ftqEntries
    val idxBits = p.ftqIdxBits

    val entries = Reg(Vec.fill(numEntries)(Valid(FTQEntry(p))))
    val enq_ptr = RegInit(0.U(idxBits.W))
    val deq_ptr = RegInit(0.U(idxBits.W))

    dontTouch(entries)
    dontTouch(enq_ptr)
    dontTouch(deq_ptr)

    val maybe_full = RegInit(false.B)
    val ptr_match = enq_ptr === deq_ptr
    val empty = ptr_match && !maybe_full
    val full = ptr_match && maybe_full

    io.enq.ready := !full

    val do_enq = io.enq.fire && io.enq.bits.insts.map(_.valid).reduce(_ || _)
    val do_deq = io.commit.valid
    val do_redirect = io.redirect.valid

    def next_ptr(ptr: UInt): UInt =
      Mux(ptr === (numEntries - 1).U, 0.U, ptr + 1.U)

    def in_range(idx: UInt, start: UInt, end: UInt): Bool =
      Mux(start <= end, idx >= start && idx < end, idx >= start || idx < end)

    when (do_enq) {
      val idx = enq_ptr
      entries(idx).valid := true.B
      entries(idx).bits.pc := p.fetchAlign(io.enq.bits.pc)
      entries(idx).bits.taken    := io.enq.bits.target_pc.valid
      entries(idx).bits.target   := io.enq.bits.target_pc.bits
      entries(idx).bits.ras_ptr  := io.enq.bits.ras_ptr
      entries(idx).bits.ras_top  := io.enq.bits.ras_top
      entries(idx).bits.ghist    := io.enq.bits.ghist
      entries(idx).bits.lhist    := io.enq.bits.lhist
      entries(idx).bits.is_call  := io.enq.bits.is_call
      entries(idx).bits.is_ret   := io.enq.bits.is_ret
      entries(idx).bits.cfi_mask := io.enq.bits.cfi_mask
      entries(idx).bits.cfi_idx  := io.enq.bits.cfi_idx
      enq_ptr := next_ptr(idx)
    }

    io.redirect_resp := DontCare
    when (do_redirect) {
      val new_tail = next_ptr(io.redirect.ftq_idx)
      for (i <- 0 until numEntries) {
        when (in_range(i.U(idxBits.W), new_tail, enq_ptr)) {
          entries(i).valid := false.B
        }
      }
      enq_ptr := new_tail
      val redirect_entry = entries(io.redirect.ftq_idx)
      redirect_entry.bits.taken   := io.redirect.taken
      redirect_entry.bits.target  := io.redirect.target
      redirect_entry.bits.cfi_idx.bits := io.redirect.fb_idx
      redirect_entry.bits.cfi_idx.valid := true.B
      redirect_entry.bits.is_call := redirect_entry.bits.is_call && (redirect_entry.bits.cfi_idx.bits === io.redirect.fb_idx)
      redirect_entry.bits.is_ret  := redirect_entry.bits.is_ret  && (redirect_entry.bits.cfi_idx.bits === io.redirect.fb_idx)

      io.redirect_resp := redirect_entry

      maybe_full := false.B
    } .otherwise {
      when (do_enq =/= do_deq) {
        maybe_full := Mux(do_enq, next_ptr(enq_ptr) === deq_ptr, false.B)
      }
    }

    io.bht_btb_update.valid := false.B
    io.bht_btb_update.bits  := DontCare

    when (do_deq) {
      val deq_entry = entries(io.commit.ftq_idx)
      deq_entry.valid := false.B

      io.bht_btb_update.valid := deq_entry.bits.cfi_idx.valid
      io.bht_btb_update.bits.pc := deq_entry.bits.pc + (deq_entry.bits.cfi_idx.bits << 2)
      io.bht_btb_update.bits.taken := deq_entry.bits.taken
      io.bht_btb_update.bits.target := deq_entry.bits.target
      io.bht_btb_update.bits.is_call := deq_entry.bits.is_call
      io.bht_btb_update.bits.is_ret := deq_entry.bits.is_ret

      deq_ptr := next_ptr(io.commit.ftq_idx)
    }

    io.enq_idx.valid := !full
    io.enq_idx.bits  := enq_ptr

    when (reset.asBool) {
      enq_ptr := 0.U
      deq_ptr := 0.U
      maybe_full := false.B
      for (i <- 0 until numEntries) {
        entries(i).valid := false.B
      }
    }
  }
