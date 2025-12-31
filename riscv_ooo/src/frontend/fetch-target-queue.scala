package riscv_ooo

import hdl.core._
import hdl.util._
import hdl.elaboration._

case class FTQEntry(
  pc: UInt,

  ras_ptr: UInt,
  ras_top: UInt,
  ghist: UInt,
  lhist: UInt,

  is_call: Bool,
  is_ret: Bool,

  cfi_mask: UInt,
  cfi_taken: Bool,
  cfi_idx: UInt,
  target_pc: UInt,
) extends Bundle[FTQEntry]

object FTQEntry:
  def apply(p: CoreParams): FTQEntry =
    FTQEntry(
      pc = UInt(p.pcBits.W),

      ras_ptr = UInt(log2Ceil(p.bpu.rasEntries + 1).W),
      ras_top = UInt(p.pcBits.W),
      ghist = UInt(p.bpu.ghistBits.W),
      lhist = UInt(p.bpu.lhistBits.W),

      is_call = Bool(),
      is_ret = Bool(),

      cfi_mask = UInt(p.coreWidth.W),
      cfi_taken = Bool(),
      cfi_idx = UInt(log2Ceil(p.coreWidth + 1).W),
      target_pc = UInt(p.pcBits.W),
    )

case class FTQIO(
  enq: Decoupled[FetchBundle],
  enq_idx: UInt,
  bpu_restore: BPURestoreIf,
  redirect: RedirectIf,
  bpu_update: Valid[BPUUpdate],
  commit: BranchCommitIf
) extends Bundle[FTQIO]

object FTQIO:
  def apply(p: CoreParams): FTQIO =
    FTQIO(
      enq = Flipped(Decoupled(FetchBundle(p))),
      enq_idx = Output(UInt(p.ftqIdxBits.W)),
      bpu_restore = Output(BPURestoreIf(p)),
      redirect = RedirectIf(p),
      bpu_update = Valid(BPUUpdate(p)),
      commit = BranchCommitIf(p)
    )

class FetchTargetQueue(p: CoreParams) extends Module with CoreCacheable(p):
  given Module = this
  val io = IO(FTQIO(p))

  body {
    val numEntries = p.br.ftqEntries
    val idxBits = p.ftqIdxBits

    val entries = Reg(Vec.fill(numEntries)(FTQEntry(p)))
    val enq_ptr = RegInit(0.U(idxBits.W))
    val deq_ptr = RegInit(0.U(idxBits.W))
    val bpd_ptr = RegInit(0.U(idxBits.W))

    dontTouch(entries)
    dontTouch(enq_ptr)
    dontTouch(deq_ptr)

    val maybe_full = RegInit(false.B)
    val ptr_match = enq_ptr === deq_ptr
    val empty = ptr_match && !maybe_full
    val full = ptr_match && maybe_full

    io.enq.ready := !full

    val do_enq = io.enq.fire
    val do_deq = io.commit.valid
    val do_redirect = io.redirect.valid

    def next_ptr(ptr: UInt): UInt =
      Mux(ptr === (numEntries - 1).U, 0.U, ptr + 1.U)

    when (do_enq) {
      val idx = enq_ptr
      entries(idx).pc := p.fetchAlign(io.enq.bits.pc)
      entries(idx).ras_top   := io.enq.bits.ras_top
      entries(idx).ghist     := io.enq.bits.ghist
      entries(idx).lhist     := io.enq.bits.lhist
      entries(idx).is_call   := io.enq.bits.is_call
      entries(idx).is_ret    := io.enq.bits.is_ret
      entries(idx).cfi_mask  := io.enq.bits.cfi_mask
      entries(idx).cfi_idx   := io.enq.bits.cfi_idx
      entries(idx).cfi_taken := io.enq.bits.cfi_taken
      entries(idx).target_pc := io.enq.bits.target_pc
      enq_ptr := next_ptr(idx)
    }

    io.bpu_restore := DontCare
    io.bpu_restore.valid := RegNext(do_redirect)

    when (do_redirect) {
      enq_ptr := next_ptr(io.redirect.ftq_idx)
      val redirect_entry = entries(io.redirect.ftq_idx)
      redirect_entry.cfi_taken := io.redirect.taken
      redirect_entry.target_pc := io.redirect.target
      redirect_entry.cfi_idx := io.redirect.cfi_idx
      redirect_entry.cfi_mask := redirect_entry.cfi_mask & ((1.U << io.redirect.cfi_idx) - 1.U)
      redirect_entry.is_call := redirect_entry.is_call && (redirect_entry.cfi_idx === io.redirect.cfi_idx)
      redirect_entry.is_ret  := redirect_entry.is_ret  && (redirect_entry.cfi_idx === io.redirect.cfi_idx)

      val fetch_pc = redirect_entry.pc + (io.redirect.cfi_idx << 2)
      io.bpu_restore.pc    := RegNext(fetch_pc)
      io.bpu_restore.ghist := RegNext(redirect_entry.ghist)
      io.bpu_restore.lhist := RegNext(redirect_entry.lhist)
      io.bpu_restore.ras.ptr := RegNext(redirect_entry.ras_ptr)
      io.bpu_restore.ras.top := RegNext(redirect_entry.ras_top)

      maybe_full := false.B
    }


    when (do_deq) {
      val deq_entry = entries(io.commit.ftq_idx)
      deq_ptr := next_ptr(io.commit.ftq_idx)
      maybe_full := false.B
    }

    io.bpu_update.valid := false.B
    io.bpu_update.bits  := DontCare
    val bpd_entry = RegNext(entries(bpd_ptr))
    val do_bpd_update = (bpd_ptr =/= deq_ptr) && !do_redirect && !full

    when (RegNext(do_bpd_update)) {
      io.bpu_update.valid := bpd_entry.cfi_mask =/= 0.U
      io.bpu_update.bits.pc := bpd_entry.pc
      io.bpu_update.bits.taken := bpd_entry.cfi_taken
      io.bpu_update.bits.target := bpd_entry.target_pc
      io.bpu_update.bits.is_call := bpd_entry.is_call
      io.bpu_update.bits.is_ret := bpd_entry.is_ret
    }


    io.enq_idx  := enq_ptr

    when (do_enq && !do_deq) {
      maybe_full := (enq_ptr + 1.U) % numEntries.U === deq_ptr
    }
  }
