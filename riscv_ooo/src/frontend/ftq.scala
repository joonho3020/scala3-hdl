package riscv_ooo

import hdl.core._
import hdl.util._
import hdl.elaboration._

case class FTQEntry(
  pc: UInt,
  target: UInt,
  ras_ptr: UInt,
  ras_top: UInt,
  ghist: UInt,
  taken: Bool,
  is_call: Bool,
  is_ret: Bool,
) extends Bundle[FTQEntry]

object FTQEntry:
  def apply(p: CoreParams): FTQEntry =
    FTQEntry(
      pc = UInt(p.pcBits.W),
      target = UInt(p.pcBits.W),
      ras_ptr = UInt(log2Ceil(p.bpu.rasEntries + 1).W),
      ras_top = UInt(p.pcBits.W),
      ghist = UInt(p.bpu.ghistBits.W),
      taken = Bool(),
      is_call = Bool(),
      is_ret = Bool(),
    )

case class FTQIO(
  enq: Valid[FTQEntry],
  enq_idx: Valid[UInt],
  redirect_resp: Valid[FTQEntry],
  redirect: RedirectIf,
  bht_btb_update: Valid[BPUUpdate],
  commit: BranchCommitIf
) extends Bundle[FTQIO]

object FTQIO:
  def apply(p: CoreParams): FTQIO =
    FTQIO(
      enq = Input(Valid(FTQEntry(p))),
      enq_idx = Output(Valid(UInt(p.ftqIdxBits.W))),
      redirect_resp = Output(Valid(FTQEntry(p))),
      redirect = RedirectIf(p),
      bht_btb_update = Valid(BPUUpdate(p)),
      commit = BranchCommitIf(p)
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

    val do_enq = io.enq.valid && !full
    val do_deq = io.commit.valid
    val do_redirect = io.redirect.valid

    def bump_ptr(ptr: UInt): UInt =
      Mux(ptr === (numEntries - 1).U, 0.U, ptr + 1.U)

    when (do_enq) {
      val idx = enq_ptr
      entries(idx).valid := true.B
      entries(idx).bits := io.enq.bits
      enq_ptr := bump_ptr(idx)
    }

    io.redirect_resp := DontCare
    when (do_redirect) {
      enq_ptr := io.redirect.ftq_idx
      entries(io.redirect.ftq_idx).bits.target := io.redirect.target
      entries(io.redirect.ftq_idx).bits.taken := io.redirect.taken
      maybe_full := false.B
      io.redirect_resp := entries(io.redirect.ftq_idx)
    } .elsewhen (do_enq && !do_deq) {
      maybe_full := (enq_ptr + 1.U) % numEntries.U === deq_ptr
    }

    io.bht_btb_update.valid := false.B
    io.bht_btb_update.bits  := DontCare

    when (do_deq) {
      val deq_entry = entries(io.commit.ftq_idx)

      io.bht_btb_update.valid := true.B
      io.bht_btb_update.bits.pc := deq_entry.bits.pc
      io.bht_btb_update.bits.taken := deq_entry.bits.taken
      io.bht_btb_update.bits.target := deq_entry.bits.target
      io.bht_btb_update.bits.is_call := deq_entry.bits.is_call
      io.bht_btb_update.bits.is_ret := deq_entry.bits.is_ret

      deq_ptr := bump_ptr(io.commit.ftq_idx)
      maybe_full := false.B
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
