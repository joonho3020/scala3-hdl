package riscv_ooo

import hdl._
import CoreConstants.{ALUOp1, ALUOp2, MemOp}
import MagicMemMsg.Read

case class RedirectIf(
  valid:  Bool,
  target: UInt,
) extends Bundle[RedirectIf]

object RedirectIf:
  def apply(p: CoreParams): RedirectIf =
    RedirectIf(
      valid  = Output(Bool()),
      target = Output(UInt(p.pcBits.W)),
    )

case class RetireInfoIf(
  valid: Bool,
  pc: UInt,
  wb_valid: Bool,
  wb_data: UInt,
  wb_rd: UInt,
  bpu_preds: UInt,
  bpu_hits: UInt
) extends Bundle[RetireInfoIf]

object RetireInfoIf:
  def apply(p: CoreParams): RetireInfoIf =
    RetireInfoIf(
      valid    = Output(Bool()),
      pc       = Output(UInt(p.pcBits.W)),
      wb_valid = Output(Bool()),
      wb_data  = Output(UInt(p.xlenBits.W)),
      wb_rd    = Output(UInt(p.xlenBits.W)),
      bpu_preds = Output(UInt(p.xlenBits.W)),
      bpu_hits = Output(UInt(p.xlenBits.W))
    )

case class CoreIf(
  fetch_uops: Vec[Decoupled[UOp]],
  redirect: RedirectIf,
  retire_info: Vec[RetireInfoIf],
  rn2_uops: Vec[Valid[UOp]],
  bpu_update: Valid[BPUUpdate],
  mem: MagicMemIf
) extends Bundle[CoreIf]

object CoreIf:
  def apply(p: CoreParams): CoreIf =
    CoreIf(
      fetch_uops    = Flipped(Vec.fill(p.coreWidth)(Decoupled(UOp(p)))),
      redirect      = RedirectIf(p),
      retire_info   = Vec.fill(p.coreWidth)(RetireInfoIf(p)),
      rn2_uops      = Output(Vec.fill(p.coreWidth)(Valid(UOp(p)))),
      bpu_update    = Valid(BPUUpdate(p)),
      mem           = MagicMemIf(p)
    )

class Core(p: CoreParams) extends Module with CoreCacheable(p):
  given Module = this
  val io = IO(CoreIf(p))

  val XLEN = p.xlenBits
  val coreWidth = p.coreWidth

  body {
    io.redirect.valid := false.B
    io.redirect.target := 0.U

    io.bpu_update.valid := false.B
    io.bpu_update.bits.pc := 0.U
    io.bpu_update.bits.target := 0.U
    io.bpu_update.bits.taken := false.B
    io.bpu_update.bits.is_call := false.B
    io.bpu_update.bits.is_ret := false.B

    io.mem.req.valid := false.B
    io.mem.req.bits.addr := 0.U
    io.mem.req.bits.tpe := Read.EN
    io.mem.req.bits.data.foreach(_ := 0.U)
    io.mem.req.bits.mask := 0.U

    io.retire_info.foreach(ri => {
      ri.valid := false.B
      ri.pc := DontCare
      ri.wb_valid := DontCare
      ri.wb_data := DontCare
      ri.wb_rd := DontCare
      ri.bpu_preds := DontCare
      ri.bpu_hits := DontCare
    })

    val renamer = Module(new Renamer(p))

    // -----------------------------------------------------------------------
    // Decode
    // -----------------------------------------------------------------------
    val fb_stall = WireInit(false.B) // Don't fire uops coming out from fetch buffer

    val decoder = Module(new Decoder(p))
    decoder.io.enq.zip(io.fetch_uops).foreach((d, f) => {
      d.valid := f.valid && !fb_stall
      d.bits  := f.bits
      f.ready := d.ready && !fb_stall
    })

    // -----------------------------------------------------------------------
    // Rename 0 & 1
    // -----------------------------------------------------------------------
    val dec_stall = WireInit(false.B) // Don't fire uops coming out from decoder
    val dec_uops = Reg(Vec.fill(coreWidth)(Valid(UOp(p))))

    decoder.io.deq.zip(dec_uops).foreach((d, u) => {
      when(!dec_stall) {
        u.bits := d.bits
        u.valid := d.valid
      }
      d.ready := !dec_stall
    })

    renamer.io.dec_uops := dec_uops

    when (!renamer.io.dec_ready) {
      dec_stall := true.B
    }


    // -----------------------------------------------------------------------
    // Dispatch
    // -----------------------------------------------------------------------

    // -----------------------------------------------------------------------
    // Issue
    // -----------------------------------------------------------------------

    // -----------------------------------------------------------------------
    // Mem
    // -----------------------------------------------------------------------

    // -----------------------------------------------------------------------
    // WB
    // -----------------------------------------------------------------------
    renamer.io.wb_done_phys := DontCare

    // -----------------------------------------------------------------------
    // Commit
    // -----------------------------------------------------------------------
    renamer.io.comm_free_phys := DontCare

    //////////////////////////////////////////////////////////

    when (reset.asBool) {
      for (i <- 0 until coreWidth) {
        dec_uops(i).valid := false.B
      }
    }

    dontTouch(io.redirect)
    io.rn2_uops := renamer.io.rn2_uops
  }
