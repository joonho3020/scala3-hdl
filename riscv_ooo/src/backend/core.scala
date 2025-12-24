package riscv_ooo

import hdl._
import riscv_inorder.CoreConstants.{ALUOp1, ALUOp2, MemOp}
import riscv_inorder.{ALU, ALUParams, ImmGen}
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
  debug_rn2_uops: Option[Vec[Valid[UOp]]],
  bpu_update: Valid[BPUUpdate],
  mem: MagicMemIf
) extends Bundle[CoreIf]

object CoreIf:
  def apply(p: CoreParams): CoreIf =
    CoreIf(
      fetch_uops     = Flipped(Vec.fill(p.coreWidth)(Decoupled(UOp(p)))),
      redirect       = RedirectIf(p),
      retire_info    = Vec.fill(p.coreWidth)(RetireInfoIf(p)),
      debug_rn2_uops = if p.debug then Some(Output(Vec.fill(p.coreWidth)(Valid(UOp(p))))) else None,
      bpu_update     = Valid(BPUUpdate(p)),
      mem            = MagicMemIf(p)
    )

class Core(p: CoreParams) extends Module with CoreCacheable(p):
  given Module = this
  val io = IO(CoreIf(p))

  val XLEN = p.xlenBits
  val coreWidth = p.coreWidth
  val issueWidth = p.issueWidth
  val retireWidth = p.retireWidth

  body {
    val bpu_pred_count = WireInit(0.U(p.xlenBits.W))
    val bpu_hit_count = WireInit(0.U(p.xlenBits.W))

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
      ri.pc := 0.U
      ri.wb_valid := false.B
      ri.wb_data := 0.U
      ri.wb_rd := 0.U
      ri.bpu_preds := bpu_pred_count
      ri.bpu_hits := bpu_hit_count
    })

    val renamer     = Module(new Renamer(p))
    val rob         = Module(new ROB(p))
    val issue_queue = Module(new IssueQueue(p))
    val prf         = Module(new PhysicalRegfile(p))

    val imm_gens = (0 until issueWidth).map(_ => Module(new ImmGen(XLEN)))
    val alus     = (0 until issueWidth).map(_ => Module(new ALU(ALUParams(XLEN))))

    // -----------------------------------------------------------------------
    // Decode
    // -----------------------------------------------------------------------
    val fb_stall = WireInit(false.B)

    val decoder = Module(new Decoder(p))
    decoder.io.enq.zip(io.fetch_uops).foreach((d, f) => {
      d.valid := f.valid && !fb_stall
      d.bits  := f.bits
      f.ready := d.ready && !fb_stall
    })

    // -----------------------------------------------------------------------
    // Rename 0 & 1
    // -----------------------------------------------------------------------
    val dec_stall = WireInit(false.B)
    val dec_uops = Reg(Vec.fill(coreWidth)(Valid(UOp(p))))

    decoder.io.deq.zip(dec_uops).foreach((d, u) => {
      when(!dec_stall) {
        u.bits := d.bits
      }
      u.valid := d.valid && !dec_stall
      d.ready := !dec_stall
    })

    renamer.io.dec_uops := dec_uops

    when (!renamer.io.dec_ready) {
      dec_stall := true.B
    }

    // -----------------------------------------------------------------------
    // Dispatch (ROB & IssueQueue allocation)
    // -----------------------------------------------------------------------
    val dis_stall = Wire(Bool())
    val dis_uops = Reg(Vec.fill(coreWidth)(Valid(UOp(p))))

    dis_stall := rob.io.full || !issue_queue.io.dis_ready
    dis_uops.zip(renamer.io.rn2_uops).foreach((dis, ren) => {
      dis.valid := ren.valid && !dis_stall
      dis.bits  := ren.bits
    })

    rob.io.dispatch_req := dis_uops

    for (i <- 0 until coreWidth) {
      issue_queue.io.dis_uops(i).valid := dis_uops(i).valid && !rob.io.full
      issue_queue.io.dis_uops(i).bits := dis_uops(i).bits
      issue_queue.io.dis_uops(i).bits.rob_idx := rob.io.dispatch_rob_idxs(i)
    }

    when (dis_stall) {
      dec_stall := true.B
    }

    val di_compactor = Module(new Compactor(p, p.coreWidth))
    di_compactor.io.enq <> issue_queue.io.issue_uops

    // -----------------------------------------------------------------------
    // Issue
    // -----------------------------------------------------------------------
    val iss_uops = Reg(Vec.fill(coreWidth)(Valid(UOp(p))))
    iss_uops := di_compactor.io.deq

    // -----------------------------------------------------------------------
    // Register Read
    // -----------------------------------------------------------------------
    val rrd_uops = Reg(Vec.fill(issueWidth)(Valid(UOp(p))))
    rrd_uops := iss_uops

    for (i <- 0 until issueWidth) {
      prf.io.read_ports(i * 2 + 0).addr := iss_uops(i).bits.prs1
      prf.io.read_ports(i * 2 + 1).addr := iss_uops(i).bits.prs2
    }

    val rrd_rs1_data = (0 until issueWidth).map(i => prf.io.read_ports(i * 2 + 0).data)
    val rrd_rs2_data = (0 until issueWidth).map(i => prf.io.read_ports(i * 2 + 1).data)

    // -----------------------------------------------------------------------
    // Execute
    // -----------------------------------------------------------------------
    val exe_uops = Reg(Vec.fill(issueWidth)(Valid(UOp(p))))
    val exe_rs1_data = Reg(Vec.fill(issueWidth)(UInt(XLEN.W)))
    val exe_rs2_data = Reg(Vec.fill(issueWidth)(UInt(XLEN.W)))

    for (i <- 0 until issueWidth) {
      exe_uops(i) := rrd_uops(i)
      exe_rs1_data(i) := rrd_rs1_data(i)
      exe_rs2_data(i) := rrd_rs2_data(i)
    }

    for (i <- 0 until issueWidth) {
      val uop = exe_uops(i).bits
      val ctrl = uop.ctrl

      imm_gens(i).io.inst := uop.inst
      imm_gens(i).io.sel := ctrl.sel_imm

      val imm = imm_gens(i).io.out

      val alu_in1 = Wire(UInt(XLEN.W))
      switch (ctrl.sel_alu1) {
        is (ALUOp1.RS1.EN)    { alu_in1 := exe_rs1_data(i) }
        is (ALUOp1.PC.EN)     { alu_in1 := uop.pc }
        is (ALUOp1.ZERO.EN)   { alu_in1 := 0.U }
        default               { alu_in1 := DontCare }
      }

      val alu_in2 = Wire(UInt(XLEN.W))
      switch (ctrl.sel_alu2) {
        is (ALUOp2.RS2.EN)    { alu_in2 := exe_rs2_data(i) }
        is (ALUOp2.IMM.EN)    { alu_in2 := imm }
        default               { alu_in2 := DontCare }
      }

      alus(i).io.fn := ctrl.alu_op.asUInt
      alus(i).io.in1 := alu_in1
      alus(i).io.in2 := alu_in2
      alus(i).io.dw := DontCare
    }

    // -----------------------------------------------------------------------
    // Writeback
    // -----------------------------------------------------------------------
    val wb_uops = Reg(Vec.fill(issueWidth)(Valid(UOp(p))))
    val wb_data = Reg(Vec.fill(issueWidth)(UInt(XLEN.W)))

    for (i <- 0 until issueWidth) {
      wb_uops(i) := exe_uops(i)
      wb_data(i) := alus(i).io.out
    }

    rob.io.wb_req := wb_uops
    rob.io.wb_data := wb_data

    for (i <- 0 until issueWidth) {
      prf.io.write_ports(i).valid := wb_uops(i).valid && wb_uops(i).bits.ctrl.rd_wen
      prf.io.write_ports(i).addr := wb_uops(i).bits.prd
      prf.io.write_ports(i).data := wb_data(i)
    }

    for (i <- 0 until issueWidth) {
      issue_queue.io.wakeup_idx(i).valid := wb_uops(i).valid && wb_uops(i).bits.ctrl.rd_wen
      issue_queue.io.wakeup_idx(i).bits := wb_uops(i).bits.prd
    }

    for (i <- 0 until coreWidth) {
      renamer.io.wb_wakeup(i).prd.valid := {
        if (i < issueWidth) wb_uops(i).valid && wb_uops(i).bits.ctrl.rd_wen
        else false.B
      }
      renamer.io.wb_wakeup(i).prd.bits := {
        if (i < issueWidth) wb_uops(i).bits.prd
        else 0.U
      }
    }

    val wbcomm_compactor = Module(new Compactor(p, issueWidth))
    wbcomm_compactor.io.enq := wb_uops

    // -----------------------------------------------------------------------
    // Commit
    // -----------------------------------------------------------------------
    val comm_uops = Wire(Vec.fill(retireWidth)(Valid(UOp(p))))
    comm_uops := rob.io.commit
    val comm_wb_data = rob.io.commit_data

    for (i <- 0 until retireWidth) {
      io.retire_info(i).valid := comm_uops(i).valid
      io.retire_info(i).pc := comm_uops(i).bits.pc
      io.retire_info(i).wb_valid := comm_uops(i).bits.ctrl.rd_wen
      io.retire_info(i).wb_data := comm_wb_data(i)
      io.retire_info(i).wb_rd := comm_uops(i).bits.lrd
      io.retire_info(i).bpu_preds := bpu_pred_count
      io.retire_info(i).bpu_hits := bpu_hit_count
    }

    renamer.io.comm_free_phys := DontCare
    for (i <- 0 until retireWidth) {
      renamer.io.comm_free_phys(i).valid := comm_uops(i).valid && comm_uops(i).bits.ctrl.rd_wen
      renamer.io.comm_free_phys(i).bits  := comm_uops(i).bits.stale_prd
    }

    // -----------------------------------------------------------------------
    // Reset
    // -----------------------------------------------------------------------
    when (reset.asBool) {
      for (i <- 0 until coreWidth) {
        dec_uops(i).valid := false.B
      }
      for (i <- 0 until issueWidth) {
        rrd_uops(i).valid := false.B
        exe_uops(i).valid := false.B
        wb_uops(i).valid := false.B
      }
    }

    io.debug_rn2_uops.map(_ := renamer.io.rn2_uops)

    dontTouch(dec_stall)
    dontTouch(dec_uops)

    dontTouch(dis_stall)
    dontTouch(dis_uops)

    dontTouch(iss_uops)

    dontTouch(iss_uops)
    dontTouch(rrd_uops)
    dontTouch(exe_uops)
    dontTouch(wb_uops)
    dontTouch(wb_data)
    dontTouch(comm_uops)


    dontTouch(io.redirect)
  }
