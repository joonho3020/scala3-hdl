package riscv_ooo

import hdl.core._
import hdl.util._
import hdl.elaboration._

import riscv_inorder.CoreConstants.{ALUOp1, ALUOp2, MemOp}
import riscv_inorder.{ALU, ALUParams, ImmGen}
import MagicMemMsg.Read

case class RetireTraceIf(
  valid: Bool,
  pc: UInt,
  wb_valid: Bool,
  wb_data: UInt,
  wb_rd: UInt,
  bpu_preds: UInt,
  bpu_hits: UInt
) extends Bundle[RetireTraceIf]

object RetireTraceIf:
  def apply(p: CoreParams): RetireTraceIf =
    RetireTraceIf(
      valid    = Output(Bool()),
      pc       = Output(UInt(p.pcBits.W)),
      wb_valid = Output(Bool()),
      wb_data  = Output(UInt(p.xlenBits.W)),
      wb_rd    = Output(UInt(p.xlenBits.W)),
      bpu_preds = Output(UInt(p.xlenBits.W)),
      bpu_hits = Output(UInt(p.xlenBits.W))
    )

case class CoSimInfoIf(
  valid: Bool,
  pc:    UInt,
  next_pc: UInt,
  wb_valid: Bool,
  wb_data: UInt,
  wb_rd:   UInt,
  mismatch: Bool
) extends Bundle[CoSimInfoIf]

object CoSimInfoIf:
  def apply(p: CoreParams): CoSimInfoIf =
    CoSimInfoIf(
      valid    = Input(Bool()),
      pc       = Input(UInt(p.pcBits.W)),
      next_pc  = Input(UInt(p.pcBits.W)),
      wb_valid = Input(Bool()),
      wb_data  = Input(UInt(p.xlenBits.W)),
      wb_rd    = Input(UInt(p.xlenBits.W)),
      mismatch = Input(Bool())
    )

case class CoreIf(
  ifu: FrontendCoreIf,
  mem: MagicMemIf,
  retire_info: Vec[RetireTraceIf],
  cosim_info: Vec[CoSimInfoIf]
) extends Bundle[CoreIf]

object CoreIf:
  def apply(p: CoreParams): CoreIf =
    CoreIf(
      ifu            = Flipped(FrontendCoreIf(p)),
      mem            = MagicMemIf(p),
      retire_info    = Vec.fill(p.coreWidth)(RetireTraceIf(p)),
      cosim_info     = Vec.fill(p.coreWidth)(CoSimInfoIf(p))
    )

class Core(p: CoreParams) extends Module with CoreCacheable(p):
  given Module = this
  val io = IO(CoreIf(p))

  val XLEN = p.xlenBits
  val coreWidth = p.coreWidth
  val issueWidth = p.issueWidth
  val retireWidth = p.retireWidth

  body {
    val bpu_pred_count = RegInit(0.U(p.xlenBits.W))
    val bpu_hit_count = RegInit(0.U(p.xlenBits.W))
    val kill_on_mispred = Wire(Bool())

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

    val renamer       = Module(new Renamer(p))
    val rob           = Module(new ROB(p))
    val issue_queue   = Module(new IssueQueue(p))
    val prf           = Module(new PhysicalRegfile(p))
    val br_tag_mgr    = Module(new BranchTagManager(p))

    val imm_gens = (0 until issueWidth).map(_ => Module(new ImmGen(XLEN)))
    val alus     = (0 until issueWidth).map(_ => Module(new ALU(ALUParams(XLEN))))

    val wb_uops = Wire(Vec.fill(issueWidth)(Valid(UOp(p))))

    // -----------------------------------------------------------------------
    // Decode
    // -----------------------------------------------------------------------
    val decoder = Module(new Decoder(p))
    decoder.io.enq.zip(io.ifu.fetch_uops).foreach((d, f) => d <> f)

    // -----------------------------------------------------------------------
    // BranchTag Allocation & Rename 0 & 1
    // -----------------------------------------------------------------------
    val dec_cfi_mask = decoder.io.deq.map(d => d.valid && d.bits.ctrl.is_cfi)
    val dec_cfi_cnt = dec_cfi_mask.map(_.asUInt).reduce(_ +& _)
    val dec_stall = WireInit(false.B)
    val br_tag_stall = (dec_cfi_cnt > br_tag_mgr.io.count).asWire
    dontTouch(br_tag_stall)

    for (i <- 0 until coreWidth) {
      val renamer_ready = !dec_stall && !br_tag_stall && !kill_on_mispred && (i.U < renamer.io.free_count)

      decoder.io.deq(i).ready := renamer_ready

      br_tag_mgr.io.req_valid(i) := decoder.io.deq(i).valid && renamer_ready
      br_tag_mgr.io.req_cfi(i) := renamer_ready && decoder.io.deq(i).bits.ctrl.is_cfi

      renamer.io.dec_uops(i).valid := decoder.io.deq(i).valid && renamer_ready
      renamer.io.dec_uops(i).bits  := decoder.io.deq(i).bits
      renamer.io.dec_uops(i).bits.br_tag  := br_tag_mgr.io.resp_tag(i)
      renamer.io.dec_uops(i).bits.br_mask := br_tag_mgr.io.resp_mask(i)
    }

    // -----------------------------------------------------------------------
    // Dispatch (ROB & IssueQueue allocation)
    // -----------------------------------------------------------------------
    val dis_stall = Wire(Bool())
    val dis_uops  = Wire(Vec.fill(coreWidth)(Valid(UOp(p))))

    for (i <- 0 until coreWidth) {
      dis_uops(i) := renamer.io.rn2_uops(i)
    }

    dis_stall := rob.io.full || !issue_queue.io.dis_ready
    renamer.io.dis_stall := dis_stall

    for (i <- 0 until coreWidth) {
      rob.io.dispatch_req(i).valid := dis_uops(i).valid && !dis_stall
      rob.io.dispatch_req(i).bits  := dis_uops(i).bits

      issue_queue.io.dis_uops(i).valid := dis_uops(i).valid && !dis_stall
      issue_queue.io.dis_uops(i).bits := dis_uops(i).bits
      issue_queue.io.dis_uops(i).bits.rob_idx := rob.io.dispatch_rob_idxs(i)
    }

    when (dis_stall) {
      dec_stall := true.B
    }

    val di_compactor = Module(new Compactor(p, p.coreWidth))
    di_compactor.io.enq <> issue_queue.io.issue_uops

    // -----------------------------------------------------------------------
    // Issue & Read PRF
    // -----------------------------------------------------------------------
    val iss_uops = Wire(Vec.fill(coreWidth)(Valid(UOp(p))))
    iss_uops := di_compactor.io.deq

    for (i <- 0 until issueWidth) {
      prf.io.read_ports(i * 2 + 0).addr := iss_uops(i).bits.prs1
      prf.io.read_ports(i * 2 + 1).addr := iss_uops(i).bits.prs2
    }

    // -----------------------------------------------------------------------
    // Execute
    // -----------------------------------------------------------------------
    val exe_uops = Reg(Vec.fill(issueWidth)(Valid(UOp(p))))
    val exe_rs1_data = (0 until issueWidth).map(i => prf.io.read_ports(i * 2 + 0).data)
    val exe_rs2_data = (0 until issueWidth).map(i => prf.io.read_ports(i * 2 + 1).data)
    val exe_resolve_info = Wire(BranchResolve(p))

    for (i <- 0 until issueWidth) {
      val iss_uop = iss_uops(i)
      val br_invalidate = exe_resolve_info.valid &&
                          exe_resolve_info.mispredict &&
                          ((exe_resolve_info.tag & iss_uop.bits.br_mask) =/= 0.U)
      exe_uops(i).valid := iss_uops(i).valid && !br_invalidate
      exe_uops(i).bits  := iss_uops(i).bits

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
    // Branch Resolution (at end of execute)
    // -----------------------------------------------------------------------
    val exe_wb_data = Wire(Vec.fill(issueWidth)(UInt(p.xlenBits.W)))
    val exe_is_cfi = exe_uops.map(u => u.valid && u.bits.ctrl.is_cfi)
    val exe_cfi_valid = exe_is_cfi.reduce(_ || _)
    val exe_cfi_idx = PriorityEncoder(Cat(exe_is_cfi.reverse))
    val cfi_uop = exe_uops(exe_cfi_idx)
    val exe_cfi_cnt = exe_is_cfi.map(_.asUInt).reduce(_ +& _)
    Assert(exe_cfi_cnt <= 1.U, s"Cannot issue more than one CFI, got ${exe_cfi_cnt}")

    val exe_cfi_cmp_out = MuxOneHot(PriorityEncoderOH(Cat(exe_is_cfi.reverse)), alus.map(_.io.cmp_out).toSeq)
    val exe_cfi_imm = MuxOneHot(PriorityEncoderOH(Cat(exe_is_cfi.reverse)), imm_gens.map(_.io.out).toSeq)
    val exe_cfi_alu_out = MuxOneHot(PriorityEncoderOH(Cat(exe_is_cfi.reverse)), alus.map(_.io.out).toSeq)

    val branch_taken = cfi_uop.bits.ctrl.br && exe_cfi_cmp_out
    val jump_taken = cfi_uop.bits.ctrl.jal || cfi_uop.bits.ctrl.jalr

    val exe_branch_target = cfi_uop.bits.pc + exe_cfi_imm(p.pcBits-1, 0)
    val exe_jump_target = exe_cfi_alu_out(p.pcBits-1, 0)
    val exe_cfi_target = Mux(cfi_uop.bits.ctrl.br, exe_branch_target, exe_jump_target)(p.pcBits-1, 0)
    val exe_cfi_taken = branch_taken || jump_taken

    val wrong_next_pc = (cfi_uop.bits.next_pc.bits =/= exe_cfi_target).asWire
    val mispred_taken = (exe_cfi_taken && (wrong_next_pc || !cfi_uop.bits.next_pc.valid)).asWire
    val mispred_not_taken = (cfi_uop.bits.ctrl.br && !exe_cfi_taken && cfi_uop.bits.next_pc.valid).asWire
    val has_mispred = (exe_cfi_valid && (mispred_taken || mispred_not_taken)).asWire

    dontTouch(wrong_next_pc)
    dontTouch(mispred_taken)
    dontTouch(mispred_not_taken)
    dontTouch(has_mispred)

    val resolve_target = Mux(mispred_not_taken, cfi_uop.bits.pc + p.instBytes.U, exe_cfi_target)
    exe_resolve_info.valid := exe_cfi_valid
    exe_resolve_info.tag := cfi_uop.bits.br_tag
    exe_resolve_info.mispredict := has_mispred
    exe_resolve_info.taken := exe_cfi_taken
    exe_resolve_info.target := resolve_target

    dontTouch(exe_resolve_info)

    br_tag_mgr.io.br_resolve  := exe_resolve_info
    renamer.io.br_resolve     := exe_resolve_info
    issue_queue.io.br_resolve := exe_resolve_info

    rob.io.branch_update.valid   := exe_resolve_info.valid
    rob.io.branch_update.mispred := exe_resolve_info.mispredict
    rob.io.branch_update.rob_idx := cfi_uop.bits.rob_idx

    kill_on_mispred := exe_resolve_info.valid && exe_resolve_info.mispredict

    io.ifu.redirect.valid := has_mispred
    io.ifu.redirect.target := resolve_target
    io.ifu.redirect.ftq_idx := cfi_uop.bits.ftq_idx
    io.ifu.redirect.taken := exe_cfi_taken

    when (exe_cfi_valid) {
      bpu_pred_count := bpu_pred_count + 1.U
      when (!has_mispred) {
        bpu_hit_count := bpu_hit_count + 1.U
      }
    }

    for (i <- 0 until issueWidth) {
      val is_link = exe_uops(i).bits.ctrl.jal || exe_uops(i).bits.ctrl.jalr
      exe_wb_data(i) := Mux(is_link, exe_uops(i).bits.pc + 4.U, alus(i).io.out)
    }
    dontTouch(kill_on_mispred)

    // -----------------------------------------------------------------------
    // Writeback
    // -----------------------------------------------------------------------
    val wb_data = Wire(Vec.fill(issueWidth)(UInt(XLEN.W)))

    for (i <- 0 until issueWidth) {
      wb_uops(i) := exe_uops(i)
      wb_data(i) := exe_wb_data(i)
    }

    rob.io.wb_req := wb_uops
    rob.io.wb_data := wb_data

    for (i <- 0 until issueWidth) {
      prf.io.write_ports(i).valid := wb_uops(i).valid && wb_uops(i).bits.ctrl.rd_wen
      prf.io.write_ports(i).addr := wb_uops(i).bits.prd
      prf.io.write_ports(i).data := Mux(wb_uops(i).bits.lrd === 0.U, 0.U, wb_data(i))
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

    val comm_cfi_mask = comm_uops.map(u => u.valid && u.bits.ctrl.is_cfi)
    val comm_cfi_idx  = PriorityEncoder(Cat(comm_cfi_mask.reverse))
    io.ifu.commit.valid := comm_cfi_mask.reduce(_ || _)
    io.ifu.commit.ftq_idx := comm_uops(comm_cfi_idx).bits.ftq_idx

    // -----------------------------------------------------------------------
    // Reset
    // -----------------------------------------------------------------------
    when (reset.asBool) {
      for (i <- 0 until issueWidth) {
        exe_uops(i).valid := false.B
      }
    }

    dontTouch(io.cosim_info)
    dontTouch(dec_stall)
    dontTouch(dis_stall)
    dontTouch(dis_uops)
    dontTouch(iss_uops)
    dontTouch(iss_uops)
    dontTouch(exe_uops)
    dontTouch(wb_uops)
    dontTouch(wb_data)
    dontTouch(comm_uops)
  }
