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
    val kill_on_mispred = Wire(Bool())

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
    decoder.io.enq.zip(io.fetch_uops).foreach((d, f) => d <> f)

    // -----------------------------------------------------------------------
    // Branch Tag Allocation (between decode and rename)
    // -----------------------------------------------------------------------
    val dec_cfi_mask = decoder.io.deq.map(d => d.valid && d.bits.ctrl.is_cfi)
    val dec_cfi_cnt = dec_cfi_mask.map(_.asUInt).reduce(_ +& _)
    val br_tag_stall = dec_cfi_cnt > br_tag_mgr.io.count

    val dec_uops_with_branch = Wire(Vec.fill(coreWidth)(Valid(UOp(p))))

    for (i <- 0 until coreWidth) {
      val req_valid = decoder.io.deq(i).valid && !br_tag_stall && !kill_on_mispred
      br_tag_mgr.io.req_valid(i) := req_valid
      br_tag_mgr.io.req_cfi(i) := req_valid && decoder.io.deq(i).bits.ctrl.is_cfi

      dec_uops_with_branch(i).valid := decoder.io.deq(i).valid
      dec_uops_with_branch(i).bits := decoder.io.deq(i).bits
      dec_uops_with_branch(i).bits.br_tag := br_tag_mgr.io.resp_tag(i)
      dec_uops_with_branch(i).bits.br_mask := br_tag_mgr.io.resp_mask(i)
    }

    // -----------------------------------------------------------------------
    // Rename 0 & 1
    // -----------------------------------------------------------------------
    val dec_stall = WireInit(false.B)

    for (i <- 0 until coreWidth) {
      val renamer_ready = !dec_stall && !br_tag_stall && !kill_on_mispred && (i.U < renamer.io.free_count)
      renamer.io.dec_uops(i).valid := dec_uops_with_branch(i).valid && renamer_ready
      renamer.io.dec_uops(i).bits  := dec_uops_with_branch(i).bits
      decoder.io.deq(i).ready := renamer_ready
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

    for (i <- 0 until issueWidth) {
      exe_uops(i).valid := iss_uops(i).valid && !kill_on_mispred
      exe_uops(i).bits := iss_uops(i).bits

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
    val exe_is_cfi = exe_uops.map(u => u.valid && u.bits.ctrl.is_cfi)
    val exe_cfi_valid = exe_is_cfi.reduce(_ || _)
    val exe_cfi_idx = PriorityEncoder(Cat(exe_is_cfi.reverse))
    val cfi_uop = exe_uops(exe_cfi_idx)
    val exe_cfi_cnt = exe_is_cfi.map(_.asUInt).reduce(_ +& _)
    Assert(exe_cfi_cnt <= 1.U, s"Cannot issue more than one CFI, got ${exe_cfi_cnt}")

    val cfi_cmp_out = MuxOneHot(PriorityEncoderOH(Cat(exe_is_cfi.reverse)), alus.map(_.io.cmp_out).toSeq)
    val cfi_imm = MuxOneHot(PriorityEncoderOH(Cat(exe_is_cfi.reverse)), imm_gens.map(_.io.out).toSeq)
    val cfi_alu_out = MuxOneHot(PriorityEncoderOH(Cat(exe_is_cfi.reverse)), alus.map(_.io.out).toSeq)

    val branch_taken = cfi_uop.bits.ctrl.br && cfi_cmp_out
    val jump_taken = cfi_uop.bits.ctrl.jal || cfi_uop.bits.ctrl.jalr

    val branch_target = cfi_uop.bits.pc + cfi_imm(p.pcBits-1, 0)
    val jump_target = cfi_alu_out(p.pcBits-1, 0)
    val cfi_target = Mux(cfi_uop.bits.ctrl.br, branch_target, jump_target)(p.pcBits-1, 0)
    val cfi_taken = branch_taken || jump_taken

    val wrong_next_pc = (cfi_uop.bits.next_pc.bits =/= cfi_target).asWire
    val mispred_taken = (cfi_taken && (wrong_next_pc || !cfi_uop.bits.next_pc.valid)).asWire
    val mispred_not_taken = (cfi_uop.bits.ctrl.br && !cfi_taken && cfi_uop.bits.next_pc.valid).asWire
    val has_mispred = (exe_cfi_valid && (mispred_taken || mispred_not_taken)).asWire

    dontTouch(wrong_next_pc)
    dontTouch(mispred_taken)
    dontTouch(mispred_not_taken)
    dontTouch(has_mispred)

    val resolve_target = Mux(mispred_not_taken, cfi_uop.bits.pc + p.instBytes.U, cfi_target)
    val resolve_info = Wire(BranchResolve(p))
    resolve_info.valid := exe_cfi_valid
    resolve_info.tag := cfi_uop.bits.br_tag
    resolve_info.mispredict := has_mispred
    resolve_info.taken := cfi_taken
    resolve_info.target := resolve_target

    dontTouch(resolve_info)

    br_tag_mgr.io.br_resolve  := resolve_info
    renamer.io.br_resolve     := resolve_info
    issue_queue.io.br_resolve := resolve_info

    rob.io.branch_update.valid   := resolve_info.valid
    rob.io.branch_update.mispred := resolve_info.mispredict
    rob.io.branch_update.rob_idx := cfi_uop.bits.rob_idx

    kill_on_mispred := resolve_info.valid && resolve_info.mispredict

    io.redirect.valid := has_mispred
    io.redirect.target := resolve_target

    io.bpu_update.valid := exe_cfi_valid
    io.bpu_update.bits.pc := cfi_uop.bits.pc
    io.bpu_update.bits.target := cfi_target(p.pcBits - 1, 0)
    io.bpu_update.bits.taken := cfi_taken
    io.bpu_update.bits.is_call := (cfi_uop.bits.ctrl.jal || cfi_uop.bits.ctrl.jalr) && cfi_uop.bits.lrd === 1.U
    io.bpu_update.bits.is_ret := cfi_uop.bits.ctrl.jalr && cfi_uop.bits.lrs1 === 1.U && cfi_uop.bits.lrd === 0.U

    // -----------------------------------------------------------------------
    // Writeback
    // -----------------------------------------------------------------------
    val wb_data = Wire(Vec.fill(issueWidth)(UInt(XLEN.W)))

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
      for (i <- 0 until issueWidth) {
        exe_uops(i).valid := false.B
      }
    }

    io.debug_rn2_uops.map(_ := renamer.io.rn2_uops)

    dontTouch(dec_stall)
    dontTouch(dis_stall)
    dontTouch(dis_uops)
    dontTouch(iss_uops)
    dontTouch(iss_uops)
    dontTouch(exe_uops)
    dontTouch(wb_uops)
    dontTouch(wb_data)
    dontTouch(comm_uops)
    dontTouch(io.redirect)
  }
