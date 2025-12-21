package riscv

import hdl._
import CoreConstants.{ALUOp1, ALUOp2, MemOp}

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
) extends Bundle[RetireInfoIf]

object RetireInfoIf:
  def apply(p: CoreParams): RetireInfoIf =
    RetireInfoIf(
      valid    = Output(Bool()),
      pc       = Output(UInt(p.pcBits.W)),
      wb_valid = Output(Bool()),
      wb_data  = Output(UInt(p.xlenBits.W)),
      wb_rd    = Output(UInt(p.xlenBits.W)),
    )

case class CoreIf(
  fetch_uops: Vec[Decoupled[UOp]],
  redirect: RedirectIf,
  retire_info: Vec[RetireInfoIf],
  bpu_update: Valid[BPUUpdate],
  mem: MagicMemIf
) extends Bundle[CoreIf]

object CoreIf:
  def apply(p: CoreParams): CoreIf =
    CoreIf(
      fetch_uops    = Flipped(Vec.fill(p.coreWidth)(Decoupled(UOp(p)))),
      redirect      = RedirectIf(p),
      retire_info   = Vec.fill(p.coreWidth)(RetireInfoIf(p)),
      bpu_update    = Valid(BPUUpdate(p)),
      mem           = MagicMemIf(p)
    )

class Core(p: CoreParams) extends Module with CoreCacheable(p):
  given Module = this
  val io = IO(CoreIf(p))

  val XLEN = p.xlenBits
  val coreWidth = p.coreWidth

  body {
    io.retire_info.foreach(ri => {
      ri.valid := false.B
      ri.pc := DontCare
      ri.wb_valid := DontCare
      ri.wb_data := DontCare
      ri.wb_rd := DontCare
    })

    val dec_clear = WireInit(false.B)
    val  ex_clear = WireInit(false.B)
    val mem_clear = WireInit(false.B)

    val dec_stall = WireInit(false.B)
    val  ex_stall = WireInit(false.B)
    val mem_stall = WireInit(false.B)

    dontTouch(dec_clear)
    dontTouch( ex_clear)
    dontTouch(mem_clear)
    dontTouch(dec_stall)
    dontTouch( ex_stall)
    dontTouch(mem_stall)

    val dcache = Module(new DCache(p, log2Ceil(coreWidth)))
    dcache.io.mem.req.ready := io.mem.req.ready
    io.mem.req.valid := dcache.io.mem.req.valid
    io.mem.req.bits := dcache.io.mem.req.bits
    dcache.io.mem.resp := io.mem.resp

    dcache.io.core.s0_req.valid := false.B
    dcache.io.core.s0_req.bits := DontCare
    dcache.io.core.s1_kill := false.B
    dcache.io.core.s1_paddr := DontCare
    dcache.io.core.s2_kill := false.B

    val dc_req_fire = dcache.io.core.s0_req.fire
    val dc_resp_valid = dcache.io.core.s2_resp.valid
    val dc_resp_data = dcache.io.core.s2_resp.bits.data

    val mem_waiting = Reg(Bool())

    val mem_has_mem_op = Wire(Bool())

    // -----------------------------------------------------------------------
    // Stage 0: Decode & read register operands
    // -----------------------------------------------------------------------
    val dec = Module(new Decoder(p))
    dec.io.enq.zip(io.fetch_uops).foreach((d, f) => {
      d.valid := f.valid && !dec_stall
      d.bits  := f.bits
      f.ready := d.ready && !dec_stall
    })

    dec.io.deq.foreach(x => x.ready := false.B)

    val rf = Reg(Vec.fill(32)(UInt(XLEN.W)))

    val dec_uops   = Wire(Vec.fill(coreWidth)(Valid(UOp(p))))
    val dec_ready = Wire(Vec.fill(coreWidth)(Bool()))
    val dec_hazards = Wire(Vec.fill(coreWidth)(Bool()))
    dec_uops .zip(dec.io.deq.map(_.bits )).foreach((u, d) => u.bits  := d)
    dec_uops .zip(dec.io.deq.map(_.valid)).foreach((u, d) => u.valid := d)
    dec_ready.zip(dec.io.deq.map(_.ready)).foreach((u, d) => d := u)

    for (i <- 0 until coreWidth) {
      dec_ready(i)  := !dec_hazards.take(i+1).reduce(_ || _) && !dec_stall
    }

    val imm_gens = Seq.fill(coreWidth)(Module(new ImmGen(XLEN)))
    val dec_imms = Wire(Vec.fill(coreWidth)(UInt(XLEN.W)))

    for (i <- 0 until coreWidth) {
      imm_gens(i).io.inst := dec_uops(i).bits.inst
      imm_gens(i).io.sel := dec_uops(i).bits.ctrl.sel_imm
      dec_imms(i) := imm_gens(i).io.out
    }

    dontTouch(dec_uops)

    // -----------------------------------------------------------------------
    // Stage 1: Execute
    // -----------------------------------------------------------------------
    val ex_uops    = Reg(Vec.fill(coreWidth)(Valid(UOp(p))))
    val ex_rs1_rf  = Reg(Vec.fill(coreWidth)(UInt(XLEN.W)))
    val ex_rs2_rf  = Reg(Vec.fill(coreWidth)(UInt(XLEN.W)))
    val ex_imm     = Reg(Vec.fill(coreWidth)(UInt(XLEN.W)))

    for (i <- 0 until coreWidth) {
      when (!ex_stall) {
        when (dec_uops(i).valid && dec_ready(i) && !ex_clear) {
          ex_uops(i).valid := true.B
          ex_uops(i).bits  := dec_uops(i).bits

          ex_rs1_rf(i) := rf(dec_uops(i).bits.rs1)
          ex_rs2_rf(i) := rf(dec_uops(i).bits.rs2)
          ex_imm(i)    := dec_imms(i)
        } .otherwise {
          ex_uops(i).valid := false.B
        }
      }
    }

    val alu = Seq.fill(coreWidth)(Module(new ALU(ALUParams(XLEN))))

    for (i <- 0 until coreWidth) {
      val pc_ext =
        if XLEN > p.pcBits then Cat(Seq(0.U((XLEN - p.pcBits).W), ex_uops(i).bits.pc))
        else ex_uops(i).bits.pc

      val rs2_shamt = ex_rs2_rf(i)(log2Ceil(XLEN) - 1, 0)
      val imm_shamt = ex_imm(i)(log2Ceil(XLEN) - 1, 0)
      val rs2_oh = (1.U(XLEN.W) << rs2_shamt)(XLEN - 1, 0)
      val imm_oh = (1.U(XLEN.W) << imm_shamt)(XLEN - 1, 0)

      alu(i).io.fn  := ex_uops(i).bits.ctrl.alu_op

      alu(i).io.dw  := CoreConstants.DW.DW64.EN

      switch (ex_uops(i).bits.ctrl.sel_alu1) {
        import ALUOp1._
        is(ZERO  .EN) { alu(i).io.in1 := 0.U(XLEN.W)  }
        is(RS1   .EN) { alu(i).io.in1 := ex_rs1_rf(i) }
        is(PC    .EN) { alu(i).io.in1 := pc_ext       }
        is(RS1SHL.EN) { alu(i).io.in1 := (ex_rs1_rf(i) << 12)(XLEN - 1, 0) }
        default       { alu(i).io.in1 := DontCare     }
      }

      switch (ex_uops(i).bits.ctrl.sel_alu2) {
        import ALUOp2._
        is(ZERO  .EN) { alu(i).io.in2 := 0.U(XLEN.W)  }
        is(RS2   .EN) { alu(i).io.in2 := ex_rs2_rf(i) }
        is(IMM   .EN) { alu(i).io.in2 := ex_imm(i)    }
        is(RS2OH .EN) { alu(i).io.in2 := rs2_oh       }
        is(IMMOH .EN) { alu(i).io.in2 := imm_oh       }
        default       { alu(i).io.in2 := DontCare     }
      }

    }

    dontTouch(ex_uops)
    dontTouch(ex_rs1_rf)
    dontTouch(ex_rs2_rf)
    dontTouch(ex_imm)

    // -----------------------------------------------------------------------
    // Stage 2: Mem
    // -----------------------------------------------------------------------
    val mem_uops    = Reg(Vec.fill(coreWidth)(Valid(UOp(p))))
    val mem_alu_out = Reg(Vec.fill(coreWidth)(UInt(XLEN.W)))
    val mem_alu_cmp_out = Reg(Vec.fill(coreWidth)(Bool()))
    val mem_imm     = Reg(Vec.fill(coreWidth)(UInt(XLEN.W)))
    val mem_rs2     = Reg(Vec.fill(coreWidth)(UInt(XLEN.W)))

    for (i <- 0 until coreWidth) {
      when (!mem_stall) {
        mem_uops(i).valid := ex_uops(i).valid && !ex_clear
        mem_uops(i).bits  := ex_uops(i).bits
        mem_uops(i).bits.taken := alu(i).io.cmp_out

        mem_alu_out(i)     := alu(i).io.out
        mem_alu_cmp_out(i) := alu(i).io.cmp_out
        mem_imm(i) := ex_imm(i)
        mem_rs2(i) := ex_rs2_rf(i)
      }
    }

    val mem_branch_target = Wire(Vec.fill(coreWidth)(UInt(p.pcBits.W)))
    val mem_cfi_target = Wire(Vec.fill(coreWidth)(UInt(p.pcBits.W)))
    val mem_cfi_valids = Wire(Vec.fill(coreWidth)(Bool()))
    val mem_cfi_taken = Wire(Vec.fill(coreWidth)(Bool()))
    val mem_cfi_idx = Wire(UInt(log2Ceil(coreWidth + 1).W))
    val bpu_update = Wire(Valid(BPUUpdate(p)))

    for (i <- 0 until coreWidth) {
      val pc   = mem_uops(i).bits.pc
      val ctrl = mem_uops(i).bits.ctrl

      val jump_target = mem_alu_out(i)(p.pcBits - 1, 0)
      mem_branch_target(i) := (pc + mem_imm(i)(p.pcBits - 1, 0))
      mem_cfi_target(i) := Mux(ctrl.jal || ctrl.jalr,  jump_target, mem_branch_target(i))

      val branch_taken = ctrl.br && mem_alu_cmp_out(i)
      mem_cfi_taken(i) := mem_uops(i).valid && (ctrl.jal || ctrl.jalr || branch_taken)
    }

    dontTouch(mem_branch_target)
    dontTouch(mem_cfi_target)
    dontTouch(mem_cfi_taken)
    dontTouch(mem_cfi_valids)
    dontTouch(mem_cfi_idx)
    dontTouch(bpu_update)

    mem_cfi_valids.zip(mem_uops).foreach((v, u) => {
      v := u.valid && u.bits.ctrl.is_cfi
    })

    mem_cfi_idx := PriorityEncoder(Cat(mem_cfi_valids.reverse))

    val cfi_uop = mem_uops(mem_cfi_idx)
    val mem_target_pc = mem_cfi_target(mem_cfi_idx)
    val mem_brjmp_taken = mem_cfi_taken(mem_cfi_idx)
    bpu_update.valid       := mem_cfi_valids.reduce(_ || _)
    bpu_update.bits.pc     := mem_uops(mem_cfi_idx).bits.pc
    bpu_update.bits.target := mem_target_pc
    bpu_update.bits.taken  := mem_brjmp_taken
    bpu_update.bits.is_call := (cfi_uop.bits.ctrl.jal || cfi_uop.bits.ctrl.jalr) && cfi_uop.bits.rd === 1.U
    bpu_update.bits.is_ret  := cfi_uop.bits.ctrl.jalr && cfi_uop.bits.rs1 === 1.U && cfi_uop.bits.rd === 0.U

    val mem_wrong_next_pc = cfi_uop.bits.next_pc.bits =/= mem_target_pc
    val mem_mispred_taken = mem_brjmp_taken && (mem_wrong_next_pc || !cfi_uop.bits.next_pc.valid)
    val mem_mispred_not_taken = (cfi_uop.bits.ctrl.br && !mem_brjmp_taken) && cfi_uop.bits.next_pc.valid
    val mem_redirect_valid = mem_mispred_taken || mem_mispred_not_taken

    val mem_predict_success = WireInit(false.B)

    mem_predict_success := !mem_redirect_valid && bpu_update.valid
    dontTouch(mem_predict_success)

    io.redirect.valid  := mem_redirect_valid && !mem_stall
    io.redirect.target := mem_target_pc
    io.bpu_update.valid := bpu_update.valid && !mem_stall
    io.bpu_update.bits := bpu_update.bits

    when (mem_redirect_valid && !mem_stall) {
      dec_clear := true.B
      ex_clear := true.B
    }

    val mem_flush = Wire(Vec.fill(coreWidth)(Bool()))
    for (i <- 0 until coreWidth) {
      val earlier_cfi_taken = if (i == 0) false.B else mem_cfi_taken.take(i).reduce(_ || _)
      mem_flush(i) := earlier_cfi_taken
    }
    dontTouch(mem_flush)

    val mem_wdata = Wire(Vec.fill(coreWidth)(UInt(XLEN.W)))
    for (i <- 0 until coreWidth) {
      val is_link = mem_uops(i).bits.ctrl.jal || mem_uops(i).bits.ctrl.jalr
      mem_wdata(i) := Mux(is_link, mem_uops(i).bits.pc + 4.U, mem_alu_out(i))
    }
    dontTouch(mem_uops)
    dontTouch(mem_wdata)

    val mem_mem_valids = mem_uops.map(u => u.valid && u.bits.ctrl.is_mem)
    mem_has_mem_op := mem_mem_valids.reduce(_ || _)
    val mem_mem_idx = PriorityEncoder(Cat(mem_mem_valids.reverse))
    val mem_mem_uop = mem_uops(mem_mem_idx)
    val mem_mem_addr = mem_alu_out(mem_mem_idx)
    val mem_mem_wdata = mem_rs2(mem_mem_idx)

    val mem_req_sent = Reg(Bool())

    when (!mem_waiting && mem_has_mem_op && !mem_flush(mem_mem_idx)) {
      dcache.io.core.s0_req.valid := !mem_req_sent
      dcache.io.core.s0_req.bits.vaddr := mem_mem_addr
      dcache.io.core.s0_req.bits.data := mem_mem_wdata
      dcache.io.core.s0_req.bits.size := mem_mem_uop.bits.ctrl.mem_width
      dcache.io.core.s0_req.bits.signed := mem_mem_uop.bits.ctrl.mem_signed
      dcache.io.core.s0_req.bits.tpe := mem_mem_uop.bits.ctrl.mem_op
      dcache.io.core.s0_req.bits.tag := mem_mem_idx

      when (dc_req_fire) {
        mem_waiting := true.B
        mem_req_sent := true.B
      }
    }

    // No virtual addressing for now
    dcache.io.core.s1_paddr.valid := RegNext(dc_req_fire)
    dcache.io.core.s1_paddr.bits := RegNext(mem_mem_addr)

    dcache.io.core.s1_kill := false.B
    dcache.io.core.s2_kill := false.B

    when (dc_resp_valid) {
      mem_waiting := false.B
    }

    mem_stall := (mem_has_mem_op && !mem_flush(mem_mem_idx) && !dc_resp_valid) || mem_waiting
    ex_stall := mem_stall
    dec_stall := mem_stall

    when (!mem_stall) {
      mem_req_sent := false.B
    }

    // -----------------------------------------------------------------------
    // Stage 3: Writeback
    // -----------------------------------------------------------------------
    val wb_uops  = Reg(Vec.fill(coreWidth)(Valid(UOp(p))))
    val wb_wdata = Reg(Vec.fill(coreWidth)(UInt(XLEN.W)))
    val wb_is_load = Reg(Vec.fill(coreWidth)(Bool()))
    val wb_load_data = Reg(UInt(XLEN.W))

    for (i <- 0 until coreWidth) {
      when (!mem_stall) {
        wb_uops(i)  := mem_uops(i)
        wb_uops(i).valid := mem_uops(i).valid && !mem_flush(i)
        wb_wdata(i) := mem_wdata(i)
        wb_is_load(i) := mem_uops(i).bits.ctrl.is_load
      }
    }

    when (dc_resp_valid) {
      wb_load_data := dc_resp_data
    }

    for (i <- 0 until coreWidth) {
      when (wb_uops(i).valid && wb_uops(i).bits.ctrl.rd_wen) {
        val wdata = Mux(wb_is_load(i), wb_load_data, wb_wdata(i))
        rf(wb_uops(i).bits.rd) := wdata
      }
    }

    for (i <- 0 until coreWidth) {
      io.retire_info(i).valid    := wb_uops(i).valid
      io.retire_info(i).pc       := wb_uops(i).bits.pc
      io.retire_info(i).wb_valid := wb_uops(i).bits.ctrl.rd_wen
      io.retire_info(i).wb_data  := Mux(wb_is_load(i), wb_load_data, wb_wdata(i))
      io.retire_info(i).wb_rd    := wb_uops(i).bits.rd
    }

    dontTouch(wb_uops)
    dontTouch(wb_wdata)
    ///////////////////////////////////////////////////////////////////////////

    val inflight_uops: Seq[Valid[UOp]] = ex_uops.toSeq ++ mem_uops.toSeq ++ wb_uops.toSeq

    dec_hazards.zipWithIndex.foreach((h, i) => {
      val dec_hazards_local = dec_uops.take(i).map(prev_uop => {
        val prev_rd = prev_uop.bits.rd
        prev_uop.valid &&
        (prev_rd === dec_uops(i).bits.rs1 ||
         prev_rd === dec_uops(i).bits.rs2 ||
         prev_rd === dec_uops(i).bits.rd)
      }).foldLeft(false.B)(_ || _)

      val dec_prev_has_cfi = dec_uops.take(i).map(prev_uop => {
        prev_uop.valid && prev_uop.bits.ctrl.is_cfi
      }).foldLeft(false.B)(_ || _)

      val dec_prev_has_mem = dec_uops.take(i).map(prev_uop => {
        prev_uop.valid && prev_uop.bits.ctrl.is_mem
      }).foldLeft(false.B)(_ || _)

      val inflight_hazards = inflight_uops.map(prev_uop => {
        val prev_rd = prev_uop.bits.rd
        prev_uop.valid &&
        (prev_rd === dec_uops(i).bits.rs1 ||
         prev_rd === dec_uops(i).bits.rs2)
      }).reduce(_ || _)

      h := dec_hazards_local || inflight_hazards || dec_prev_has_cfi || dec_prev_has_mem
    })

    ///////////////////////////////////////////////////////////

    rf(0) := 0.U

    when (reset.asBool) {
      for (i <- 0 until coreWidth) {
        dec_uops(i).valid := false.B
        ex_uops(i).valid := false.B
        mem_uops(i).valid := false.B
        wb_uops(i).valid := false.B
      }
      mem_waiting := false.B
      mem_req_sent := false.B
    }

    dontTouch(io.redirect)
  }
