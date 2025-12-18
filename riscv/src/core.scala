package riscv

import hdl._
import CoreConstants.{ALUOp1, ALUOp2}

case class RetireInfoIf(
  wb_valid: Vec[Bool],
  wb_data: Vec[UInt],
  wb_rd: Vec[UInt],
) extends Bundle[RetireInfoIf]

object RetireInfoIf:
  def apply(p: CoreParams): RetireInfoIf =
    RetireInfoIf(
      wb_valid = Output(Vec.fill(p.coreWidth)(Bool())),
      wb_data  = Output(Vec.fill(p.coreWidth)(UInt(p.xlenBits.W))),
      wb_rd    = Output(Vec.fill(p.coreWidth)(UInt(p.xlenBits.W))),
    )

case class CoreIf(
  fetch_uops: Vec[Decoupled[UOp]],
  retire_info: RetireInfoIf
) extends Bundle[CoreIf]

object CoreIf:
  def apply(p: CoreParams): CoreIf =
    CoreIf(
      fetch_uops    = Flipped(Vec.fill(p.coreWidth)(Decoupled(UOp(p)))),
      retire_info   = RetireInfoIf(p)
    )

class Core(p: CoreParams) extends Module:
  val io = IO(CoreIf(p))

  val XLEN = p.xlenBits
  val coreWidth = p.coreWidth

  body {
    io.retire_info.wb_valid.foreach(_ := false.B)
    io.retire_info.wb_data.foreach(_ := DontCare)
    io.retire_info.wb_rd.foreach(_ := DontCare)

    // -----------------------------------------------------------------------
    // Stage 0: Decode & read register operands
    // -----------------------------------------------------------------------
    val dec = Module(new Decoder(p))
    dec.io.enq.zip(io.fetch_uops).foreach((d, f) => {
      d.valid := f.valid
      d.bits  := f.bits
      f.ready := d.ready
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
      dec_ready(i)  := !dec_hazards.take(i+1).reduce(_ || _)
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
    val ex_alu_out = Wire(Vec.fill(coreWidth)(UInt(XLEN.W)))

    for (i <- 0 until coreWidth) {
      when (dec_uops(i).valid && dec_ready(i)) {
        ex_uops(i).valid := true.B
        ex_uops(i).bits  := dec_uops(i).bits

        ex_rs1_rf(i) := Mux(dec_uops(i).bits.rd === 0.U, 0.U, rf(dec_uops(i).bits.rs1))
        ex_rs2_rf(i) := Mux(dec_uops(i).bits.rd === 0.U, 0.U, rf(dec_uops(i).bits.rs2))
        ex_imm(i)    := dec_imms(i)
      } .otherwise {
        ex_uops(i).valid := false.B
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

      ex_alu_out(i) := alu(i).io.out
    }

    dontTouch(ex_uops)
    dontTouch(ex_rs1_rf)
    dontTouch(ex_rs2_rf)
    dontTouch(ex_imm)
    dontTouch(ex_alu_out)
    // -----------------------------------------------------------------------
    // Stage 2: Mem
    // -----------------------------------------------------------------------
    val mem_uops    = Reg(Vec.fill(coreWidth)(Valid(UOp(p))))
    val mem_alu_out = Reg(Vec.fill(coreWidth)(UInt(XLEN.W)))

    for (i <- 0 until coreWidth) {
      mem_uops(i) := ex_uops(i)
      mem_alu_out(i) := ex_alu_out(i)
    }

    dontTouch(mem_uops)
    dontTouch(mem_alu_out)
    // -----------------------------------------------------------------------
    // Stage 3: Writeback
    // -----------------------------------------------------------------------
    val wb_uops  = Reg(Vec.fill(coreWidth)(Valid(UOp(p))))
    val wb_wdata = Reg(Vec.fill(coreWidth)(UInt(XLEN.W)))

    for (i <- 0 until coreWidth) {
      wb_uops(i)  := mem_uops(i)
      wb_wdata(i) := mem_alu_out(i)
    }

    for (i <- 0 until coreWidth) {
      when (wb_uops(i).valid && wb_uops(i).bits.ctrl.rd_wen) {
        rf(wb_uops(i).bits.rd) := wb_wdata(i)

        io.retire_info.wb_valid(i) := true.B
        io.retire_info.wb_data(i)  := wb_wdata(i)
        io.retire_info.wb_rd(i)    := wb_uops(i).bits.rd
      }
    }

    dontTouch(wb_uops)
    dontTouch(wb_wdata)
    ///////////////////////////////////////////////////////////////////////////

    val inflight_uops: Seq[Valid[UOp]] = ex_uops.toSeq ++ mem_uops.toSeq ++ wb_uops.toSeq

    // No bypass network for now
    dec_hazards.zipWithIndex.foreach((h, i) => {
      val dec_hazards = dec_uops.take(i).map(prev_uop => {
        val prev_rd = prev_uop.bits.rd
        prev_uop.valid &&
        (prev_rd === dec_uops(i).bits.rs1 ||
         prev_rd === dec_uops(i).bits.rs2 ||
         prev_rd === dec_uops(i).bits.rd)
      }).foldLeft(false.B)(_ || _)

      val inflight_hazards = inflight_uops.map(prev_uop => {
        val prev_rd = prev_uop.bits.rd
        prev_uop.valid &&
        (prev_rd === dec_uops(i).bits.rs1 ||
         prev_rd === dec_uops(i).bits.rs2)
      }).reduce(_ || _)

      h := dec_hazards || inflight_hazards
    })

    ///////////////////////////////////////////////////////////

    // x0 is always 0
    rf(0) := 0.U

    when (reset.asBool) {
      for (i <- 0 until coreWidth) {
        dec_uops(i).valid := false.B
        ex_uops(i).valid := false.B
        mem_uops(i).valid := false.B
        wb_uops(i).valid := false.B
      }
    }
  }
