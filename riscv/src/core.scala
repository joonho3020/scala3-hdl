package riscv

import hdl._

case class CoreIf(
  fetch_uops: Vec[Decoupled[UOp]],

  alu_valid: Bool,
  alu_out: UInt,
  alu_adder_out: UInt,
  alu_cmp_out: Bool
) extends Bundle[CoreIf]

object CoreIf:
  def apply(p: CoreParams): CoreIf =
    CoreIf(
      fetch_uops    = Flipped(Vec.fill(p.coreWidth)(Decoupled(UOp(p)))),

      alu_valid     = Output(Bool()),
      alu_out       = Output(UInt(p.xlenBits.W)),
      alu_adder_out = Output(UInt(p.xlenBits.W)),
      alu_cmp_out   = Output(Bool()),
    )

class Core(p: CoreParams) extends Module:
  val io = IO(CoreIf(p))

  val XLEN = p.xlenBits
  val coreWidth = p.coreWidth

  body {
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

    val rf = RegInit(Vec.fill(32)(0.U(XLEN.W)))

    val dec_uops   = Wire(Vec.fill(coreWidth)(Valid(UOp(p))))
    val dec_ready = Wire(Vec.fill(coreWidth)(Bool()))
    val dec_hazards = Wire(Vec.fill(coreWidth)(Bool()))
    dec_uops  .zip(dec.io.deq.map(_.bits )).foreach((u, d) => u.bits  := d)
    dec_uops  .zip(dec.io.deq.map(_.valid)).foreach((u, d) => u.valid := d)
    dec_ready.zip(dec.io.deq.map(_.ready)).foreach((u, d) => d := u)

    for (i <- 0 until coreWidth) {
      dec_ready(i)  := !dec_hazards(i)
    }


    // -----------------------------------------------------------------------
    // Stage 1: Execute
    // -----------------------------------------------------------------------
    val ex_uops     = Reg(Vec.fill(coreWidth)(Valid(UOp(p))))
    val ex_rs1_rf  = Reg(Vec.fill(coreWidth)(UInt(XLEN.W)))
    val ex_rs2_rf  = Reg(Vec.fill(coreWidth)(UInt(XLEN.W)))
    val ex_alu_out = Wire(Vec.fill(coreWidth)(UInt(XLEN.W)))

    for (i <- 0 until coreWidth) {
      when (dec_uops(i).valid && dec_ready(i)) {
        ex_uops(i).valid := true.B
        ex_uops(i).bits  := dec_uops(i).bits

        ex_rs1_rf   := Mux(dec_uops(i).bits.rd === 0.U, 0.U, rf(dec_uops(i).bits.rs1))
        ex_rs2_rf   := Mux(dec_uops(i).bits.rd === 0.U, 0.U, rf(dec_uops(i).bits.rs2))
      } .otherwise {
        ex_uops(i).valid := false.B
      }
    }

    val alu = Seq.fill(coreWidth)(Module(new ALU(ALUParams(XLEN))))

    for (i <- 0 until coreWidth) {
      alu(i).io.fn  := ex_uops(i).bits.aluOp
      alu(i).io.in1 := ex_rs1_rf(i)
      alu(i).io.in2 := ex_rs2_rf(i)

      ex_alu_out(i) := alu(i).io.out
    }

    // -----------------------------------------------------------------------
    // Stage 2: Mem
    // -----------------------------------------------------------------------
    val mem_uops    = Reg(Vec.fill(coreWidth)(Valid(UOp(p))))
    val mem_alu_out = Reg(Vec.fill(coreWidth)(UInt(XLEN.W)))

    for (i <- 0 until coreWidth) {
      mem_uops(i) := ex_uops(i)
      mem_alu_out(i) := ex_alu_out(i)
    }

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
      when (wb_uops(i).valid && wb_uops(i).bits.rd_wen) {
        rf(wb_uops(i).bits.rd) := wb_wdata(i)
      }
    }

    ///////////////////////////////////////////////////////////////////////////

    val inflight_uops: Seq[Valid[UOp]] = ex_uops.toSeq ++ mem_uops.toSeq ++ wb_uops.toSeq

    dec_hazards.zipWithIndex.foreach((h, i) => {
      val dec_hazards = dec_uops.take(i).map(prev_uop => {
        val prev_rd = prev_uop.bits.rd
        prev_uop.valid &&
        (prev_rd === dec_uops(i).bits.rs1 ||
         prev_rd === dec_uops(i).bits.rs2 ||
         prev_rd === dec_uops(i).bits.rd)
      }).reduce(_ || _)

      val inflight_hazards = inflight_uops.map(prev_uop => {
        val prev_rd = prev_uop.bits.rd
        prev_uop.valid &&
        (prev_rd === dec_uops(i).bits.rs1 ||
         prev_rd === dec_uops(i).bits.rs2)
      }).reduce(_ || _)

      h := dec_hazards || inflight_hazards
    })

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

