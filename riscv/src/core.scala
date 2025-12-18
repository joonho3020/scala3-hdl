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

    val s0_uop   = Wire(Vec.fill(p.coreWidth)(UOp(p)))
    val s0_valid = Wire(Vec.fill(p.coreWidth)(Bool()))
    val s0_ready = Wire(Vec.fill(p.coreWidth)(Bool()))
    s0_uop  .zip(dec.io.deq.map(_.bits )).foreach((u, d) => u := d)
    s0_valid.zip(dec.io.deq.map(_.valid)).foreach((u, d) => u := d)
    s0_ready.zip(dec.io.deq.map(_.ready)).foreach((u, d) => d := u)

    // -----------------------------------------------------------------------
    // Stage 1: Decode & read register operands
    // -----------------------------------------------------------------------

    val s1_valid   = RegInit(Vec.fill(p.coreWidth)(false.B))
    val s1_uop     = Reg(Vec.fill(p.coreWidth)(UOp(p)))
    val s1_rs1_rf  = Reg(Vec.fill(p.coreWidth)(UInt(XLEN.W)))
    val s1_rs2_rf  = Reg(Vec.fill(p.coreWidth)(UInt(XLEN.W)))

    for (i <- 0 until p.coreWidth) {
      when (s0_valid(i) && s0_ready(i)) {
        s1_valid(i) := true.B
        s1_uop(i)   := s0_uop(i)
        s1_rs1_rf   := Mux(s0_uop(i).rd === 0.U, 0.U, rf(s0_uop(i).rs1))
        s1_rs2_rf   := Mux(s0_uop(i).rd === 0.U, 0.U, rf(s0_uop(i).rs2))
      }
    }


    val alu = Seq.fill(p.coreWidth)(Module(new ALU(ALUParams(XLEN))))


    for (i <- 0 until p.coreWidth) {
      dec.io.deq(i).ready := ???
    }

    // x0 is always 0
    rf(0) := 0.U
  }

case class CoreTopIO(
  mem: MagicMemIf,

  alu_valid: Bool,
  alu_out: UInt,
  alu_adder_out: UInt,
  alu_cmp_out: Bool
) extends Bundle[CoreTopIO]

object CoreTopIO:
  def apply(p: CoreParams): CoreTopIO =
    CoreTopIO(
      mem = MagicMemIf(p),

      alu_valid     = Output(Bool()),
      alu_out       = Output(UInt(p.xlenBits.W)),
      alu_adder_out = Output(UInt(p.xlenBits.W)),
      alu_cmp_out   = Output(Bool()),
    )

class Tile(p: CoreParams) extends Module:
  val io = IO(CoreTopIO(p))
  body {
    dontTouch(io)

    val frontend = Module(new Frontend(p))

    io.mem := frontend.io.mem

    val core = Module(new Core(p))
    core.io.fetch_uops := frontend.io.uops


    io.alu_valid     := core.io.alu_valid
    io.alu_out       := core.io.alu_out
    io.alu_adder_out := core.io.alu_adder_out
    io.alu_cmp_out   := core.io.alu_cmp_out
  }
