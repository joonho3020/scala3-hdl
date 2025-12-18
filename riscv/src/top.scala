package riscv

import hdl._

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
