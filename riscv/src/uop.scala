package riscv

import hdl._

case class UOp(
  pc: UInt,
  inst: UInt,
  opcode: UInt,
  funct3: UInt,
  funct7: UInt,
  rs1: UInt,
  rs2: UInt,
  rd:  UInt,
  rd_valid: Bool,
  aluOp: HWEnum[ALUParams.Opcode],
) extends Bundle[UOp]

object UOp:
  def apply(p: CoreParams): UOp =
    UOp(
      pc = UInt(p.pcBits.W),
      inst = UInt(p.xlenBits.W),
      opcode = UInt(7.W),
      funct3 = UInt(3.W),
      funct7 = UInt(7.W),
      rs1    = UInt(5.W),
      rs2    = UInt(5.W),
      rd     = UInt(5.W),
      rd_valid = Bool(),
      aluOp  = new HWEnum(ALUParams.Opcode)
    )
