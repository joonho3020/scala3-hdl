package riscv

import hdl._
import riscv.ALUParams.Opcode

case class UOp(
  pc: UInt,
  inst: UInt,
  opcode: UInt,
  funct3: UInt,
  funct7: UInt,
  rs1: UInt,
  rs2: UInt,
  rd:  UInt,
  taken: Bool,
  next_pc: Valid[UInt],
  ctrl: CtrlSignals,
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
      taken  = Bool(),
      next_pc = Valid(UInt(p.pcBits.W)),
      ctrl   = CtrlSignals()
    )

object CoreConstants:
  enum Immediates:
    case
      IMM_S,
      IMM_SB,
      IMM_U,
      IMM_UJ,
      IMM_I,
      IMM_Z

  enum ALUOp1:
    case
      ZERO,
      RS1,
      PC,
      RS1SHL

  enum ALUOp2:
    case
      ZERO,
      SIZE,
      RS2,
      IMM,
      RS2OH,
      IMMOH

  enum DW:
    case
      DW32,
      DW64

case class CtrlSignals(
  valid:    Bool,
  br:       Bool,
  jal:      Bool,
  jalr:     Bool,
  rd_wen:   Bool,
  sel_imm:  HWEnum[CoreConstants.Immediates],
  sel_alu1: HWEnum[CoreConstants.ALUOp1],
  sel_alu2: HWEnum[CoreConstants.ALUOp2],
  alu_op:   HWEnum[ALUParams.Opcode],
) extends Bundle[CtrlSignals]:

  def is_cfi(using m: Module): Bool =
    valid && (br || jal || jalr)

object CtrlSignals:
  import Instructions._
  import CoreConstants._
  import ALUParams._
  import ALUParams.Opcode._
  import Immediates._
  import ALUOp1._
  import ALUOp2._

  def apply(): CtrlSignals =
    new CtrlSignals(
      valid    = Bool(),
      br       = Bool(),
      jal      = Bool(),
      jalr     = Bool(),
      rd_wen   = Bool(),
      sel_imm  = HWEnum(Immediates),
      sel_alu1 = HWEnum(ALUOp1),
      sel_alu2 = HWEnum(ALUOp2),
      alu_op   = HWEnum(Opcode),
    )

  def default_assign(ctrl: CtrlSignals)(using m: Module) =
    ctrl.valid    := false.B
    ctrl.br       := DontCare
    ctrl.jal      := DontCare
    ctrl.jalr     := DontCare
    ctrl.rd_wen   := DontCare
    ctrl.sel_imm  := DontCare
    ctrl.sel_alu1 := DontCare
    ctrl.sel_alu2 := DontCare
    ctrl.alu_op   := DontCare

  def set(
    ctrl: CtrlSignals,
    valid:    Bool,
    br:       Bool                     | DontCare.type,
    jal:      Bool                     | DontCare.type,
    jalr:     Bool                     | DontCare.type,
    rd_wen:   Bool                     | DontCare.type,
    sel_imm:  HWEnum[Immediates]       | DontCare.type,
    sel_alu1: HWEnum[ALUOp1]           | DontCare.type,
    sel_alu2: HWEnum[ALUOp2]           | DontCare.type,
    alu_op:   HWEnum[ALUParams.Opcode] | DontCare.type
  )(using m: Module) =
    ctrl.valid := valid
    ctrl.br       := br
    ctrl.jal      := jal
    ctrl.jalr     := jalr
    ctrl.rd_wen := rd_wen
    ctrl.sel_imm := sel_imm
    ctrl.sel_alu1 := sel_alu1
    ctrl.sel_alu2 := sel_alu2
    ctrl.alu_op := alu_op

  // TODO: Generate proper decoding logic w/ logic minimizer
  def decode(ctrl: CtrlSignals, inst: UInt)(using m: Module): CtrlSignals =
    def Y = true.B
    def N = false.B
    def X = DontCare

    default_assign(ctrl)
    switch(inst) {
      /*
       *                       rd_wen
       *                        jalr|
       *                       jal| |
       *                     br | | |                        sel_alu2
       *                valid | | | |   sel_imm       sel_alu1      |     alu_op
                            | | | | |         |              |      |          | */
      is(ADD  ) { set(ctrl, Y,N,N,N,Y,        X,        RS1.EN,RS2.EN, FN_ADD.EN) }
      is(SUB  ) { set(ctrl, Y,N,N,N,Y,        X,        RS1.EN,RS2.EN, FN_SUB.EN) }
      is( OR  ) { set(ctrl, Y,N,N,N,Y,        X,        RS1.EN,RS2.EN,  FN_OR.EN) }
      is(AND  ) { set(ctrl, Y,N,N,N,Y,        X,        RS1.EN,RS2.EN, FN_AND.EN) }
      is(XOR  ) { set(ctrl, Y,N,N,N,Y,        X,        RS1.EN,RS2.EN, FN_XOR.EN) }
      is(ADDI ) { set(ctrl, Y,N,N,N,Y, IMM_I.EN,        RS1.EN,IMM.EN, FN_ADD.EN) }
      is(XORI ) { set(ctrl, Y,N,N,N,Y, IMM_I.EN,        RS1.EN,IMM.EN, FN_XOR.EN) }
      is( ORI ) { set(ctrl, Y,N,N,N,Y, IMM_I.EN,        RS1.EN,IMM.EN,  FN_OR.EN) }
      is(ANDI ) { set(ctrl, Y,N,N,N,Y, IMM_I.EN,        RS1.EN,IMM.EN, FN_AND.EN) }
      is( LUI ) { set(ctrl, Y,N,N,N,Y, IMM_U.EN,ALUOp1.ZERO.EN,IMM.EN, FN_ADD.EN) }
      is(AUIPC) { set(ctrl, Y,N,N,N,Y, IMM_U.EN,         PC.EN,IMM.EN, FN_ADD.EN) }
      is(SLT  ) { set(ctrl, Y,N,N,N,Y,        X,        RS1.EN,RS2.EN, FN_SLT.EN) }
      is(SLTI ) { set(ctrl, Y,N,N,N,Y, IMM_I.EN,        RS1.EN,IMM.EN, FN_SLT.EN) }
      is(SLTU ) { set(ctrl, Y,N,N,N,Y, IMM_I.EN,        RS1.EN,RS2.EN,FN_SLTU.EN) }
      is(SLTIU) { set(ctrl, Y,N,N,N,Y, IMM_I.EN,        RS1.EN,IMM.EN,FN_SLTU.EN) }
      is(SLL  ) { set(ctrl, Y,N,N,N,Y,        X,        RS1.EN,RS2.EN,  FN_SL.EN) }
      is(SLLI ) { set(ctrl, Y,N,N,N,Y, IMM_I.EN,        RS1.EN,IMM.EN,  FN_SL.EN) }
      is(SRL  ) { set(ctrl, Y,N,N,N,Y,        X,        RS1.EN,RS2.EN,  FN_SR.EN) }
      is(SRLI ) { set(ctrl, Y,N,N,N,Y, IMM_I.EN,        RS1.EN,IMM.EN,  FN_SR.EN) }
      is(SRA  ) { set(ctrl, Y,N,N,N,Y,        X,        RS1.EN,RS2.EN, FN_SRA.EN) }
      is(SRAI ) { set(ctrl, Y,N,N,N,Y, IMM_I.EN,        RS1.EN,IMM.EN, FN_SRA.EN) }
      is(JAL  ) { set(ctrl, Y,N,Y,N,Y,IMM_UJ.EN,         PC.EN,IMM.EN, FN_ADD.EN) }
      is(JALR ) { set(ctrl, Y,N,N,Y,Y, IMM_I.EN,        RS1.EN,IMM.EN, FN_ADD.EN) }
      is(BEQ  ) { set(ctrl, Y,Y,N,N,N,IMM_SB.EN,        RS1.EN,RS2.EN, FN_SEQ.EN) }
      is(BNE  ) { set(ctrl, Y,Y,N,N,N,IMM_SB.EN,        RS1.EN,RS2.EN, FN_SNE.EN) }
      is(BLT  ) { set(ctrl, Y,Y,N,N,N,IMM_SB.EN,        RS1.EN,RS2.EN, FN_SLT.EN) }
      is(BGE  ) { set(ctrl, Y,Y,N,N,N,IMM_SB.EN,        RS1.EN,RS2.EN, FN_SGE.EN) }
      is(BLTU ) { set(ctrl, Y,Y,N,N,N,IMM_SB.EN,        RS1.EN,RS2.EN,FN_SLTU.EN) }
      is(BGEU ) { set(ctrl, Y,Y,N,N,N,IMM_SB.EN,        RS1.EN,RS2.EN,FN_SGEU.EN) }
    }
    ctrl
