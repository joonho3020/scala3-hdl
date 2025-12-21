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

  enum MemWidth:
    case
      B,
      H,
      W,
      D

  enum MemOp:
    case
      Ld,
      St

case class CtrlSignals(
  valid:    Bool,
  br:       Bool,
  jal:      Bool,
  jalr:     Bool,
  rd_wen:   Bool,
  is_mem:   Bool,
  sel_imm:  HWEnum[CoreConstants.Immediates],
  sel_alu1: HWEnum[CoreConstants.ALUOp1],
  sel_alu2: HWEnum[CoreConstants.ALUOp2],
  alu_op:   HWEnum[ALUParams.Opcode],
  mem_op:   HWEnum[CoreConstants.MemOp],
  mem_width: HWEnum[CoreConstants.MemWidth],
  mem_signed: Bool,
) extends Bundle[CtrlSignals]:

  def is_cfi(using m: Module): Bool =
    valid && (br || jal || jalr)

  def is_load(using m: Module): Bool =
    valid && is_mem && (mem_op === CoreConstants.MemOp.Ld.EN)

  def is_store(using m: Module): Bool =
    valid && is_mem && (mem_op === CoreConstants.MemOp.St.EN)

object CtrlSignals:
  import Instructions._
  import CoreConstants._
  import ALUParams._
  import ALUParams.Opcode._
  import Immediates._
  import ALUOp1._
  import ALUOp2._
  import MemWidth._
  import MemOp._

  def apply(): CtrlSignals =
    new CtrlSignals(
      valid    = Bool(),
      br       = Bool(),
      jal      = Bool(),
      jalr     = Bool(),
      rd_wen   = Bool(),
      is_mem   = Bool(),
      sel_imm  = HWEnum(Immediates),
      sel_alu1 = HWEnum(ALUOp1),
      sel_alu2 = HWEnum(ALUOp2),
      alu_op   = HWEnum(Opcode),
      mem_op   = HWEnum(MemOp),
      mem_width = HWEnum(MemWidth),
      mem_signed = Bool(),
    )

  def default_assign(ctrl: CtrlSignals)(using m: Module) =
    ctrl.valid    := false.B
    ctrl.br       := DontCare
    ctrl.jal      := DontCare
    ctrl.jalr     := DontCare
    ctrl.rd_wen   := DontCare
    ctrl.is_mem   := DontCare
    ctrl.sel_imm  := DontCare
    ctrl.sel_alu1 := DontCare
    ctrl.sel_alu2 := DontCare
    ctrl.alu_op   := DontCare
    ctrl.mem_op   := DontCare
    ctrl.mem_width := DontCare
    ctrl.mem_signed := DontCare

  def set(
    ctrl: CtrlSignals,
    valid:    Bool,
    br:       Bool                     | DontCare.type,
    jal:      Bool                     | DontCare.type,
    jalr:     Bool                     | DontCare.type,
    rd_wen:   Bool                     | DontCare.type,
    is_mem:   Bool                     | DontCare.type,
    sel_imm:  HWEnum[Immediates]       | DontCare.type,
    sel_alu1: HWEnum[ALUOp1]           | DontCare.type,
    sel_alu2: HWEnum[ALUOp2]           | DontCare.type,
    alu_op:   HWEnum[ALUParams.Opcode] | DontCare.type,
    mem_op:   HWEnum[MemOp]            | DontCare.type,
    mem_width: HWEnum[MemWidth]        | DontCare.type,
    mem_signed: Bool                   | DontCare.type,
  )(using m: Module) =
    ctrl.valid := valid
    ctrl.br       ::= br
    ctrl.jal      ::= jal
    ctrl.jalr     ::= jalr
    ctrl.rd_wen ::= rd_wen
    ctrl.is_mem ::= is_mem
    ctrl.sel_imm ::= sel_imm
    ctrl.sel_alu1 ::= sel_alu1
    ctrl.sel_alu2 ::= sel_alu2
    ctrl.alu_op ::= alu_op
    ctrl.mem_op ::= mem_op
    ctrl.mem_width ::= mem_width
    ctrl.mem_signed ::= mem_signed

  // TODO: Generate proper decoding logic w/ logic minimizer
  def decode(ctrl: CtrlSignals, inst: UInt)(using m: Module): CtrlSignals =
    def Y = true.B
    def N = false.B
    def X = DontCare

    default_assign(ctrl)
    switch(inst) {
      /*
       *                         is_mem
       *                       rd_wen |
       *                        jalr| |                                              mem_signed
       *                       jal| | |                                             mem_width |
       *                     br | | | |                        sel_alu2           mem_op    | |
       *                valid | | | | |   sel_imm       sel_alu1      |     alu_op     |    | |
                           | | | | | |         |              |      |          |      |    | |  */
      is(ADD  ) { set(ctrl, Y,N,N,N,Y,N,        X,        RS1.EN,RS2.EN, FN_ADD.EN,    X,   X,X) }
      is(SUB  ) { set(ctrl, Y,N,N,N,Y,N,        X,        RS1.EN,RS2.EN, FN_SUB.EN,    X,   X,X) }
      is( OR  ) { set(ctrl, Y,N,N,N,Y,N,        X,        RS1.EN,RS2.EN,  FN_OR.EN,    X,   X,X) }
      is(AND  ) { set(ctrl, Y,N,N,N,Y,N,        X,        RS1.EN,RS2.EN, FN_AND.EN,    X,   X,X) }
      is(XOR  ) { set(ctrl, Y,N,N,N,Y,N,        X,        RS1.EN,RS2.EN, FN_XOR.EN,    X,   X,X) }
      is(ADDI ) { set(ctrl, Y,N,N,N,Y,N, IMM_I.EN,        RS1.EN,IMM.EN, FN_ADD.EN,    X,   X,X) }
      is(XORI ) { set(ctrl, Y,N,N,N,Y,N, IMM_I.EN,        RS1.EN,IMM.EN, FN_XOR.EN,    X,   X,X) }
      is( ORI ) { set(ctrl, Y,N,N,N,Y,N, IMM_I.EN,        RS1.EN,IMM.EN,  FN_OR.EN,    X,   X,X) }
      is(ANDI ) { set(ctrl, Y,N,N,N,Y,N, IMM_I.EN,        RS1.EN,IMM.EN, FN_AND.EN,    X,   X,X) }
      is( LUI ) { set(ctrl, Y,N,N,N,Y,N, IMM_U.EN,ALUOp1.ZERO.EN,IMM.EN, FN_ADD.EN,    X,   X,X) }
      is(AUIPC) { set(ctrl, Y,N,N,N,Y,N, IMM_U.EN,         PC.EN,IMM.EN, FN_ADD.EN,    X,   X,X) }
      is(SLT  ) { set(ctrl, Y,N,N,N,Y,N,        X,        RS1.EN,RS2.EN, FN_SLT.EN,    X,   X,X) }
      is(SLTI ) { set(ctrl, Y,N,N,N,Y,N, IMM_I.EN,        RS1.EN,IMM.EN, FN_SLT.EN,    X,   X,X) }
      is(SLTU ) { set(ctrl, Y,N,N,N,Y,N, IMM_I.EN,        RS1.EN,RS2.EN,FN_SLTU.EN,    X,   X,X) }
      is(SLTIU) { set(ctrl, Y,N,N,N,Y,N, IMM_I.EN,        RS1.EN,IMM.EN,FN_SLTU.EN,    X,   X,X) }
      is(SLL  ) { set(ctrl, Y,N,N,N,Y,N,        X,        RS1.EN,RS2.EN,  FN_SL.EN,    X,   X,X) }
      is(SLLI ) { set(ctrl, Y,N,N,N,Y,N, IMM_I.EN,        RS1.EN,IMM.EN,  FN_SL.EN,    X,   X,X) }
      is(SRL  ) { set(ctrl, Y,N,N,N,Y,N,        X,        RS1.EN,RS2.EN,  FN_SR.EN,    X,   X,X) }
      is(SRLI ) { set(ctrl, Y,N,N,N,Y,N, IMM_I.EN,        RS1.EN,IMM.EN,  FN_SR.EN,    X,   X,X) }
      is(SRA  ) { set(ctrl, Y,N,N,N,Y,N,        X,        RS1.EN,RS2.EN, FN_SRA.EN,    X,   X,X) }
      is(SRAI ) { set(ctrl, Y,N,N,N,Y,N, IMM_I.EN,        RS1.EN,IMM.EN, FN_SRA.EN,    X,   X,X) }
      is(JAL  ) { set(ctrl, Y,N,Y,N,Y,N,IMM_UJ.EN,         PC.EN,IMM.EN, FN_ADD.EN,    X,   X,X) }
      is(JALR ) { set(ctrl, Y,N,N,Y,Y,N, IMM_I.EN,        RS1.EN,IMM.EN, FN_ADD.EN,    X,   X,X) }
      is(BEQ  ) { set(ctrl, Y,Y,N,N,N,N,IMM_SB.EN,        RS1.EN,RS2.EN, FN_SEQ.EN,    X,   X,X) }
      is(BNE  ) { set(ctrl, Y,Y,N,N,N,N,IMM_SB.EN,        RS1.EN,RS2.EN, FN_SNE.EN,    X,   X,X) }
      is(BLT  ) { set(ctrl, Y,Y,N,N,N,N,IMM_SB.EN,        RS1.EN,RS2.EN, FN_SLT.EN,    X,   X,X) }
      is(BGE  ) { set(ctrl, Y,Y,N,N,N,N,IMM_SB.EN,        RS1.EN,RS2.EN, FN_SGE.EN,    X,   X,X) }
      is(BLTU ) { set(ctrl, Y,Y,N,N,N,N,IMM_SB.EN,        RS1.EN,RS2.EN,FN_SLTU.EN,    X,   X,X) }
      is(BGEU ) { set(ctrl, Y,Y,N,N,N,N,IMM_SB.EN,        RS1.EN,RS2.EN,FN_SGEU.EN,    X,   X,X) }
      is(LB   ) { set(ctrl, Y,N,N,N,Y,Y, IMM_I.EN,        RS1.EN,IMM.EN, FN_ADD.EN,Ld.EN,B.EN,Y) }
      is(LH   ) { set(ctrl, Y,N,N,N,Y,Y, IMM_I.EN,        RS1.EN,IMM.EN, FN_ADD.EN,Ld.EN,H.EN,Y) }
      is(LW   ) { set(ctrl, Y,N,N,N,Y,Y, IMM_I.EN,        RS1.EN,IMM.EN, FN_ADD.EN,Ld.EN,W.EN,Y) }
      is(LD   ) { set(ctrl, Y,N,N,N,Y,Y, IMM_I.EN,        RS1.EN,IMM.EN, FN_ADD.EN,Ld.EN,D.EN,Y) }
      is(LBU  ) { set(ctrl, Y,N,N,N,Y,Y, IMM_I.EN,        RS1.EN,IMM.EN, FN_ADD.EN,Ld.EN,B.EN,N) }
      is(LHU  ) { set(ctrl, Y,N,N,N,Y,Y, IMM_I.EN,        RS1.EN,IMM.EN, FN_ADD.EN,Ld.EN,H.EN,N) }
      is(LWU  ) { set(ctrl, Y,N,N,N,Y,Y, IMM_I.EN,        RS1.EN,IMM.EN, FN_ADD.EN,Ld.EN,W.EN,N) }
      is(SB   ) { set(ctrl, Y,N,N,N,N,Y, IMM_S.EN,        RS1.EN,IMM.EN, FN_ADD.EN,St.EN,B.EN,X) }
      is(SH   ) { set(ctrl, Y,N,N,N,N,Y, IMM_S.EN,        RS1.EN,IMM.EN, FN_ADD.EN,St.EN,H.EN,X) }
      is(SW   ) { set(ctrl, Y,N,N,N,N,Y, IMM_S.EN,        RS1.EN,IMM.EN, FN_ADD.EN,St.EN,W.EN,X) }
      is(SD   ) { set(ctrl, Y,N,N,N,N,Y, IMM_S.EN,        RS1.EN,IMM.EN, FN_ADD.EN,St.EN,D.EN,X) }
    }
    ctrl
