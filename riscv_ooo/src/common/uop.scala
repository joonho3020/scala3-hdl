package riscv_ooo

import hdl.core._
import hdl.util._
import hdl.elaboration._

import riscv_inorder.ALUParams.Opcode
import riscv_inorder.Instructions
import riscv_inorder.CoreConstants._

case class UOp(
  pc: UInt,
  inst: UInt,
  lrs1: UInt,
  lrs2: UInt,
  lrd:  UInt,

  prs1: UInt,
  prs2: UInt,
  prd:  UInt,
  stale_prd: UInt,

  prs1_busy: Bool,
  prs2_busy: Bool,

  rob_idx: UInt,
  ftq_idx: UInt,

  br_tag:  OneHot,
  br_mask: UInt,

  taken: Bool,
  next_pc: Valid[UInt],
  ctrl: CtrlSignals,
) extends Bundle[UOp]:
  import riscv_inorder.ALUParams

  def lrs1_val(using m: Module): Bool =
    ctrl.valid &&
      (ctrl.sel_alu1 === ALUOp1.RS1.EN) ||
      (ctrl.sel_alu1 === ALUOp1.RS1SHL.EN)

  def lrs2_val(using m: Module): Bool =
    ctrl.valid &&
      (ctrl.sel_alu2 === ALUOp2.RS2.EN) ||
      (ctrl.sel_alu2 === ALUOp2.RS2OH.EN) ||
      (ctrl.is_mem && (ctrl.mem_op === MemOp.St.EN))

  def lrd_val(using m: Module): Bool =
    ctrl.valid && ctrl.rd_wen

  def lrd_dep(prev: UOp)(using m: Module): Bool =
    lrd === prev.lrd && lrd_val && prev.lrd_val

  def lrs1_dep(prev: UOp)(using m: Module): Bool =
    lrs1 === prev.lrd && lrs1_val && prev.lrd_val

  def lrs2_dep(prev: UOp)(using m: Module): Bool =
    lrs2 === prev.lrd && lrs2_val && prev.lrd_val


object UOp:
  def apply(p: CoreParams): UOp =
    UOp(
      pc = UInt(p.pcBits.W),
      inst = UInt(p.instBits.W),

      lrs1    = UInt(5.W),
      lrs2    = UInt(5.W),
      lrd     = UInt(5.W),

      prs1    = UInt(p.pRegIdxBits.W),
      prs2    = UInt(p.pRegIdxBits.W),
      prd     = UInt(p.pRegIdxBits.W),
      stale_prd = UInt(p.pRegIdxBits.W),

      prs1_busy = Bool(),
      prs2_busy = Bool(),

      rob_idx = UInt(p.robIdxBits.W),
      ftq_idx = UInt(p.ftqIdxBits.W),

      br_tag  = OneHot(p.branchTagBits.W),
      br_mask = UInt(p.branchTagBits.W),

      taken  = Bool(),
      next_pc = Valid(UInt(p.pcBits.W)),
      ctrl   = CtrlSignals()
    )

trait DecoderLogic:
  def Y = true.B
  def N = false.B
  def X = DontCare

case class CtrlSignals(
  valid:    Bool,
  br:       Bool,
  jal:      Bool,
  jalr:     Bool,
  rd_wen:   Bool,
  is_mem:   Bool,
  sel_imm:  HWEnum[Immediates],
  sel_alu1: HWEnum[ALUOp1],
  sel_alu2: HWEnum[ALUOp2],
  alu_op:   HWEnum[riscv_inorder.ALUParams.Opcode],
  mem_op:   HWEnum[MemOp],
  mem_width: HWEnum[MemWidth],
  mem_signed: Bool,
) extends Bundle[CtrlSignals]:

  def is_cfi(using m: Module): Bool =
    valid && (br || jal || jalr)

  def is_load(using m: Module): Bool =
    valid && is_mem && (mem_op === MemOp.Ld.EN)

  def is_store(using m: Module): Bool =
    valid && is_mem && (mem_op === MemOp.St.EN)

object CtrlSignals extends DecoderLogic:
  import Instructions._
  import riscv_inorder.ALUParams._
  import riscv_inorder.ALUParams.Opcode._
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
    alu_op:   HWEnum[riscv_inorder.ALUParams.Opcode] | DontCare.type,
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

case class BrJmpSignal(
  is_br: Bool,
  is_jal: Bool,
  is_jalr: Bool
) extends Bundle[BrJmpSignal]

object BrJmpSignal extends DecoderLogic:
  def apply(): BrJmpSignal =
    BrJmpSignal(
      is_br = Bool(),
      is_jal = Bool(),
      is_jalr = Bool()
    )

  def set(
    signal: BrJmpSignal,
    br:       Bool | DontCare.type,
    jal:      Bool | DontCare.type,
    jalr:     Bool | DontCare.type,
  )(using m: Module): BrJmpSignal =
    signal.is_br ::= br
    signal.is_jal ::= jal
    signal.is_jalr ::= jalr
    signal

  def predecode(signal: BrJmpSignal, inst: UInt)(using m: Module): BrJmpSignal =
    import Instructions._
    signal.is_br := false.B
    signal.is_jal := false.B
    signal.is_jalr := false.B

    switch(inst) {
      /*                    is_jalr
       *                   is_jal |
       *                  is_br | |
       *                      | | |                                               */
      is(JAL  ) { set(signal, N,Y,N) }
      is(JALR ) { set(signal, N,N,Y) }
      is(BEQ  ) { set(signal, Y,N,N) }
      is(BNE  ) { set(signal, Y,N,N) }
      is(BLT  ) { set(signal, Y,N,N) }
      is(BGE  ) { set(signal, Y,N,N) }
      is(BLTU ) { set(signal, Y,N,N) }
      is(BGEU ) { set(signal, Y,N,N) }
    }
    signal
