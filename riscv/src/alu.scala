package riscv

import hdl._

case class ALUParams(xlen: Int)

object ALUParams:
  def fnBits: Int = 5

  enum Opcode:
    case
      FN_ADD ,
      FN_SL  ,
      FN_SEQ ,
      FN_SNE ,
      FN_XOR ,
      FN_SR  ,
      FN_OR  ,
      FN_AND ,
      FN_CZE ,
      FN_CZN ,
      FN_SUB ,
      FN_SRA ,
      FN_SLT ,
      FN_SGE ,
      FN_SLTU,
      FN_SGEU,
      FN_UNA ,
      FN_ROL ,
      FN_ROR ,
      FN_BEX
// FN_ANDN = 24.U
// FN_ORN  = 25.U
// FN_XNOR = 26.U
// 
// FN_MAX  = 28.U
// FN_MIN  = 29.U
// FN_MAXU = 30.U
// FN_MINU = 31.U
// // ef FN_MAXMIN = BitPat("b111??")
// 
// FN_DIV  = FN_XOR
// FN_DIVU = FN_SR
// FN_REM  = FN_OR
// FN_REMU = FN_AND
// 
// FN_MUL    = FN_ADD
// FN_MULH   = FN_SL
// FN_MULHSU = FN_SEQ
// FN_MULHU  = FN_SNE

  def isOneOf(cmd: UInt, candidate: Seq[Opcode])(using m: Module): Bool =
    candidate.map(_.toHWEnum.asUInt === cmd).reduce(_ || _)

  def isMulFN(fn: UInt, cmp: UInt)(using m: Module): Bool = fn(1,0) === cmp(1,0)
  def isSub(cmd: UInt)(using m: Module): Bool = cmd(3).asBool
  def isCmp(cmd: UInt)(using m: Module): Bool =
    (cmd >= Opcode.FN_SLT.toHWEnum.asUInt && cmd <= Opcode.FN_SGEU.toHWEnum.asUInt)
// def isMaxMin(cmd: UInt)(using m: Module): Bool =
// (cmd >= Opcode.FN_MAX.toHWEnum.asUInt && cmd <= Opcode.FN_MINU.toHWEnum.asUInt)
  def cmpUnsigned(cmd: UInt)(using m: Module): Bool = cmd(1).asBool
  def cmpInverted(cmd: UInt)(using m: Module): Bool = cmd(0).asBool
  def cmpEq(cmd: UInt)(using m: Module): Bool = !cmd(3).asBool
// def shiftReverse(cmd: UInt)(using m: Module) =
// !isOneOf(cmd, Seq(Opcode.FN_SR, Opcode.FN_SRA, Opcode.FN_ROR, Opcode.FN_BEXT))
// def bwInvRs2(cmd: UInt)(using m: Module) = isOneOf(cmd, Seq(Opcode.FN_ANDN, Opcode.FN_ORN, Opcode.FN_XNOR))

import ALUParams.Opcode._
import ALUParams._

case class ALUIO(
  fn: UInt, // FIXME: use proper HWEnum
  in2: UInt,
  in1: UInt,
  out: UInt,
  adder_out: UInt,
  cmp_out: Bool
) extends Bundle[ALUIO]

object ALUIO:
  def apply(p: ALUParams): ALUIO =
    ALUIO(
      fn  = Input(UInt(ALUParams.fnBits.W)),
      in2 = Input(UInt(p.xlen.W)),
      in1 = Input(UInt(p.xlen.W)),

      out       = Output(UInt(p.xlen.W)),
      adder_out = Output(UInt(p.xlen.W)),
      cmp_out   = Output(Bool())
    )

class ALU(p: ALUParams) extends Module:
  given Module = this
  val xLen = p.xlen

  val io = IO(ALUIO(p))

  // ADD, SUB
  val in2_inv = Mux(isSub(io.fn), ~io.in2, io.in2)
  val in1_xor_in2 = io.in1 ^ in2_inv
  val in1_and_in2 = io.in1 & in2_inv
  io.adder_out := io.in1 + in2_inv + isSub(io.fn).asUInt

  io.cmp_out := DontCare
  io.out     := DontCare

  // // SLT, SLTU
  // val slt =
  //   Mux(io.in1(xLen-1) === io.in2(xLen-1), io.adder_out(xLen-1),
  //   Mux(cmpUnsigned(io.fn), io.in2(xLen-1), io.in1(xLen-1)))
  // io.cmp_out := cmpInverted(io.fn) ^ Mux(cmpEq(io.fn), in1_xor_in2 === 0.U, slt.asBool)
  // 
  // // SLL, SRL, SRA
  // val (shamt, shin_r) =
  //   if (xLen == 32) (io.in2(4,0), io.in1)
  //   else {
  //     require(xLen == 64)
  //     val shin_hi_32 = Fill(32, isSub(io.fn) && io.in1(31))
  //     val shin_hi = Mux(io.dw === DW_64, io.in1(63,32), shin_hi_32)
  //     val shamt = Cat(io.in2(5) & (io.dw === DW_64), io.in2(4,0))
  //     (shamt, Cat(shin_hi, io.in1(31,0)))
  //   }
  // val shin = Mux(shiftReverse(io.fn), Reverse(shin_r), shin_r)
  // val shout_r = (Cat(isSub(io.fn) & shin(xLen-1), shin).asSInt >> shamt)(xLen-1,0)
  // val shout_l = Reverse(shout_r)
  // val shout = Mux(io.fn === FN_SR || io.fn === FN_SRA || io.fn === FN_BEXT, shout_r, 0.U) |
  //             Mux(io.fn === FN_SL,                                          shout_l, 0.U)
  // 
  // // CZEQZ, CZNEZ
  // val in2_not_zero = io.in2.orR
  // val cond_out = Option.when(usingConditionalZero)(
  //   Mux((io.fn === FN_CZEQZ && in2_not_zero) || (io.fn === FN_CZNEZ && !in2_not_zero), io.in1, 0.U)
  // )
  // 
  // // AND, OR, XOR
  // val logic = Mux(io.fn === FN_XOR || io.fn === FN_OR || io.fn === FN_ORN || io.fn === FN_XNOR, in1_xor_in2, 0.U) |
  //             Mux(io.fn === FN_OR || io.fn === FN_AND || io.fn === FN_ORN || io.fn === FN_ANDN, in1_and_in2, 0.U)
  // 
  // val bext_mask = Mux(coreParams.useZbs.B && io.fn === FN_BEXT, 1.U, ~(0.U(xLen.W)))
  // val shift_logic = (isCmp (io.fn) && slt) | logic | (shout & bext_mask)
  // val shift_logic_cond = cond_out match {
  //   case Some(co) => shift_logic | co
  //   case _ => shift_logic
  // }
  // 
  // // CLZ, CTZ, CPOP
  // val tz_in = MuxLookup((io.dw === DW_32) ## !io.in2(0), 0.U)(Seq(
  //   0.U -> io.in1,
  //   1.U -> Reverse(io.in1),
  //   2.U -> 1.U ## io.in1(31,0),
  //   3.U -> 1.U ## Reverse(io.in1(31,0))
  // ))
  // val popc_in = Mux(io.in2(1),
  //   Mux(io.dw === DW_32, io.in1(31,0), io.in1),
  //   PriorityEncoderOH(1.U ## tz_in) - 1.U)(xLen-1,0)
  // val count = PopCount(popc_in)
  // val in1_bytes = io.in1.asTypeOf(Vec(xLen / 8, UInt(8.W)))
  // val orcb = VecInit(in1_bytes.map(b => Fill(8, b =/= 0.U))).asUInt
  // val rev8 = VecInit(in1_bytes.reverse).asUInt
  // val unary = MuxLookup(io.in2(11,0), count)(Seq(
  //   0x287.U -> orcb,
  //   (if (xLen == 32) 0x698 else 0x6b8).U -> rev8,
  //   0x080.U -> io.in1(15,0),
  //   0x604.U -> Fill(xLen-8, io.in1(7)) ## io.in1(7,0),
  //   0x605.U -> Fill(xLen-16, io.in1(15)) ## io.in1(15,0)
  // ))
  // 
  // // MAX, MIN, MAXU, MINU
  // val maxmin_out = Mux(io.cmp_out, io.in2, io.in1)
  // 
  // // ROL, ROR
  // val rot_shamt = Mux(io.dw === DW_32, 32.U, xLen.U) - shamt
  // val rotin = Mux(io.fn(0), shin_r, Reverse(shin_r))
  // val rotout_r = (rotin >> rot_shamt)(xLen-1,0)
  // val rotout_l = Reverse(rotout_r)
  // val rotout = Mux(io.fn(0), rotout_r, rotout_l) | Mux(io.fn(0), shout_l, shout_r)
  // 
  // val out = MuxLookup(io.fn, shift_logic_cond)(Seq(
  //   FN_ADD -> io.adder_out,
  //   FN_SUB -> io.adder_out
  // ) ++ (if (coreParams.useZbb) Seq(
  //   FN_UNARY -> unary,
  //   FN_MAX -> maxmin_out,
  //   FN_MIN -> maxmin_out,
  //   FN_MAXU -> maxmin_out,
  //   FN_MINU -> maxmin_out,
  //   FN_ROL -> rotout,
  //   FN_ROR -> rotout,
  // ) else Nil))
  // 
  // 
  // io.out := out
  // if (xLen > 32) {
  //   require(xLen == 64)
  //   when (io.dw === DW_32) { io.out := Cat(Fill(32, out(31)), out(31,0)) }
  // }
