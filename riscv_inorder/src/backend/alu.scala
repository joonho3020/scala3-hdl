package riscv_inorder

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
    candidate.map(_.EN.asUInt === cmd).reduce(_ || _)

  def isMulFN(fn: UInt, cmp: UInt)(using m: Module): Bool = fn(1,0) === cmp(1,0)
  def isSub(cmd: UInt)(using m: Module): Bool = cmd(3).asBool
  def isCmp(cmd: UInt)(using m: Module): Bool =
    (cmd >= Opcode.FN_SLT.EN.asUInt && cmd <= Opcode.FN_SGEU.EN.asUInt)
  def cmpUnsigned(cmd: UInt)(using m: Module): Bool = cmd(1).asBool
  def cmpInverted(cmd: UInt)(using m: Module): Bool = cmd(0).asBool
  def cmpEq(cmd: UInt)(using m: Module): Bool = !cmd(3).asBool
  def shiftReverse(cmd: UInt)(using m: Module): Bool =
    cmd === Opcode.FN_SR.EN.asUInt ||
    cmd === Opcode.FN_SRA.EN.asUInt

import ALUParams.Opcode._
import ALUParams._

case class ALUIO(
  fn: UInt,
  in2: UInt,
  in1: UInt,
  out: UInt,
  dw: HWEnum[CoreConstants.DW],
  adder_out: UInt,
  cmp_out: Bool
) extends Bundle[ALUIO]

object ALUIO:
  def apply(p: ALUParams): ALUIO =
    ALUIO(
      fn  = Input(UInt(ALUParams.fnBits.W)),
      in2 = Input(UInt(p.xlen.W)),
      in1 = Input(UInt(p.xlen.W)),
      dw  = Input(HWEnum(CoreConstants.DW)),

      out       = Output(UInt(p.xlen.W)),
      adder_out = Output(UInt(p.xlen.W)),
      cmp_out   = Output(Bool())
    )

class ALU(p: ALUParams) extends Module with CacheableModule:
  type ElabParams = ALUParams
  given stableHashElabParams: StableHash[ALUParams] = summon[StableHash[ALUParams]]
  def elabParams: ALUParams = p
  given Module = this

  val io = IO(ALUIO(p))

  val xLen = p.xlen

  body:
    val in2_inv = Mux(isSub(io.fn), ~io.in2, io.in2)
    val in1_xor_in2 = io.in1 ^ in2_inv
    val in1_and_in2 = io.in1 & in2_inv
    io.adder_out := io.in1 + in2_inv + isSub(io.fn).asUInt

    require(xLen == 64)
    // TODO
    // 64 bit?

    val logic = Wire(UInt(xLen.W))
    switch (io.fn) {
      is (FN_XOR.EN.asUInt) { logic := io.in1 ^ io.in2 }
      is ( FN_OR.EN.asUInt) { logic := io.in1 | io.in2 }
      is (FN_AND.EN.asUInt) { logic := io.in1 & io.in2 }
      default               { logic := 0.U             }
    }
    dontTouch(logic)

    val slt =
      Mux(io.in1(xLen-1) === io.in2(xLen-1), io.adder_out(xLen-1),
        Mux(cmpUnsigned(io.fn), io.in2(xLen-1), io.in1(xLen-1)))
    io.cmp_out := cmpInverted(io.fn) ^ Mux(cmpEq(io.fn), in1_xor_in2 === 0.U, slt.asBool)

    val shamt = io.in2(5, 0)
    val shright = io.in1 >> shamt
    val shright_arith = (io.in1.asSInt >> shamt).asUInt

    val shlout = io.in1 << shamt

    val shift_out = Wire(UInt(xLen.W))
    switch (io.fn) {
      is (FN_SL .EN.asUInt) { shift_out := shlout        }
      is (FN_SR .EN.asUInt) { shift_out := shright       }
      is (FN_SRA.EN.asUInt) { shift_out := shright_arith }
      default               { shift_out := 0.U           }
    }
    dontTouch(shift_out)

    val cmp_val = Wire(UInt(xLen.W))
    cmp_val := Mux(isCmp(io.fn) && slt.asBool, 1.U(xLen.W), 0.U(xLen.W))
    dontTouch(cmp_val)

    val shift_logic = cmp_val | logic | shift_out

    io.out := Mux(io.fn === FN_ADD.EN.asUInt || io.fn === FN_SUB.EN.asUInt,
                  io.adder_out,
                  shift_logic)
