package riscv

import hdl._

case class RTypeDecode(rd: UInt, rs1: UInt, rs2: UInt, aluOp: UInt, pc: UInt, inst: UInt) extends Bundle[RTypeDecode]

object RTypeDecode:
  def apply(p: CoreParams): RTypeDecode =
    RTypeDecode(
      rd = UInt(5.W),
      rs1 = UInt(5.W),
      rs2 = UInt(5.W),
      aluOp = UInt(AluOpCode.SZ_ALU_FN.W),
      pc = UInt(p.pcBits.W),
      inst = UInt(p.xlenBits.W)
    )

case class RTypeDecoderIO(in: Decoupled[UOp], out: Decoupled[RTypeDecode]) extends Bundle[RTypeDecoderIO]

object RTypeDecoderIO:
  def apply(p: CoreParams): RTypeDecoderIO =
    RTypeDecoderIO(
      in = Flipped(Decoupled(UOp(p))),
      out = Decoupled(RTypeDecode(p))
    )

// class RTypeDecoder(p: CoreParams) extends Module:
//   val io = IO(RTypeDecoderIO(p))
//   body {
//     val opcode = io.in.bits.inst(6, 0)
//     val funct3 = io.in.bits.inst(14, 12)
//     val funct7 = io.in.bits.inst(31, 25)
//     val isRType = opcode === 0x33.U(7.W)
//     val aluOp = Wire(UInt(AluOpCode.width.W))
//     aluOp := AluOpCode.ADD
//     when(funct3 === 0.U(3.W) && funct7 === 0x20.U(7.W)) {
//       aluOp := AluOpCode.SUB
//     }.elsewhen(funct3 === 1.U(3.W) && funct7 === 0.U(7.W)) {
//       aluOp := AluOpCode.SLL
//     }.elsewhen(funct3 === 2.U(3.W) && funct7 === 0.U(7.W)) {
//       aluOp := AluOpCode.SLT
//     }.elsewhen(funct3 === 3.U(3.W) && funct7 === 0.U(7.W)) {
//       aluOp := AluOpCode.SLTU
//     }.elsewhen(funct3 === 4.U(3.W) && funct7 === 0.U(7.W)) {
//       aluOp := AluOpCode.XOR
//     }.elsewhen(funct3 === 5.U(3.W) && funct7 === 0.U(7.W)) {
//       aluOp := AluOpCode.SRL
//     }.elsewhen(funct3 === 5.U(3.W) && funct7 === 0x20.U(7.W)) {
//       aluOp := AluOpCode.SRA
//     }.elsewhen(funct3 === 6.U(3.W) && funct7 === 0.U(7.W)) {
//       aluOp := AluOpCode.OR
//     }.elsewhen(funct3 === 7.U(3.W) && funct7 === 0.U(7.W)) {
//       aluOp := AluOpCode.AND
//     }
//     io.out.bits.rd := io.in.bits.inst(11, 7)
//     io.out.bits.rs1 := io.in.bits.inst(19, 15)
//     io.out.bits.rs2 := io.in.bits.inst(24, 20)
//     io.out.bits.aluOp := aluOp
//     io.out.bits.pc := io.in.bits.pc
//     io.out.bits.inst := io.in.bits.inst
//     io.out.valid := io.in.valid && isRType
//     io.in.ready := io.out.ready || !isRType
//   }

case class FetchBufferIO(in: FetchIf, out: Decoupled[UOp]) extends Bundle[FetchBufferIO]

object FetchBufferIO:
  def apply(p: CoreParams): FetchBufferIO =
    FetchBufferIO(
      in = Input(FetchIf(p)),
      out = Decoupled(UOp(p))
    )

class FetchBuffer(p: CoreParams, entries: Int) extends Module:
  val io = IO(FetchBufferIO(p))
  body {
    val queue = Module(new Queue(UOp(p), entries))
    val enqValid = WireInit(false.B)
    val enqBits = Wire(UOp(p))
    enqBits.pc := 0.U(p.pcBits.W)
    enqBits.inst := 0.U(p.xlenBits.W)
    io.in.insts.foreach { slot =>
      when(slot.valid && !enqValid) {
        enqValid := true.B
        enqBits := slot.bits
      }
    }
    queue.io.enq.valid := enqValid
    queue.io.enq.bits := enqBits
    io.out.valid := queue.io.deq.valid
    io.out.bits := queue.io.deq.bits
    queue.io.deq.ready := io.out.ready
  }
