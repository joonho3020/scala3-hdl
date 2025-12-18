package riscv

import hdl._


object Opcodes:
  val OPCODE_LOAD      = 0x03.U(7.W)
  val OPCODE_OP_IMM    = 0x13.U(7.W)
  val OPCODE_AUIPC     = 0x17.U(7.W)
  val OPCODE_OP_IMM_32 = 0x1b.U(7.W)
  val OPCODE_STORE     = 0x23.U(7.W)
  val OPCODE_OP        = 0x33.U(7.W)
  val OPCODE_LUI       = 0x37.U(7.W)
  val OPCODE_OP_32     = 0x3b.U(7.W)
  val OPCODE_JALR      = 0x67.U(7.W)
  val OPCODE_JAL       = 0x6f.U(7.W)
  val OPCODE_MISC_MEM  = 0x0f.U(7.W)
  val OPCODE_SYSTEM    = 0x73.U(7.W)

  def writesIntRd(opcode: UInt, funct3: UInt)(using m: Module): Bool =
    (opcode === OPCODE_OP_32)     ||
    (opcode === OPCODE_OP_IMM)    ||
    (opcode === OPCODE_OP_IMM_32) ||
    (opcode === OPCODE_LOAD)      ||
    (opcode === OPCODE_LUI)       ||
    (opcode === OPCODE_AUIPC)     ||
    (opcode === OPCODE_JAL)       ||
    (opcode === OPCODE_JALR)      ||
    // Zicsr: CSRR* / CSRRS* / CSRRC* (funct3 != 0). ECALL/EBREAK/MRET/etc have funct3==0.
    ((opcode === OPCODE_SYSTEM) && (funct3 =/= 0.U(3.W)))


case class DecoderIO(
  enq: Vec[Decoupled[UOp]],
  deq: Vec[Decoupled[UOp]],
) extends Bundle[DecoderIO]

object DecoderIO:
  def apply(p: CoreParams): DecoderIO =
    DecoderIO(
      enq = Flipped(Vec.fill(p.coreWidth)(Decoupled(UOp(p)))),
      deq =         Vec.fill(p.coreWidth)(Decoupled(UOp(p)))
    )

class Decoder(p: CoreParams) extends Module:
  val io = IO(DecoderIO(p))
  body {
    import ALUParams.Opcode._

    def aluOp(funct3: UInt, funct7: UInt): HWEnum[ALUParams.Opcode] =
      val op = Wire(new HWEnum(ALUParams.Opcode))
      op := DontCare
      when (funct3 === 0.U(3.W) && funct7 === 0.U(7.W)) {
        op := FN_ADD.toHWEnum
      } .elsewhen(funct3 === 0.U(3.W) && funct7 === 0x20.U(7.W)) {
        op := FN_SUB.toHWEnum
      }.elsewhen(funct3 === 1.U(3.W) && funct7 === 0.U(7.W)) {
        op := FN_SL.toHWEnum
      }.elsewhen(funct3 === 2.U(3.W) && funct7 === 0.U(7.W)) {
        op := FN_SLT.toHWEnum
      }.elsewhen(funct3 === 3.U(3.W) && funct7 === 0.U(7.W)) {
        op := FN_SLTU.toHWEnum
      }.elsewhen(funct3 === 4.U(3.W) && funct7 === 0.U(7.W)) {
        op := FN_XOR.toHWEnum
      }.elsewhen(funct3 === 5.U(3.W) && funct7 === 0.U(7.W)) {
        op := FN_SR.toHWEnum
      }.elsewhen(funct3 === 5.U(3.W) && funct7 === 0x20.U(7.W)) {
        op := FN_SRA.toHWEnum
      }.elsewhen(funct3 === 6.U(3.W) && funct7 === 0.U(7.W)) {
        op := FN_OR.toHWEnum
      }.elsewhen(funct3 === 7.U(3.W) && funct7 === 0.U(7.W)) {
        op := FN_AND.toHWEnum
      }
      op

    io.enq.zip(io.deq).foreach((enq, deq) => {
      enq.ready := deq.ready
      deq.valid := enq.valid

      val enq_uop = enq.bits
      val deq_uop = deq.bits
      deq_uop        := enq_uop
      deq_uop.opcode := enq_uop.inst(6,  0)
      deq_uop.funct3 := enq_uop.inst(14, 12)
      deq_uop.funct7 := enq_uop.inst(31, 25)
      deq_uop.funct7 := enq_uop.inst(31, 25)
      deq_uop.rs1    := enq_uop.inst(19, 15)
      deq_uop.rs2    := enq_uop.inst(24, 20)
      deq_uop.rd     := enq_uop.inst(11, 7)
      deq_uop.rd_valid := Opcodes.writesIntRd(deq_uop.opcode, deq_uop.funct3)

      deq_uop.aluOp  := aluOp(deq_uop.funct3, deq_uop.funct7)
    })
  }

