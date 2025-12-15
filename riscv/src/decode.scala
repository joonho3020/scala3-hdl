package riscv

import hdl._

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
      when(funct3 === 0.U(3.W) && funct7 === 0x20.U(7.W)) {
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
      deq_uop.opcode := enq_uop.inst(6,  0)
      deq_uop.funct3 := enq_uop.inst(14, 12)
      deq_uop.funct7 := enq_uop.inst(31, 25)
      deq_uop.funct7 := enq_uop.inst(31, 25)
      deq_uop.rs1    := enq_uop.inst(19, 15)
      deq_uop.rs2    := enq_uop.inst(24, 20)
      deq_uop.rd     := enq_uop.inst(11, 7)
      deq_uop.aluOp  := aluOp(deq_uop.funct3, deq_uop.funct7)
    })
  }

