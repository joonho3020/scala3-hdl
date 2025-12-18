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

    def aluOp(opcode: UInt, funct3: UInt, funct7: UInt): HWEnum[ALUParams.Opcode] =
      val op = Wire(new HWEnum(ALUParams.Opcode))

      switch (opcode) {
        is (0b0110011.U) { // R-type
          switch ((funct3, funct7)) {
            is ((0.U,    0.U)) { op :=  FN_ADD.EN }
            is ((0.U, 0x20.U)) { op :=  FN_SUB.EN }
            is ((1.U,    0.U)) { op :=   FN_SL.EN }
            is ((2.U,    0.U)) { op :=  FN_SLT.EN }
            is ((3.U,    0.U)) { op := FN_SLTU.EN }
            is ((4.U,    0.U)) { op :=  FN_XOR.EN }
            is ((5.U,    0.U)) { op :=   FN_SR.EN }
            is ((5.U, 0x20.U)) { op :=  FN_SRA.EN }
            is ((6.U,    0.U)) { op :=   FN_OR.EN }
            is ((7.U,    0.U)) { op :=  FN_AND.EN }
            default { op := DontCare }
          }
        }
        is (0b0010011.U) { // immediate operations
        }
        default { op := DontCare }
      }
      op

    io.enq.zip(io.deq).foreach((enq, deq) => {
      enq.ready := deq.ready
      deq.valid := enq.valid

      val enq_uop = enq.bits
      val deq_uop = deq.bits
      val inst = enq_uop.inst

      deq_uop        := enq_uop
      deq_uop.opcode := inst(6,  0)
      deq_uop.funct3 := inst(14, 12)
      deq_uop.funct7 := inst(31, 25)
      deq_uop.rs1    := inst(19, 15)
      deq_uop.rs2    := inst(24, 20)
      deq_uop.rd     := inst(11, 7)
      deq_uop.rd_wen := Opcodes.writesIntRd(deq_uop.opcode, deq_uop.funct3)

      val imm_i_upper = Mux(inst(31).asBool, ~0.U((p.xlenBits - 12).W), 0.U((p.xlenBits - 12).W))
      deq_uop.imm_i  := Cat(Seq(imm_i_upper, inst(31, 20)))
      deq_uop.imm_u  := Cat(Seq(inst(31, 12), 0.U(12.W)))
      deq_uop.aluOp  := aluOp(deq_uop.opcode, deq_uop.funct3, deq_uop.funct7)
    })
  }

