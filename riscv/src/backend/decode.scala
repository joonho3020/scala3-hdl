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

class Decoder(p: CoreParams) extends Module with CoreCacheable(p):
  val io = IO(DecoderIO(p))
  body {
    import ALUParams.Opcode._

    io.enq.zip(io.deq).foreach((enq, deq) => {
      enq.ready := deq.ready
      deq.valid := enq.valid

      val enq_uop = enq.bits
      val deq_uop = deq.bits
      val inst = enq_uop.inst

      deq_uop        := enq_uop
      deq_uop.lrs1    := inst(19, 15)
      deq_uop.lrs2    := inst(24, 20)
      deq_uop.lrd     := inst(11,  7)
      deq_uop.taken  := false.B
      CtrlSignals.decode(deq_uop.ctrl, enq_uop.inst)
    })
  }

