package riscv_ooo

import hdl._

case class CompactorIO(
  enq: Vec[Valid[UOp]],
  deq: Vec[Valid[UOp]]
) extends Bundle[CompactorIO]

class Compactor(p: CoreParams, width: Int) extends Module with CacheableModule:
  this: Module =>
  type ElabParams = (CoreParams, Int)
  given stableHashElabParams: StableHash[(CoreParams, Int)] = summon[StableHash[(CoreParams, Int)]]
  def elabParams: (CoreParams, Int) = (p, width)
  given Module = this

  val io = IO(CompactorIO(
    enq = Input(Vec.fill(width)(Valid(UOp(p)))),
    deq = Output(Vec.fill(width)(Valid(UOp(p))))
  ))

  body {
    val valids = io.enq.map(_.valid)

    val prefix_sum = Wire(Vec.fill(width)(UInt(log2Ceil(width + 1).W)))
    prefix_sum(0) := 0.U
    for (i <- 1 until width) {
      prefix_sum(i) := prefix_sum(i - 1) + valids(i - 1).asUInt
    }

    val total_valid = prefix_sum(width - 1) + valids(width - 1).asUInt

    for (out_idx <- 0 until width) {
      val sel_oh = (0 until width).map { in_idx =>
        valids(in_idx) && prefix_sum(in_idx) === out_idx.U
      }

      io.deq(out_idx).valid := out_idx.U < total_valid
      io.deq(out_idx).bits  := MuxOneHot(Cat(sel_oh.reverse).asOH, io.enq.map(_.bits).toSeq)
    }

    dontTouch(io)
  }
