package riscv

import hdl._

case class FetchBufferIO(
  enq: Decoupled[FetchBundle],
  deq: Vec[Decoupled[UOp]],
  clear: Bool,
) extends Bundle[FetchBufferIO]

object FetchBufferIO:
  def apply(p: CoreParams): FetchBufferIO =
    FetchBufferIO(
      enq = Flipped(Decoupled(FetchBundle(p))),
      deq = Vec(Seq.fill(p.coreWidth)(Decoupled(UOp(p)))),
      clear = Input(Bool())
    )

class FetchBuffer(p: CoreParams, depth: Int) extends Module:
  val io = IO(FetchBufferIO(p))
  body {
    def row(ptr: UInt): UInt =
      ptr >> log2Ceil(p.coreWidth)

    def col(ptr: UInt): UInt =
      ptr & ((1 << log2Ceil(p.coreWidth)) - 1).U

    val mem = Reg(
      Vec.fill(depth)(
        Vec.fill(p.coreWidth)(Valid(UOp(p)))
      ))

    val entries  = depth * p.coreWidth
    val addrBits = log2Ceil(entries + 1)

    val enq_ptr = RegInit(0.U(addrBits.W))
    val deq_ptr = RegInit(0.U(addrBits.W))

    // 0                N - 1
    //     ^            ^
    //     deq          enq         almost_full := (enqptr + W) % N >= deq
    //               ^   ^
    //               enq deq        almost_full := (deq - enq) <= W
    val almost_full = Mux(enq_ptr >= deq_ptr,
                          (enq_ptr + p.coreWidth.U) % entries.U >= deq_ptr,
                          (deq_ptr - enq_ptr) <= p.coreWidth.U)

    io.enq.ready := !almost_full

    when (io.enq.fire) {
      val next_offset = Vec.fill(p.coreWidth)(Wire(UInt(addrBits.W)))
      for (i <- 0 until p.coreWidth) {
        if i == 0 then
          next_offset(i) := 0.U
        else
          next_offset(i) := next_offset(i-1) +
                            Mux(io.enq.bits.insts(i-1).valid, 1.U, 0.U)

        val cur_ptr = enq_ptr + next_offset(i)
        val r = row(cur_ptr)
        val c = col(cur_ptr)

        mem(r)(c).valid     := io.enq.bits.insts(i).valid
        mem(r)(c).bits      := DontCare
        mem(r)(c).bits.inst := io.enq.bits.insts(i).bits
        mem(r)(c).bits.pc   := io.enq.bits.pc + (p.instBytes * i).U
      }
      enq_ptr := enq_ptr +
                 next_offset(p.coreWidth - 1) +
                 Mux(io.enq.bits.insts(p.coreWidth-1).valid, 1.U, 0.U)
    }

    val deq_fire_cnt = io.deq.map(x => x.fire.asUInt).reduce(_ + _)
    deq_ptr := deq_ptr + deq_fire_cnt

    for (i <- 0 until p.coreWidth) {
      val cur_ptr = deq_ptr + i.U
      val r = row(cur_ptr)
      val c = col(cur_ptr)
      io.deq(i) := mem(r)(c)
    }

    when (io.clear) {
      enq_ptr := 0.U
      deq_ptr := 0.U
    }
  }
