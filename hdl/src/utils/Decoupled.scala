package hdl

case class Decoupled[T <: HWData](valid: Bool, ready: Bool, bits: T) extends Bundle[Decoupled[T]]:
  def fire(using m: Module): Bool =
    ready && valid

object Decoupled:
  def apply[T <: HWData](x: T): Decoupled[T] =
    Decoupled(
      valid = Output(Bool()),
      ready =  Input(Bool()),
      bits  = Output(x)
    )

case class QueueBundle[T <: HWData](
  enq: Decoupled[T],
  deq: Decoupled[T]
) extends Bundle[QueueBundle[T]]

object QueueBundle:
  def apply[T <: HWData](x: T): QueueBundle[T] =
    QueueBundle(
      enq = Flipped(Decoupled(x)),
      deq =         Decoupled(x))

class Queue[T <: HWData](x: T, entries: Int) extends Module:
  val io = IO(QueueBundle(x))

  body:
    val addrBits = log2Ceil(entries + 1)
    val mem = Reg(Vec.fill(entries)(x))

    val enq_ptr = RegInit(0.U(addrBits.W))
    val deq_ptr = RegInit(0.U(addrBits.W))
    val full    = RegInit(false.B)
    val empty   = (enq_ptr === deq_ptr) && !full

    io.enq.ready := !full
    io.deq.valid := !empty
    io.deq.bits  := mem(deq_ptr)

    val enq_fire = io.enq.valid && io.enq.ready
    val deq_fire = io.deq.valid && io.deq.ready
    val almost_full = (enq_ptr + 1.U) % entries.U === deq_ptr

    when (enq_fire) {
      enq_ptr := (enq_ptr + 1.U) % entries.U
      mem(enq_ptr) := io.enq.bits
    }

    when (deq_fire) {
      deq_ptr := (deq_ptr + 1.U) % entries.U
    }

    when (enq_fire && deq_fire) {
    } .elsewhen (enq_fire && almost_full) {
      full := true.B
    } .elsewhen (deq_fire) {
      full := false.B
    }
