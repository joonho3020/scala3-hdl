package hdl

/** Ready/valid interface bundle. */
case class Decoupled[T <: HWData](valid: Bool, ready: Bool, bits: T) extends Bundle[Decoupled[T]]:
  /** True when both ready and valid are asserted. */
  def fire(using m: Module): Bool =
    ready && valid

object Decoupled:
  /** Construct a Decoupled bundle with standard directions. */
  def apply[T <: HWData](x: T): Decoupled[T] =
    Decoupled(
      valid = Output(Bool()),
      ready =  Input(Bool()),
      bits  = Output(x)
    )

object DecoupledHelper:
  /** Build a helper for combining ready/valid signals. */
  def apply(rvs: Bool*) = new DecoupledHelper(rvs)

/** Helper for combining multiple ready/valid signals into a single fire. */
class DecoupledHelper(val rvs: Seq[Bool]):
  /** Combine all signals except one excluded, plus any additional includes. */
  def fire(exclude: Bool, includes: Bool*)(using m: Module) =
    require(rvs.contains(exclude), "Excluded Bool not present in DecoupledHelper! Note that DecoupledHelper uses referential equality for exclusion! If you don't want to exclude anything, use fire()!")
    (rvs.filter(_ ne exclude) ++ includes).reduce(_ && _)

  /** Combine all signals with logical AND. */
  def fire()(using m: Module) = rvs.reduce(_ && _)

/** Enqueue/dequeue bundle pair for queues. */
case class QueueBundle[T <: HWData](
  enq: Decoupled[T],
  deq: Decoupled[T]
) extends Bundle[QueueBundle[T]]

object QueueBundle:
  /** Construct a QueueBundle with flipped enqueue and normal dequeue. */
  def apply[T <: HWData](x: T): QueueBundle[T] =
    QueueBundle(
      enq = Flipped(Decoupled(x)),
      deq =         Decoupled(x))

/** Simple ready/valid queue module. */
class Queue[T <: HWData](x: T, entries: Int) extends Module:
  val io = IO(QueueBundle(x))

  body:
    val addrBits = log2Ceil(entries + 1)
    val mem = Reg(Vec.fill(entries)(x))

    val enq_ptr = RegInit(0.U(Width(addrBits)))
    val deq_ptr = RegInit(0.U(Width(addrBits)))
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
