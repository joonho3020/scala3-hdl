package hdl.util

import hdl.core._

/**
 * Bit replication utility for creating repeated patterns.
 *
 * Replicates a Bool or UInt value n times through concatenation, useful for
 * creating masks, sign extension, and constant patterns in hardware.
 *
 * @example
 * {{{
 * val allOnes = Fill(8, true.B)           // 0b11111111
 * val pattern = Fill(4, 0b11.U(2.W))      // 0b11111111
 * }}}
 */
object Fill:
  /** Replicate a Bool n times to build a UInt.
    * Example: Fill(4, true.B) => 0b1111
    * Example: Fill(3, false.B) => 0b000
    */
  def apply(n: Int, x: Bool)(using m: Module): UInt =
    if n <= 0 then
      throw new IllegalArgumentException("Fill requires n > 0")
    Seq.fill(n)(x.asUInt).Cat

  /** Replicate a UInt n times by concatenation.
    * Example: Fill(3, 2.U(2.W)) => 0b10_10_10
    */
  def apply(n: Int, x: UInt)(using m: Module): UInt =
    if n <= 0 then
      throw new IllegalArgumentException("Fill requires n > 0")
    Seq.fill(n)(x).Cat

/**
 * Bit order reversal utility.
 *
 * Reverses the bit ordering of a Bits value (LSB becomes MSB and vice versa).
 * Commonly used for endianness conversion and bit-reversed addressing schemes.
 *
 * @example
 * {{{
 * val reversed = Reverse(0b1100.U(4.W))  // Returns 0b0011
 * }}}
 */
object Reverse:
  /** Reverse the bit order of a value.
    * Example: Reverse(0b1100) => 0b0011
    */
  def apply(x: Bits)(using m: Module): UInt =
    val width = x.requireKnownWidth("Reverse")
    if width < 2 then
      throw new IllegalArgumentException("Reverse requires width >= 2")
    val u = x match
      case u: UInt => u
      case other => ModuleOps.prim1Op(UInt(other.getWidth), IR.PrimOp.AsUInt, other, m)
    val bits = (0 until width).map(i => u(i))
    var acc = bits.last
    var i = width - 2
    while i >= 0 do
      acc = ModuleOps.prim2Op(UInt(acc.getWidth + bits(i).getWidth), IR.PrimOp.Cat, acc, bits(i), m)
      i -= 1
    acc

/**
 * Population count (popcount/Hamming weight) utility.
 *
 * Counts the number of set bits (1s) in a Bits value or Vec of Bools.
 * Widely used for counting active signals, computing Hamming distance,
 * and detecting sparse or dense bit patterns.
 *
 * @example
 * {{{
 * val count = PopCount(0b10110.U)              // Returns 3.U
 * val active = PopCount(Vec(true.B, false.B))  // Returns 1.U
 * }}}
 */
object PopCount:
  /** Count the number of set bits in a Bool Vec.
    * Example: PopCount(Vec(false.B, true.B, true.B)) => 2
    */
  def apply(x: Vec[Bool])(using m: Module): UInt =
    x.map(_.asUInt).reduce(_ +& _)

  /** Count the number of set bits in a Bits value.
    * Example: PopCount(0b10110) => 3
    */
  def apply(x: Bits)(using m: Module): UInt =
    val width = x.requireKnownWidth("PopCount")
    val u = x match
      case u: UInt => u
      case other => ModuleOps.prim1Op(UInt(other.getWidth), IR.PrimOp.AsUInt, other, m)
    val bits = (0 until width).map(i => u(i))
    bits.reduce(_ +& _)

/**
 * Priority encoder for finding the highest (most significant) set bit.
 *
 * Returns the index of the highest set bit in a Bits value, with LSB = 0.
 * If no bits are set, returns 0. Commonly used in arbitration, finding
 * the highest priority request, and bit scanning operations.
 *
 * For one-hot encoded output, see [[PriorityEncoderOH]].
 *
 * @example
 * {{{
 * val idx = PriorityEncoder(0b010100.U)  // Returns 4.U (bit 4 is highest)
 * val none = PriorityEncoder(0b000000.U) // Returns 0.U (no bits set)
 * }}}
 *
 * @see [[PriorityEncoderOH]] for one-hot output
 */
object PriorityEncoder:
  /** Return the index of the highest set bit (LSB = 0).
    * Example: PriorityEncoder(0b010100) => 4
    */
  def apply(x: Bits)(using m: Module): UInt =
    val width = x.requireKnownWidth("PriorityEncoder")
    val u = x match
      case u: UInt => u
      case other => ModuleOps.prim1Op(UInt(other.getWidth), IR.PrimOp.AsUInt, other, m)
    val bits = (0 until width).map(i => u(i).asBool)
    bits.zipWithIndex.reverse.foldLeft(0.U)((acc, x) => {
      Mux(x._1, x._2.U, acc)
    })

object PriorityEncoderOH:
  /** Return a one-hot for the highest set bit (LSB = 0).
    * Example: PriorityEncoderOH(0b010100) => 0b010000
    */
  def apply(x: Bits)(using m: Module): OneHot =
    val width = x.requireKnownWidth("PriorityEncoderOH")
    val u = x match
      case u: UInt => u
      case other => ModuleOps.prim1Op(UInt(other.getWidth), IR.PrimOp.AsUInt, other, m)
    val bits = (0 until width).map(i => u(i).asBool)
    bits.zipWithIndex.foldLeft(0.U(width.W))((acc, b) => {
      val oneHot = 1.U(width.W) << b._2
      Mux(acc.orR, acc, Mux(b._1, oneHot, acc))
    }).asOH

object MuxOneHot:
  /** Select from inputs using a one-hot selector.
    * Example: sel=0b0100, in=Seq(a,b,c,d) => c
    */
  def apply[T <: HWData](sel: OneHot, in: Seq[T])(using m: Module): T =
    val sel_uint = sel.asUInt
    val width = sel.requireKnownWidth("MuxOneHot")
    (0 until width)
      .map(i => sel_uint(i).asBool)
      .zipWithIndex
      .foldLeft(in(0))((acc, y) => {
        Mux(y._1, in(y._2), acc)
      })

object UIntToOH:
  /** Convert a UInt index to one-hot.
    * Example: UIntToOH(3.U) => 0b1000
    */
  def apply(in: UInt)(using m: Module): OneHot = (1.U(1.W) << in).asOH

object MaskLower:
  /** Create a mask with all bits at or below the one-hot position set.
    * Example: MaskLower(0b001000) => 0b001111
    */
  def apply(in: OneHot)(using m: Module): UInt =
    val width = in.requireKnownWidth("MaskLower")
    (0 until width).map(i => in.asUInt >> i.U).reduce(_|_)

object MaskUpper:
  /** Create a mask with all bits at or above the one-hot position set.
    * Example: MaskUpper(0b001000) => 0b111000
    */
  def apply(in: OneHot)(using m: Module): UInt =
    val width = in.requireKnownWidth("MaskUpper")
    (0 until width).map(i => (in.asUInt << i.U)(width-1,0)).reduce(_|_)

object Splice:
  /** Split a UInt into slices with explicit widths (low bits first).
    * Example: Splice(0b11010110, Seq(3, 5)) => Vec(0b110, 0b11010)
    */
  def apply(x: UInt, widths: Seq[Int])(using m: Module): Vec[UInt] =
    val inputWidth = x.requireKnownWidth("Splice")
    val totalWidth = widths.sum
    if totalWidth != inputWidth then
      throw new IllegalArgumentException(
        s"Splice widths must sum to input width (expected $inputWidth, got $totalWidth)"
      )
    if widths.exists(_ <= 0) then
      throw new IllegalArgumentException("Splice widths must all be positive")
    var offset = 0
    val elems = widths.map { w =>
      val elem = x(offset + w - 1, offset)
      offset += w
      elem
    }
    Vec(elems.toSeq)

  /** Split a UInt into equal-width slices (low bits first).
    * Example: Splice(0b11010110, 4) => Vec(0b0110, 0b1101)
    */
  def apply(x: UInt, width: Int)(using m: Module): Vec[UInt] =
    val inputWidth = x.requireKnownWidth("Splice")
    val count = inputWidth / width
    if inputWidth % width != 0 then
      throw new IllegalArgumentException(
        s"Input bitwidth ${inputWidth} is not a multiple of splice width ${width}"
      )
    var offset = 0
    val elems = (0 until count).map { _ =>
      val elem = x(offset + width - 1, offset)
      offset += width
      elem
    }
    Vec(elems.toSeq)

  /** Split a SInt into slices with explicit widths (low bits first).
    * Example: Splice(0b11010110.S, Seq(3, 5)) => Vec(0b110.S, 0b11010.S)
    */
  def apply(x: SInt, widths: Seq[Int])(using m: Module): Vec[SInt] =
    val inputWidth = x.requireKnownWidth("Splice")
    val totalWidth = widths.sum
    if totalWidth != inputWidth then
      throw new IllegalArgumentException(
        s"Splice widths must sum to input width (expected $inputWidth, got $totalWidth)"
      )
    if widths.exists(_ <= 0) then
      throw new IllegalArgumentException("Splice widths must all be positive")
    var offset = 0
    val elems = widths.map { w =>
      val bits = x(offset + w - 1, offset)
      offset += w
      ModuleOps.prim1Op(SInt(Width(w)), IR.PrimOp.AsSInt, bits, m)
    }
    Vec(elems.toSeq)
