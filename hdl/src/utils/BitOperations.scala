package hdl

object Fill:
  def apply(n: Int, x: Bool)(using m: Module): UInt =
    if n <= 0 then
      throw new IllegalArgumentException("Fill requires n > 0")
    Seq.fill(n)(x.asUInt).Cat

  def apply(n: Int, x: UInt)(using m: Module): UInt =
    if n <= 0 then
      throw new IllegalArgumentException("Fill requires n > 0")
    Seq.fill(n)(x).Cat

object Reverse:
  def apply(x: UInt)(using m: Module): UInt =
    val width = x.getWidth match
      case KnownWidth(v) => v
      case _ => throw new IllegalArgumentException("Reverse requires UInt with known width")
    if width < 2 then
      throw new IllegalArgumentException("Reverse requires UInt with width >= 2")
    val bits = (0 until width).map(i => x(i))
    var acc = bits.last
    var i = width - 2
    while i >= 0 do
      acc = ModuleOps.prim2Op(UInt(acc.getWidth + bits(i).getWidth), IR.PrimOp.Cat, acc, bits(i), m)
      i -= 1
    acc

  def apply(x: SInt)(using m: Module): UInt =
    val width = x.getWidth match
      case KnownWidth(v) => v
      case _ => throw new IllegalArgumentException("Reverse requires SInt with known width")
    if width < 2 then
      throw new IllegalArgumentException("Reverse requires SInt with width >= 2")
    val bits = (0 until width).map(i => x(i))
    var acc = bits.last
    var i = width - 2
    while i >= 0 do
      acc = ModuleOps.prim2Op(UInt(acc.getWidth + bits(i).getWidth), IR.PrimOp.Cat, acc, bits(i), m)
      i -= 1
    acc

object PopCount:
  def apply(x: Vec[Bool])(using m: Module): UInt =
    x.map(_.asUInt).reduce(_ + _)

  def apply(x: UInt)(using m: Module): UInt =
    val width = x.getWidth match
      case KnownWidth(v) => v
      case _ => throw new IllegalArgumentException("PopCount requires UInt with known width")
    val bits = (0 until width).map(i => x(i))
    bits.reduce(_ + _)

  def apply(x: OneHot)(using m: Module): UInt =
    PopCount(x.asUInt)

object PriorityEncoder:
  def apply(x: UInt)(using m: Module): UInt =
    val width = x.getWidth match
      case KnownWidth(v) => v
      case _ => throw new IllegalArgumentException("PriorityEncoderOH requires UInt with known width")
    val bits = (0 until width).map(i => x(i).asBool)
    bits.zipWithIndex.reverse.foldLeft(0.U)((acc, x) => {
      Mux(x._1, x._2.U, acc)
    })

  def apply(x: OneHot)(using m: Module): UInt =
    PriorityEncoder(x.asUInt)

// TODO: Find a way to abstract of types such as UInt/SInt here
object Splice:
  def apply(x: UInt, widths: Seq[Int])(using m: Module): Vec[UInt] =
    val inputWidth = x.getWidth match
      case KnownWidth(v) => v
      case _ => throw new IllegalArgumentException("Splice requires UInt with known width")

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

  def apply(x: UInt, width: Int)(using m: Module): Vec[UInt] =
    val inputWidth = x.getWidth match
      case KnownWidth(v) => v
      case _ => throw new IllegalArgumentException("Splice requires UInt with known width")

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

  def apply(x: SInt, widths: Seq[Int])(using m: Module): Vec[SInt] =
    val inputWidth = x.getWidth match
      case KnownWidth(v) => v
      case _ => throw new IllegalArgumentException("Splice requires SInt with known width")

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
