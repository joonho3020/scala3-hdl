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

object PopCount:
  def apply(x: Vec[Bool])(using m: Module): UInt =
    x.map(_.asUInt).reduce(_ +& _)

  def apply(x: Bits)(using m: Module): UInt =
    val width = x.requireKnownWidth("PopCount")
    val u = x match
      case u: UInt => u
      case other => ModuleOps.prim1Op(UInt(other.getWidth), IR.PrimOp.AsUInt, other, m)
    val bits = (0 until width).map(i => u(i))
    bits.reduce(_ +& _)

object PriorityEncoder:
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
  def apply(in: UInt)(using m: Module): OneHot = (1.U << in).asOH

object MaskLower:
  def apply(in: OneHot)(using m: Module): UInt =
    val width = in.requireKnownWidth("MaskLower")
    (0 until width).map(i => in.asUInt >> i.U).reduce(_|_)

object MaskUpper:
  def apply(in: OneHot)(using m: Module): UInt =
    val width = in.requireKnownWidth("MaskUpper")
    (0 until width).map(i => (in.asUInt << i.U)(width-1,0)).reduce(_|_)

object Splice:
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
