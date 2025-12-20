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
