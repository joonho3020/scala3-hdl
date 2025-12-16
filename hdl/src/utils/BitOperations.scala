package hdl

// In the future, this should really just be an IR node...
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
