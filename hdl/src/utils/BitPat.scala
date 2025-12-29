package hdl.util

import hdl.core._

/** Bit pattern with don't-care support for matching UInt values. */
final class BitPat private (
  val width: Int,
  val value: BigInt,
  val mask: BigInt
):
  require(width > 0, "BitPat width must be positive")
  require((value & ~mask) == 0, "BitPat value has bits set that are masked as don't-care")

  private def createLiteral(v: BigInt, w: Int): UInt =
    val u = UInt(Width(w))
    u.setNodeKind(NodeKind.Lit)
    u.setLitVal(v)
    u

  /** Match a UInt against this pattern. */
  def ===(that: UInt)(using m: Module): Bool =
    val maskLit = createLiteral(mask, width)
    val valueLit = createLiteral(value, width)
    (that & maskLit) === valueLit

  /** Negated match against this pattern. */
  def =/=(that: UInt)(using m: Module): Bool =
    !(this === that)

  override def toString: String =
    val chars = (0 until width).reverse.map { i =>
      if ((mask >> i) & 1) == 0 then '?'
      else if ((value >> i) & 1) == 1 then '1'
      else '0'
    }
    s"BitPat(${chars.mkString})"

  override def equals(obj: Any): Boolean = obj match
    case other: BitPat =>
      width == other.width && value == other.value && mask == other.mask
    case _ => false

  override def hashCode(): Int =
    (width, value, mask).hashCode()

object BitPat:
  /** Parse a pattern string using 0, 1, and ? with optional b/0b prefix. */
  def apply(pattern: String): BitPat =
    val cleaned = pattern.trim
    val stripped = if cleaned.startsWith("b") then cleaned.drop(1)
                   else if cleaned.startsWith("0b") then cleaned.drop(2)
                   else cleaned

    val width = stripped.length
    var value = BigInt(0)
    var mask = BigInt(0)

    stripped.foreach { c =>
      value = value << 1
      mask = mask << 1
      c match
        case '0' =>
          mask = mask | 1
        case '1' =>
          value = value | 1
          mask = mask | 1
        case '?' =>
        case _ =>
          throw new IllegalArgumentException(s"Invalid character '$c' in BitPat pattern")
    }

    new BitPat(width, value, mask)

  /** Create a pattern with all bits set to don't-care. */
  def dontCare(width: Int): BitPat =
    new BitPat(width, BigInt(0), BigInt(0))

  /** Create a fully specified pattern with no don't-care bits. */
  def literal(value: BigInt, width: Int): BitPat =
    new BitPat(width, value, (BigInt(1) << width) - 1)
