package hdl

/** Constructors for width values. */
object Width {
  /** Create a known width. */
  def apply(x: Int): Width = KnownWidth(x)
  /** Create an unknown width. */
  def apply(): Width = UnknownWidth()
}

sealed abstract class Width:
  type W = Int
  def min(that:              Width): Width = this.op(that, _ min _)
  def max(that:              Width): Width = this.op(that, _ max _)
  def +(that:                Width): Width = this.op(that, _ + _)
  def +(that:                Int):   Width = this.op(this, (a, b) => a + that)
  def shiftRight(that:       Int): Width = this.op(this, (a, b) => 0.max(a - that))
  def dynamicShiftLeft(that: Width): Width =
    this.op(that, (a, b) => a + (1 << b) - 1)

  def known: Boolean
  def get:   W
  protected def op(that: Width, f: (W, W) => W): Width

sealed case class UnknownWidth() extends Width:
  def known: Boolean = false
  def get:   Int = None.get
  def op(that: Width, f: (W, W) => W): Width = this
  override def toString: String = ""

sealed case class KnownWidth(value: Int) extends Width:
  require(value >= 0)
  def known: Boolean = true
  def get:   Int = value
  def op(that: Width, f: (W, W) => W): Width = that match {
    case KnownWidth(x) => KnownWidth(f(value, x))
    case _             => that
  }
  override def toString: String = s"<${value.toString}>"

/** Returns ceil(log2(x)) for x >= 0 with log2Ceil(0) == 0 and log2Ceil(1) == 0. */
def log2Ceil(x: Int): Int =
  if x <= 1 then 0 else 32 - Integer.numberOfLeadingZeros(x - 1)

/** Returns the number of bits required to encode x, with log2Up(0) == 1. */
def log2Up(x: Int): Int =
  if x < 0 then throw new IllegalArgumentException("log2Up requires x >= 0")
  math.max(1, (BigInt(x) - 1).bitLength)

extension (n: Int)
  def W: Width =
    Width(n)
