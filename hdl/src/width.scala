package hdl

sealed class Width(val value: Int):
  override def toString: String = s"${value}"

object Width:
  def apply(x: Int): Width = new Width(x)

def log2Ceil(x: Int): Int =
  if x <= 1 then 0 else 32 - Integer.numberOfLeadingZeros(x - 1)

extension (n: Int)
  def W: Width =
    Width(n)
