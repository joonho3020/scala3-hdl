package hdl.util

import hdl.core._

/** Bundle carrying a valid bit and associated payload. */
case class Valid[T <: HWData](valid: Bool, bits: T) extends Bundle[Valid[T]]
object Valid:
  /** Construct a Valid bundle with output fields for valid and bits. */
  def apply[T <: HWData](x: T): Valid[T] =
    Valid(
      valid = Output(Bool()),
      bits  = Output(x)
    )
