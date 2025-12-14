package hdl

case class Valid[T <: HWData](valid: Bool, bits: T) extends Bundle[Valid[T]]
object Valid:
  def apply[T <: HWData](x: T): Valid[T] =
    Valid(
      valid = Output(Bool()),
      bits  = Output(x)
    )
