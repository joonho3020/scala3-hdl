package hdl


import scala.deriving.Mirror
import scala.compiletime.{erasedValue, summonInline}

/** Typeclass for connecting leaf hardware values with :=. */
trait LeafConnect[D <: HWData, S <: HWData]:
  /** Connect dst from src. */
  def :=(dst: D, src: S)(using Module): Unit

object LeafConnect:
  given sameHWData[T <: HWData]: LeafConnect[T, T] with
    def :=(dst: T, src: T)(using m: Module) =
      ModuleOps.connect(dst, src, m)

  given dontCare[T <: HWData]: LeafConnect[T, DontCare.type] with
    def :=(dst: T, src: DontCare.type)(using m: Module) =
      ModuleOps.connect(dst, src, m)

/** Typeclass for connecting aggregate hardware values with <>. */
trait AggregateConnect[D <: AggregateHWData, S <: HWData]:
  /** Bulk-connect dst from src. */
  def <>(dst: D, src: S)(using Module): Unit

object AggregateConnect:
  given sameHWData[T <: AggregateHWData]: AggregateConnect[T, T] with
    def <>(dst: T, src: T)(using m: Module) =
      ModuleOps.connect(dst, src, m)

  given dontCare[T <: AggregateHWData]: AggregateConnect[T, DontCare.type] with
    def <>(dst: T, src: DontCare.type)(using m: Module) =
      ModuleOps.connect(dst, src, m)

extension [D <: HWData](dst: D)
  /** Connect a leaf value from src using :=. */
  def :=[S <: HWData](src: S)(using lc: LeafConnect[D, S], m: Module): Unit =
    lc.:=(dst, src)

extension [D <: AggregateHWData](dst: D)
  /** Bulk-connect an aggregate from src using <>. */
  def <>[S <: HWData](src: S)(using lc: AggregateConnect[D, S], m: Module): Unit =
    lc.<>(dst, src)


// Fallback method to when typeclass derivation doesn't work
// Example of this case is union types such as: `UInt | DontCare.type`
// Users may want to use union types for metaprogramming, but enforcing too
// strong of a type requirement may hamper this ability.
extension [T <: HWData](dst: T)
  /** Fallback connect for cases where typeclass derivation is not used. */
  def ::=(src: T)(using m: Module): Unit =
    ModuleOps.connect(dst, src, m)
