
package hdl8

import scala.deriving.*
import scala.compiletime.*
import scala.NamedTuple
import scala.util.NotGiven
import scala.quoted.*

sealed class Width(val value: Int):
  override def toString: String = s"${value}"

object Width:
  def apply(x: Int): Width = new Width(x)

sealed trait ValueType

sealed class UInt(val w: Width) extends ValueType:
  override def toString(): String = s"UInt($w.W)"

object UInt:
  def apply(w: Width): UInt = new UInt(w)

sealed class Bool extends ValueType:
  override def toString(): String = s"Bool()"

object Bool:
  def apply(): Bool = new Bool

trait Bundle extends ValueType

// Direction types
sealed trait Direction

sealed trait InputDir extends Direction
sealed trait OutputDir extends Direction

// Directional value types
sealed trait DirectionalValue[T <: ValueType, D <: Direction] extends ValueType:
  val value: T

final class Input[T <: ValueType](val value: T) extends DirectionalValue[T, InputDir]:
  override def toString(): String = s"${value}.in"

final class Output[T <: ValueType](val value: T) extends DirectionalValue[T, OutputDir]:
  override def toString(): String = s"${value}.out"

final class Vec[T <: ValueType](val elem: T, val len: Int) extends ValueType:
  override def toString(): String = s"Vec($elem, $len)"

object Vec:
  def apply[T <: ValueType](elem: T, len: Int): Vec[T] = new Vec(elem, len)
  // TODO: Add foreach, map...
