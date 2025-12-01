
package hdl9

import scala.deriving.*
import scala.compiletime.*
import scala.NamedTuple
import scala.util.NotGiven
import scala.quoted.*

enum Direction:
  case In, Out

object Direction:
  inline def flip(d: Direction): Direction = d match
    case Direction.In  => Direction.Out
    case Direction.Out => Direction.In

sealed class Width(val value: Int):
  override def toString: String = s"${value}"

object Width:
  def apply(x: Int): Width = new Width(x)

sealed trait ValueType

sealed class UInt(val w: Width, val dir: Direction = Direction.Out) extends ValueType:
  override def toString(): String = s"UInt($w.W, $dir)"

object UInt:
  def apply(w: Width): UInt = new UInt(w)

sealed class Bool(val dir: Direction = Direction.Out) extends ValueType:
  override def toString(): String = s"Bool($dir)"

object Bool:
  def apply(): Bool = new Bool
  def apply(u: Unit): Bool = new Bool

trait Bundle extends ValueType

final class Vec[T <: ValueType](val elem: T, val len: Int) extends ValueType:
  override def toString(): String = s"Vec($elem, $len)"

object Vec:
  def apply[T <: ValueType](elem: T, len: Int): Vec[T] = new Vec(elem, len)
  // TODO: Add foreach, map...
