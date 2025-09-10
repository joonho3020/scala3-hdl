
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

// Direction types are now defined in direction_literal.scala
// This file contains the basic value types

// Literal type aliases for semantic clarity
type Input = 1
type Output = -1
type Flip = -1

// Directional value types using literal types
sealed trait DirectionalValue[T <: ValueType, D <: Int] extends ValueType:
  val value: T

final class InputValue[T <: ValueType](val value: T) extends DirectionalValue[T, Input]:
  override def toString(): String = s"${value}.in"

final class OutputValue[T <: ValueType](val value: T) extends DirectionalValue[T, Output]:
  override def toString(): String = s"${value}.out"

// Type-level arithmetic for direction flipping
type FlipDirection[D <: Int] = D match
  case 1 => -1
  case -1 => 1

// Extension methods for creating directional types
extension [T <: ValueType](t: T)
  def in: InputValue[T] = new InputValue(t)
  def out: OutputValue[T] = new OutputValue(t)

// Flip operation for runtime values
def flip[T <: ValueType, D <: Int](dv: DirectionalValue[T, D]): DirectionalValue[T, FlipDirection[D]] = 
  dv match
    case iv: InputValue[T] => new OutputValue(iv.value)
    case ov: OutputValue[T] => new InputValue(ov.value)

// Type-level flip for bundles - recursive transformation
type FlipBundle[T] = T match
  case DirectionalValue[t, d] => DirectionalValue[t, FlipDirection[d]]
  case Vec[t] => Vec[FlipBundle[t]]
  case _ => NamedTuple.Map[NamedTuple.From[T], [X] =>> FlipBundle[X & ValueType]]

// Runtime flip for bundles (simplified version)
def flipBundle[T](bundle: T): FlipBundle[T] = 
  bundle match
    case dv: DirectionalValue[?, ?] => flip(dv).asInstanceOf[FlipBundle[T]]
    case _ => bundle.asInstanceOf[FlipBundle[T]]

// Type aliases for backward compatibility and convenience
type InputType[T <: ValueType] = InputValue[T]
type OutputType[T <: ValueType] = OutputValue[T]

// Convenience constructors
object Input:
  def apply[T <: ValueType](t: T): InputType[T] = new InputValue(t)

object Output:
  def apply[T <: ValueType](t: T): OutputType[T] = new OutputValue(t)

// Utility functions for working with directions
object DirectionOps:
  
  // Runtime direction checking
  def isInput[T <: ValueType, D <: Int](dv: DirectionalValue[T, D]): Boolean = 
    dv.isInstanceOf[InputValue[T]]
  
  def isOutput[T <: ValueType, D <: Int](dv: DirectionalValue[T, D]): Boolean = 
    dv.isInstanceOf[OutputValue[T]]
  
  // Get the underlying value regardless of direction
  def getValue[T <: ValueType, D <: Int](dv: DirectionalValue[T, D]): T = 
    dv.value
  
  // Create a directional value from a base type and direction
  def createDirectional[T <: ValueType](t: T, isInput: Boolean): DirectionalValue[T, ?] = 
    if isInput then new InputValue(t) else new OutputValue(t)

final class Vec[T <: ValueType](val elem: T, val len: Int) extends ValueType:
  override def toString(): String = s"Vec($elem, $len)"

object Vec:
  def apply[T <: ValueType](elem: T, len: Int): Vec[T] = new Vec(elem, len)
  // TODO: Add foreach, map...
