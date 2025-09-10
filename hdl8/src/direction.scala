package hdl8

import scala.deriving.*
import scala.compiletime.*
import scala.NamedTuple
import scala.util.NotGiven

// Extension methods for creating directional types
extension [T <: ValueType](t: T)
  def in: Input[T] = new Input(t)
  def out: Output[T] = new Output(t)

// Flip operation for runtime values
def flip[T <: ValueType](dv: DirectionalValue[T, ?]): DirectionalValue[T, ?] = dv match
  case iv: Input[T] => new Output(iv.value)
  case ov: Output[T] => new Input(ov.value)

// Flip operation for bundles (simplified - would need more sophisticated implementation for full functionality)
def flipBundle[T <: Bundle](bundle: T): T = bundle
