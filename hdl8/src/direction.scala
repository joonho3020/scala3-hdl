package hdl8

import scala.deriving.*
import scala.compiletime.*
import scala.NamedTuple
import scala.util.NotGiven


sealed trait Direction
sealed trait Input extends Direction
sealed trait Output extends Direction

extension [T <: ValueType](t: T)
  inline def in: T & Input = t.asInstanceOf[T & Input]
  inline def out: T & Output = t.asInstanceOf[T & Output]
