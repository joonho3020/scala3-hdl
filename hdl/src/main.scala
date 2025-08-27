package hdl

import scala.compiletime.{erasedValue, constValue, summonInline}
import scala.deriving.*
import scala.compiletime.ops.int.*
import scala.annotation.targetName



sealed trait Signal


sealed trait WidthType
sealed trait Width[N <: Int] extends WidthType

sealed case class UInt[W <: WidthType]() extends Signal

sealed trait Dir
sealed trait In  extends Dir
sealed trait Out extends Dir

type Opp[D <: Dir] = D match
  case In  => Out
  case Out => In

final case class Port[D <: Dir, T](payload: T)





object Main:
  def main(args: Array[String]): Unit =
    case class X(
      a: Port[Out, UInt[Width[12]]],
      b: Port[Out, UInt[Width[8]]]
    )

    object X:
      def apply(w1: Int, w2: Int) =
        new X(
          Port[Out, UInt[Width[w1]]](UInt[Width[12]]()), 
          Port[Out, UInt[Width[8]]](UInt[Width[8]]())
        )

    println("Hello, HDL!")

    val x = new X()
    print(x)

// case class Y(
// x: Port[Out, X],
// c: Port[In,  UInt[Width[w1 * w2]]]
// )

