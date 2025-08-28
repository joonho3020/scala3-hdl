package hdl

import scala.compiletime.{erasedValue, constValue, summonInline}
import scala.deriving.*
import scala.compiletime.ops.int.*
import scala.annotation.targetName
import scala.compiletime.erasedValue


sealed class Width(val value: Int):
  override def toString: String = s"${value}"

object Width:
  def apply(x: Int): Width = new Width(x)

sealed trait Flavor
sealed trait Bit extends Flavor
sealed trait Lit  extends Flavor
sealed trait Dpi  extends Flavor

sealed trait Signal[F <: Flavor]

sealed case class UInt[F <: Flavor](val w: Width) extends Signal[F]

trait Bundle[F <: Flavor] extends Signal[F]

object Main:
  def main(args: Array[String]): Unit =
    val uint_3 = UInt(Width(3))
    println(s"UInt(3): $uint_3")


    class MyBundle[F <: Flavor](w1: Int, w2: Int) extends Bundle:
      val a = UInt[F](Width(w1))
      val b = UInt[F](Width(w2))

    val my_bundle = new MyBundle[Bit](2, 3)
    println(s"my_bundle ${my_bundle} ${my_bundle.a} ${my_bundle.b}")

    val my_bundle_dpi = new MyBundle[Dpi](2, 3)
    println(s"my_bundle_dpi ${my_bundle_dpi} ${my_bundle_dpi.a} ${my_bundle_dpi.b}")
