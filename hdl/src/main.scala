package hdl

import scala.compiletime.{erasedValue, constValue, summonInline}
import scala.deriving.*
import scala.compiletime.ops.int.*
import scala.annotation.targetName


sealed class Width(val value: Int):
  override def toString: String = s"${value}"

object Width:
  def apply(x: Int): Width = new Width(x)

sealed trait Signal

sealed case class UInt(val w: Width) extends Signal:
  override def toString(): String = s"UInt($w)"

sealed case class UIntLit(val w: Width, val v: Int) extends Signal:
  override def toString(): String = s"UIntLit($w, $v)"

trait Bundle extends Signal

object Main:
  def main(args: Array[String]): Unit =
    val uint_3 = UInt(Width(3))
    println(s"UInt(3): $uint_3")


    class MyBundle(w1: Int, w2: Int) extends Bundle:
      val a = UInt(Width(w1))
      val b = UInt(Width(w2))

    val my_bundle = new MyBundle(2, 3)
    println(s"my_bundle ${my_bundle} ${my_bundle.a} ${my_bundle.b}")

