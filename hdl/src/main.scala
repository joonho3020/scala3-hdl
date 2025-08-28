package hdl

import scala.compiletime.{erasedValue, constValue, summonInline}
import scala.deriving.*
import scala.compiletime.ops.int.*
import scala.annotation.targetName
import scala.compiletime.erasedValue
import scala.quoted.*

sealed class Width(val value: Int):
  override def toString: String = s"${value}"

object Width:
  def apply(x: Int): Width = new Width(x)

sealed trait Signal

sealed class UInt(val w: Width) extends Signal:
  def apply(w: Width): UInt = new UInt(w)
  override def toString(): String = s"UInt($w.W)"

sealed class UIntLit(val w: Width)(val v: Int) extends Signal:
  override def toString(): String = s"UIntLit($v($w.W))"

object Main:
  def main(args: Array[String]): Unit =
    println("Hello World")

    println(s"${UInt(Width(3))}")
    println(s"${UIntLit(Width(3))(4)}")
