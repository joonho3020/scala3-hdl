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

class Bundle(elems: (String, Any)*) extends Selectable:
  private val fields = elems.toMap
  def selectDynamic(name: String): Any = fields(name)

object Main:
  def main(args: Array[String]): Unit =
    println("Hello World")

    println(s"${UInt(Width(3))}")
    println(s"${UIntLit(Width(3))(4)}")

    class MyBundle(x: Int, y: Int) extends Bundle:
      val a = UInt(Width(x))
      val b = UInt(Width(y))

    val my_bundle = new MyBundle(2, 3)

    println(s"${my_bundle}")
    println(s"${my_bundle.a}")
    println(s"${my_bundle.b}")

    class NestedBundle(x: Int, y: Int, z: Int) extends Bundle:
      val inner = new MyBundle(x, y)
      val outer = UInt(Width(z))

    val nested_bundle = new NestedBundle(2, 3, 4)
    println(s"${nested_bundle.outer}")
    println(s"${nested_bundle.inner.a}")
    println(s"${nested_bundle.inner.b}")