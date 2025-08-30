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

sealed class UIntLit(override val w: Width)(val v: Int) extends UInt(w):
  override def toString(): String = s"UIntLit($v($w.W))"

class Bundle(elems: (String, Any)*) extends Selectable with Signal:
  private val fields = elems.toMap
  def selectDynamic(name: String): Any = fields(name)
  override def toString: String =
    val body = fields.map { case (k, v) => s"$k=$v" }.mkString(", ")
    s"Bundle($body)"

object Bundle:
  inline def lit[B <: Bundle](inline elems: (String, Signal)*): Bundle =
    ${ BundleMacros.bundleLitImpl[B]('elems) }

object Main:
  def main(args: Array[String]): Unit =
    println("Hello World")

    println(s"${UInt(Width(3))}")
    println(s"${UIntLit(Width(3))(4)}")

    class MyBundle(x: Int, y: Int) extends Bundle:
      val a = UInt(Width(x))
      val b = UInt(Width(y))

    object MyBundle:
      inline def lit(inline a: UIntLit, inline b: UIntLit): Bundle =
        Bundle.lit[MyBundle]("a" -> a, "b" -> b)

    val my_bundle = new MyBundle(2, 3)

    println(s"${my_bundle}")
    println(s"${my_bundle.a}")
    println(s"${my_bundle.b}")

    class NestedBundle(x: Int, y: Int, z: Int) extends Bundle:
      val width_outer = x + y + z
      val inner = new MyBundle(x, y)
      val outer = UInt(Width(width_outer))

    object NestedBundle:
      inline def lit(inline inner: Bundle, inline outer: UIntLit): Bundle =
        Bundle.lit[NestedBundle]("inner" -> inner, "outer" -> outer)

    val nested_bundle = new NestedBundle(2, 3, 4)
    println(s"${nested_bundle.outer}")
    println(s"${nested_bundle.inner.a}")
    println(s"${nested_bundle.inner.b}")

    val my_bundle_lit = MyBundle.lit(
      a = UIntLit(Width(1))(3),
      b = UIntLit(Width(2))(4)
    )
    println(s"${my_bundle_lit}")

    val nested_bundle_lit = NestedBundle.lit(
      inner = MyBundle.lit(a = UIntLit(Width(1))(3), b = UIntLit(Width(2))(4)),
      outer = UIntLit(Width(9))(6)
    )
    println(s"${nested_bundle_lit}")

// Compile error
// val nested_bundle_lit_2 = NestedBundle.lit(
// inner = MyBundle.lit(a = UInt(Width(1)), b = UIntLit(Width(2))(4)),
// outer = UInt(Width(9))
// )
