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

sealed trait Sig[+T]

final case class Wire[T]() extends Sig[T]
final case class Const[T](value: Any) extends Sig[T]
final case class DpiPort[T](name: String) extends Sig[T]

final class UInt[W <: Int & Singleton]

object UInt:
  inline def widthOf[W <: Int & Singleton]: Int = constValue[W]

  def wire[W <: Int & Singleton]: Sig[UInt[W]] = Wire[UInt[W]]()

  def const[W <: Int & Singleton](value: BigInt): Sig[UInt[W]] =
    Const[UInt[W]](value)

  def dpi[W <: Int & Singleton](name: String): Sig[UInt[W]] =
    DpiPort[UInt[W]](name)

object Main:
  def main(args: Array[String]): Unit =
    println("HDL scaffolding loaded: Bundle, Sig, Wire, Const, UInt, DPI stubs.")

    trait A extends Bundle:
      val x: Sig[UInt[8]]
      val y: Sig[UInt[8]]

    trait B extends Bundle:
      val a1: A
      val a2: A
      val z: Sig[UInt[16]]

    // Create concrete implementations of A and B
    val a = new A:
      val x: Sig[UInt[8]] = UInt.wire[8]
      val y: Sig[UInt[8]] = UInt.wire[8]

    val b = new B:
      val a1: A = new A:
        val x: Sig[UInt[8]] = UInt.wire[8]
        val y: Sig[UInt[8]] = UInt.wire[8]
      val a2: A = new A:
        val x: Sig[UInt[8]] = UInt.wire[8]
        val y: Sig[UInt[8]] = UInt.wire[8]
      val z: Sig[UInt[16]] = UInt.wire[16]

    println(s"a: ${a}")
    println(s"b: ${b}")

    // Use Bundle.wire to create wire instances of bundles
    val a_wire: A = Bundle.wire[A]
    val b_wire: B = Bundle.wire[B]
    println(s"a_wire: ${a_wire}")
    println(s"b_wire: ${b_wire}")

    // Use Bundle.lit to create literal instances with values
    // Note: This will fail with "not yet implemented" since it's just a scaffold
    // val a_lit: A = Bundle.lit[A]((UInt.const[8](42), UInt.const[8](100)))
    // val b_lit: B = Bundle.lit[B]((a_lit, a_lit, UInt.const[16](1234)))

    // Use Bundle.dpi to create DPI port instances
    // Note: This will fail with "not yet implemented" since it's just a scaffold
    // val a_dpi: A = Bundle.dpi[A]("my_module_a")
    // val b_dpi: B = Bundle.dpi[B]("my_module_b")

    // For now, let's create some individual signals to demonstrate the API
// val wire_8bit = UInt.wire[8]
// val const_8bit = UInt.const[8](42)
// val dpi_8bit = UInt.dpi[8]("my_signal")
// println(s"wire_8bit: ${wire_8bit}")
// println(s"const_8bit: ${const_8bit}")
// println(s"dpi_8bit: ${dpi_8bit}")
