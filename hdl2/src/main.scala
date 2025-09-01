package hdl2

import scala.deriving.Mirror
import scala.compiletime.{constValue, erasedValue, summonInline}
import scala.language.implicitConversions

object Width:
  opaque type Width = Int

  def apply(w: Int): Width = w

  extension (x: Width)
    def toInt: Int = x
    def + (y: Width): Width = x + y
    def - (y: Width): Width = x - y
    def * (y: Width): Width = x * y

    def show: String = s"Width($x)"

import Width.Width

sealed trait ValueType

final class UInt(w: Width) extends ValueType:
  override def toString(): String = s"UInt(${w.show})"

final class Clock extends ValueType:
  override def toString(): String = "Clock"

trait Bundle extends ValueType


sealed trait RefType[V <: ValueType]

class Reg[V <: ValueType](v: V) extends RefType[V]:
  override def toString(): String = s"Reg(${v})"

class Lit[V <: ValueType](v: V)(using h: HostLit[V])(lit: h.Repr) extends RefType[V]:
  override def toString(): String = s"Lit(${v}, ${lit})"



// Allow passing Int where BigInt is expected
given Conversion[Int, BigInt] with
  def apply(i: Int): BigInt = BigInt(i)

// Identity conversions to satisfy the generic (v, r) overload
given Conversion[BigInt, BigInt] with
  def apply(b: BigInt): BigInt = b


object Lit:
  def apply[V <: ValueType, R](v: V, r: R)(using h: HostLit[V], c: Conversion[R, h.Repr]): Lit[V] =
    new Lit[V](v)(using h)(c(r))

trait HostLit[V <: ValueType]:
  type Repr

object HostLit:
  type Aux[V <: ValueType, R] = HostLit[V] { type Repr = R }

  given uint_bigint: Aux[UInt, BigInt] = new HostLit[UInt]{ type Repr = BigInt }
  given clock: Aux[Clock, Boolean]    = new HostLit[Clock]{ type Repr = Boolean }

object Main:
  def main(args: Array[String]): Unit =
    println("Hello World")

    val a = UInt(Width(2))
    println(a)

    val r1 = Reg(UInt(Width(4)))
    println(r1)

    val uhost = summon[HostLit[UInt]]
    println(uhost)

    val l1 = Lit(UInt(Width(4)), BigInt(2))
    println(l1)

    val l2 = Lit(UInt(Width(4)), 2)
    println(l2)