package hdl3

import compiletime.*
import scala.deriving.*
import scala.quoted.*
import scala.deriving.Mirror
import scala.language.implicitConversions
import scala.language.dynamics

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

sealed trait RefType

final class Reg[V <: ValueType](val value: V) extends Dynamic:
  override def toString(): String = s"Reg($value)"
  transparent inline def selectDynamic(name: String): Any =
    ${ RegMacros.selectFieldFromReg[V]('this, 'name) }

// trait RegImpl[V <: ValueType] extends RefType:
// def asReg(v: V): Reg[V]

// object RegImpl:
// given RegImpl[Clock]:
// def asReg(v: Clock): Reg[Clock] = new Reg(v)

// given RegImpl[UInt]:
// def asReg(v: UInt): Reg[UInt] = new Reg(v)

// inline def derived[T <: Bundle](using m: Mirror.Of[T]): RegImpl[T] =
// inline m match
// case p: Mirror.ProductOf[T] =>
// new RegImpl[T]:
// def asReg(v: T): Reg[T] = new Reg(v)
// case s: Mirror.SumOf[T] =>
// compiletime.error("RegImpl cannot be derived for sum types")

// private inline def summonAll[T <: Tuple]: List[RegImpl[?]] =
// inline erasedValue[T] match
// case _: EmptyTuple => Nil
// case _: (t *: ts) => summonInline[RegImpl[t & ValueType]] :: summonAll[ts]

object Reg:
  def apply[V <: ValueType](v: V) = new Reg(v)
  ()

// macro moved to separate file to avoid cyclic macro dependencies

object Main:
  def main(args: Array[String]): Unit =
    println("Hello World")

    val w1 = Width(2)
    val w2 = Width(7)
    val w3 = w1 + w2
    println(w3)

    val a = UInt(Width(2))
// val b = UInt(2)
    println(a)

    val r1 = Reg(UInt(Width(2)))
    println(r1)

    case class MyBundle(a: UInt, b: UInt) extends Bundle

    object MyBundle:
      def apply(w1: Int, w2: Int): MyBundle =
        new MyBundle(a = UInt(Width(w1)), b = UInt(Width(w2)))

    val mybundle = MyBundle(2, 3)
    val rmb = Reg(MyBundle(4, 5))
    println(rmb.a)
