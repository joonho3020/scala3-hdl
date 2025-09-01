package hdl2

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

type HostLitValue[V <: ValueType] = V match
  case UInt => Int | BigInt
  case Clock => Boolean
  case _ => Nothing

sealed trait RefType[V <: ValueType]

class Reg[V <: ValueType](value: V) extends RefType[V]:
  override def toString(): String = s"Reg($value)"

class Lit[V <: ValueType](value: V)(lit: HostLitValue[V]) extends RefType[V]:
  override def toString(): String = s"$value($lit)"

object Main:
  def main(args: Array[String]): Unit =
    println("Hello World")

    val a = UInt(Width(2))
    println(a)

    val r1 = Reg(a)
    println(r1)

    val r2 = Reg(Clock())
    println(r2)

    val l1 = Lit(UInt(Width(3)))(4)
    println(l1)
