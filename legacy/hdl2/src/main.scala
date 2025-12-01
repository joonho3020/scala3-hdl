package hdl2

import scala.deriving.Mirror
import scala.compiletime.{constValue, erasedValue, summonInline}
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

class Bundle extends Selectable with ValueType:
  def selectDynamic(name: String): ValueType =
    try
      val m = this.getClass.getMethod(name)
      m.invoke(this).asInstanceOf[ValueType]
    catch
      case _: NoSuchMethodException =>
        throw new NoSuchElementException(s"Bundle has no field '$name'")

  override def toString: String =
    val methods = this.getClass.getMethods
      .filter(m => m.getParameterCount == 0)
    val entries =
      methods
        .flatMap { m =>
          val v =
            try m.invoke(this)
            catch case _: Throwable => null
          v match
            case vt: ValueType => Some(s"${m.getName}=$vt")
            case _             => None
        }
    s"Bundle(${entries.mkString(", ")})"

sealed trait RefType[V <: ValueType]

final class FieldRef[V <: ValueType](name: String, v: V) extends RefType[V]:
  override def toString(): String = s".${name} : ${v}"

class Reg[V <: ValueType](v: V) extends RefType[V] with Dynamic:
  override def toString(): String = s"Reg(${v})"
  def selectDynamic(name: String): FieldRef[ValueType] =
    v match
      case b: Bundle =>
        new FieldRef(
          name,
          b.selectDynamic(name)
        )
      case _ =>
        throw new UnsupportedOperationException("Subfield selection is only supported on Reg[Bundle]")

// Allow passing Int where BigInt is expected
given Conversion[Int, BigInt] with
  def apply(i: Int): BigInt = BigInt(i)

// Identity conversions to satisfy the generic (v, r) overload
given Conversion[BigInt, BigInt] with
  def apply(b: BigInt): BigInt = b

class Lit[V <: ValueType](v: V)(using h: HostLit[V])(lit: h.Repr) extends RefType[V]:
  override def toString(): String = s"Lit(${v}, ${lit})"

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

    class MyBundle(wa: Int, wb: Int) extends Bundle:
      val a = UInt(Width(wa))
      val b = UInt(Width(wb))

    val mb = new MyBundle(2, 3)
    println(mb)
    println(mb.a)

    // NOTE Okay. This works, but is undesirable for a few reaons
    // - One, we have to use reflection in Bundle in order to find the subfields for Reg[MyBundle]
    // - Second, we lose LSP support when performing subfield access of Reg[MyBundle]. So `regbundle.` will not suggest `a: UInt` for example
    val regbundle = new Reg(new MyBundle(2, 3))
    println(regbundle.a)
// regbundle.a
