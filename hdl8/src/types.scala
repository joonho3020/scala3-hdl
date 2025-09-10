package hdl8

import scala.deriving.*
import scala.compiletime.*
import scala.NamedTuple
import scala.util.NotGiven
import scala.quoted.*

final class Reg[T](val t: T) extends Selectable:
  type Fields = NamedTuple.Map[
    NamedTuple.From[T],
    [X] =>> Reg[X & ValueType]]

  inline def selectDynamic(name: String): Reg[?] =
    summonFrom {
      case m: Mirror.ProductOf[T] =>
        val labels = constValueTuple[m.MirroredElemLabels].toArray
        val idx = labels.indexOf(name)
        val child = t.asInstanceOf[Product].productElement(idx).asInstanceOf[ValueType]
        new Reg(child)
      case _ =>
        throw new NoSuchElementException(s"${t.getClass.getName} has no field '$name'")
    }
  override def toString(): String =
    s"Reg(${t})"

object Reg:
  def apply[T <: ValueType](t: T): Reg[T] = new Reg(t)

object RegVecOps:
  extension [A <: ValueType](rv: Reg[Vec[A]])
    def apply(index: Int): Reg[A] =
      new Reg(rv.t.elem)

    def apply(start: Int, end: Int): Reg[Vec[A]] =
      val sliceLen = (end - start) + 1
      new Reg(Vec(rv.t.elem, sliceLen))

// type Undir[T] = T match
// case (t & Input)      => Undir[t]
// case (Input & t)      => Undir[t]
// case (t & Output)     => Undir[t]
// case (Output & t)     => Undir[t]
// case (t & Direction)  => Undir[t]
// case (Direction & t)  => Undir[t]
// case (a & b)          => Undir[a] & Undir[b]
// case t                => t

type HostTypeOf[T] = T match
  case UInt      => Int
  case Bool      => Boolean
  case Vec[t]    => Seq[HostTypeOf[t & ValueType]]
  case Input[t]  => HostTypeOf[t]
  case Output[t] => HostTypeOf[t]
  case _         => NamedTuple.Map[NamedTuple.From[T], [X] =>> HostTypeOf[X & ValueType]]

final class Lit[T](private val payload: Any) extends Selectable:
  type Fields = NamedTuple.Map[
    NamedTuple.From[T],
    [X] =>> Lit[X & ValueType]
  ]

  inline def selectDynamic(name: String): Lit[?] =
    summonFrom {
      case m: Mirror.ProductOf[T] =>
        val labels = constValueTuple[m.MirroredElemLabels].toArray
        val idx = labels.indexOf(name)
        val subpayload = payload.asInstanceOf[Product].productElement(idx)
        new Lit[Any](subpayload)
      case _ =>
        throw new NoSuchElementException(s"${summonInline[ValueOf[String]]}")
    }
  transparent inline def get: HostTypeOf[T] =
    payload.asInstanceOf[HostTypeOf[T]]

object LitVecOps:
  extension [A <: ValueType](lv: Lit[Vec[A]])
    def apply(index: Int): Lit[A] =
      val seq = lv.get
      new Lit[A](seq(index))

    def apply(start: Int, end: Int): Lit[Vec[A]] =
      val seq = lv.get
      new Lit[Vec[A]](seq.slice(start, end + 1))

object Lit:
  // This is required to make sure that the order of the names in the input
  // named tuple matches that of T <: Bundle
  // Technically, we need to add an assertion in selectDynamic of Lit,
  // but lets now worry about this for now...
  inline def apply[T <: ValueType](inline v: HostTypeOf[T]): Lit[T] =
    new Lit[T](v)
