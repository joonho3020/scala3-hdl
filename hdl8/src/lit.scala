package hdl8

import scala.deriving.*
import scala.compiletime.*
import scala.NamedTuple
import scala.util.NotGiven
import scala.quoted.*

type HostTypeOf[T] = T match
  case UInt    => Int
  case Bool    => Boolean
  case Vec[t]  => Seq[HostTypeOf[t & ValueType]]
  case _       => NamedTuple.Map[NamedTuple.From[T], [X] =>> HostTypeOf[X & ValueType]]

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
