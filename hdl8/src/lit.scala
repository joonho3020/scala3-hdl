package hdl8

import scala.deriving.*
import scala.compiletime.*
import scala.NamedTuple
import scala.util.NotGiven
import scala.quoted.*

type HostTypeOf[T] = T match
  case UInt => Int
  case Bool => Boolean
  case _    => NamedTuple.Map[NTOf[T], [X] =>> HostTypeOf[X & ValueType]]

final class Lit[T](private val payload: Any) extends Selectable:
  type Fields = NamedTuple.NamedTuple[
    ShapeOf[T]#Labels,
    MapElems[ShapeOf[T]#Elems, [X] =>> Lit[X]]
  ]

  inline def selectDynamic(name: String): Lit[?] =
    val s = summonInline[ShapeOf[T]]
    val idx = s.labels.indexOf(name)
    val p = payload.asInstanceOf[Product]
    new Lit[Any](p.productElement(idx))

// summonFrom {
// case m: Mirror.ProductOf[T] =>
// val labels = constValueTuple[m.MirroredElemLabels].toArray
// val idx = labels.indexOf(name)
// val subpayload = payload.asInstanceOf[Product].productElement(idx)
// new Lit[Any](subpayload)
// case _ =>
// throw new NoSuchElementException(s"${summonInline[ValueOf[String]]}") // never reached, just keep total
// }
  transparent inline def get: HostTypeOf[T] =
    payload.asInstanceOf[HostTypeOf[T]]

object Lit:
  inline def apply[T <: ValueType](inline v: HostTypeOf[T]): Lit[T] =
    new Lit[T](v)
