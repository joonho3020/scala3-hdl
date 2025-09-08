package hdl8

import scala.deriving.*
import scala.compiletime.*

trait ShapeOf[T]:
  type Labels <: Tuple
  type Elems  <: Tuple
  val labels: Array[String]
  def getValueType(t: T, name: String): ValueType

object ShapeOf:
  private inline def labelsList[L <: Tuple]: List[String] =
    inline erasedValue[L] match
      case _: EmptyTuple => Nil
      case _: (h *: t)   => constValue[h].asInstanceOf[String] :: labelsList[t]

  inline def product[T](using m: Mirror.ProductOf[T]): ShapeOf[T] =
    new ShapeOf[T]:
      type Labels = m.MirroredElemLabels
      type Elems = m.MirroredElemTypes
      val labels: Array[String] = labelsList[m.MirroredElemLabels].toArray
      def getValueType(t: T, name: String): ValueType =
        val idx = labels.indexOf(name)
        t.asInstanceOf[Product].productElement(idx).asInstanceOf[ValueType]

  inline given productGiven[T](using m: Mirror.ProductOf[T]): ShapeOf[T] =
    product(using m)

  given ShapeOf[UInt]:
    type Labels = EmptyTuple
    type Elems  = EmptyTuple
    val labels: Array[String] = Array()
    def getValueType(t: UInt, name: String): ValueType =
      sys.error(s"UInt does not have subfield ${name}")
