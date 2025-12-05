package hdl

import scala.deriving.*
import scala.compiletime.*
import scala.NamedTuple
import scala.util.NotGiven
import scala.quoted.*

enum Direction:
  case Default, Flipped

object Direction:
  inline def In: Direction = Direction.Flipped
  inline def Out: Direction = Direction.Default
  inline def flip(d: Direction): Direction = d match
    case Direction.Default => Direction.Flipped
    case Direction.Flipped => Direction.Default

sealed class Width(val value: Int):
  override def toString: String = s"${value}"

object Width:
  def apply(x: Int): Width = new Width(x)

sealed trait HasDirectionality:
  var dir: Direction = Direction.Default
  def flip: Unit =
    this.dir = Direction.flip(this.dir)

sealed trait HWData:
  var literal: Option[Any] = None
  var kind: NodeKind = NodeKind.Unset

  def setNodeKind(kind: NodeKind) = this.kind = kind
  def setLitVal(payload: Any): Unit
  def getLitVal: Any

sealed class UInt(
  val w: Width
) extends HWData with HasDirectionality:

  def setLitVal(payload: Any): Unit =
    this.literal = Some(payload.asInstanceOf[HostTypeOf[UInt]])

  def getLitVal: HostTypeOf[UInt] =
    this.literal match
      case Some(v) => v.asInstanceOf[HostTypeOf[UInt]]
      case None    => throw new NoSuchElementException("UInt does not carry a literal value")

  override def toString(): String = s"UInt($w.W, $dir)"

object UInt:
  def apply(w: Width): UInt = new UInt(w)

sealed class Bool extends HWData with HasDirectionality:
  def setLitVal(payload: Any): Unit =
    this.literal = Some(payload.asInstanceOf[HostTypeOf[Bool]])

  def getLitVal: HostTypeOf[Bool] =
    literal match
      case Some(v) => v.asInstanceOf[HostTypeOf[Bool]]
      case None    => throw new NoSuchElementException("Bool does not carry a literal value")

  override def toString(): String = s"Bool($dir)"

object Bool:
  def apply(): Bool = new Bool
  def apply(u: Unit): Bool = new Bool

type HostTypeOf[T] = T match
  case UInt  => BigInt
  case Bool  => Boolean
  case _     => NamedTuple.Map[NamedTuple.From[T], [X] =>> HostTypeOf[X]]

type FieldTypeFromTuple[Labels <: Tuple, Elems <: Tuple, L <: String] = (Labels, Elems) match
  case (L *: _, h *: _)    => h
  case (_ *: lt, _ *: et)  => FieldTypeFromTuple[lt, et, L]
  case _                   => Nothing

// NOTE: Literals assume that the BundleIf is pure. That is, all fields are of HWData
// and there is no mixing with Scala's library types such as Option, Seq, List
trait Bundle[T] extends Selectable with HWData with HasDirectionality { self: T =>
  type FieldToNode[X] = X match
    case _           => X

  type Fields = NamedTuple.Map[NamedTuple.From[T], [X] =>> FieldToNode[X]]

  transparent inline def selectDynamic[L <: String & Singleton](label: L): Any =
    summonFrom {
      case m: Mirror.ProductOf[T] =>
        type Labels = m.MirroredElemLabels
        type Elems = m.MirroredElemTypes
        type FT = FieldTypeFromTuple[Labels, Elems, L]

        val labels = constValueTuple[Labels].toArray
        val idx = labels.indexOf(constValue[L])

        if idx < 0 then throw new NoSuchElementException(s"${self.getClass.getName} has no field '${label}'")
        val childT = self.asInstanceOf[Product].productElement(idx).asInstanceOf[FT]
        val childLit = literal.map(_.asInstanceOf[Product].productElement(idx))
        val childRef = constValue[L]
        println(s"label: ${label} childRef: ${childRef} childLit: ${childLit}")
        inline erasedValue[FT] match
          case _: HWData =>
            childLit.map(lit => childT.asInstanceOf[FT & HWData].setLitVal(lit))
            childT
          case _ =>
            childT
      case _ =>
        throw new NoSuchElementException(s"${self.getClass.getName} has no field '${label}'")
    }

  def setLitVal(payload: Any): Unit =
    this.literal = Some(payload.asInstanceOf[HostTypeOf[T]])

  def getLitVal: HostTypeOf[T] =
    literal match
      case Some(v) => v.asInstanceOf[HostTypeOf[T]]
      case None    => throw new NoSuchElementException("Node does not carry a literal value")
}

object Input:
  def apply[T <: HasDirectionality](t: T): T =
    t.flip
    t

object Output:
  def apply[T <: HasDirectionality](t: T): T =
    t

object Flipped:
  def apply[T <: HasDirectionality](t: T): T =
    Input(t)
