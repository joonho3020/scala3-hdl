package hdl

import scala.deriving.*
import scala.compiletime.*
import scala.NamedTuple

enum NodeKind:
  case Reg, Wire, IO, PrimOp, Lit

type HostTypeOf[T] = T match
  case UInt  => Int
  case Bool  => Boolean
  case _     => NamedTuple.Map[NamedTuple.From[T], [X] =>> HostTypeOf[X & ValueType]]

type FieldTypeFromTuple[Labels <: Tuple, Elems <: Tuple, L <: String] <: ValueType = (Labels, Elems) match
  case (L *: _, h *: _)    => h & ValueType
  case (_ *: lt, _ *: et)  => FieldTypeFromTuple[lt, et, L]
  case _                   => Nothing & ValueType

inline def indexOfLabel[Labels <: Tuple, L <: String & Singleton]: Int =
  inline erasedValue[Labels] match
    case _: (L *: t)      => 0
    case _: (_ *: tail)   =>
      val idx = indexOfLabel[tail, L]
      if idx < 0 then -1 else idx + 1
    case _: EmptyTuple    => -1

final case class Node[T <: ValueType](tpe: T, kind: NodeKind, name: Option[String] = None, literal: Option[Any] = None) extends Selectable:
  type Fields = NamedTuple.Map[NamedTuple.From[T], [X] =>> Node[X & ValueType]]

  transparent inline def selectDynamic[L <: String & Singleton](inline label: L) =
    summonFrom {
      case m: Mirror.ProductOf[T] =>
        type Labels = m.MirroredElemLabels
        type Elems = m.MirroredElemTypes
        type FT = FieldTypeFromTuple[Labels, Elems, L]
        val idx = indexOfLabel[Labels, L]
        if idx < 0 then throw new NoSuchElementException(s"${tpe.getClass.getName} has no field '${label}'")
        val childT = tpe.asInstanceOf[Product].productElement(idx).asInstanceOf[FT]
        val childLit = literal.map(_.asInstanceOf[Product].productElement(idx))
        Node(childT, kind, None, childLit)
      case _ =>
        throw new NoSuchElementException(s"${tpe.getClass.getName} has no field '${label}'")
    }

  def getValue: HostTypeOf[T] =
    literal match
      case Some(v) => v.asInstanceOf[HostTypeOf[T]]
      case None    => throw new NoSuchElementException("Node does not carry a literal value")

  override def toString(): String =
    val suffix = name.fold("")(n => s":$n")
    s"$kind(${tpe}$suffix)"

type HW[T <: ValueType] = Node[T]
type Lit[T <: ValueType] = Node[T]

object Reg:
  def apply[T <: ValueType](t: T, name: Option[String] = None): Node[T] = Node(t, NodeKind.Reg, name)

object Wire:
  def apply[T <: ValueType](t: T, name: Option[String] = None): Node[T] = Node(t, NodeKind.Wire, name)

object IO:
  def apply[T <: ValueType](t: T, name: Option[String] = None): Node[T] = Node(t, NodeKind.IO, name)

object PrimOp:
  def apply[T <: ValueType](t: T, name: Option[String] = None): Node[T] = Node(t, NodeKind.PrimOp, name)

object Lit:
  def apply[T <: ValueType](t: T, name: Option[String] = None)(payload: HostTypeOf[T]): Node[T] =
    Node(t, NodeKind.Lit, name, Some(payload))
