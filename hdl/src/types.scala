package hdl

import scala.deriving.*
import scala.compiletime.*
import scala.NamedTuple
import scala.language.dynamics

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

final case class Node[T <: ValueType](
  tpe: T,
  kind: NodeKind,
  name: Option[String] = None,
  literal: Option[Any] = None,
  private var _ref: String = ""
) extends Selectable:
  type Fields = NamedTuple.Map[NamedTuple.From[T], [X] =>> Node[X & ValueType]]

  def setRef(ref: String) = _ref = ref
  def ref: String = _ref

  transparent inline def selectDynamic[L <: String & Singleton](label: L) =
    summonFrom {
      case m: Mirror.ProductOf[T] =>
        type Labels = m.MirroredElemLabels
        type Elems = m.MirroredElemTypes
        type FT = FieldTypeFromTuple[Labels, Elems, L]

        val labels = constValueTuple[Labels].toArray
        val idx = labels.indexOf(constValue[L])

        if idx < 0 then throw new NoSuchElementException(s"${tpe.getClass.getName} has no field '${label}'")
        val childT = tpe.asInstanceOf[Product].productElement(idx).asInstanceOf[FT]
        val childLit = literal.map(_.asInstanceOf[Product].productElement(idx))
        val childRef = if _ref.isEmpty then constValue[L] else s"$_ref.${constValue[L]}"
        Node(childT, kind, Some(constValue[L]), childLit, childRef)
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

object Node:
  def apply[T <: ValueType](
    tpe: T,
    kind: NodeKind,
    name: Option[String] = None,
    literal: Option[Any] = None,
    ref: String = ""
  ): Node[T] =
    new Node(tpe, kind, name, literal, if ref.isEmpty then name.getOrElse("") else ref)

object Reg:
  def apply[T <: ValueType](t: T, name: Option[String] = None): Node[T] =
    Node(t, NodeKind.Reg, name, None, name.getOrElse(""))

object Wire:
  def apply[T <: ValueType](t: T, name: Option[String] = None): Node[T] =
    Node(t, NodeKind.Wire, name, None, name.getOrElse(""))

object IO:
  def apply[T <: ValueType](t: T, name: Option[String] = None): Node[T] =
    Node(t, NodeKind.IO, name, None, name.getOrElse(""))

object PrimOp:
  def apply[T <: ValueType](t: T, name: Option[String] = None): Node[T] =
    Node(t, NodeKind.PrimOp, name, None, name.getOrElse(""))

object Lit:
  def apply[T <: ValueType](t: T, name: Option[String] = None)(payload: HostTypeOf[T]): Node[T] =
    Node(t, NodeKind.Lit, name, Some(payload), name.getOrElse(""))
