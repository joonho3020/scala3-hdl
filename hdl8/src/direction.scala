package hdl8

import scala.deriving.*
import scala.compiletime.*
import scala.NamedTuple
import scala.compiletime.ops.int as int


type Input = 1
type Output = -1

type Direction = Input | Output
type Flip[D <: Int] = int.*[D, -1]

final case class Dir[T <: ValueType, D <: Direction](value: T)

// ---- Type-level recursion to flip directions everywhere ----
type FlipAll[T] <: Any = T match
  // Directional leaf: flip the direction
  case Dir[t, d]     => Dir[t, Flip[d]]
  // Vectors: flip the element type recursively (length is a value)
  case Vec[e]        => Vec[FlipAll[e]]
  // Case-class ("Bundle") products: flip each field type
  case p => p // guard against tuples by defaulting
  case _ => FlipProduct[T]


type FlipProduct[T] <: Any =
  T match
    case p =>
      MapProduct[p, Mirror.ProductOf[p]]
    case _ => T

type MapTuple[Elems <: Tuple] <: Tuple = Elems match
  case EmptyTuple    => EmptyTuple
  case h *: t        => FlipAll[h] *: MapTuple[t]

// Given a product mirror, rebuild the product with flipped field types
type MapProduct[T, M] <: Any = M match
  case Mirror.ProductOf[T] { type MirroredElemTypes = elems } =>
    Tuple.InverseMap[MapTuple[elems], T] // reassemble into T with new element types

trait FlipDerive[T]:
  type Out
  def apply(x: T): Out

object FlipDerive:
  type Aux[T, O] = FlipDerive[T] { type Out = O }

  // Dir leaf: flip to the negated direction at the type level
  given flipDirLeaf[T <: ValueType, D <: Dir]: Aux[Dir[T, D], Dir[T, Flip[D]]] =
    new FlipDerive[Dir[T, D]]:
      type Out = Dir[T, Flip[D]]
      def apply(x: Dir[T, D]): Out = Dir[T, Flip[D]](x.value)

  // Vec: map element
  given flipVec[E <: ValueType](using fe: FlipDerive[E]): Aux[Vec[E], Vec[fe.Out]] =
    new FlipDerive[Vec[E]]:
      type Out = Vec[fe.Out]
      def apply(v: Vec[E]): Out = Vec(fe(v.elem), v.len)

  // Pass-through for non-directional leaves
  given flipPass[T <: ValueType](using NotGiven[T <:< Dir[?, ?]], NotGiven[T <:< Vec[?]]): Aux[T, T] =
    new FlipDerive[T]:
      type Out = T
      def apply(x: T): T = x

  // Products (Bundles): derive via Mirror
  inline given flipProduct[T](using m: Mirror.ProductOf[T]): Aux[T, MapProduct[T, m.type]] =
    new FlipDerive[T]:
      type Out = MapProduct[T, m.type]
      def apply(x: T): Out =
        val elems = Tuple.fromProduct(x)
        val flipped = mapElems(elems)
        m.fromProduct(flipped).asInstanceOf[Out]

  private inline def mapElems[Ts <: Tuple](ts: Ts): MapTuple[Ts] =
    inline ts match
      case EmptyTuple => EmptyTuple.asInstanceOf
      case h *: t =>
        val fh = summonInline[FlipDerive[h.type]]
        fh(h).asInstanceOf[FlipAll[h.type]] *: mapElems(t).asInstanceOf[MapTuple[t.type]]

// Public API
def flip[T](x: T)(using fd: FlipDerive[T]): fd.Out = fd(x)
