package hdl

import scala.deriving.*
import scala.compiletime.*
import scala.NamedTuple
import scala.util.NotGiven
import scala.quoted.*

enum Direction:
  case In, Out

object Direction:
  inline def flip(d: Direction): Direction = d match
    case Direction.In  => Direction.Out
    case Direction.Out => Direction.In

sealed class Width(val value: Int):
  override def toString: String = s"${value}"

object Width:
  def apply(x: Int): Width = new Width(x)

sealed trait HWData:
  def setNodeKind(kind: NodeKind): Unit
  def setLitVal(payload: Any): Unit
  def getLitVal: Any

sealed class UInt(
  val w: Width,
  var dir: Direction = Direction.Out,
  var kind: NodeKind = NodeKind.Unset,
  var literal: Option[HostTypeOf[UInt]] = None
) extends HWData:
  def setNodeKind(kind: NodeKind) = this.kind = kind

  def setLitVal(payload: Any): Unit =
    this.literal = Some(payload.asInstanceOf[HostTypeOf[UInt]])

  def getLitVal: HostTypeOf[UInt] =
    literal match
      case Some(v) => v.asInstanceOf[HostTypeOf[UInt]]
      case None    => throw new NoSuchElementException("UInt does not carry a literal value")

  override def toString(): String = s"UInt($w.W, $dir)"

object UInt:
  def apply(w: Width): UInt = new UInt(w)

sealed class Bool(
  var dir: Direction = Direction.Out,
  var kind: NodeKind = NodeKind.Unset,
  var literal: Option[HostTypeOf[Bool]] = None
) extends HWData:
  def setNodeKind(kind: NodeKind) = this.kind = kind

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

trait BundleIf

// NOTE: Literals assume that the BundleIf is pure. THat is, all fields are of HWData
// and there is no mixing with Scala's library types such as Option, Seq, List
class Bundle[T <: BundleIf](
  val tpe: T,
  var kind: NodeKind = NodeKind.Unset,
  var name: Option[String] = None,
  var literal: Option[Any] = None,
  private var _ref: String = ""
) extends Selectable with HWData:

  type FieldToNode[X] = X match
    case BundleIf => Bundle[X]
    case _           => X
// type FieldToNode[X] = X match
// case _           => X

  type Fields = NamedTuple.Map[NamedTuple.From[T], [X] =>> FieldToNode[X]]

  def setRef(ref: String) = _ref = ref
  def ref: String = _ref

  transparent inline def selectDynamic[L <: String & Singleton](label: L): Any =
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
        inline erasedValue[FT] match
          case _: BundleIf =>
            new Bundle(tpe = childT.asInstanceOf[FT & BundleIf], literal = childLit)
          case _: HWData =>
            childLit.map(lit => childT.asInstanceOf[FT & HWData].setLitVal(lit))
            childT
          case _ =>
            childT
      case _ =>
        throw new NoSuchElementException(s"${tpe.getClass.getName} has no field '${label}'")
    }

  def setNodeKind(kind: NodeKind) = this.kind = kind

  def setLitVal(payload: Any): Unit =
    this.literal = Some(payload.asInstanceOf[HostTypeOf[T]])

  def getLitVal: HostTypeOf[T] =
    literal match
      case Some(v) => v.asInstanceOf[HostTypeOf[T]]
      case None    => throw new NoSuchElementException("Node does not carry a literal value")

  override def toString(): String =
    val suffix = name.fold("")(n => s":$n")
    s"$kind(${tpe}$suffix)"

object Bundle:
  def apply[T <: BundleIf](
    tpe: T,
  ): Bundle[T] =
    new Bundle(tpe)

// object Node:
// def apply[T <: HWData](
// tpe: T,
// kind: NodeKind,
// name: Option[String] = None,
// literal: Option[Any] = None,
// ref: String = ""
// ): Node[T] =
// new Node(tpe, kind, name, literal, if ref.isEmpty then name.getOrElse("") else ref)


//  trait DirLike[T <: HWData]:
//    def setAll(t: T, dir: Direction): T
//    def flipAll(t: T): T
//  
//  object DirLike:
//    inline def summonAll[Elems <: Tuple]: List[DirLike[? <: HWData]] =
//      inline erasedValue[Elems] match
//        case _: EmptyTuple => Nil
//        case _: (h *: t) =>
//          summonInline[DirLike[h & HWData]] :: summonAll[t]
//  
//    given DirLike[UInt] with
//      def setAll(t: UInt, dir: Direction): UInt = new UInt(t.w, dir)
//      def flipAll(t: UInt): UInt = new UInt(t.w, Direction.flip(t.dir))
//  
//    given DirLike[Bool] with
//      def setAll(t: Bool, dir: Direction): Bool = new Bool(dir)
//      def flipAll(t: Bool): Bool = new Bool(Direction.flip(t.dir))
//  
//    inline given [T <: Bundle](using m: Mirror.ProductOf[T]): DirLike[T] =
//      new DirLike[T]:
//        def setAll(t: T, dir: Direction): T =
//          val p = t.asInstanceOf[Product]
//          val typeclasses = DirLike.summonAll[m.MirroredElemTypes]
//          val arr = new Array[Any](p.productArity)
//          var i = 0
//          while i < arr.length do
//            val dl = typeclasses(i).asInstanceOf[DirLike[HWData]]
//            val v = p.productElement(i).asInstanceOf[HWData]
//            arr(i) = dl.setAll(v, dir)
//            i += 1
//          m.fromProduct(Tuple.fromArray(arr)).asInstanceOf[T]
//  
//        def flipAll(t: T): T =
//          val p = t.asInstanceOf[Product]
//          val typeclasses = DirLike.summonAll[m.MirroredElemTypes]
//          val arr = new Array[Any](p.productArity)
//          var i = 0
//          while i < arr.length do
//            val dl = typeclasses(i).asInstanceOf[DirLike[HWData]]
//            val v = p.productElement(i).asInstanceOf[HWData]
//            arr(i) = dl.flipAll(v)
//            i += 1
//          m.fromProduct(Tuple.fromArray(arr)).asInstanceOf[T]
//  
//  inline def Input[T <: HWData](t: T): T = summonInline[DirLike[T]].setAll(t, Direction.In)
//  inline def Output[T <: HWData](t: T): T = summonInline[DirLike[T]].setAll(t, Direction.Out)
//  inline def Flipped[T <: HWData](t: T): T = summonInline[DirLike[T]].flipAll(t)
