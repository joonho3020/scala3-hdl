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

sealed trait HWData

sealed class UInt(val w: Width, val dir: Direction = Direction.Out) extends HWData:
  override def toString(): String = s"UInt($w.W, $dir)"

object UInt:
  def apply(w: Width): UInt = new UInt(w)

sealed class Bool(val dir: Direction = Direction.Out) extends HWData:
  override def toString(): String = s"Bool($dir)"

object Bool:
  def apply(): Bool = new Bool
  def apply(u: Unit): Bool = new Bool

trait Bundle extends HWData

// trait DirLike[T <: HWData]:
// def setAll(t: T, dir: Direction): T
// def flipAll(t: T): T

// object DirLike:
// inline def summonAll[Elems <: Tuple]: List[DirLike[? <: HWData]] =
// inline erasedValue[Elems] match
// case _: EmptyTuple => Nil
// case _: (h *: t) =>
// summonInline[DirLike[h & HWData]] :: summonAll[t]

// given DirLike[UInt] with
// def setAll(t: UInt, dir: Direction): UInt = new UInt(t.w, dir)
// def flipAll(t: UInt): UInt = new UInt(t.w, Direction.flip(t.dir))

// given DirLike[Bool] with
// def setAll(t: Bool, dir: Direction): Bool = new Bool(dir)
// def flipAll(t: Bool): Bool = new Bool(Direction.flip(t.dir))

// inline given [T <: Bundle](using m: Mirror.ProductOf[T]): DirLike[T] =
// new DirLike[T]:
// def setAll(t: T, dir: Direction): T =
// val p = t.asInstanceOf[Product]
// val typeclasses = DirLike.summonAll[m.MirroredElemTypes]
// val arr = new Array[Any](p.productArity)
// var i = 0
// while i < arr.length do
// val dl = typeclasses(i).asInstanceOf[DirLike[HWData]]
// val v = p.productElement(i).asInstanceOf[HWData]
// arr(i) = dl.setAll(v, dir)
// i += 1
// m.fromProduct(Tuple.fromArray(arr)).asInstanceOf[T]

// def flipAll(t: T): T =
// val p = t.asInstanceOf[Product]
// val typeclasses = DirLike.summonAll[m.MirroredElemTypes]
// val arr = new Array[Any](p.productArity)
// var i = 0
// while i < arr.length do
// val dl = typeclasses(i).asInstanceOf[DirLike[HWData]]
// val v = p.productElement(i).asInstanceOf[HWData]
// arr(i) = dl.flipAll(v)
// i += 1
// m.fromProduct(Tuple.fromArray(arr)).asInstanceOf[T]

// inline def Input[T <: HWData](t: T): T = summonInline[DirLike[T]].setAll(t, Direction.In)
// inline def Output[T <: HWData](t: T): T = summonInline[DirLike[T]].setAll(t, Direction.Out)
// inline def Flipped[T <: HWData](t: T): T = summonInline[DirLike[T]].flipAll(t)
