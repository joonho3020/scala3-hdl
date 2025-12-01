package hdl

import scala.deriving.*
import scala.compiletime.*
import scala.NamedTuple
import scala.util.NotGiven
import scala.quoted.*

final class Wire[T](val t: T, val name: String = "") extends Selectable:
  type Fields = NamedTuple.Map[
    NamedTuple.From[T],
    [X] =>> Wire[X & ValueType]]

  inline def selectDynamic(fieldName: String): Wire[?] =
    summonFrom {
      case m: Mirror.ProductOf[T] =>
        val labels = constValueTuple[m.MirroredElemLabels].toArray
        val idx = labels.indexOf(fieldName)
        val child = t.asInstanceOf[Product].productElement(idx).asInstanceOf[ValueType]
        val childName = if name.isEmpty then fieldName else s"${name}_$fieldName"
        new Wire(child, childName)
      case _ =>
        throw new NoSuchElementException(s"${t.getClass.getName} has no field '$fieldName'")
    }
  override def toString(): String =
    s"Wire($t, $name)"

object Wire:
  def apply[T <: ValueType](t: T): Wire[T] = new Wire(t)

object WireVecOps:
  extension [A <: ValueType](wv: Wire[Vec[A]])
    def apply(index: Int): Wire[A] =
      val childName = if wv.name.isEmpty then s"$index" else s"${wv.name}_$index"
      new Wire(wv.t.elem, childName)

    def apply(start: Int, end: Int): Wire[Vec[A]] =
      val sliceLen = (end - start) + 1
      val childName = if wv.name.isEmpty then s"${start}_$end" else s"${wv.name}_${start}_$end"
      new Wire(Vec(wv.t.elem, sliceLen), childName)

final class Reg[T](val t: T, val name: String = "") extends Selectable:
  type Fields = NamedTuple.Map[
    NamedTuple.From[T],
    [X] =>> Reg[X & ValueType]]

  inline def selectDynamic(fieldName: String): Reg[?] =
    summonFrom {
      case m: Mirror.ProductOf[T] =>
        val labels = constValueTuple[m.MirroredElemLabels].toArray
        val idx = labels.indexOf(fieldName)
        val child = t.asInstanceOf[Product].productElement(idx).asInstanceOf[ValueType]
        val childName = if name.isEmpty then fieldName else s"${name}_$fieldName"
        new Reg(child, childName)
      case _ =>
        throw new NoSuchElementException(s"${t.getClass.getName} has no field '$fieldName'")
    }
  override def toString(): String =
    s"Reg($t, $name)"

object Reg:
  def apply[T <: ValueType](t: T): Reg[T] = new Reg(t)

object RegVecOps:
  extension [A <: ValueType](rv: Reg[Vec[A]])
    def apply(index: Int): Reg[A] =
      val childName = if rv.name.isEmpty then s"$index" else s"${rv.name}_$index"
      new Reg(rv.t.elem, childName)

    def apply(start: Int, end: Int): Reg[Vec[A]] =
      val sliceLen = (end - start) + 1
      val childName = if rv.name.isEmpty then s"${start}_$end" else s"${rv.name}_${start}_$end"
      new Reg(Vec(rv.t.elem, sliceLen), childName)

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

final class IO[T](val t: T, val name: String = "") extends Selectable:
  type Fields = NamedTuple.Map[
    NamedTuple.From[T],
    [X] =>> IO[X & ValueType]]

  inline def selectDynamic(fieldName: String): IO[?] =
    summonFrom {
      case m: Mirror.ProductOf[T] =>
        val labels = constValueTuple[m.MirroredElemLabels].toArray
        val idx = labels.indexOf(fieldName)
        val child = t.asInstanceOf[Product].productElement(idx).asInstanceOf[ValueType]
        val childName = if name.isEmpty then fieldName else s"${name}_$fieldName"
        new IO(child, childName)
      case _ =>
        throw new NoSuchElementException(s"${t.getClass.getName} has no field '$fieldName'")
    }
  override def toString(): String =
    s"IO($t, $name)"

object IO:
  def apply[T <: ValueType](t: T): IO[T] = new IO(t)
  def apply[T <: ValueType](t: T, name: String): IO[T] = new IO(t, name)

object IOVecOps:
  extension [A <: ValueType](iov: IO[Vec[A]])
    def apply(index: Int): IO[A] =
      val childName = if iov.name.isEmpty then s"$index" else s"${iov.name}_$index"
      new IO(iov.t.elem, childName)

    def apply(start: Int, end: Int): IO[Vec[A]] =
      val sliceLen = (end - start) + 1
      val childName = if iov.name.isEmpty then s"${start}_$end" else s"${iov.name}_${start}_$end"
      new IO(Vec(iov.t.elem, sliceLen), childName)
