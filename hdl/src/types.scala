package hdl

import scala.deriving.*
import scala.compiletime.*
import scala.NamedTuple
import scala.util.NotGiven
import scala.quoted.*

trait Connectable:
  def refName: String
  def toExpr: ExprIR = ExprIR.Ref(refName)

trait TypedConnectable[T] extends Connectable:
  def innerType: T
  override def toExpr: ExprIR = ExprIR.Ref(refName)

trait Node[T <: ValueType] extends TypedConnectable[T]

final class Wire[T <: ValueType](val t: T, val name: String = "") extends Selectable with Node[T]:
  type Fields = NamedTuple.Map[
    NamedTuple.From[T],
    [X] =>> Wire[X & ValueType]]

  def innerType: T = t
  def refName: String = name

  inline def selectDynamic(fieldName: String): Wire[?] =
    summonFrom {
      case m: Mirror.ProductOf[T] =>
        val labels = constValueTuple[m.MirroredElemLabels].toArray
        val idx = labels.indexOf(fieldName)
        val child = t.asInstanceOf[Product].productElement(idx).asInstanceOf[ValueType]
        val childName = if name.isEmpty then fieldName else s"${name}.$fieldName"
        new Wire(child, childName)
      case _ =>
        throw new NoSuchElementException(s"${t.getClass.getName} has no field '$fieldName'")
    }
  override def toString(): String =
    s"Wire($t, $name)"

object WireVecOps:
  extension [A <: ValueType](wv: Wire[Vec[A]])
    def apply(index: Int): Wire[A] =
      val childName = if wv.name.isEmpty then s"($index)" else s"${wv.name}($index)"
      new Wire(wv.t.elem, childName)

    def apply(start: Int, end: Int): Wire[Vec[A]] =
      val sliceLen = (end - start) + 1
      val childName = if wv.name.isEmpty then s"(${start}, $end)" else s"${wv.name}(${start}, $end)"
      new Wire(Vec(wv.t.elem, sliceLen), childName)

final class Reg[T <: ValueType](val t: T, val name: String = "") extends Selectable with Node[T]:
  type Fields = NamedTuple.Map[
    NamedTuple.From[T],
    [X] =>> Reg[X & ValueType]]

  def innerType: T = t
  def refName: String = name

  inline def selectDynamic(fieldName: String): Reg[?] =
    summonFrom {
      case m: Mirror.ProductOf[T] =>
        val labels = constValueTuple[m.MirroredElemLabels].toArray
        val idx = labels.indexOf(fieldName)
        val child = t.asInstanceOf[Product].productElement(idx).asInstanceOf[ValueType]
        val childName = if name.isEmpty then fieldName else s"${name}.$fieldName"
        new Reg(child, childName)
      case _ =>
        throw new NoSuchElementException(s"${t.getClass.getName} has no field '$fieldName'")
    }
  override def toString(): String =
    s"Reg($t, $name)"

object RegVecOps:
  extension [A <: ValueType](rv: Reg[Vec[A]])
    def apply(index: Int): Reg[A] =
      val childName = if rv.name.isEmpty then s"($index)" else s"${rv.name}($index)"
      new Reg(rv.t.elem, childName)

    def apply(start: Int, end: Int): Reg[Vec[A]] =
      val sliceLen = (end - start) + 1
      val childName = if rv.name.isEmpty then s"(${start}, $end)" else s"${rv.name}(${start}, $end)"
      new Reg(Vec(rv.t.elem, sliceLen), childName)

type HostTypeOf[T] = T match
  case UInt    => BigInt
  case Bool    => Boolean
  case Vec[t]  => Seq[HostTypeOf[t & ValueType]]
  case _       => NamedTuple.Map[NamedTuple.From[T], [X] =>> HostTypeOf[X & ValueType]]

final class Lit[T](private val payload: Any) extends Selectable with TypedConnectable[T]:
  type Fields = NamedTuple.Map[
    NamedTuple.From[T],
    [X] =>> Lit[X & ValueType]
  ]

  // Note: We can't expose innerType for Lit since we don't store the type parameter
  // For Lit, we'll need to use a different approach
  def innerType: T = throw new UnsupportedOperationException("Lit does not store type information at runtime")
  def refName: String = ""
  override def toExpr: ExprIR =
    ExprIR.Lit(payload)

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
  inline def apply[T <: ValueType](inline v: HostTypeOf[T]): Lit[T] =
    new Lit[T](v)

final class IO[T <: ValueType](val t: T, private var _name: String = "") extends Selectable with Node[T]:
  type Fields = NamedTuple.Map[
    NamedTuple.From[T],
    [X] =>> IO[X & ValueType]]

  def innerType: T = t
  def name: String = _name
  private[hdl] def setName(n: String): Unit = _name = n
  def refName: String = _name

  inline def selectDynamic(fieldName: String): IO[?] =
    summonFrom {
      case m: Mirror.ProductOf[T] =>
        val labels = constValueTuple[m.MirroredElemLabels].toArray
        val idx = labels.indexOf(fieldName)
        val child = t.asInstanceOf[Product].productElement(idx).asInstanceOf[ValueType]
        val childName = if _name.isEmpty then fieldName else s"${_name}.$fieldName"
        new IO(child, childName)
      case _ =>
        throw new NoSuchElementException(s"${t.getClass.getName} has no field '$fieldName'")
    }
  override def toString(): String =
    s"IO($t, $_name)"

object IO:
  def apply[T <: ValueType](t: T): IO[T] = new IO(t)
  def apply[T <: ValueType](t: T, name: String): IO[T] = new IO(t, name)

object IOVecOps:
  extension [A <: ValueType](iov: IO[Vec[A]])
    def apply(index: Int): IO[A] =
      val childName = if iov.name.isEmpty then s"($index)" else s"${iov.name}($index)"
      new IO(iov.t.elem, childName)

    def apply(start: Int, end: Int): IO[Vec[A]] =
      val sliceLen = (end - start) + 1
      val childName = if iov.name.isEmpty then s"(${start}, $end)" else s"${iov.name}(${start}, $end)"
      new IO(Vec(iov.t.elem, sliceLen), childName)

trait TypeCompatible[LEFT, RIGHT]

object TypeCompatible:
  given [T]: TypeCompatible[T, T] = new TypeCompatible[T, T] {}

final class OpNode[T <: ValueType](val t: T, val expr: ExprIR) extends Node[T]:
  def innerType: T = t
  def refName: String = ""
  override def toExpr: ExprIR = expr

object Operations:
  private def widthOf(tc: Node[UInt]): Int =
    tc.innerType.w.value

  extension (lhs: Node[UInt])
    infix def +(rhs: Node[UInt]): Node[UInt] =
      val lw = widthOf(lhs)
      val rw = widthOf(rhs)
      val outW = Width(math.max(lw, rw) + 1)
      OpNode(UInt(outW), ExprIR.PrimOp("add", Seq(lhs.toExpr, rhs.toExpr)))

  extension (xs: Seq[Node[UInt]])
    def sumAll: Node[UInt] =
      xs.reduce(_ + _)

object ConnectOps:
  extension [L <: ValueType](lhs: TypedConnectable[L])
    def :=[R <: ValueType](rhs: TypedConnectable[R])(using ctx: ElabContext, ev: TypeCompatible[L, R]): Unit =
      ctx.emit(StmtIR.Connect(lhs.toExpr, rhs.toExpr))

    def :=[R <: ValueType](rhs: Lit[R])(using ctx: ElabContext, ev: TypeCompatible[L, R]): Unit =
      ctx.emit(StmtIR.Connect(lhs.toExpr, rhs.toExpr))
