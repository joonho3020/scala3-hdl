package hdl

import scala.deriving.*
import scala.compiletime.*
import scala.NamedTuple
import scala.reflect.ClassTag
import scala.language.implicitConversions
import scala.util.NotGiven
import scala.quoted.*
import scala.reflect.Selectable.reflectiveSelectable

enum NodeKind:
  case Reg, Wire, IO, PrimOp, Lit, Unset

enum Direction:
  case Default, Flipped

object Direction:
  inline def In: Direction = Direction.Flipped
  inline def Out: Direction = Direction.Default
  inline def flip(d: Direction): Direction = d match
    case Direction.Default => Direction.Flipped
    case Direction.Flipped => Direction.Default

sealed trait HWData extends Cloneable:
  var dir: Direction = Direction.Default
  def flip: Unit =
    this.dir = Direction.flip(this.dir)

  var literal: Option[Any] = None
  var kind: NodeKind = NodeKind.Unset
  var ref: Option[IR.Identifier] = None
  var owner: Option[Module] = None
  var irExpr: Option[IR.Expr] = None
  var width: Width = Width()

  def setNodeKind(kind: NodeKind) = this.kind = kind
  def setLitVal(payload: Any): Unit
  def getLitVal: Any
  def setRef(r: IR.Identifier): Unit = ref = Some(r)
  def getRef: Option[IR.Identifier] = ref
  def setOwner(m: Module): Unit = owner = Some(m)
  def getOwner: Option[Module] = owner
  def setIRExpr(expr: IR.Expr): Unit = irExpr = Some(expr)
  def getIRExpr: Option[IR.Expr] = irExpr
  def getWidth: Width = width
  def setWidth(w: Width): Unit = width = w
  override protected def clone(): this.type = super.clone().asInstanceOf[this.type]

  def cloneType: this.type =
    val ret: this.type = this.clone()
    ret.literal = None
    ret.kind = NodeKind.Unset
    ret.ref = None
    ret.owner = None
    ret.irExpr = None
    ret

sealed class UInt extends HWData:
  def setLitVal(payload: Any): Unit =
    this.literal = Some(payload.asInstanceOf[HostTypeOf[UInt]])

  def getLitVal: HostTypeOf[UInt] =
    this.literal match
      case Some(v) => v.asInstanceOf[HostTypeOf[UInt]]
      case None    => throw new NoSuchElementException("UInt does not carry a literal value")

  override def toString(): String = s"UInt($getWidth, $dir)"

object UInt:
  def apply(w: Width): UInt =
    var ret = new UInt
    ret.setWidth(w)
    ret

  def apply(): UInt =
    new UInt

sealed class Bool extends HWData:
  def setLitVal(payload: Any): Unit =
    this.literal = Some(payload.asInstanceOf[HostTypeOf[Bool]])

  def getLitVal: HostTypeOf[Bool] =
    literal match
      case Some(v) => v.asInstanceOf[HostTypeOf[Bool]]
      case None    => throw new NoSuchElementException("Bool does not carry a literal value")

  this.width = Width(1)

  override def toString(): String = s"Bool($dir)"

object Bool:
  def apply(): Bool = new Bool
  def apply(u: Unit): Bool = new Bool

sealed class Clock extends HWData:
  def setLitVal(payload: Any): Unit =
    this.literal = Some(payload.asInstanceOf[HostTypeOf[Clock]])

  def getLitVal: HostTypeOf[Clock] =
    literal match
      case Some(v) => v.asInstanceOf[HostTypeOf[Clock]]
      case None    => throw new NoSuchElementException("Clock does not carry a literal value")

  this.width = Width(1)

  override def toString(): String = s"Clock($dir)"

object Clock:
  def apply(): Clock = new Clock

sealed class Reset extends HWData:
  def setLitVal(payload: Any): Unit =
    this.literal = Some(payload.asInstanceOf[HostTypeOf[Reset]])

  def getLitVal: HostTypeOf[Reset] =
    literal match
      case Some(v) => v.asInstanceOf[HostTypeOf[Reset]]
      case None    => throw new NoSuchElementException("Reset does not carry a literal value")

  this.width = Width(1)

  override def toString(): String = s"Reset($dir)"

object Reset:
  def apply(): Reset = new Reset

sealed class OneHot extends HWData:
  def setLitVal(payload: Any): Unit =
    this.literal = Some(payload.asInstanceOf[HostTypeOf[OneHot]])

  def getLitVal: HostTypeOf[OneHot] =
    this.literal match
      case Some(v) => v.asInstanceOf[HostTypeOf[OneHot]]
      case None    => throw new NoSuchElementException("OneHot does not carry a literal value")

  override def toString(): String = s"OneHot($getWidth, $dir)"

object OneHot:
  def apply(w: Width): OneHot =
    var ret = new OneHot()
    ret.setWidth(w)
    ret

  def apply(): OneHot =
    OneHot(Width())

class HWEnum[E <: scala.reflect.Enum](
  val enumObj: { def values: Array[E] }
) extends HWData:
  def setLitVal(payload: Any): Unit = literal = Some(payload.asInstanceOf[E])
  def getLitVal: E =
    literal match
      case Some(v) => v.asInstanceOf[E]
      case None => throw new NoSuchElementException("Enum does not carry a literal value")
  override def cloneType: this.type = new HWEnum[E](enumObj).asInstanceOf[this.type]
  this.width = Width(log2Ceil(math.max(1, enumObj.values.length)))

extension [E <: scala.reflect.Enum](payload: E)
  inline def toHWEnum: HWEnum[E] =
    ${ HWEnumMacros.toHWEnumImpl[E]('payload) }

object HWEnumMacros:
  def toHWEnumImpl[E <: scala.reflect.Enum: Type](value: Expr[E])(using Quotes): Expr[HWEnum[E]] =
    import quotes.reflect.*

    // If the receiver is a singleton case type (e.g. Color.Red.type),
    // widen to the enum type (e.g. Color) before grabbing the companion.
    val enumTpe  = value.asTerm.tpe.dealias.widen
    val enumSym  = enumTpe.typeSymbol
    val comp     = enumSym.companionModule

    if comp == Symbol.noSymbol then
      report.errorAndAbort(
        s"Can't find enum companion for ${enumTpe.show}. " +
        s"Make sure the receiver is typed as the enum itself."
      )

    val enumObj = Ref(comp).asExprOf[{ def values: Array[E] }]

    '{
      val e = new HWEnum[E]($enumObj)
      e.setNodeKind(NodeKind.Lit)
      e.setLitVal($value)
      e
    }

object DontCare extends HWData:
  def setLitVal(payload: Any): Unit = ()
  def getLitVal: Any =
    throw new NoSuchElementException("DontCare does not carry a literal value")
  override def toString(): String = "DontCare"

sealed class Vec[T <: HWData](val elems: Seq[T]) extends HWData with IterableOnce[T]:
  def iterator: Iterator[T] = elems.iterator

  def length: Int = elems.length

  def apply(i: Int): T = elems(i)

  def apply(i: UInt)(using m: Module): T =
    val proto = elems.headOption.getOrElse(
      throw new IllegalArgumentException("Cannot index empty Vec"))
    val baseExpr = ModuleOps.exprFor(this, m)
    val idxExpr = ModuleOps.exprFor(i, m)
    val accessExpr = IR.SubAccess(baseExpr, idxExpr)
    HWAggregate.rebind(proto, accessExpr)

  override def flip: Unit =
    dir = Direction.flip(dir)
    elems.foreach(_.flip)

  def setLitVal(payload: Any): Unit =
    HWLiteral.set(this, payload)

  def getLitVal: HostTypeOf[Vec[T]] =
    literal match
      case Some(v) => v.asInstanceOf[HostTypeOf[Vec[T]]]
      case None    => throw new NoSuchElementException("Vec does not carry a literal value")

  override def toString(): String = s"Vec(${elems.mkString(",")}, $dir)"

  def map[U <: HWData](f: T => U): Vec[U] =
    Vec(elems.map(f))

  def flatMap[U <: HWData](f: T => IterableOnce[U]): Vec[U] =
    Vec(elems.iterator.flatMap(f).toSeq)

  def filter(p: T => Boolean): Vec[T] =
    Vec(elems.filter(p))

  def reduce(op: (T, T) => T): T =
    elems.reduce(op)

  def reduceOption(op: (T, T) => T): Option[T] =
    elems.reduceOption(op)

  def reverse: Vec[T] =
    Vec(elems.reverse)

  export elems.{ foreach, foldLeft, foldRight, exists, forall, count, mkString, zip, zipWithIndex }

  this.width = if elems.nonEmpty then elems.map(_.getWidth).reduce(_ + _) else Width()

object Vec:
  def apply[T <: HWData](elems: Seq[T]): Vec[T] = new Vec[T](elems)
  def tabulate[T <: HWData](n: Int)(gen: Int => T): Vec[T] = Vec(Seq.tabulate(n)(gen))
  def fill[T <: HWData](n: Int)(gen: => T): Vec[T] = Vec(Seq.fill(n)(gen))

type HostTypeOf[T] = T match
  case UInt   => BigInt
  case Bool   => Boolean
  case Clock  => Boolean
  case Reset  => Boolean
  case OneHot => BigInt
  case Vec[t] => Seq[HostTypeOf[t]]
  case HWEnum[t] => t
  case _ =>
    NamedTuple.Map[NamedTuple.From[T], [X] =>> HostTypeOf[X]]

extension (x: Int)
  def U: UInt =
    val u = UInt()
    u.setNodeKind(NodeKind.Lit)
    u.setLitVal(BigInt(x))
    u

  def U(width: Width): UInt =
    val u = UInt(width)
    u.setNodeKind(NodeKind.Lit)
    u.setLitVal(BigInt(x))
    u

extension (x: Boolean)
  def B: Bool =
    val b = Bool()
    b.setNodeKind(NodeKind.Lit)
    b.setLitVal(x)
    b

type FieldTypeFromTuple[Labels <: Tuple, Elems <: Tuple, L <: String] = (Labels, Elems) match
  case (L *: _, h *: _)    => h
  case (_ *: lt, _ *: et)  => FieldTypeFromTuple[lt, et, L]
  case _                   => Nothing

// NOTE: Literals assume that the BundleIf is pure. That is, all fields are of HWData
// and there is no mixing with Scala's library types such as Option, Seq, List
trait Bundle[T] extends Selectable with HWData { self: T =>
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
        inline erasedValue[FT] match
          case _: HWData =>
            childLit.map(lit => childT.asInstanceOf[FT & HWData].setLitVal(lit))
            childT.asInstanceOf[FT & HWData].getOwner.orElse(this.getOwner).foreach(childT.asInstanceOf[FT & HWData].setOwner)
            this.getRef.foreach { prefix =>
              childT.asInstanceOf[FT & HWData].setRef(IR.Identifier(s"${prefix.value}.${constValue[L]}"))
            }
            childT
          case _ =>
            childT
      case _ =>
        throw new NoSuchElementException(s"${self.getClass.getName} has no field '${label}'")
    }

  def setLitVal(payload: Any): Unit =
    HWLiteral.set(this, payload)
    this.literal = Some(payload.asInstanceOf[HostTypeOf[T]])

  def getLitVal: HostTypeOf[T] =
    literal match
      case Some(v) => v.asInstanceOf[HostTypeOf[T]]
      case None    => throw new NoSuchElementException("Node does not carry a literal value")
}

object Input:
  def apply[T <: HWData](t: T): T =
    t.flip
    t

object Output:
  def apply[T <: HWData](t: T): T =
    t

object Flipped:
  def apply[T <: HWData](t: T): T =
    Input(t)

private[hdl] object HWLiteral:
  def set(data: Any, value: Any): Unit =
    (data, value) match
      case (u: UInt, v: BigInt) => u.setLitVal(v)
      case (b: Bool, v: Boolean) => b.setLitVal(v)
      case (c: Clock, v: Boolean) => c.setLitVal(v)
      case (r: Reset, v: Boolean) => r.setLitVal(v)
      case (e: HWEnum[?], v: HWEnum[?]) =>
        e.setLitVal(v)
      case (v: Vec[?], seq: Seq[?]) =>
        v.elems.zip(seq).foreach { case (e, v2) => set(e, v2) }
        v.literal = Some(seq)
      case (b: Bundle[?], p: Product) =>
        val bp = b.asInstanceOf[Product]
        val arity = bp.productArity
        assert(arity == p.productArity)
        var i = 0
        while i < arity do
          set(bp.productElement(i), p.productElement(i))
          i += 1
        b.literal = Some(value)

      // Option
      case (Some(hd), Some(vv)) => set(hd, vv)
      case (None, None) => ()

      // Iterable
      case (it: Iterable[?], iv: Iterable[?]) =>
        assert(it.iterator.length == iv.iterator.length)
        it.iterator.zip(iv.iterator).foreach { case (d, v) => set(d, v) }

      // Unknown
      case _ =>
        throw new IllegalStateException("Option shape mismatch")
