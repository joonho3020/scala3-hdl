package hdl

import scala.deriving.*
import scala.compiletime.*
import scala.NamedTuple
import scala.util.NotGiven
import scala.quoted.*

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

sealed class Width(val value: Int):
  override def toString: String = s"${value}"

object Width:
  def apply(x: Int): Width = new Width(x)

extension (n: Int)
  def W: Width =
    Width(n)

sealed trait HWData:
  var dir: Direction = Direction.Default
  def flip: Unit =
    this.dir = Direction.flip(this.dir)

  var literal: Option[Any] = None
  var kind: NodeKind = NodeKind.Unset
  var ref: Option[String] = None
  private var owner: Option[Module] = None
  private var irExpr: Option[IR.Expr] = None

  def setNodeKind(kind: NodeKind) = this.kind = kind
  def setLitVal(payload: Any): Unit
  def getLitVal: Any
  def setRef(r: String): Unit = ref = Some(r)
  def getRef: Option[String] = ref
  def setOwner(m: Module): Unit = owner = Some(m)
  def getOwner: Option[Module] = owner
  def setIRExpr(expr: IR.Expr): Unit = irExpr = Some(expr)
  def clearIRExpr(): Unit = irExpr = None
  def getIRExpr: Option[IR.Expr] = irExpr

sealed class UInt(
  val w: Width
) extends HWData:

  def setLitVal(payload: Any): Unit =
    this.literal = Some(payload.asInstanceOf[HostTypeOf[UInt]])

  def getLitVal: HostTypeOf[UInt] =
    this.literal match
      case Some(v) => v.asInstanceOf[HostTypeOf[UInt]]
      case None    => throw new NoSuchElementException("UInt does not carry a literal value")

  override def toString(): String = s"UInt($w.W, $dir)"

object UInt:
  def apply(w: Width): UInt = new UInt(w)

sealed class Bool extends HWData:
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

sealed class Vec[T <: HWData](val elems: Seq[T]) extends HWData with IterableOnce[T]:
  def iterator: Iterator[T] = elems.iterator
  def length: Int = elems.length
  def apply(i: Int): T = elems(i)
  def apply(i: UInt)(using m: Module): T =
    val proto = elems.headOption.getOrElse(throw new IllegalArgumentException("Cannot index empty Vec"))
    val baseExpr = ModuleOps.exprFor(this, m)
    val idxExpr = ModuleOps.exprFor(i, m)
    val accessExpr = IR.SubAccess(baseExpr, idxExpr)
    HWDataRebinder.rebind(proto, accessExpr)
  override def flip: Unit =
    dir = Direction.flip(dir)
    elems.foreach(_.flip)
  def setLitVal(payload: Any): Unit =
    val seq = payload.asInstanceOf[Seq[HostTypeOf[T]]]
    elems.zip(seq).foreach((e, v) => e.setLitVal(v))
    literal = Some(seq)
  def getLitVal: HostTypeOf[Vec[T]] =
    literal match
      case Some(v) => v.asInstanceOf[HostTypeOf[Vec[T]]]
      case None    => throw new NoSuchElementException("Vec does not carry a literal value")
  override def toString(): String = s"Vec(${elems.mkString(",")}, $dir)"

object Vec:
  def apply[T <: HWData](elems: Seq[T]): Vec[T] = new Vec[T](elems)
  def tabulate[T <: HWData](n: Int)(gen: Int => T): Vec[T] = Vec(Seq.tabulate(n)(gen))
  def fill[T <: HWData](n: Int)(gen: => T): Vec[T] = Vec(Seq.fill(n)(gen))

type HostTypeOf[T] = T match
  case UInt  => BigInt
  case Bool  => Boolean
  case Vec[t] => Seq[HostTypeOf[t]]
  case _     => NamedTuple.Map[NamedTuple.From[T], [X] =>> HostTypeOf[X]]

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
              childT.asInstanceOf[FT & HWData].setRef(s"$prefix.${constValue[L]}")
            }
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

private object HWDataRebinder:
  private def copyCommon(from: HWData, to: HWData): Unit =
    to.dir = from.dir
    to.kind = from.kind
    to.literal = from.literal
    from.getOwner.foreach(to.setOwner)

  def rebind[T <: HWData](template: T, expr: IR.Expr): T =
    val rebound = template match
      case u: UInt =>
        val n = UInt(u.w)
        copyCommon(u, n)
        n.setIRExpr(expr)
        n.asInstanceOf[T]
      case b: Bool =>
        val n = Bool()
        copyCommon(b, n)
        n.setIRExpr(expr)
        n.asInstanceOf[T]
      case v: Vec[t] =>
        val elems = v.elems.zipWithIndex.map { case (e, idx) =>
          rebind(e, IR.SubIndex(expr, idx)).asInstanceOf[t]
        }
        val nv = Vec(elems)
        copyCommon(v, nv)
        nv.setIRExpr(expr)
        nv.asInstanceOf[T]
      case b: Bundle[bt] =>
        val prod = b.asInstanceOf[Product]
        val arity = prod.productArity
        val values = Array.ofDim[Any](arity)
        var i = 0
        while i < arity do
          val fieldName = prod.productElementName(i)
          val value = prod.productElement(i) match
            case h: HWData =>
              rebind(h, IR.SubField(expr, fieldName))
            case other => other
          values(i) = value
          i += 1
        val cls = b.getClass
        val ctor = cls.getConstructors.find(_.getParameterCount == values.length)
        val args = values.map(_.asInstanceOf[AnyRef])
        val rebuilt = ctor
          .map(_.newInstance(args*))
          .getOrElse {
            val companionCls = Class.forName(s"${cls.getName}$$")
            val module = companionCls.getField("MODULE$").get(null)
            val apply = module.getClass.getMethods.find(m => m.getName == "apply" && m.getParameterCount == values.length)
              .getOrElse(throw new IllegalArgumentException(s"No apply method for ${cls.getName}"))
            apply.invoke(module, args*)
          }
        val nb = rebuilt.asInstanceOf[Bundle[bt]]
        copyCommon(b, nb)
        nb.setIRExpr(expr)
        nb.asInstanceOf[T]
    rebound

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
