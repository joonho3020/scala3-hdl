package hdl

import scala.deriving.*
import scala.compiletime.*
import scala.NamedTuple
import scala.util.NotGiven

trait Bundle

object Bundle:
  def apply[T <: NamedTuple.AnyNamedTuple](fields: T): NamedTuple.From[T] =
    fields.asInstanceOf[NamedTuple.From[T]]

type HostTypeOf[T] = T match
  case UInt => Int
  case Bool => Boolean
  case Option[t] => Option[HostTypeOf[t]]
  case Seq[t] => Seq[HostTypeOf[t]]
  case _ => NamedTuple.Map[NamedTuple.From[T], [X] =>> HostTypeOf[X]]

type FieldTypeFromTuple[Labels <: Tuple, Elems <: Tuple, L <: String] = (Labels, Elems) match
  case (L *: _, h *: _) => h
  case (_ *: lt, _ *: et) => FieldTypeFromTuple[lt, et, L]
  case _ => Nothing

trait Shape[T]:
  def map(t: T, path: List[String])(f: (LeafValue, List[String]) => LeafValue): T
  def zip(t: T, host: HostTypeOf[T], path: List[String])(f: (LeafValue, Any, List[String]) => LeafValue): T
  def host(t: T): HostTypeOf[T]

object Shape:
  inline def summonAll[Elems <: Tuple]: List[Shape[?]] = inline erasedValue[Elems] match
    case _: EmptyTuple => Nil
    case _: (h *: t) => summonInline[Shape[h]] :: summonAll[t]

given Shape[UInt] with
  def map(t: UInt, path: List[String])(f: (LeafValue, List[String]) => LeafValue): UInt =
    f(t, path).asInstanceOf[UInt]
  def zip(t: UInt, host: HostTypeOf[UInt], path: List[String])(f: (LeafValue, Any, List[String]) => LeafValue): UInt =
    f(t, host, path).asInstanceOf[UInt]
  def host(t: UInt): HostTypeOf[UInt] =
    t.literal.getOrElse(throw new NoSuchElementException("Literal missing")).asInstanceOf[Int]

given Shape[Bool] with
  def map(t: Bool, path: List[String])(f: (LeafValue, List[String]) => LeafValue): Bool =
    f(t, path).asInstanceOf[Bool]
  def zip(t: Bool, host: HostTypeOf[Bool], path: List[String])(f: (LeafValue, Any, List[String]) => LeafValue): Bool =
    f(t, host, path).asInstanceOf[Bool]
  def host(t: Bool): HostTypeOf[Bool] =
    t.literal.getOrElse(throw new NoSuchElementException("Literal missing")).asInstanceOf[Boolean]

given [A](using sa: Shape[A]): Shape[Option[A]] with
  def map(t: Option[A], path: List[String])(f: (LeafValue, List[String]) => LeafValue): Option[A] =
    t.map(sa.map(_, path)(f))
  def zip(t: Option[A], host: HostTypeOf[Option[A]], path: List[String])(f: (LeafValue, Any, List[String]) => LeafValue): Option[A] =
    (t, host) match
      case (Some(tv), Some(hv)) => Some(sa.zip(tv, hv, path)(f))
      case _ => None
  def host(t: Option[A]): HostTypeOf[Option[A]] =
    t.map(sa.host)

given [A](using sa: Shape[A]): Shape[Seq[A]] with
  def map(t: Seq[A], path: List[String])(f: (LeafValue, List[String]) => LeafValue): Seq[A] =
    t.zipWithIndex.map { case (e, i) => sa.map(e, path :+ i.toString)(f) }
  def zip(t: Seq[A], host: HostTypeOf[Seq[A]], path: List[String])(f: (LeafValue, Any, List[String]) => LeafValue): Seq[A] =
    t.zip(host).zipWithIndex.map { case ((tv, hv), i) => sa.zip(tv, hv, path :+ i.toString)(f) }
  def host(t: Seq[A]): HostTypeOf[Seq[A]] =
    t.map(sa.host)

private inline def mapProductElems[Elems <: Tuple](p: Product, labels: Tuple, path: List[String], idx: Int)(f: (LeafValue, List[String]) => LeafValue): Tuple =
  inline erasedValue[Elems] match
    case _: EmptyTuple => EmptyTuple
    case _: (h *: t) =>
      val label = labels.productElement(idx).asInstanceOf[String]
      val elem = p.productElement(idx).asInstanceOf[h]
      val mapped = summonInline[Shape[h]].map(elem, path :+ label)(f)
      mapped *: mapProductElems[t](p, labels, path, idx + 1)(f)

private inline def zipProductElems[Elems <: Tuple](p: Product, h: Product, labels: Tuple, path: List[String], idx: Int)(f: (LeafValue, Any, List[String]) => LeafValue): Tuple =
  inline erasedValue[Elems] match
    case _: EmptyTuple => EmptyTuple
    case _: (h0 *: t0) =>
      val label = labels.productElement(idx).asInstanceOf[String]
      val elem = p.productElement(idx).asInstanceOf[h0]
      val hv = h.productElement(idx).asInstanceOf[HostTypeOf[h0]]
      val mapped = summonInline[Shape[h0]].zip(elem, hv, path :+ label)(f)
      mapped *: zipProductElems[t0](p, h, labels, path, idx + 1)(f)

private inline def hostProductElems[Elems <: Tuple](p: Product, idx: Int): Tuple =
  inline erasedValue[Elems] match
    case _: EmptyTuple => EmptyTuple
    case _: (h *: t) =>
      val elem = p.productElement(idx).asInstanceOf[h]
      val hv = summonInline[Shape[h]].host(elem)
      hv *: hostProductElems[t](p, idx + 1)

inline given productShape[T](using m: Mirror.ProductOf[T], notLeaf: NotGiven[T <:< LeafValue], notOpt: NotGiven[T <:< Option[?]], notSeq: NotGiven[T <:< Seq[?]]): Shape[T] =
  new Shape[T]:
    def map(t: T, path: List[String])(f: (LeafValue, List[String]) => LeafValue): T =
      val p = t.asInstanceOf[Product]
      val labels = constValueTuple[m.MirroredElemLabels]
      val mappedTuple = mapProductElems[m.MirroredElemTypes](p, labels, path, 0)(f)
      m.fromProduct(mappedTuple).asInstanceOf[T]

    def zip(t: T, host: HostTypeOf[T], path: List[String])(f: (LeafValue, Any, List[String]) => LeafValue): T =
      val p = t.asInstanceOf[Product]
      val h = host.asInstanceOf[Product]
      val labels = constValueTuple[m.MirroredElemLabels]
      val zippedTuple = zipProductElems[m.MirroredElemTypes](p, h, labels, path, 0)(f)
      m.fromProduct(zippedTuple).asInstanceOf[T]

    def host(t: T): HostTypeOf[T] =
      val p = t.asInstanceOf[Product]
      val hostTuple = hostProductElems[m.MirroredElemTypes](p, 0)
      hostTuple.asInstanceOf[HostTypeOf[T]]

def mapLeaves[T](t: T)(f: (LeafValue, List[String]) => LeafValue)(using s: Shape[T]): T =
  s.map(t, Nil)(f)

def zipLeaves[T](t: T, host: HostTypeOf[T])(f: (LeafValue, Any, List[String]) => LeafValue)(using s: Shape[T]): T =
  s.zip(t, host, Nil)(f)

object Reg:
  def apply[T](t: T)(using s: Shape[T]): T =
    s.map(t, Nil) { (lv, path) =>
      val newName = if path.isEmpty then None else Some(path.mkString("."))
      lv.duplicate(kind = LeafKinds.Reg, name = newName, literal = None)
    }

object Lit:
  def apply[T](t: T)(payload: HostTypeOf[T])(using s: Shape[T]): T =
    s.zip(t, payload, Nil) { (lv, hv, path) =>
      val newName = if path.isEmpty then None else Some(path.mkString("."))
      lv.duplicate(kind = LeafKinds.Lit, name = newName, literal = Some(hv))
    }

extension [T](t: T)(using s: Shape[T])
  def getValue: HostTypeOf[T] = s.host(t)
