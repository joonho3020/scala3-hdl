package hdl

import scala.NamedTuple
import scala.language.dynamics

type HostTypeOf[T] = T match
  case UInt    => BigInt
  case Bool    => Boolean
  case Vec[t]  => Seq[HostTypeOf[t & ValueType]]
  case _       => NamedTuple.Map[NamedTuple.From[T], [X] =>> HostTypeOf[X & ValueType]]

enum NodeKind:
  case Wire, Reg, IO, Op, Lit

sealed trait Node[+T <: ValueType] extends Selectable, scala.Dynamic:
  def t: T
  def expr: ExprIR
  def kind: NodeKind
  def refName: String

  def selectDynamic(fieldName: String): Node[?] =
    t match
      case b: Bundle =>
        val p = b.asInstanceOf[Product]
        val idx = (0 until p.productArity).indexWhere(i => p.productElementName(i) == fieldName)
        if idx < 0 then throw new NoSuchElementException(s"${t.getClass.getName} has no field '$fieldName'")
        val childT = p.productElement(idx).asInstanceOf[ValueType]
        this match
          case l: LitNode[?] =>
            val payload = l.payload.asInstanceOf[Product].productElement(idx)
            LitNode(childT, payload)
          case _ =>
            FieldNode(this.asInstanceOf[Node[Bundle]], fieldName, childT)
      case _ =>
        throw new NoSuchElementException(s"${t.getClass.getName} has no field '$fieldName'")

  def apply(index: Int): Node[?] =
    t match
      case v: Vec[?] =>
        val childT = v.elem.asInstanceOf[ValueType]
        this match
          case l: LitNode[?] =>
            val seq = l.payload.asInstanceOf[Seq[Any]]
            LitNode(childT, seq(index))
          case _ =>
            IndexNode(this.asInstanceOf[Node[Vec[ValueType]]], index, childT)
      case _ =>
        throw new NoSuchElementException(s"${t.getClass.getName} is not indexable")

  def apply(start: Int, end: Int): Node[Vec[ValueType]] =
    t match
      case v: Vec[?] =>
        val childT = v.elem.asInstanceOf[ValueType]
        val len = (end - start) + 1
        this match
          case l: LitNode[?] =>
            val seq = l.payload.asInstanceOf[Seq[Any]].slice(start, end + 1)
            LitNode(Vec(childT, len), seq)
          case _ =>
            SliceNode(this.asInstanceOf[Node[Vec[ValueType]]], start, end, Vec(childT, len))
      case _ =>
        throw new NoSuchElementException(s"${t.getClass.getName} is not sliceable")

final class RefNode[T <: ValueType](private var name: String, val t: T, val kind: NodeKind) extends Node[T]:
  def expr: ExprIR = ExprIR.Ref(name)
  def refName: String = name
  private[hdl] def setName(n: String): Unit = name = n
  override def toString: String = s"${kind}($t,$name)"

final class LitNode[T <: ValueType](val t: T, val payload: Any) extends Node[T]:
  def expr: ExprIR = ExprIR.Lit(payload)
  def kind: NodeKind = NodeKind.Lit
  def refName: String = ""
  def get: HostTypeOf[T] = payload.asInstanceOf[HostTypeOf[T]]
  override def toString: String = s"Lit($payload)"

final class FieldNode[T <: ValueType](val parent: Node[Bundle], val field: String, val t: T) extends Node[T]:
  def expr: ExprIR = ExprIR.SubField(parent.expr, field)
  def kind: NodeKind = NodeKind.Op
  def refName: String = ""
  override def toString: String = s"Field($field,$t)"

final class IndexNode[A <: ValueType](val parent: Node[Vec[A]], val index: Int, val t: A) extends Node[A]:
  def expr: ExprIR = ExprIR.SubIndex(parent.expr, index)
  def kind: NodeKind = NodeKind.Op
  def refName: String = ""
  override def toString: String = s"Index($index,$t)"

final class SliceNode[A <: ValueType](val parent: Node[Vec[A]], val start: Int, val end: Int, val t: Vec[A]) extends Node[Vec[A]]:
  def expr: ExprIR = ExprIR.PrimOp("slice", Seq(parent.expr, ExprIR.Lit(start), ExprIR.Lit(end)))
  def kind: NodeKind = NodeKind.Op
  def refName: String = ""
  override def toString: String = s"Slice($start,$end,$t)"

final class PrimOpNode[T <: ValueType](val t: T, val op: String, val args: Seq[Node[?]]) extends Node[T]:
  def expr: ExprIR = ExprIR.PrimOp(op, args.map(_.expr))
  def kind: NodeKind = NodeKind.Op
  def refName: String = ""
  override def toString: String = s"PrimOp($op,$t)"

object Lit:
  def apply[T <: ValueType](t: T)(value: HostTypeOf[T]): LitNode[T] =
    new LitNode[T](t, value)

object IO:
  def apply[T <: ValueType](t: T): Node[T] = new RefNode("", t, NodeKind.IO)
  def apply[T <: ValueType](t: T, name: String): Node[T] = new RefNode(name, t, NodeKind.IO)

object Operations:
  private def widthOf(tc: Node[?]): Int =
    tc.t match
      case u: UInt => u.w.value
      case _ => throw new IllegalArgumentException("add requires UInt")

  extension (lhs: Node[?])
    infix def +(rhs: Node[?]): Node[UInt] =
      (lhs.t, rhs.t) match
        case (lu: UInt, ru: UInt) =>
          val lw = lu.w.value
          val rw = ru.w.value
          val outW = Width(math.max(lw, rw) + 1)
          PrimOpNode(UInt(outW), "add", Seq(lhs, rhs))
        case _ =>
          throw new IllegalArgumentException("add requires UInt")

  extension (xs: Seq[Node[UInt]])
    def sumAll: Node[UInt] =
      xs.reduce(_ + _)

object ConnectOps:
  extension (lhs: Node[?])
    def :=(rhs: Node[?])(using ctx: ElabContext): Unit =
      ctx.emit(StmtIR.Connect(lhs.expr, rhs.expr))
