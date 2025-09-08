package hdl8

import scala.deriving.*
import scala.compiletime.*
import scala.NamedTuple
import scala.util.NotGiven
import scala.quoted.*

sealed class Width(val value: Int):
  override def toString: String = s"${value}"

object Width:
  def apply(x: Int): Width = new Width(x)

sealed trait ValueType

sealed class UInt(val w: Width) extends ValueType:
  def apply(w: Width): UInt = new UInt(w)
  override def toString(): String = s"UInt($w.W)"

sealed class Bool extends ValueType

trait Bundle extends ValueType

type MapElems[E <: Tuple, F[_]] <: Tuple = E match
  case EmptyTuple => EmptyTuple
  case h *: t     => F[h & ValueType] *: MapElems[t, F]

final class Reg[T](val t: T)(using s: ShapeOf[T]) extends Selectable:
  val shape = s
  type Fields = NamedTuple.NamedTuple[
    shape.Labels,
    MapElems[shape.Elems, [X] =>> Reg[X]]
  ]
// type Fields = NamedTuple.Map[
// NamedTuple.From[T],
// [X] =>> Reg[X & ValueType]]

  // inline def selectDynamic(name: String): Reg[?] =
  //   // val s = summonInline[ShapeOf[T]]
  //   new Reg(shape.getValueType(t, name))

  // provide child Reg[X] by also providing ShapeOf[X]
  private inline def mkChild[X <: ValueType](x: X): Reg[X] =
    new Reg[X](x)(using scala.compiletime.summonInline[ShapeOf[X]])

  inline def selectDynamic(name: String): Reg[?] =
    mkChild(shape.getValueType(t, name))

  override def toString(): String =
    s"Reg(${t})"

object Reg:
  def apply[T <: ValueType](t: T)(using ShapeOf[T]): Reg[T] = new Reg[T](t)

@main def demo(): Unit =
  // // What I want
  // class InnerBundle(wa: Int, wb: Int) extends Bundle {
  //   val a = UInt(Width(wa))
  //   val b = UInt(Width(wb))
  // }
  // class MyBundle(wa: Int, wb: Int, wx: Int, wy: Int) extends Bundle {
  //   val x = UInt(Width(wx))
  //   val y = UInt(Width(wy))
  //   val i = new InnerBundle(wa, wb)
  // }
  // val mybundle_reg = Reg[MyBundle](new MyBundle(2, 3, 4, 5))
  // val x: Reg[UInt] = mybundle_reg.x
  // val i: Reg[InnerBundle] = mybundle_reg.i
  // val a: Reg[UInt] = mybundle_reg.i.a

  // val mybundle_lit = Lit[MyBundle]((
  //     x = UIntLit(3),
  //     y = UIntLit(2),
  //     i = (
  //       a = UIntLit(4),
  //       b = UIntLit(5)
  //     )
  //   ))
  // val xl: UIntLit = mybundle_lit.x
  // val yl: UIntLit = mybundle_lit.y
  // val al: UIntLit = mybundle_lit.i.a

  println("Hello World")


  val reg_uint = Reg(UInt(Width(4)))
  println(s"reg_uint ${reg_uint}")

  final case class InnerBundle(a: UInt, b: UInt) extends Bundle
  final case class MyBundle(x: UInt, y: UInt, i: InnerBundle) extends Bundle
  val mb = MyBundle(UInt(Width(2)), UInt(Width(3)), InnerBundle(UInt(Width(4)), UInt(Width(5))))

  val reg = Reg(mb)
  val reg_x: Reg[UInt] = reg.x
  val reg_y: Reg[UInt] = reg.y
  // val reg_y: Reg[UIntLit] = reg.y // Type mismatch, doesn't compile
  val reg_i: Reg[InnerBundle] = reg.i
  // val reg_i: Reg[UIntLit] = reg.i // Type mismatch doesn't compile
  val reg_i_a: Reg[UInt] = reg_i.a
  // val reg_i_a: Reg[UIntLit] = reg_i.a // Type mismatch doesn't compile

  val reg_i_b: Reg[UInt] = reg.i.b
  // val reg_i_b: Reg[UIntLit] = reg.i.b // Type mismatch doesn't compile
  println(s"reg_x: ${reg_x} reg_y: ${reg_y} reg_i: ${reg_i} reg_i_a ${reg_i_a} reg_i_b ${reg_i_b}")

  val ulit = Lit[UInt](3)
  println(s"ulit.get: ${ulit.get}")

  val inner_bundle_host_type: HostTypeOf[InnerBundle] = (
    a = 3,
    b = 2,
  )
  // val inner_bundle_host_type: HostTypeOf[InnerBundle] = (
  //   a = 3,
  //   b = 2,
  //   c = 4
  // ) // compile fails, type mismatch

  println(s"inner_bundle_host_type ${inner_bundle_host_type}")


  val ilit = Lit[InnerBundle]((a = 3, b = 4))

  val ilit_a: Lit[UInt] = ilit.a

  // val ilit_a: Lit[Bool] = ilit.a // Type mismatch doesn't compile

  println(s"ilit.a ${ilit.a.get} ilit_a.get ${ilit_a.get}")

  val mylit = Lit[MyBundle]((
    x = 2,
    y = 3,
    i = (a = 4, b = 5)))

  val mylit_x: Lit[UInt] = mylit.x
  println(s"mylit_x.get ${mylit_x.get} mylit.x.get ${mylit.x.get}")

  val mylit_i: Lit[InnerBundle] = mylit.i
  // val mylit_i: Lit[MyBundle] = mylit.i // Type mismatch doesn't compile
  // val mylit_i: Lit[UInt] = mylit.i // Type mismatch doesn't compile
  println(s"mylit_i.get ${mylit_i.get} mylit.i.get ${mylit.i.get}")

  val mylit_i_a: Lit[UInt] = mylit.i.a
  println(s"mylit_i_a.get ${mylit_i_a.get} ${mylit.i.a.get} ${mylit_i.a.get}")

  // val mylit_2 = Lit[MyBundle]((
  //   y = 3,
  //   x = 2,
  //   i = (a = 4, b = 5))) // Doesn't compile because we mixed up the order of named tuples
