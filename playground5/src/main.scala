package playground5

import scala.deriving.*
import scala.compiletime.*
import scala.NamedTuple
import scala.quoted.*

sealed trait ValueType

sealed class IntValue(val x: Int) extends ValueType:
  override def toString(): String = s"IntValue($x)"

sealed class BoolValue(val x: Boolean) extends ValueType:
  override def toString(): String = s"BoolValue($x)"

trait Aggregate extends ValueType

@main def demo(): Unit =

  final case class AggExample(a: IntValue, b: IntValue) extends Aggregate
  val agg = AggExample(new IntValue(2), new IntValue(3))

  val wrap = Wrapper(agg)
  val wrap_a: Wrapper[IntValue] = wrap.a

  // Structural type example (non-product)
  class AggSelectable extends Aggregate:
    val a = new IntValue(2)
    val b = new IntValue(3)

  class NestedAggSelectable extends Aggregate:
    val agg = new AggSelectable
    val c   = new BoolValue(true)

  val wrap2 = Wrapper(new AggSelectable)
  // val wrap2_a: Wrapper[BoolValue] = wrap2.a // compile error, doesn't compiler
  val wrap2_a: Wrapper[IntValue] = wrap2.a
  val wrap2_b: Wrapper[IntValue] = wrap2.b
  println(s"wrap2_a ${wrap2_a} wrap2_b ${wrap2_b}")

  val wrap3 = Wrapper(new NestedAggSelectable)
  val wrap3_agg = wrap3.agg
  // val wrap3_agg_a: Wrapper[BoolValue] = wrap3.agg.a // shouldn't compile
  val wrap3_agg_a: Wrapper[IntValue] = wrap3.agg.a

  println(s"wrap3_agg ${wrap3_agg} wrap3_agg_a ${wrap3_agg_a}")

  // What I wish:
  //
  // trait AggregateSelectable extends Aggregate with Selectable
  // class AggSelectable extends AggregateSelectable:
  //  val a = IntValue(2)
  //  val b = IntValue(3)
  //
  // val wrap = Wrapper(new AggSelectable)
