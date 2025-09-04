package hdl6


sealed class Width(val value: Int):
  override def toString: String = s"${value}"

object Width:
  def apply(x: Int): Width = new Width(x)

sealed trait Signal

trait BundleLike

sealed class UInt(val w: Width) extends Signal:
  def apply(w: Width): UInt = new UInt(w)
  override def toString(): String = s"UInt($w.W)"

sealed class UIntLit(override val w: Width)(val v: Int) extends UInt(w):
  override def toString(): String = s"UIntLit($v($w.W))"

class Bundle(elems: (String, Any)*) extends Selectable with Signal with BundleLike:
  private val fields = elems.toMap
  def selectDynamic(name: String): Any = fields(name)
  override def toString: String =
    val body = fields.map { case (k, v) => s"$k=$v" }.mkString(", ")
    s"Bundle($body)"

object NT:
  inline def pair[K <: String & Singleton, V](label: K, value: V): (K, V) = (label, value)

object Main:
  def main(args: Array[String]): Unit =
    println("Hello world")

    type Person = (name: String, age: Int)
    val Bob: Person = (name = "Bob", age = 33)

    type Student = (id: Int, person: Person)
    val Joonho: Student = (id = 30384, person = (name = "Joonho", age = 29))
    println(s"Joonho ${Joonho}")


    class MyBundle(w1: Int, w2: Int) extends BundleLike:
      val a = UInt(Width(w1))
      val b = UInt(Width(w2))

    val mybundle = new MyBundle(2, 3)
    println(s"mybundle ${mybundle}")

    // println(s"schema of MyBundle ${BundleSchema.labelsOf[MyBundle]}")
    // println(s"type of MyBundle ${BundleSchema.labelsOf[MyBundle]}")

// type MyT = TupleSchema.Of[MyBundle]          // (UInt, UInt)
    //
      // Literal assignment (positional):
    val my = new MyBundle(2, 3)
    val x  = TupleSchema.derived[MyBundle].value(my)



    class NestedBundle(x: Int, y: Int, z: Int) extends BundleLike:
      val width_outer = x + y + z
      val inner = new MyBundle(x, y)
      val outer = UInt(Width(width_outer))

    val nb = new NestedBundle(1, 2, 3)
    val y  = TupleSchema.derived[NestedBundle].value(nb)

    // val nbs = BundleSchema.derived[NestedBundle]
    // val nb_labels = BundleSchema.labelsOf[NestedBundle]
    // println(s"nb_labels ${nb_labels}")
    // val nestedbundle: BundleSchema.Of[NestedBundle] = (inner = (a = UInt(Width(3)), b = UInt(Width(4))), outer = UInt(Width(5)))
// println(s"schema of NestedBundle ${BundleSchema.Of[NestedBundle]}")

    // Assign concrete values to the derived tuple schemas
// val aK: "a" = "a"
// val bK: "b" = "b"
// val mySchema = BundleSchema.derived[MyBundle]
// val aPair: ("a", UInt) = (aK, UInt(Width(2)))
// val bPair: ("b", UInt) = (bK, UInt(Width(3)))
// val myBundleSchemaValue: mySchema.Out = aPair *: bPair *: EmptyTuple
// println(s"myBundleSchemaValue ${myBundleSchemaValue}")

    // Note: current NestedBundle schema includes only 'outer' per labelsOf output
    // val innerK: "inner" = "inner"
    // val outerK: "outer" = "outer"
    // val nbSchema = BundleSchema.derived[NestedBundle]
    // val nestedBundleSchemaValue: nbSchema.Out =
    //   NT.pair(innerK, (NT.pair(aK, UInt(Width(3))) *: NT.pair(bK, UInt(Width(4))) *: EmptyTuple)) *:
    //     NT.pair(outerK, UInt(Width(5))) *:
    //     EmptyTuple
    // println(s"nestedBundleSchemaValue ${nestedBundleSchemaValue}")


// NOTE:
// Tried deriving a named Tuple from a structural type.
// After this, we can perform derivation on the named tuple.
// The problem was that named tuples are almost the same thing as case classes
// and scala macros does not allow named parameters unless the user specifies
// the names as a string argument to the macro.
//
// Anonymous tuples are probably possible, but you now lose the ability to
// access fields via names.
