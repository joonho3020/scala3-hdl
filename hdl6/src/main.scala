package hdl6


sealed class Width(val value: Int):
  override def toString: String = s"${value}"

object Width:
  def apply(x: Int): Width = new Width(x)

sealed trait Signal

sealed class UInt(val w: Width) extends Signal:
  def apply(w: Width): UInt = new UInt(w)
  override def toString(): String = s"UInt($w.W)"

sealed class UIntLit(override val w: Width)(val v: Int) extends UInt(w):
  override def toString(): String = s"UIntLit($v($w.W))"

class Bundle(elems: (String, Any)*) extends Selectable with Signal:
  private val fields = elems.toMap
  def selectDynamic(name: String): Any = fields(name)
  override def toString: String =
    val body = fields.map { case (k, v) => s"$k=$v" }.mkString(", ")
    s"Bundle($body)"

object Main:
  def main(args: Array[String]): Unit =
    println("Hello world")

    type Person = (name: String, age: Int)
    val Bob: Person = (name = "Bob", age = 33)

    type Student = (id: Int, person: Person)
    val Joonho: Student = (id = 30384, person = (name = "Joonho", age = 29))
    println(s"Joonho ${Joonho}")


    class MyBundle(w1: Int, w2: Int):
      val a = UInt(Width(w1))
      val b = UInt(Width(w2))

    val mybundle = new MyBundle(2, 3)
    println(s"mybundle ${mybundle}")

    println(s"schema of MyBundle ${BundleSchema.labelsOf[MyBundle]}")
    // println(s"type of MyBundle ${BundleSchema.labelsOf[MyBundle]}")

    class NestedBundle(x: Int, y: Int, z: Int) extends Bundle:
      val width_outer = x + y + z
      val inner = new MyBundle(x, y)
      val outer = UInt(Width(width_outer))

    val nbs = BundleSchema.derived[NestedBundle]
    val nb_labels = BundleSchema.labelsOf[NestedBundle]
    println(s"nb_labels ${nb_labels}")
    // val nestedbundle: BundleSchema.Of[NestedBundle] = (inner = (a = UInt(Width(3)), b = UInt(Width(4))), outer = UInt(Width(5)))
// println(s"schema of NestedBundle ${BundleSchema.Of[NestedBundle]}")
