package hdl7


sealed class Width(val value: Int):
  override def toString: String = s"${value}"

object Width:
  def apply(x: Int): Width = new Width(x)


sealed trait Binding
case object Unbound extends Binding
final case class WireBinding() extends Binding
final case class RegBinding()  extends Binding
final case class LitBinding()  extends Binding

sealed trait Signal {
  var binding: Binding = Unbound
}

sealed trait Data extends Signal {
  def cloneShape: Data
}

sealed class UInt(val w: Width) extends Data:
  def apply(w: Width): UInt = new UInt(w)
  override def toString(): String = s"UInt($w.W)"
  override def cloneShape: Data = UInt(w)

sealed class UIntLit(override val w: Width)(val v: Int) extends UInt(w):
  override def toString(): String = s"UIntLit($v($w.W))"

class Bundle(elems: (String, Data)*) extends Selectable with Data:
  private lazy val fields: Map[String, Data] =
    this.getClass.getMethods.iterator
      .filter(m => m.getParameterCount == 0 && m.getDeclaringClass == this.getClass && classOf[Data].isAssignableFrom(m.getReturnType))
      .map(m => m.getName -> m.invoke(this).asInstanceOf[Data])
      .toMap
  def selectDynamic(name: String): Data = fields(name)
  override def toString: String =
    val body = fields.map { case (k, v) => s"$k=$v" }.mkString(", ")
    println(s"fields ${fields}")
    s"Bundle($body)"
  override def cloneShape: Data = this
// override def cloneShape: Data =
// new Bundle {
// private val fields = fields.map { case (n, d) => n -> d.cloneShape }
// override def toString = s"Bundle(${cloned.map{case(n,d)=>s"$n=$d"}.mkString(", ")})"
// }

object DataConstructors {
  def Wire[T <: Data](template: T): T = {
    val d = template.cloneShape.asInstanceOf[T]
    d.binding = WireBinding(); d
  }
  def Reg[T <: Data](template: T): T = {
    val d = template.cloneShape.asInstanceOf[T]
    d.binding = RegBinding(); d
  }
  // Literal factory for leaves; bundles would be composed of leaf lits
  def UIntLit(width: Int, value: BigInt): UInt = {
    val u = UInt(Width(width)); u.binding = LitBinding(); u
  }
}

object Main:
  def main(args: Array[String]): Unit =
    import DataConstructors._

    println("Hello world")

    class MyBundle(w1: Int, w2: Int) extends Bundle:
      val a = UInt(Width(w1))
      val b = UInt(Width(w2))

    val mybundle = new MyBundle(2, 3)
    println(s"mybundle ${mybundle.toString}")

    val reg = Reg(new MyBundle(4, 5))

    class NestedBundle(x: Int, y: Int, z: Int) extends Bundle:
      val width_outer = x + y + z
      val inner = new MyBundle(x, y)
      val outer = UInt(Width(width_outer))

// Understanding prior work is important when you want to improve upon it.
// This version is a Chisel like implementation to understand how it works under the hood.
// Basically Reg and Wire are just functions that set bindings to the bundle leaves.
// The return type of Reg(MyBundle) is still a MyBundle!
