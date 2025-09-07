package hdl8

// import scala.deriving.Mirror
// import scala.compiletime.{constValueTuple, summonInline, summonFrom}
// import scala.util.Try
// import scala.language.dynamics
// import scala.NamedTuple
// import scala.NamedTuple.Map
// import scala.NamedTuple.From

// trait Reg[T] extends Selectable:
//   type Fields = NamedTuple.Map[NamedTuple.From[T], Reg]
//   def selectDynamic(fieldName: String): Reg[?]
// 
// final case class RegLeaf[A](payload: A) extends Reg[A]:
//   def selectDynamic(fieldName: String): Reg[?] =
//     throw NoSuchFieldException(s"'$fieldName' on leaf (${payload.getClass.getName})")
// 
// final case class RegNode[T](value: T)(using m: Mirror.ProductOf[T]) extends Reg[T]:
//   private def labels: List[String] =
//     value.asInstanceOf[Product].productElementNames.toList
// 
//   def selectDynamic(fieldName: String): Reg[?] =
//     val i = labels.indexOf(fieldName)
//     if i < 0 then
//       throw NoSuchFieldException(s"'$fieldName' not in ${labels.mkString("[", ", ", "]")}")
//     val elem = value.asInstanceOf[Product].productElement(i)
//     Reg(elem)
// 
// object Reg:
//   inline def apply[T](value: T): Reg[T] =
//     summonFrom {
//       case m: Mirror.ProductOf[T] => RegNode[T](value)(using m)
//       case _                      => RegLeaf[T](value)
//     }
// 
// object Main:
//   def main(args: Array[String]): Unit =
//     println("Hello World")
// 
//     case class MyBundle(a: Int, b: Int)
//     val mybundle = MyBundle(2, 3)
// 
// // val reg_mybundle: Reg[MyBundle]
// // reg_mybundle.a
//     val reg_mybundle = Reg(mybundle)
//     val reg_a = reg_mybundle.a
//     println(s"reg_mybundle.a => ${reg_mybundle.a}")


// import compiletime.*
// import scala.deriving.*
// import scala.NamedTuple
// 
// trait Bundle
// 
// case class MyBundle(a: Int, b: Boolean) extends Bundle
// 
// trait Reg[T] extends Selectable:
//   type Fields
//   def regs: Map[String, Reg[?]]
//   inline def selectDynamic(name: String): Reg[?]
// 
// given Reg[Int]:
//   type Fields = ()
//   def regs: Map[String, Reg[?]] = Map()
//   inline def selectDynamic(name: String): Reg[?] =
//     sys.error(s"Leaf bundle does not have subfield access to ${name}")
// // 
// // given Reg[MyBundle](x: MyBundle):
// //   type Fields = (
// //     a: Reg[Int],
// //     b: Reg[Boolean]
// //   )
// //   def regs = Map(
// //     "a" -> Reg[Int](x.a),
// //     "b" -> Reg[Boolean](x.b)
// //   )
// //   inline def selectDynamic(name: String): Reg[?] =
// //     regs.getOrElse(
// //       name,
// //       sys.error(s"Invalid field name ${name}"))
// //   def getValue: MyBundle = x
// 
// ///////////////////////
// 
// @main
// def example(): Unit =
//   import hdl8.*
// 
// 
// // object MyBundleReg:
// // val a = Reg[Int](_.a)
// // val b = Reg[Boolean](_.b)
// 
//   val mybundle = MyBundle(42, true)

import scala.deriving.*
import scala.compiletime.*
import scala.NamedTuple
import scala.util.NotGiven

trait Ref[T] extends Selectable:
  type Fields
  def selectDynamic(name: String): Ref[?] =
    sys.error(s"Ref[T] has no field '$name'") // overridden for products
  def get: T
  def set(t: T): Unit

final class Register[T](private var v: T) extends Ref[T]:
  type Fields = EmptyTuple
  override def get: T = v
  override def set(t: T): Unit = v = t

trait MakeLeaf[T]:
  def apply(init: T): Ref[T]

object MakeLeaf:
  given [T]: MakeLeaf[T] with
    def apply(init: T): Ref[T] = Register(init)

final class ProductRef[T](
  val fieldsMap: Map[String, Ref[?]],
  val toValue: () => T,
  val fromValue: T => Unit
) extends Ref[T]:
  type Fields = NamedTuple.Map[NamedTuple.From[T], Ref]
  override inline def selectDynamic(name: String): Ref[?] =
    fieldsMap.getOrElse(name, sys.error(s"Invalid field name: $name"))
  def get: T = toValue()
  def set(t: T): Unit = fromValue(t)

trait MakeRef[T]:
  def apply(init: T): Ref[T]

object MakeRef:
  // Leaves: Int, Boolean, etc.
  inline given leaf[T](using ev: NotGiven[Mirror.ProductOf[T]], ml: MakeLeaf[T]): MakeRef[T] with
    def apply(init: T): Ref[T] =
      println(s"MakeRef leaf ${init}")
      ml.apply(init)

  // Products (case classes)
  inline given product[T <: Bundle](using p: Mirror.ProductOf[T]): MakeRef[T] =
    println(s"MakeRef product")
    val labelsCT: List[String] = constValueTuple[p.MirroredElemLabels].toList.asInstanceOf[List[String]]
    println(s"labelsCT ${labelsCT}")
    val elemMakersCT: List[MakeRef[Any]] = summonAllElems[p.MirroredElemTypes]
    new MakeRef[T]:
      def apply(init: T): Ref[T] =
        val childRefs: List[Ref[?]] =
          elemMakersCT.zipWithIndex.map { case (mk, i) =>
            val prod = init.asInstanceOf[Product]
            val childInit = prod.productElement(i)
            mk(childInit)
          }

        val fields = labelsCT.zip(childRefs).toMap

        def toT(): T =
          val elems = childRefs.map(_.get).asInstanceOf[List[Any]]
          p.fromProduct(Tuple.fromArray(elems.toArray))

        def fromT(t: T): Unit =
          val prod = t.asInstanceOf[Product]
          var i = 0
          while i < childRefs.length do
            childRefs(i).asInstanceOf[Ref[Any]].set(prod.productElement(i))
            i += 1

        println(s"fields ${fields}")
        ProductRef[T](fields, () => toT(), fromT)

  // Helper: summon MakeRef for each element in a tuple type
  private inline def summonAllElems[T <: Tuple]: List[MakeRef[Any]] =
    inline erasedValue[T] match
      case _: EmptyTuple => Nil
      case _: (h *: t)   => summonInline[MakeRef[h]].asInstanceOf[MakeRef[Any]] :: summonAllElems[t]

object HDL:
  def reg[T](init: T)(using mk: MakeRef[T]): Ref[T] = mk(init)

trait Bundle

case class Inner(x: Int, y: Boolean) extends Bundle
case class MyBundle(a: Int, b: Boolean, inner: Inner) extends Bundle

@main def demo(): Unit =
  import HDL.*

  val t = reg[Inner](Inner(2, true))
  // t.x

  val r = reg[MyBundle](MyBundle(3, true, Inner(2, false)))
// r.a.set(42)
// r.b.set(true)
// r.inner.x.set(7)
// r.inner.y.set(false)

  val v1 = r.get
  println(s"v1 ${v1}")
  r.set(MyBundle(1, false, Inner(99, true)))
