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

// import scala.deriving.*
// import scala.compiletime.*
// import scala.NamedTuple
// import scala.util.NotGiven
// 
// trait Bundle
// 
// trait Ref[T] extends Selectable:
//   type Fields
//   def selectDynamic(name: String): Ref[?] =
//     sys.error(s"Ref[T] has no field '$name'") // overridden for products
//   def get: T
//   def set(t: T): Unit
// 
// final class Register[T](private var v: T) extends Ref[T]:
//   type Fields = EmptyTuple
//   override def get: T = v
//   override def set(t: T): Unit = v = t
// 
// trait MakeLeaf[T]:
//   def apply(init: T): Ref[T]
// 
// object MakeLeaf:
//   given [T]: MakeLeaf[T] with
//     def apply(init: T): Ref[T] = Register(init)
// 
// final class ProductRef[T](
//   val fieldsMap: Map[String, Ref[?]],
//   val toValue: () => T,
//   val fromValue: T => Unit
// ) extends Ref[T]:
//   type Fields = NamedTuple.Map[NamedTuple.From[T], Ref]
//   override def selectDynamic(name: String): Ref[?] =
//     fieldsMap.getOrElse(name, sys.error(s"Invalid field name: $name"))
//   transparent inline def at[K <: String](using k: ValueOf[K]): Ref[Types.FieldTypeByName[T, K]] =
//     fieldsMap.getOrElse(k.value, sys.error(s"Invalid field name: ${k.value}")).asInstanceOf[Ref[Types.FieldTypeByName[T, K]]]
//   def get: T = toValue()
//   def set(t: T): Unit = fromValue(t)
// 
// object Types:
//   type ElemTypeByLabel[Labels <: Tuple, Types <: Tuple, K <: String] = (Labels, Types) match
//     case (K *: _, h *: _) => h
//     case (_ *: lt, _ *: tt) => ElemTypeByLabel[lt, tt, K]
//     case (EmptyTuple, EmptyTuple) => Nothing
// 
//   type FieldTypeByName[T, K <: String] = ElemTypeByLabel[
//     Mirror.ProductOf[T]#MirroredElemLabels,
//     Mirror.ProductOf[T]#MirroredElemTypes,
//     K
//   ]
// 
// trait MakeRef[T]:
//   type Out <: Ref[T]
//   def apply(init: T): Out
// 
// object MakeRef:
//   // Leaves: Int, Boolean, etc.
//   inline given leaf[T](using ev: NotGiven[Mirror.ProductOf[T]], ml: MakeLeaf[T]): MakeRef[T] =
//     new MakeRef[T]:
//       type Out = Ref[T]
//       def apply(init: T): Out =
//         println(s"MakeRef leaf ${init}")
//         ml.apply(init)
// 
//   // Products (case classes)
//   inline given product[T <: Bundle](using p: Mirror.ProductOf[T]): MakeRef[T] =
//     println(s"MakeRef product")
//     val labelsCT: List[String] = constValueTuple[p.MirroredElemLabels].toList.asInstanceOf[List[String]]
//     println(s"labelsCT ${labelsCT}")
//     val elemMakersCT: List[MakeRef[Any]] = summonAllElems[p.MirroredElemTypes]
//     new MakeRef[T]:
//       type Out = ProductRef[T]
//       def apply(init: T): Out =
//         val childRefs: List[Ref[?]] =
//           elemMakersCT.zipWithIndex.map { case (mk, i) =>
//             val prod = init.asInstanceOf[Product]
//             val childInit = prod.productElement(i)
//             mk(childInit)
//           }
// 
//         val fields = labelsCT.zip(childRefs).toMap
// 
//         def toT(): T =
//           val elems = childRefs.map(_.get).asInstanceOf[List[Any]]
//           p.fromProduct(Tuple.fromArray(elems.toArray))
// 
//         def fromT(t: T): Unit =
//           val prod = t.asInstanceOf[Product]
//           var i = 0
//           while i < childRefs.length do
//             childRefs(i).asInstanceOf[Ref[Any]].set(prod.productElement(i))
//             i += 1
// 
//         println(s"fields ${fields}")
//         ProductRef[T](fields, () => toT(), fromT)
// 
//   // Helper: summon MakeRef for each element in a tuple type
//   private inline def summonAllElems[T <: Tuple]: List[MakeRef[Any]] =
//     inline erasedValue[T] match
//       case _: EmptyTuple => Nil
//       case _: (h *: t)   => summonInline[MakeRef[h]].asInstanceOf[MakeRef[Any]] :: summonAllElems[t]
// 
// // object MakeLeafRef:
// // Leaves: Int, Boolean, etc.
// // inline given leaf[T](using ev: NotGiven[Mirror.ProductOf[T]], ml: MakeLeaf[T]): MakeProductRef[T] with
// // def apply(init: T): Ref[T] =
// // println(s"MakeProductRef leaf ${init}")
// // ml.apply(init)
// 
// object HDL:
//   inline def reg[T](init: T)(using mk: MakeRef[T]): mk.Out =
//     mk(init).asInstanceOf[mk.Out]
// 
// case class Inner(x: Int, y: Boolean) extends Bundle
// case class MyBundle(a: Int, b: Boolean, inner: Inner) extends Bundle
// 
// @main def demo(): Unit =
//   import HDL.*
// 
//   val t = reg[Inner](Inner(2, true))
//   t.x
// 
//   val r = reg[MyBundle](MyBundle(3, true, Inner(2, false)))
//   r.a
// // r.a.set(42)
// // r.b.set(true)
// // r.inner.x.set(7)
// // r.inner.y.set(false)
// 
//   val v1 = r.get
//   println(s"v1 ${v1}")
//   r.set(MyBundle(1, false, Inner(99, true)))


import scala.deriving.*
import scala.compiletime.*
import scala.NamedTuple
import scala.util.NotGiven

sealed class Width(val value: Int):
  override def toString: String = s"${value}"

object Width:
  def apply(x: Int): Width = new Width(x)

sealed trait ValueType

sealed class UInt(val w: Width) extends ValueType:
  def apply(w: Width): UInt = new UInt(w)
  override def toString(): String = s"UInt($w.W)"

sealed class UIntLit(val v: Int) extends ValueType:
  def apply(v: Int): UIntLit = new UIntLit(v)
  override def toString(): String = s"UInLitt($v)"

trait Bundle extends ValueType

trait FieldsOf[T]:
  type Labels <: Tuple
  type Elems <: Tuple
  type Out = NamedTuple.NamedTuple[Labels, Elems]

  def get(t: T, name: String): ValueType

object FieldsOf:
  given FieldsOf[UInt] with
    type Labels = EmptyTuple
    type Elems = EmptyTuple

    def get(t: UInt, name: String): ValueType =
      throw new NoSuchElementException(s"UInt does not have  a subfield $name")

  given FieldsOf[UIntLit] with
    type Labels = EmptyTuple
    type Elems = EmptyTuple

    def get(t: UIntLit, name: String): ValueType =
      throw new NoSuchElementException(s"UInt does not have  a subfield $name")

  inline def derived[T](using m: Mirror.ProductOf[T]): FieldsOf[T] =
    new FieldsOf[T]:
      type Labels = m.MirroredElemLabels
      type Elems = m.MirroredElemTypes

      private val labels: Array[String] =
        constValueTuple[Labels].toArray.asInstanceOf[Array[String]]

      def get(t: T, name: String): ValueType =
        val p = t.asInstanceOf[Product]
        val idx = labels.indexOf(name)
        if idx < 0 then
          throw new NoSuchElementException(s"${p.getClass.getName} has no field $name")
        p.productElement(idx).asInstanceOf[ValueType]

type MapElems[E <: Tuple, F[_]] <: Tuple = E match
  case EmptyTuple => EmptyTuple
  case (h *: t)   => F[h & ValueType] *: MapElems[t, F]

final class Reg[T](val t: T) extends Selectable:
  type Fields =
    NamedTuple.NamedTuple[
      FieldsOf[T]#Labels,
      MapElems[FieldsOf[T]#Elems, [x] =>> Reg[x]]
    ]

  inline def selectDynamic(name: String): Reg[?] =
    val f = summonInline[FieldsOf[T]]
    val child = f.get(t, name)
    new Reg(child)

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

  // val mybundle_lit = Literal[MyBundle]((
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

  final case class InnerBundle(a: UInt, b: UInt) extends Bundle
  final case class MyBundle(x: UInt, y: UInt, i: InnerBundle) extends Bundle
  val mb = MyBundle(UInt(Width(2)), UInt(Width(3)), InnerBundle(UInt(Width(4)), UInt(Width(5))))
  val reg = Reg(mb)
  reg.x
