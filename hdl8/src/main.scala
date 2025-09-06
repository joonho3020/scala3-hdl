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


import compiletime.*
import scala.deriving.*

trait Bundle

trait Reg[V]:
  def get(v: V): V

object Reg:

///////////////////////

@main
def example(): Unit =
  import hdl8.*

  case class MyBundle(a: Int, b: Boolean) extends Bundle

  object MyBundleReg:
    val a = Reg[Int](_.a)
    val b = Reg[Boolean](_.b)

  val mybundle = MyBundle(42, true)
