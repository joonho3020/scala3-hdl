package playground2

import compiletime.*
import scala.deriving.*
import scala.quoted.*

// Typeclass enables you to add methods to existing types
trait Show[-T]:
  def show(t: T): String

object Show:
  // Implementation of typeclass Show[Any]
  given Show[Any]:
    def show(x: Any): String = x.toString

  // Implementation of typeclass Show[Int]
  given Show[Int]:
    def show(t: Int): String = t.toString

  // Implementation of typeclass Show[String]
  given Show[String]:
    def show(t: String): String = t

  // If typeclass Show[A] is defined for type A,
  // we can use it to derive the typeclass Show for Option[A] (i.e., derive Show[Option[A]])
  given [A](using sa: Show[A]): Show[Option[A]] with
    def show(a: Option[A]): String =
      a match
        case None => "None"
        case Some(x) => s"Some(${sa.show(x)})"

  // Entry point for typeclass derivation
  // We can do stuff like the below:
  // `case class Person(name: String, age: Int) derives Show`
  inline def derived[T](using m: Mirror.Of[T]): Show[T] =
    val className = m.getClass().getName()
    inline m match
      case p: Mirror.ProductOf[T] =>
        val elemShows : List[Show[?]] = summonAll[p.MirroredElemTypes]
        val elemLabels: List[String]  = labelsOf[p.MirroredElemLabels]
        showProduct(p, elemShows, elemLabels, className)
      case s: Mirror.SumOf[T]     => ???

  private inline def summonAll[T <: Tuple]: List[Show[?]] =
      inline erasedValue[T] match
        case _: EmptyTuple => Nil
        case _: (t *: ts)  => summonInline[Show[t]] :: summonAll[ts]

  // materialize field labels: ("name", "age", ...) from MirroredElemLabels
  inline def labelsOf[T <: Tuple]: List[String] =
    inline erasedValue[T] match
      case _: EmptyTuple => Nil
      case _: (h *: t)   => constValue[h].asInstanceOf[String] :: labelsOf[t]

  private inline def showProduct[T](
    p: Mirror.ProductOf[T],
    shows:  List[Show[?]],
    labels: List[String],
    clsName: String
  ): Show[T] =
    new Show[T]:
      def show(a: T): String =
        val elems = a.asInstanceOf[Product].productIterator.toList
        val zipped = labels.zip(elems).zip(shows)
        val rendered = zipped.map { case ((lab, value), sh) => s"$lab=${sh.asInstanceOf[Show[Any]].show(value)}" }
        rendered.mkString(s"${clsName}(", ", ", ")")

def printShow[A](a: A)(using s: Show[A]): Unit =
  println(s.show(a))

object Main:
  def main(args: Array[String]): Unit =
    printShow(2)                // uses given Show[Int] for s
    printShow("Hello World")    // uses given Show[String] for s

    val s_int = summon[Show[Int]] // summon fetches the in-scope given
    println(s_int.show(7))

    val s_opt_int = summon[Show[Option[Int]]]
    println(s_opt_int.show(Some(2)))
    println(s_opt_int.show(None))

    printShow(Some(2))
    // val x: Option[Int] = Some(2)
    // printShow(x)
    printShow(Some(3): Option[Int])
    printShow(Some(4))

    case class Test1(f1: String, f2: Int) derives Show
    case class Test2(token: String, tx: Long)
    case class Test3(x: Int, t2: Test2)

    printShow(Test1("x", 20))
    printShow(Test2("x", 300))
    printShow(Test3(4, Test2("HELLO", 400)))
