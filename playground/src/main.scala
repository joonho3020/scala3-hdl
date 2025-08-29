package playground

import scala.compiletime.{erasedValue, constValue, summonInline}
import scala.deriving.*
import scala.compiletime.ops.int.*
import scala.annotation.targetName
import scala.compiletime.erasedValue
import scala.quoted.*
import scala.language.strictEquality

// - Conversions

trait Book:
    def author: String
    def title: String
    def year: Int

case class PrintedBook(
    author: String,
    title: String,
    year: Int,
    pages: Int
) extends Book:
  override def equals(that: Any): Boolean = that match
    case a: AudioBook =>
      this.author == a.author
      && this.title == a.title
    case p: PrintedBook =>
      this.author == p.author
      && this.title == p.title
      && this.year == p.year
      && this.pages == p.pages
    case _ =>
      false

case class AudioBook(
    author: String,
    title: String,
    year: Int,
    lengthInMinutes: Int
) extends Book:
  override def equals(that: Any): Boolean = that match
    case a: AudioBook =>
      this.author == a.author
      && this.title == a.title
      && this.year == a.year
      && this.lengthInMinutes == a.lengthInMinutes
    case p: PrintedBook =>
      this.author == p.author
      && this.title == p.title
    case _ =>
      false

given CanEqual[PrintedBook, PrintedBook] = CanEqual.derived
given CanEqual[AudioBook, AudioBook] = CanEqual.derived

given CanEqual[PrintedBook, AudioBook] = CanEqual.derived
given CanEqual[AudioBook, PrintedBook] = CanEqual.derived

// Macros
// - splices ${...}: insert code into the AST
// - quotes  '{...}: the AST of this code block
inline def inspect(inline x: Any): Any = ${ inspectCode('x) }

inline def power(inline x: Double, inline n: Int): Double = ${ powerCode('x, 'n) }

inline def sumAll(inline nums: Int*): Int = ${ sumCode('nums) }

inline def test(inline ignore: Boolean, computation: => Unit): Boolean =
  ${ testCode('ignore, 'computation) }

inline def iterativeFunctionDeconstruction(inline f: FieldName => FieldName): List[String] =
  ${ collectUsedMethods('{f}) }

object Main:
  def main(args: Array[String]): Unit =
    println("Hello World")

    val p1 = PrintedBook("1984", "George Orwell", 1961, 328)
    val p2 = PrintedBook("1984", "George Orwell", 1961, 328)
    println(p1 == p2)

    val pBook = PrintedBook("1984", "George Orwell", 1961, 328)
    val aBook = AudioBook("1984", "George Orwell", 2006, 682)
    println(aBook == pBook)
    println(pBook == aBook)

    inspect(println("INSPECT"))
    println(s"3.0^2 = ${power(3.0, 2)}")
    println(s"1 + 2 + 3 + 4 = ${sumAll(1, 2, 3, 4)}")

    println(test(false, println("DONT   IGNORE ME")))
    println(test(true,  println("PLEASE IGNORE ME")))

    val f = (name: FieldName) => name.lowercase.uppercase.lowercase
    val used = iterativeFunctionDeconstruction(f)
    println(s"Used ${used}")
