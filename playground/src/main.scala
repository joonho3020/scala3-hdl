package hdl

import scala.compiletime.{erasedValue, constValue, summonInline}
import scala.deriving.*
import scala.compiletime.ops.int.*
import scala.annotation.targetName
import scala.compiletime.erasedValue
import scala.quoted.*
import scala.language.strictEquality

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
