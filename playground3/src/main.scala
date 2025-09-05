package playground3

import scala.NamedTuple
import scala.language.dynamics

case class City(zipCode: Int, name: String, population: Int)

trait Q[T] extends Selectable:
    type Fields = NamedTuple.Map[NamedTuple.From[T], Q]
    def selectDynamic(fieldName: String): Q[Any] = this.asInstanceOf[Q[Any]]



object Foo extends Selectable:
  type Fields = (hello: String, meaningOfLife: Int)
  def selectDynamic(field: String): Any =
    field match
      case "hello" => "world"
      case "meaningOfLife" => 42
      case _ => sys.error("Cannot happen")




@main def computedFieldsDemo(): Unit =
  val c = City(2, "me", 100)
  type CityNamedTuple = NamedTuple.From[City]
  val x: CityNamedTuple = (zipCode = 3, name = "you", population = 200)
  println(x)


  val city: Q[City] = new Q[City]{}
  val name: Q[String] = city.name
  val zipcode = city.zipCode
  val pop = city.population
  println((name, zipcode))

  val a: String = Foo.hello
  val b: Int = Foo.meaningOfLife
  println(s"a: ${a} b: ${b}")
