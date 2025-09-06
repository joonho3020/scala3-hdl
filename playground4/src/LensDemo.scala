// Adapted from:
// https://github.com/kitlangton/quotidian/blob/2c1ffb497bbc91f8860a965e757812d6609ff84c/examples/src/main/scala/quotidian/examples/lens/LensDemo.scala
package quotidian.examples.lens

case class Person(name: String, age: Int, isAlive: Boolean)

object Person extends Selectable:
  type Fields = (
    name: Lens[Person, String],
    age:  Lens[Person, Int],
    isAlive: Lens[Person, Boolean]
  )
  private val lenses = Map(
    "name" -> new Lens[Person, String] { def get(s: Person) = s.name; def set(s: Person, a: String) = s.copy(name = a); },
    "age" -> new Lens[Person, Int] { def get(s: Person) = s.age; def set(s: Person, a: Int) = s.copy(age = a); },
    "isAlive" -> new Lens[Person, Boolean] { def get(s: Person) = s.isAlive; def set(s: Person, a: Boolean) = s.copy(isAlive = a); }
  )
  inline def selectDynamic(name: String): Lens[Person, ?] =
    lenses.getOrElse(
      name,
      sys.error(s"Invalid field name [$name]"))

// object Person extends DeriveLenses[Person]

// object Person:
//   val name = Lens[Person](_.name)(p => name => p.copy(name = name))
//   val age = Lens[Person](_.age)(p => age => p.copy(age = age))
//   val isAlive = Lens[Person](_.isAlive)(p => isAlive => p.copy(isAlive = isAlive))

@main
def example(): Unit =
  val person = Person("Alice", 42, true)

  val name = Person.name.get(person)
  val age = Person.age.get(person)
  val isAlive = Person.isAlive.get(person)

  println(s"Name: $name, Age: $age, Is Alive: $isAlive")
