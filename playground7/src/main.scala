package playground7



@main def main(): Unit = {
  // Create an anonymous instance of a structural type using `Selectable`.
  class Person extends Selectable {
    val name: String = "Cassandra"
    def age: Int = 35
    private val secret = "hidden"
  }

  val person = new Person

  // Our macro derives the named tuple directly from the type's structure.
  val namedTuple = structuralToNamedTuple(person)

  println(s"Derived from Selectable: $namedTuple")
  // Expected output: Derived from Selectable: ((name,Cassandra), (age,35))
}
