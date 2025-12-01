package hdl

import scala.compiletime.testing.{typeCheckErrors, typeChecks}

/** 
 * Compile-time tests that verify type safety.
 * These tests run at compile time and verify that:
 * 1. Valid code compiles without errors
 * 2. Invalid code produces the expected type errors
 * 
 * Run with: mill hdl.runMain hdl.compileTimeTypeTests
 */

@main def compileTimeTypeTests(): Unit =
  println("=" * 50)
  println("Compile-Time Type Safety Tests")
  println("=" * 50)

  inline def validConnections = """
    import hdl.*
    import hdl.ConnectOps.*
    given ctx: ElabContext = null.asInstanceOf[ElabContext]

    val boolWire = Wire(Bool())
    val uintWire = Wire(UInt(Width(8)))

    boolWire := Lit[Bool](true)
    uintWire := Lit[UInt](42)
  """

  assert(typeChecks(validConnections), "Valid connections should compile")
  println("Test 1 passed: Valid Bool := Bool compiles")
  println("Test 1 passed: Valid UInt := UInt compiles")

  inline def boolFromUint = """
    import hdl.*
    import hdl.ConnectOps.*
    given ctx: ElabContext = null.asInstanceOf[ElabContext]

    val boolWire = Wire(Bool())
    boolWire := Lit[UInt](3)
  """

  val errors1 = typeCheckErrors(boolFromUint)
  assert(errors1.nonEmpty, "Bool := UInt should not compile")
  assert(
    errors1.exists(_.message.contains("TypeCompatible[hdl.Bool, hdl.UInt]")),
    s"Expected TypeCompatible error, got: ${errors1.map(_.message)}"
  )
  println("Test 2 passed: Bool := UInt correctly fails with TypeCompatible error")

  inline def uintFromBool = """
    import hdl.*
    import hdl.ConnectOps.*
    given ctx: ElabContext = null.asInstanceOf[ElabContext]

    val uintWire = Wire(UInt(Width(8)))
    uintWire := Lit[Bool](true)
  """

  val errors2 = typeCheckErrors(uintFromBool)
  assert(errors2.nonEmpty, "UInt := Bool should not compile")
  assert(
    errors2.exists(_.message.contains("TypeCompatible[hdl.UInt, hdl.Bool]")),
    s"Expected TypeCompatible error, got: ${errors2.map(_.message)}"
  )
  println("Test 3 passed: UInt := Bool correctly fails with TypeCompatible error")

  inline def bundleFieldMismatch = """
    import hdl.*
    import hdl.ConnectOps.*
    given ctx: ElabContext = null.asInstanceOf[ElabContext]

    case class DataBundle(valid: Bool, data: UInt) extends Bundle
    val dataWire = Wire(DataBundle(Bool(), UInt(Width(8))))

    dataWire.data := dataWire.valid
  """

  val errors3 = typeCheckErrors(bundleFieldMismatch)
  assert(errors3.nonEmpty, "Bundle field type mismatch should not compile")
  assert(
    errors3.exists(_.message.contains("TypeCompatible[hdl.UInt, hdl.Bool]")),
    s"Expected TypeCompatible error for bundle fields, got: ${errors3.map(_.message)}"
  )
  println("Test 4 passed: Bundle field type mismatch correctly fails")

  inline def validBundleConnections = """
    import hdl.*
    import hdl.ConnectOps.*
    given ctx: ElabContext = null.asInstanceOf[ElabContext]

    case class DataBundle(valid: Bool, data: UInt) extends Bundle
    val wire1 = Wire(DataBundle(Bool(), UInt(Width(8))))
    val wire2 = Wire(DataBundle(Bool(), UInt(Width(8))))

    wire1.valid := wire2.valid
    wire1.data := wire2.data
    wire1 := wire2
  """

  assert(typeChecks(validBundleConnections), "Valid bundle connections should compile")
  println("Test 5 passed: Valid bundle field connections compile")
  println("Test 5 passed: Valid whole bundle connections compile")

  println("\n" + "=" * 50)
  println("[*] All compile-time type safety tests passed")
  println("=" * 50)
