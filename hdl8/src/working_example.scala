package hdl8.working

import hdl8.*
import scala.compiletime.*

// Working example that demonstrates a concrete implementation approach

// Simple direction phantom types
sealed trait Dir
sealed trait In extends Dir
sealed trait Out extends Dir

// Directed wrapper using a simple case class
case class Directed[+T <: ValueType, D <: Dir](value: T)

// Extension methods for ergonomic creation
extension [T <: ValueType](value: T)
  def in: Directed[T, In] = Directed[T, In](value)
  def out: Directed[T, Out] = Directed[T, Out](value)

// Modified Reg that handles Directed types
class DirReg[T](val t: T) extends Selectable:
  import scala.deriving.*
  
  type Fields = NamedTuple.Map[
    NamedTuple.From[T],
    [X] =>> DirReg[X]]

  inline def selectDynamic(name: String): DirReg[?] =
    summonFrom {
      case m: Mirror.ProductOf[T] =>
        val labels = constValueTuple[m.MirroredElemLabels].toArray
        val idx = labels.indexOf(name)
        val child = t.asInstanceOf[Product].productElement(idx)
        new DirReg(child)
      case _ =>
        throw new NoSuchElementException(s"${t.getClass.getName} has no field '$name'")
    }
    
  override def toString(): String = s"DirReg($t)"

object DirReg:
  def apply[T](t: T): DirReg[T] = new DirReg(t)

// Connection validation type class
trait Connect[A, B]:
  def connect(a: A, b: B): Unit = 
    println(s"Connecting $b => $a")

object Connect:
  // Input can be driven by Output
  given connectInFromOut[T <: ValueType]: Connect[Directed[T, In], Directed[T, Out]] with {}
  
  // Reg of Input can be driven by Reg of Output
  given connectRegInFromOut[T <: ValueType]: Connect[DirReg[Directed[T, In]], DirReg[Directed[T, Out]]] with {}
  
  // Invalid connections should not have givens, causing compile errors

// Connection operator - target := source
extension [A](target: A)
  def :=[B](source: B)(using conn: Connect[A, B]): Unit = conn.connect(target, source)

// Example Bundle with directions
final case class HandshakeIO(
  valid: Directed[Bool, Out],
  ready: Directed[Bool, In],
  data: Directed[UInt, Out]
) extends Bundle

// Syntactic sugar
def Input[T <: ValueType](value: T): Directed[T, In] = value.in
def Output[T <: ValueType](value: T): Directed[T, Out] = value.out

// Working example
@main def workingDemo(): Unit =
  println("=== Working Direction Example ===")
  
  // Create bundles with directions
  val producer = HandshakeIO(
    valid = Output(Bool()),
    ready = Input(Bool()),
    data = Output(UInt(Width(8)))
  )
  
  val consumer = HandshakeIO(
    valid = Output(Bool()),
    ready = Input(Bool()),
    data = Output(UInt(Width(8)))
  )
  
  // Create directed registers
  val prodReg = DirReg(producer)
  val consReg = DirReg(consumer)
  
  println(s"Producer: $prodReg")
  println(s"Consumer: $consReg")
  
  // Field access preserves types
  val prodValid: DirReg[Directed[Bool, Out]] = prodReg.valid
  val consReady: DirReg[Directed[Bool, In]] = consReg.ready
  
  println(s"Producer valid: $prodValid")
  println(s"Consumer ready: $consReady")
  
  // Valid connection - this compiles and runs
  consReg.ready := prodReg.valid  // Input (ready) driven by Output (valid)
  
  // This would NOT compile (if you uncomment):
  // consReg.ready := prodReg.ready  // Error: no given Connect[DirReg[Directed[Bool, In]], DirReg[Directed[Bool, In]]]
  
  // For literals, we can still use your existing approach
  // Just need a type alias to strip directions
  type StripDir[T] = T match
    case Directed[t, _] => t
    case _ => T
    
  type HostTypeOfDir[T] = T match
    case Directed[t, _] => HostTypeOf[t]
    case _ => HostTypeOf[T]
  
  // This shows how literals could work with minimal changes
  println("\n=== Literals with Directions ===")
  
  // The literal strips away direction info
  val handshakeLit = Lit[UInt](42)  // For individual fields
  println(s"Literal value: ${handshakeLit.get}")
  
  // For bundles, we'd need a small wrapper
  type HandshakeHostType = (
    valid: Boolean,
    ready: Boolean, 
    data: Int
  )
  
  val bundleLit = (
    valid = false,
    ready = true,
    data = 123
  )
  
  println(s"Bundle literal: $bundleLit")
  
  println("\n=== Key Insights ===")
  println("1. Simple case class for Directed preserves all type info")
  println("2. Type classes (givens) control valid connections")
  println("3. Extension methods make API ergonomic")
  println("4. Existing Reg/Lit can be adapted with minimal changes")
  println("5. Compile-time safety without runtime overhead")
