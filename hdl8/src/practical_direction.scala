package hdl8.practical

import hdl8.*
import scala.compiletime.*
import scala.deriving.*

// sealed trait Direction
// case object Input extends Direction
// case object Output extends Direction
// case object Bidirectional extends Direction
// 
// case class Directed[T <: ValueType](value: T, direction: Direction)
// 
// type In[T <: ValueType] = Directed[T]
// type Out[T <: ValueType] = Directed[T]
// type BiDir[T <: ValueType] = Directed[T]
// 
// extension [T <: ValueType](value: T)
//   def asInput: In[T] = Directed(value, Input)
//   def asOutput: Out[T] = Directed(value, Output)
//   def asBidir: BiDir[T] = Directed(value, Bidirectional)
//   def in: In[T] = asInput
//   def out: Out[T] = asOutput
//   def io: BiDir[T] = asBidir
// 
// extension (w: Width)
//   def in: In[UInt] = UInt(w).in
//   def out: Out[UInt] = UInt(w).out
// 
// object IO:
//   def input[T <: ValueType](value: T): In[T] = value.in
//   def output[T <: ValueType](value: T): Out[T] = value.out
//   def inout[T <: ValueType](value: T): BiDir[T] = value.io
// 
// class DReg[T](private val underlying: Reg[?], private val directedType: T) extends Selectable:
//   import scala.NamedTuple
//  
//   type Fields = T match
//     case Bundle => NamedTuple.Map[NamedTuple.From[T], [X] =>> DReg[X]]
//     case _ => EmptyTuple
// 
//   transparent inline def selectDynamic(name: String): Any =
//     inline directedType match
//       case bundle: Bundle =>
//         summonFrom {
//           case m: Mirror.ProductOf[T] =>
//             val labels = constValueTuple[m.MirroredElemLabels].toArray
//             val idx = labels.indexOf(name)
//             val childValue = bundle.asInstanceOf[Product].productElement(idx)
//             val childReg = underlying.selectDynamic(name).asInstanceOf[Reg[?]]
//             new DReg(childReg, childValue)
//           case _ =>
//             throw new NoSuchElementException(s"No field $name")
//         }
//       case _ =>
//         throw new NoSuchElementException(s"Not a bundle")
// 
//   def value: T = directedType
//   override def toString: String = s"DReg($directedType)"
// 
// object DReg:
//   def apply[T](value: T): DReg[T] = 
//     val reg = value match
//       case Directed(v, _) => Reg(v)
//       case v: ValueType => Reg(v)
//       case _ => throw new IllegalArgumentException(s"Cannot create DReg from $value")
//     new DReg(reg, value)
// 
// enum ConnectionRule:
//   case Valid
//   case Invalid(reason: String)
// 
// object ConnectionRule:
//   def check(from: Direction, to: Direction): ConnectionRule =
//     (from, to) match
//       case (Output, Input) => Valid
//       case (Bidirectional, _) => Valid
//       case (_, Bidirectional) => Valid
//       case (Input, Input) => Invalid("Cannot connect input to input")
//       case (Output, Output) => Invalid("Cannot connect output to output")
//       case (Input, Output) => Invalid("Cannot drive output from input")
// 
// def connect[T <: ValueType](from: DReg[Directed[T]], to: DReg[Directed[T]]): Unit =
//   ConnectionRule.check(from.value.direction, to.value.direction) match
//     case ConnectionRule.Valid =>
//       println(s"✓ Connecting ${from.value.direction} => ${to.value.direction}")
//     case ConnectionRule.Invalid(reason) =>
//       throw new IllegalArgumentException(s"Invalid connection: $reason")
// 
// extension [T <: ValueType](to: DReg[Directed[T]])
//   def :=(from: DReg[Directed[T]]): Unit = connect(from, to)
//   def <>(other: DReg[Directed[T]]): Unit = // bidirectional
//     if to.value.direction == Bidirectional && other.value.direction == Bidirectional then
//       connect(other, to)
//     else
//       throw new IllegalArgumentException("Both sides must be bidirectional for <> operator")
// 
// type BaseValueType[T] = T match
//   case Directed[t] => t
//   case _ => T
// 
// def DLit[T](value: Any): Lit[?] =
//   value match
//     case v: Int => Lit[UInt](v)
//     case v: Boolean => Lit[Bool](v)
//     case v: Product =>
//       Lit[Bundle](v.asInstanceOf[HostTypeOf[Bundle]])
//     case _ => throw new IllegalArgumentException(s"Cannot create literal from $value")


@main def practicalDemo(): Unit =

  final case class HandshakeIO[T <: ValueType](
    valid: Out[Bool],
    ready: In[Bool],
    data: Out[T]
  ) extends Bundle

  final case class MemoryPort(
    addr: Out[UInt],
    writeData: Out[UInt], 
    writeEn: Out[Bool],
    readData: In[UInt]
  ) extends Bundle

  final case class CPU(
    imem: Out[MemoryPort],
    dmem: Out[MemoryPort],
    interrupt: In[Bool]
  ) extends Bundle

  val producer = HandshakeIO[UInt](
    valid = Bool().out,
    ready = Bool().in,
    data = Width(32).out
  )

  val consumer = HandshakeIO[UInt](
    valid = Bool().out,
    ready = Bool().in,
    data = Width(32).out
  )

  val prodReg = Reg(producer)
  val consReg = Reg(consumer)

  // For Reg[Bundle], we should be able to both read and write to each field
  // regardless of the field directionality.

  // This should succeed - the represents that we are reading the value of prodReg.valid
  // for this cycle and assigning it to consReg.ready which will show up in the next cycle
  consReg.ready := prodReg.valid

  // This should succeed - this represents an assignment to prodReg.valid
  // The assigned value should be visible in the next cycle as the register output
  prodReg.valid := Lit[Bool](true)

  // NOTE:
  // Getting type checking for directionality actually seems very difficult
  // You specify the directionality for IO
  // For registers and wires, a field can be both read and written to.
  // For single field assignments, being on the LHS means written to, and RHS means reading from
  // For bulk connections, even the above doesn't hold anymore. All we can check is that the direction of each field is different.
  // But at the same time, why would you perform bulk connections between different bundle types? Shouldn't this be disallowed in the first place?
  // To perform a true directionality check, we need to perform a graph traversal and check that all the "flows" are correct.
  // This is the same complexity as width inference and perhaps should not be checked during compile time in the first place.

  val cpu = CPU(
    imem = MemoryPort(
      addr = Width(32).out,
      writeData = Width(32).out,
      writeEn = Bool().out,
      readData = Width(32).in
    ).out,
    dmem = MemoryPort(
      addr = Width(32).out,
      writeData = Width(32).out,
      writeEn = Bool().out,
      readData = Width(32).in
    ).out,
    interrupt = Bool().in
  )

  val cpu_flip = cpu.flip
  // Type of cpu_flip should be
  // case class CPU(
  //   imem: In[MemoryPort],
  //   dmem: In[MemoryPort],
  //   interrupt: Out[Bool]
  // ) extends Bundle
