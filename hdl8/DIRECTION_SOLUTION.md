# Chisel-like HDL Direction Encoding Solution for Scala 3

## Executive Summary

This document presents a practical solution for encoding bundle directionality at the type level in your Scala 3 HDL while preserving ergonomic APIs and your existing literal syntax.

## Recommended Implementation Approach

After exploring multiple approaches, I recommend a **hybrid solution** that combines:

1. **Simple case classes** for direction wrappers (not opaque types initially)
2. **Type classes (givens)** for connection validation
3. **Extension methods** for ergonomic API
4. **Minimal changes** to existing Reg/Lit implementation

## Core Implementation

### 1. Direction Types

```scala
// Direction phantom types
sealed trait Dir
sealed trait In extends Dir
sealed trait Out extends Dir
sealed trait BiDir extends Dir

// Directed wrapper - simple case class
case class Directed[+T <: ValueType, D <: Dir](value: T)
```

### 2. Ergonomic Creation

```scala
// Extension methods for clean syntax
extension [T <: ValueType](value: T)
  def in: Directed[T, In] = Directed[T, In](value)
  def out: Directed[T, Out] = Directed[T, Out](value)
  def bidir: Directed[T, BiDir] = Directed[T, BiDir](value)

// Even cleaner with Width
extension (w: Width)
  def input: Directed[UInt, In] = UInt(w).in
  def output: Directed[UInt, Out] = UInt(w).out

extension (b: Bool.type)
  def input: Directed[Bool, In] = Bool().in
  def output: Directed[Bool, Out] = Bool().out
```

### 3. Bundle Definition

```scala
// Your bundles now look like this:
final case class DecoupledIO[T <: ValueType](
  ready: Directed[Bool, In],
  valid: Directed[Bool, Out],
  bits: Directed[T, Out]
) extends Bundle

// Or even cleaner:
final case class SimpleIO(
  clock: Directed[Bool, In],    // or Bool.input
  reset: Directed[Bool, In],    // or Bool.input
  data: Directed[UInt, Out],   // or Width(32).output
  valid: Directed[Bool, Out]   // or Bool.output
) extends Bundle
```

### 4. Enhanced Reg (Wrapper Approach)

```scala
// Wrapper that delegates to existing Reg
class DirectedReg[T](reg: Reg[T]) extends Selectable:
  def value: T = reg.t
  
  transparent inline def selectDynamic(name: String): Any =
    val fieldReg = reg.selectDynamic(name).asInstanceOf[Reg[?]]
    new DirectedReg(fieldReg)

// Smart constructor
def DReg[T](value: T): DirectedReg[T] = new DirectedReg(Reg(value))
```

### 5. Connection Validation

```scala
// Type class for valid connections
trait Connectable[From, To]

object Connectable:
  // Valid: Output to Input
  given [T <: ValueType]: Connectable[
    DirectedReg[Directed[T, Out]], 
    DirectedReg[Directed[T, In]]
  ] = new Connectable[DirectedReg[Directed[T, Out]], DirectedReg[Directed[T, In]]] {}
  
  // Valid: BiDir to anything
  given [T <: ValueType, D <: Dir]: Connectable[
    DirectedReg[Directed[T, BiDir]], 
    DirectedReg[Directed[T, D]]
  ] = new Connectable[DirectedReg[Directed[T, BiDir]], DirectedReg[Directed[T, D]]] {}

// Connection operator
extension [To](target: To)
  def :=[From](source: From)(using Connectable[From, To]): Unit =
    println(s"Connecting $source => $target")
    // Actual connection logic here
```

### 6. Preserving Literal Syntax

```scala
// Type to extract host type without directions
type StripDirections[T] = T match
  case Directed[t, _] => t
  case _ => T

// Enhanced HostTypeOf
type DirectedHostTypeOf[T] = T match
  case Directed[t, _] => HostTypeOf[t]
  case _ => HostTypeOf[T]

// Literals work exactly as before!
val io = DecoupledIO[UInt](
  ready = Bool().in,
  valid = Bool().out,
  bits = UInt(Width(32)).out
)

val ioLit = Lit[DecoupledIO[UInt]]((
  ready = true,      // No direction needed!
  valid = false,
  bits = 42
))
```

## Migration Strategy

### Phase 1: Add Direction Support (Non-Breaking)
1. Add direction types and wrappers
2. Create DirectedReg wrapper
3. Allow both directed and non-directed bundles

### Phase 2: Gradual Adoption
1. Update bundles to use directions
2. Add connection validation
3. Deprecate non-directed connections

### Phase 3: Full Migration
1. Require directions on all ports
2. Remove support for non-directed bundles
3. Optimize implementation

## Example Usage

```scala
// Define a directed bundle
final case class ProcessorIO(
  instruction: Directed[UInt, Out],
  data: Directed[UInt, BiDir],
  interrupt: Directed[Bool, In]
) extends Bundle

// Create instances
val cpu = ProcessorIO(
  instruction = Width(32).output,
  data = Width(32).bidir,
  interrupt = Bool.input
)

// Create registers
val cpuReg = DReg(cpu)

// Type-safe connections
val memReg = DReg(MemoryIO(...))
cpuReg.instruction := memReg.readData  // OK: Out to In
// cpuReg.interrupt := memReg.readData // Compile error: Out to In

// Literals work unchanged
val cpuLit = Lit[ProcessorIO]((
  instruction = 0x1000,
  data = 0xDEADBEEF,
  interrupt = false
))
```

## Advanced Features (Future)

1. **Flipped Bundles**: Automatically flip all directions
2. **Bulk Connections**: Connect entire bundles with matching fields
3. **Direction Inference**: Infer directions from usage
4. **Conditional Directions**: Parameterize directions

## Benefits

1. **Type Safety**: Invalid connections caught at compile time
2. **Zero Runtime Overhead**: Uses Scala's type system
3. **Backward Compatible**: Gradual migration path
4. **Ergonomic**: Clean, readable syntax
5. **Preserves Existing Features**: Named tuple literals work unchanged

## Conclusion

This solution provides compile-time direction checking while maintaining the ergonomics of your existing API. The gradual migration path allows you to adopt it incrementally without breaking existing code.
