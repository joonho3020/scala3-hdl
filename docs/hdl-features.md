# Main Features of the HDL

The syntax of this HDL is mostly similar to Chisel.
This documentation lists the major API changes.

## Caching & Parallel Elaboration

This HDL uses multiple threads to speed up elaboration.
Also, when module instances with the same parameters are detected during elaboration, subsequent elaborations of such instances are skipped.
Finally, elaboration artifacts are cached by tracking classfile dependencies.

## Bundle

Bundles are defined as case classes in contrast to structural types like in Chisel.
This enables a tighter LSP integration as case classes generates stable compiler symbols.
Furthermore, this enables the library users to reuse the Bundle schema to define hardware, literals, and testbench interfaces using typeclass derivation.

## Native Enum Support

We can directly create hardware enum types from Scala enums.
This integrates nicely with the switch statements.

```scala
// Source: enumBasicExample() in hdl/test/src/ModuleChecksSpec.scala:1710
// This example demonstrates:
// - Defining Scala enums and using them as hardware types with HWEnum[T]
// - Using .EN to get hardware enum values from Scala enum cases
// - Comparing and assigning enum values in conditional blocks

enum TestEnumOpcode:
  case Idle, Run, Wait

final case class EnumIO(in: HWEnum[TestEnumOpcode], out: HWEnum[TestEnumOpcode]) extends Bundle[EnumIO]

class EnumModule extends Module:
  given Module = this
  val io = IO(EnumIO(Input(HWEnum(TestEnumOpcode)), Output(HWEnum(TestEnumOpcode))))
  val reg = RegInit(TestEnumOpcode.Idle.EN)
  when(io.in.asUInt === TestEnumOpcode.Run.EN.asUInt) {
    reg := TestEnumOpcode.Wait.EN
  }
  io.out := reg
```

Enums work seamlessly with switch statements:

```scala
// Source: enumSwitchExample() in hdl/test/src/ModuleChecksSpec.scala:1774

class SwitchEnum extends Module:
  given Module = this
  val io = IO(SwitchEnumIO(Input(HWEnum(TestEnumOpcode)), Output(HWEnum(TestEnumOpcode))))
  val reg = RegInit(TestEnumOpcode.Idle.EN)
  reg switch {
    is(TestEnumOpcode.Idle.EN) { reg := TestEnumOpcode.Run.EN }
    is(TestEnumOpcode.Run.EN) { reg := TestEnumOpcode.Wait.EN }
    default { reg := TestEnumOpcode.Idle.EN }
  }
  io.out := reg
```

Using typeclass derivation, switch statements can also accept tuples as well, similar to how Scala3's match accepts tuples.
For example, we can do this:

```scala
switch ((funct3, funct7)) {
    is ((0.U,    0.U)) { op :=  FN_ADD.EN }
    is ((0.U, 0x20.U)) { op :=  FN_SUB.EN }
    is ((1.U,    0.U)) { op :=   FN_SL.EN }
    is ((2.U,    0.U)) { op :=  FN_SLT.EN }
    is ((3.U,    0.U)) { op := FN_SLTU.EN }
    is ((4.U,    0.U)) { op :=  FN_XOR.EN }
    is ((5.U,    0.U)) { op :=   FN_SR.EN }
    is ((5.U, 0x20.U)) { op :=  FN_SRA.EN }
    is ((6.U,    0.U)) { op :=   FN_OR.EN }
    is ((7.U,    0.U)) { op :=  FN_AND.EN }
    default { op := DontCare }
}
```

## SRAM APIs

SRAM ports are not inferred by the compiler.
However, it still provides behavioral APIs to perform read/writes.

```scala
// Source: sramExample() in hdl/test/src/ModuleChecksSpec.scala:1615
// This example demonstrates:
// - Creating an SRAM with read, write, and read-write ports
// - Using readPorts for read-only access
// - Using writePorts for write-only access
// - Using readwritePorts for combined read/write access

final case class SramIO(rData: UInt, rwData: UInt) extends Bundle[SramIO]

class SramModule extends Module:
  given Module = this
  val io = IO(SramIO(
    rData = Output(UInt(Width(8))),
    rwData = Output(UInt(Width(8)))
  ))
  val mem = SRAM(UInt(Width(8)), 4)(1, 1, 1)
  val r = mem.readPorts(0).read(1.U(Width(2)))
  io.rData := r
  mem.writePorts(0).write(2.U(Width(2)), 5.U(Width(8)))
  val rw = mem.readwritePorts(0).read(3.U(Width(2)))
  mem.readwritePorts(0).writeData := 0.U(Width(8))
  io.rwData := rw
```
