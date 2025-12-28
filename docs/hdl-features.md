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

See `enumBasicExample()` in hdl/test/src/ModuleChecksSpec.scala:1710 for a complete example demonstrating:
- Defining Scala enums and using them as hardware types with `HWEnum[T]`
- Using `.EN` to get hardware enum values from Scala enum cases
- Comparing and assigning enum values in conditional blocks

Enums work seamlessly with switch statements. See `enumSwitchExample()` in hdl/test/src/ModuleChecksSpec.scala:1774 for an example showing how switch/is provides cleaner syntax for multi-way branches with enums.

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

See `sramExample()` in hdl/test/src/ModuleChecksSpec.scala:1615 for a complete example demonstrating:
- Creating an SRAM with read, write, and read-write ports
- Using `readPorts` for read-only access
- Using `writePorts` for write-only access
- Using `readwritePorts` for combined read/write access
