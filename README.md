# Yet Another HDL Attempt

## Modules

### Goals

- Connection operations between hardware components should perform type checking (e.g. don't want to allow connecting `UInt` to `Bool`)
- Want to use Scala's built-in metaprogramming features as much as possible, especially regarding list operations
    - `reduce` `foreach` operations for hardware constructs should work
- Inheritance btw modules. Subclass module should contain HW logic created in the parent class and should be able to add logic
- Bundles, Modules should allow type parameterization
- Elaboration of modules should allow parallel execution across threads. Also, should be able to serialize modules and check whether it hits a cache. If a module is instantiated with different parameters, this should elaborate each instance separately

### Implementation details

- Want to capture the scala variable names as macros and use that to set IR node names as much as possible.

### IO Bundles

- IO bundles should encode directionality
- IO bundles should be able to support ad-hoc field additions (not just some case class), perhaps this can be achieved by named-tuples

## TODO

- more operators
- vectors and heterogenous vectors
- behavioral statements
- memories

## Some Commands

```bash
mill hdl.runMain hdl.demo
```
