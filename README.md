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

---

## API Sketch

```scala
def instantiation_check(): Unit =
  final case class InnerBundle(a: UInt, b: UInt) extends Bundle
  final case class MyBundle(x: UInt, y: UInt, i: InnerBundle) extends Bundle

  val mb = MyBundle(UInt(Width(2)), UInt(Width(3)), InnerBundle(UInt(Width(4)), UInt(Width(5))))
  val reg = Reg(mb)

  val reg_x: UInt = reg.x
  val reg_y: UInt = reg.y
  val reg_i: InnerBundle = reg.i
  val reg_i_a: UInt = reg_i.a

  val ulit = Lit(UInt(Width(3)))(3)
  assert(ulit.getValue == 3)

// We would like to derive the Literal definitions from Bundle types
def literal_check(): Unit =
  val ilit = Lit(InnerBundle(UInt(Width(4)), UInt(Width(5))))((a = 3, b = 4))
  val ilit_a: UInt = ilit.a

  assert(ilit.a.getValue == 3)
  assert(ilit_a.getValue == 3)
  assert(ilit.getValue == (3, 4))

  val mylit = Lit(MyBundle(
    x = UInt(Width(2)),
    y = UInt(Width(3)),
    i = InnerBundle(UInt(Width(4)), UInt(Width(5)))
  ))((
    x = 2,
    y = 3,
    i = (a = 4, b = 5)))

  val mylit_x: UInt = mylit.x
  assert(mylit.x.getValue == 2)
  assert(mylit_x.getValue == 2)

  val mylit_i: InnerBundle = mylit.i
  assert(mylit_i.getValue == (4, 5))
  assert(mylit.i.getValue == (4, 5))
  assert(mylit.i.a.getValue == 4)
  assert(mylit.i.b.getValue == 5)

// Flexible metaprogramming is key.
// We want to be able to use Scala library functions and datastructures
// to describe our hardware.
def list_operation_check(): Unit =
  final case class MultBySumIO(a: UInt, b: UInt, sum: UInt) extends Bundle
  object MultBySumIO:
    def apply(w: Int): MultBySumIO =
      MultBySumIO(
        a = Input(UInt(Width(w))),
        b = Input(UInt(Width(w))),
        sum = Output(UInt(Width(w)))
      )

  class MultBySum(width: Int, maxMult: Int) extends Module:
    given Module = this
    val io = IO(MultBySumIO(width))

    val wires = Seq.fill(maxMult)(Wire(UInt(Width(width))))

    // Using `foreach` we can connect wires
    wires.foreach(_ := io.a)

    wires(0) := wires(1) + Lit(UInt(Width(width)))(3)

    // Using `reduce` to perform reduction
    io.sum := wires.reduce(_ + _)

// We want inheritance to work
// The body of the Concrete class is essentially this:
//  val io = IO(
//    SimpleIO(Input(UInt(Width(4))), Output(UInt(Width(4))))
//  )
//  io.out := io.in
//  val add_result = io.in + io.in + io.in
//  io.out := add_result
//
def inheritance_check(): Unit =
  class Abstract extends Module:
    given Module = this
    val io = IO(
      SimpleIO(Input(UInt(Width(4))), Output(UInt(Width(4))))
    )
    io.out := io.in

  class Concrete extends Abstract:
    val add_result = io.in + io.in + io.in
    io.out := add_result

// Type parameterization of Modules are crucial.
// For components such as hardware queues, being able to support different types
// signficantly increases code reuse.
def type_parameterization_check(): Unit =
  final case class SimpleIOT[T <: ValueType](in: T, out: T) extends Bundle
  class TypeParamModule[T <: ValueType](
    val t: T
  )(
    using DirLike[T]
  ) extends Module:
    given Module = this
    val io = IO(SimpleIOT[T](
      in = Input(t),
      out = Output(t)
    ))
    io.out := io.in

// Based on arguments provided to the Module, different hardware must be generated
// If `add` is true, only `io.out := io.in + io.in` should be generated.
// Otherwise, only `io.out := io.in` should be generated.
def conditional_generation_check(): Unit =
  class A(add: Boolean) extends Module:
    given Module = this
    val io = IO(SimpleIO(Input(UInt(Width(4))), Output(UInt(Width(4)))))
    if (add)
      io.out := io.in + io.in
    else
      io.out := io.in

// We want to support ad-hoc Bundles using NamedTuples without defining a separate `case class`
def adhoc_io_check(): Unit =
  class A(w: Int) extends Module:
    given Module = this
    val io = IO(Bundle((
      a = Input(UInt(Width(w))),
      b = Input(UInt(Width(w))),
      c = Output(UInt(Width(w + 1)))
    )))
    io.c := io.a + io.b

// Being able to use Scala level datastructures and types are crucial for metaprogramming.
// In this example, we are using the `Option` type to conditionally generate hardware
def optional_io_check(): Unit =
   class A(debug: Boolean, w: Int) extends Module:
     given Module = this
     case class MyBundle(
       a: Option[UInt],
       b: UInt,
       c: UInt) extends Bundle

     val io = IO(MyBundle(
       a = if (debug) Some(Input(UInt(Width(w)))) else None,
       b = Input(UInt(Width(w))),
       c = Output(UInt(Width(w + 1)))
     ))
     io.c := io.b
     io.a.map(x => {
       io.c := x + io.b
     })
```

## Uncertain APIs

```scala
// Does this even make sense???????
def mixed_bundle(): Unit =
    val mixed_bundle = Bundle(
        a = UInt(Width(2)),
        b = Lit(UInt(Width(3)))(4)
    )
```

---


## Development Notes

### Trial 1

Can't separate base types literals `UInt` from `UIntLit` if we want typeclass derivation based Literal assignments

Suppose we do have separate `UIntLit` types

```scala
final case class InnerBundle(a: UInt, b: SInt) extends Bundle
```

The type signature of the literal version of `InnerBundle` will be:

```scala
final case class InnerBundleLit(a: UIntLit, b: SIntLit) extends Bundle
```

In Scala, there is no way of generating a `case class` during compile time.
If we really want this approach, we will have to perform source code generation, which is undesirable.
A compiler plugin also won't work.
This is because in order to generate `case class InnerBundleLit` from `case class InnerBundle`, the AST must have resolved types before we run our compiler pass.
How else will the pass figure out whether `a` has to change to `UIntLit` and `b` has to change to `SIntLit`?
However, at this point, if we have tried using `InnerBundleLit` to describe our program, the type checking pass would have failed.
A huge connundrum!

### Trial 2

We can design an HDL like Zaozi where there are value types (e.g., `UInt`, `SInt`, `Bool`) and wrapper types (e.g., `Reg`, `Wire`, `SRAM`, `IO`).
This is nice in that these types serve as documentation, integrates nicely with LSP, and that it provides a coherent implementation with Literal types discussed above (i.e. Lit, Reg, Wire are all wrappers around the ValueTypes such as UInt, SInt, and Bundles)

Implementation of `Reg`:

```scala
final class Reg[T](val t: T, val name: String = "") extends Selectable with TypedConnectable[T]:
  type Fields = NamedTuple.Map[
    NamedTuple.From[T],
    [X] =>> Reg[X & ValueType]]

  def innerType: T = t
  def refName: String = name

  inline def selectDynamic(fieldName: String): Reg[?] =
    summonFrom {
      case m: Mirror.ProductOf[T] =>
        val labels = constValueTuple[m.MirroredElemLabels].toArray
        val idx = labels.indexOf(fieldName)
        val child = t.asInstanceOf[Product].productElement(idx).asInstanceOf[ValueType]
        val childName = if name.isEmpty then fieldName else s"${name}.$fieldName"
        new Reg(child, childName)
      case _ =>
        throw new NoSuchElementException(s"${t.getClass.getName} has no field '$fieldName'")
    }
  override def toString(): String =
    s"Reg($t, $name)"
```

Usage:

```scala
final case class InnerBundle(a: UInt, b: UInt) extends Bundle
final case class MyBundle(x: UInt, y: UInt, i: InnerBundle) extends Bundle
val mb = MyBundle(UInt(Width(2)), UInt(Width(3)), InnerBundle(UInt(Width(4)), UInt(Width(5))))

val reg = Reg(mb)
val reg_x: Reg[UInt] = reg.x
val reg_y: Reg[UInt] = reg.y
val reg_i: Reg[InnerBundle] = reg.i
val reg_i_a: Reg[UInt] = reg_i.a
val reg_i_b: Reg[UInt] = reg.i.b
```

The downside is that various Scala level list operations become difficult.
Below is an example where using Scala `Seq`'s `reduce` operation results in a compiler error.
This is because the type of `c.io.out` is an `IO[UInt]` type, the result of `+` is an `Node[UInt]` type while the type signature of `reduce` is `(B, B) => B`, requiring that the operator and result type of `+` is the same.

```scala
class Fanout(level: Int, fanout: Int) extends Module:
val io = IO(LinkIO(
  in = Input(UInt(Width(4))),
  out = Output(UInt(Width(4)))
))
def body(using ctx: ElabContext): Unit =
  val children = (0 until fanout).map(i => Module(new Leaf(level * 10 + i)))
  io.out := children.map(c => c.io.out).reduce(_ + _)
```

Considering that having metaprogramming capabilities using the host language datastructures (especially lists) is a key aspect of building an embedded DSL, anything HDL feature that hinders this should be avoided.
The benefit of having types wrap `ValueType`s is also quite dubious except that it enables a better LSP support.


Takeaway:

- Having separate wrapped types (e.g. `Reg`, `Wire`, `IO` are all different types that take `UInt`, `Bool`, `Bundle` as type-parameters), make list operations such as `reduce` difficult due to the type signature mismatch

### Trial 3

So what about we have a single class that represents all possible hardware structures instead?
Wrapping results with a type (e.g. `Node`) after `selectDynamic` for subfield access is undesirable as it hinders mixing with Scala types (e.g. `Option`)

```scala
final case class Node[T](
  tpe: T,
  kind: NodeKind,
  name: Option[String] = None,
  literal: Option[Any] = None
) extends Selectable:
  type Fields = NamedTuple.Map[NamedTuple.From[T], [X] =>> X]

  transparent inline def selectDynamic[L <: String & Singleton](inline label: L) =
    summonFrom {
      case m: Mirror.ProductOf[T] =>
        type Labels = m.MirroredElemLabels
        type Elems = m.MirroredElemTypes
        type FT = FieldTypeFromTuple[Labels, Elems, L]
        val labels = constValueTuple[Labels].toArray
        val idx = labels.indexOf(constValue[L])
        if idx < 0 then throw new NoSuchElementException(s"${tpe.getClass.getName} has no field '${label}'")
        val childT = tpe.asInstanceOf[Product].productElement(idx).asInstanceOf[FT]
        val childLit = literal.map(_.asInstanceOf[Product].productElement(idx))
        Node(childT, kind, None, childLit)
      case _ =>
        throw new NoSuchElementException(s"${tpe.getClass.getName} has no field '${label}'")
    }

def optional_io_check(): Unit =
   class A(debug: Boolean, w: Int) extends Module:
     given Module = this
     case class MyBundle(
       a: Option[UInt],
       b: UInt,
       c: UInt) extends Bundle

     val io = IO(MyBundle(
       a = if (debug) Some(Input(UInt(Width(w)))) else None,
       b = Input(UInt(Width(w))),
       c = Output(UInt(Width(w + 1)))
     ))
     io.c := io.b
     io.a.map(x => {
       io.c := x + io.b
     })
```

In the above example, the type of `io.a` is `Node[Option[UInt]]`, hence we cannot use `io.a.map`.

Takeaway:

- Subfield access should return the type of the subfield unmodified


### Trial 4

#### Plan for Bundle, Reg, and Lit

- Keep user-facing types as plain Scala shapes (`UInt`, `Bool`, `Bundle case classes`, `NamedTuples`) so subfields retain their Scala types and work with `Option`/`Seq` APIs
- Track hardware metadata on leaf values (kind, name, literal) or in a side table. Avoid wrapping subfields in a generic `Node` that changes their types.

## Bundle

- Provide a marker trait `Bundle` for case-class bundles and a `Bundle.apply` helper for ad-hoc NamedTuple bundles; both should preserve field names for reflection.
- Define a `Shape[T]`/`Traverse[T]` typeclass (derived via Mirror for products and by hand for Option/Seq/NamedTuple) to map/zip leaves while rebuilding the same outer type.
- Use the shape to derive `HostTypeOf[T]` (host literal form) and to preserve field order for literal construction and subfield access.

## Reg

- Implement `Reg[T](t: T): T` by traversing `t` with `mapLeaves`, cloning each `LeafValue`, setting `kind = Reg`, and recording hierarchical names.
- For bundles/collections, reconstruction yields the same Scala types; e.g., `reg.x` has type `UInt` (not `Reg[UInt]`), enabling Scala list operations.

## Lit

- Implement `Lit[T](t: T)(payload: HostTypeOf[T]): T` by zipping the type skeleton `t` with the host payload via `zipLeaves`, producing the same Scala shape with each leaf marked `kind = Lit` and carrying the literal bits.
- Provide `getValue` on leaves plus an extension to rebuild host values for structured types, so nested bundle literals work (`mylit.i.a.getValue`).

## Shared traversal utilities

- Core operations: `mapLeaves(t)(f)` and `zipLeaves(t, host)(f)` derived from `Shape[T]`, reusable for Wire/IO/direction flipping.
- Maintain a small `LeafKind` enum and optional `name` on leaves for IR emission; direction flipping can be another traversal using the same shape metadata.

## Open questions

- Decide whether literal payloads live on leaves or in a side table keyed by identity.
- Settle on naming strategy (path-based vs. user-specified) during traversal for better emitted IR readability.
