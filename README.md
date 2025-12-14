# Yet Another HDL Attempt

## Implementation details

- Connection operations between hardware components should perform type checking (e.g. don't want to allow connecting `UInt` to `Bool`)
- Want to use Scala's built-in metaprogramming features as much as possible, especially regarding list operations
    - `reduce` `foreach` operations for hardware constructs should work
- Inheritance btw modules. Subclass module should contain HW logic created in the parent class and should be able to add logic
- Bundles, Modules should allow type parameterization
- Module elaboration must happen in parallel (i.e. it should be multithreaded) and should be able to reuse cached artifacts from previous builds
    - When a single Module is instantiated multiple times *with the same parameters (or argument values)*, it reuse the previously computed result
    - When a single Module is instantiated multiple times *with different parameters*, it should elaborate the design with the new parameter values
    - In the build-cache, when there is a Module that has the same bytecode as well as the same instantiation parameters, it should reuse the value in the cache instead of performing elaboration again
- Bundles should not be structural types. Rather, we want to have Bundles as case classes so that we can derive various Bundle definitions (e.g. Literals) from the case class
- We want to preserve the strong metaprogramming experience that Chisel provides:
    - Must be able to mix in Scala datastructures (e.g. Seq, Option, List, ...) with the hardware description
    - Use Module inheritance to share common logic across slightly different modules
    - Parameterize Modules, Bundles, etc using type parameters (e.g. `Queue[T <: HWData]` where T can be a primitive HW type or a Bundle)
- The element names in the elaborated output should resemble that of the Scala source as much as possible. This can be achieved by using macros like this:

```scala
  private def findEnclosingValName(using Quotes): Option[String] =
    import quotes.reflect.*
    def loop(sym: Symbol): Option[String] =
      if sym.isNoSymbol then None
      else if sym.isValDef && !sym.flags.is(Flags.Synthetic) && !sym.flags.is(Flags.Artifact) then Some(sym.name)
      else loop(sym.owner)
    loop(Symbol.spliceOwner)
```

- IO bundles should encode directionality
- ~~IO bundles should be able to support ad-hoc field additions (not just some case class), perhaps this can be achieved by named-tuples~~
    - Currently we are passing the `case class` as a type parameter to a `Bundle` trait. It would be nice if we can define anonymous bundles using named-tuples, but it seems like there is no good way of passing the shape of these as type parameters

## TODO

- [x] parallel elaboration & caching support
- [x] behavioral statements
- [x] proper IR emission
- [x] vectors
    - How are vectors supported in Chisel?
    - Should support indexing of vector structures using hardware types...?
    - Emission shouldn't flatten these structures
- [x] log2Ceil on integer operations for calculating widths
- [x] Clock and Reset types
- [x] Connect Reg to implicit clock signals
- [x] RegInit, WireInit support (connect to reset)
- [x] Add support for UInt without width (so that we can defer width determination during width inference pass)
- [x] Unknown width support
- [x] Operators
    - [x] on two hardware data : add, subtract, mult, division, remainder, less than, less or equal to, greater than, greater or equal to, equal, not equal, dynamic shift left, dynamic shift right, and, or, xor, concatentate
    - [x] on one hardware data and one integer: pad, shift left, shift right, head, tail
    - [x] on one hardware data and two integers: bits
    - [x] Concatenation, bitwise or/and reduction, mux implementation
- [x] Test against reference IR
- [x] Conversion operators
- [ ] Memories
    - Make the port API explicit (no inferred ports)
- [ ] Lazy elaboration (diplomacy like 2-step elaboration)
- [ ] Mixed vectors
- [ ] Per-class caching??
    - Currently, the caching is on a file basis as we are relying on hasing classfiles. Although this is robust, we can achieve finer-grained incrementalism by other methods

## Misc Notes

- Better enum support?
    - https://github.com/chipsalliance/chisel/issues/927

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
A huge conundrum!

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

Separate `BundleIf` from `Bundle` representation.
`case class`s will extend `BundleIf` and we will wrap `BundleIf` as `Bundle`s for setting literals and subfield access.


```scala
trait BundleIf

// NOTE: Literals assume that the BundleIf is pure. THat is, all fields are of HWData
// and there is no mixing with Scala's library types such as Option, Seq, List
class Bundle[T <: BundleIf](
  val tpe: T,
  var kind: NodeKind = NodeKind.Unset,
  var name: Option[String] = None,
  var literal: Option[Any] = None,
  private var _ref: String = ""
) extends Selectable with HWData:

  type FieldToNode[X] = X match
    case BundleIf => Bundle[X]
    case _           => X
// type FieldToNode[X] = X match
// case _           => X

  type Fields = NamedTuple.Map[NamedTuple.From[T], [X] =>> FieldToNode[X]]

  def setRef(ref: String) = _ref = ref
  def ref: String = _ref

  transparent inline def selectDynamic[L <: String & Singleton](label: L): Any =
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
        val childRef = if _ref.isEmpty then constValue[L] else s"$_ref.${constValue[L]}"
        inline erasedValue[FT] match
          case _: BundleIf =>
            new Bundle(tpe = childT.asInstanceOf[FT & BundleIf], literal = childLit)
          case _: HWData =>
            childLit.map(lit => childT.asInstanceOf[FT & HWData].setLitVal(lit))
            childT
          case _ =>
            childT
      case _ =>
        throw new NoSuchElementException(s"${tpe.getClass.getName} has no field '${label}'")
    }
```

There is a tension when using `BundleIf` and `Bundle`.

The first problem is that directionality is set on `Bundle`.
Hence, if we want to perform `x = Flipped(Bundle(bundle_a))`, the type of `x` must be `Bundle[A]`, not `A` (`final case class B(x: Bundle[A], y: Bool) extends BundleIf`).
However, this causes an error on `bundle_b_lit` because `Lit` expects to use `BundleIf` (i.e. the case class representing the bundle shapes) to create host types given a bundle type:

```scala
final case class A(a: UInt, b: UInt) extends BundleIf
final case class B(x: Bundle[A], y: Bool) extends BundleIf

val bundle_a = A(
  a = Input(UInt(Width(3))),
  b = UInt(Width(4))
)

assert(bundle_a.a.dir == Direction.In)
assert(bundle_a.b.dir == Direction.Out)

val bundle_b = B(
  x = Flipped(Bundle(bundle_a)),
  y = Output(Bool(()))
)
assert(bundle_b.x.a.dir == Direction.In)
assert(bundle_b.x.b.dir == Direction.Out)
assert(bundle_b.y.dir   == Direction.Out)

val bundle_b_lit = Lit(bundle_b)((
  x = (
    a = BigInt(1),
    b = BigInt(2),
  ),
  y = false))
```

We can't do this either because `Flipped` works on the `Bundle` type, not on `BundleIf`.
Hence, `bundle_b` will complain as we cannot call `Flipped` on `bundle_a` which is a `BundleIf`.

  ```scala
final case class A(a: UInt, b: UInt) extends BundleIf
final case class B(x: A, y: Bool) extends BundleIf

val bundle_a = A(
  a = Input(UInt(Width(3))),
  b = UInt(Width(4))
)

assert(bundle_a.a.dir == Direction.In)
assert(bundle_a.b.dir == Direction.Out)

val bundle_b = B(
  x = Flipped(bundle_a),
  y = Output(Bool(()))
)
// assert(bundle_b.x.a.dir == Direction.In)
// assert(bundle_b.x.b.dir == Direction.Out)
// assert(bundle_b.y.dir   == Direction.Out)

val bundle_b_lit = Lit(bundle_b)((
  x = (
    a = BigInt(1),
    b = BigInt(2),
  ),
  y = false))
```

Another problem with is approach is that it is hampered by various corner cases when trying to mix things with Scala types.


```scala
case class A(
  a: UInt,
  b: UInt
) extends BundleIf

case class B(
  x: UInt,
  y: A,
  z: Option[A],
) extends BundleIf

val b = Reg(Bundle(B))
val b_x: UInt = b.x // so far so good

val b_y: Bundle[A] = b.y // since A is of a BundleIf type, the return type is also wrapped as a Bundle

val b_z: Option[A] = b.z // Since z is of a Option type, it is returned without being wrapped as a Bundle
b_z.map(z: A => { // Now when we access z, the type is A, not Bundle[A]. Inconsistency between return types is not desirable
  z
})
```


### Trial 5

We could merge the `BundleIf` and `Bundle` interface into one like below:

```scala
// NOTE: Literals assume that the BundleIf is pure. That is, all fields are of HWData
// and there is no mixing with Scala's library types such as Option, Seq, List
trait Bundle[T] extends Selectable with HWData with HasDirectionality { self: T =>
  type FieldToNode[X] = X match
    case _           => X

  type Fields = NamedTuple.Map[NamedTuple.From[T], [X] =>> FieldToNode[X]]

  transparent inline def selectDynamic[L <: String & Singleton](label: L): Any =
    summonFrom {
      case m: Mirror.ProductOf[T] =>
        type Labels = m.MirroredElemLabels
        type Elems = m.MirroredElemTypes
        type FT = FieldTypeFromTuple[Labels, Elems, L]

        val labels = constValueTuple[Labels].toArray
        val idx = labels.indexOf(constValue[L])

        if idx < 0 then throw new NoSuchElementException(s"${self.getClass.getName} has no field '${label}'")
        val childT = self.asInstanceOf[Product].productElement(idx).asInstanceOf[FT]
        val childLit = literal.map(_.asInstanceOf[Product].productElement(idx))
        val childRef = constValue[L]
        println(s"label: ${label} childRef: ${childRef} childLit: ${childLit}")
        inline erasedValue[FT] match
          case _: HWData =>
            childLit.map(lit => childT.asInstanceOf[FT & HWData].setLitVal(lit))
            childT
          case _ =>
            childT
      case _ =>
        throw new NoSuchElementException(s"${self.getClass.getName} has no field '${label}'")
    }

  def setLitVal(payload: Any): Unit =
    this.literal = Some(payload.asInstanceOf[HostTypeOf[T]])

  def getLitVal: HostTypeOf[T] =
    literal match
      case Some(v) => v.asInstanceOf[HostTypeOf[T]]
      case None    => throw new NoSuchElementException("Node does not carry a literal value")
}
```


Now, the `Bundle` types can be mixed flexibly with the Scala library types (e.g. `Option`, `Seq`), without which destroys the point of an eDSL.
However, attentive readers will notice that accessing fields of `Bundle` instantiations will now just use the static `case class` fields instead of our `selectDynamic` function.
`selectDynamic` only works when there are no matching static fields.

```scala
def optional_io_directionality_check(): Unit =
  case class OptBundle(
    a: Option[UInt],
    b: UInt,
    c: UInt) extends Bundle[OptBundle]

  case class MyBundleIf(
    opt: OptBundle,
    lux: UInt) extends Bundle[MyBundleIf]

  case class SimpleBundle(a: UInt, b: UInt) extends Bundle[SimpleBundle]
  case class OptBundle2(
    a: Option[SimpleBundle],
    b: UInt,
    c: UInt,
    d: UInt,
  ) extends Bundle[OptBundle2]

  val io_2 = IO(
    Flipped(OptBundle2(
      a = Some(Flipped(SimpleBundle(
        a = Input(UInt(Width(2))),
        b = Output(UInt(Width(3)))
      ))),
      b = UInt(Width(4)),
      c = Input(UInt(Width(5))),
      d = Flipped(Input(UInt(Width(6))))
  )))
  io_2.a.get.a
  io_2.a.map(x => println(s"io.a = ${x}"))

// println(s"io_2 ${io_2}")

  assert(io_2.dir == Direction.Flipped)
  assert(io_2.b.dir == Direction.Default)
  assert(io_2.c.dir == Direction.Flipped)
  assert(io_2.d.dir == Direction.Default)
  assert(io_2.a.get.dir == Direction.Flipped)
  assert(io_2.a.get.a.dir == Direction.Flipped)
  assert(io_2.a.get.b.dir == Direction.Default)


def parameterized_bundle_check(): Unit =
  case class Security(
    pixelstealing: UInt,
    bpred: UInt,
    prefetcher: UInt
  ) extends Bundle[Security]

  case class SecurityParams(w: Int)

  object Security:
    def apply(p: SecurityParams): Security =
      Security(
        pixelstealing = UInt(Width(p.w)),
        bpred = UInt(Width(p.w)),
        prefetcher = UInt(Width(p.w)))

  case class Student(
    age: UInt,
    female: Bool
  ) extends Bundle[Student]

  object Student:
    def apply(): Student =
      Student(
        age = UInt(Width(3)),
        female = Bool())

  case class Child(
    age: UInt,
  ) extends Bundle[Child]

  object Child:
    def apply(): Child =
      Child(age = Input(UInt(Width(4))))

  case class Fletcher(
    private val security: Security,
    private val students: Seq[Student],
    private val childs: Option[Seq[Child]]
  ) extends Bundle[Fletcher]

  case class FletcherParams(
    num_students: Int,
    has_child: Boolean,
    num_childs: Int,
    security: SecurityParams
  )

  object Fletcher:
    def apply(p: FletcherParams): Fletcher =
      Fletcher(
        security = Input(Security(p.security)),
        students = Seq.fill(p.num_students)(
          Flipped(Student())
        ),
        childs = if (p.has_child) Some(Seq.fill(p.num_childs)(Output(Child())))
                 else None)

  val fp = FletcherParams(
    num_students = 3,
    num_childs = 2,
    has_child = true,
    SecurityParams(w = 5))

  val fletcher = Reg(Fletcher(fp))

  val students: Seq[Student] = fletcher.students
  val age: UInt = fletcher.students(0).age

  assert(fletcher.security.dir == Direction.Flipped)

  fletcher.students.foreach(x => {
    assert(x.dir        == Direction.Flipped)
    assert(x.age.dir    == Direction.Default)
    assert(x.female.dir == Direction.Default)
  })

  fletcher.childs.map(child => {
    child.foreach(c => {
      assert(c.age.dir == Direction.Flipped)
    })
  })

  // Note: Literals with Scala types doesn't work. In theary, we can add support
  // later by extending `HostTypeOf` and `FieldToNode` for `Bundle`.
  //
  // val fletcher_lit = Lit(Fletcher(fp))((
  //   security = (
  //     pixelstealing = BigInt(1),
  //     bpred = BigInt(2),
  //     prefetcher = BigInt(3)
  //   ),
  //   students = Seq.fill(fp.num_students)((
  //     age = BigInt(4),
  //     female = true
  //   )),
  //   childs = if (fp.has_child) Some(
  //     Seq.fill(fp.num_childs)((
  //       age = BigInt(5)
  //     )))
  //     else None
  // ))
```

There are ways around this though:

- First, we can declare the fields of the case class as `private val`. As these fields cannot be accessed from the outside, field accesses will now default to `selectDynamic`
- Next, we can force the type to be of a `Bundle` type by simply wrapping it: `Bundle(MyBundleIf)`

```scala
// Need to declare fields as `private val` so that when accessing subfields, selectDynamic in Bundle gets called
// This is only required for Literal types where you want to propagate the top level literal payload to subfields
final case class InnerBundleIf(private val a: UInt, private val b: UInt) extends Bundle[InnerBundleIf]
final case class MyBundleIf(private val x: UInt, private val y: UInt, private val i: InnerBundleIf) extends Bundle[MyBundleIf]

val mb = MyBundleIf(
x = UInt(Width(2)),
y = UInt(Width(3)),
i = InnerBundleIf(
  a = UInt(Width(4)),
  b = UInt(Width(5))
))

val reg = Reg(mb)
println(s"$reg")

val reg_x: UInt = reg.x
val reg_y: UInt = reg.y
val x: InnerBundleIf = reg.i

val ulit = Lit(UInt(Width(3)))(3)
assert(ulit.getLitVal == BigInt(3))

inline val tc1 = """
val rg: MyBundleIf = Reg(mb)
val reg_x: UInt = rg.x
val reg_y: UInt = rg.y
val reg_i: InnerBundleIf = rg.i
val reg_i_a: UInt = rg.i.a
val reg_i_b: UInt = rg.i.b
"""
assert(typeCheckErrors(tc1).isEmpty)
```

Although not entirely desirable as we are now forcing the library users certain behaviors for things to work properly, it's not the worse thing in the world.
This is because, `selectDynamic` is only required for `Bundle`s that want to generate literals.
To propagate the top level literal payload to its children, `selectDynamic` is required.

Another caveat of this `Literal` approach is that the `Bundle`s that define literals must consist of purely `HWData` types. No mixing with `Scala` types are allowed as we are using typeclass derivation to create literal shapes.
In theory, we can add support for important Scala library types in the future for derivation, but we should leave this as is for now.

---

## SRAMs

In chisel, SRAMs were modeled as SyncReadMemory which was a behavioral model of SRAMs.
This made programming easy, but also caused problems as the number of read, write, readwrite ports were inferred by the compiler.
Often times, a read and a write port could be merged into a readwrite port by the compiler if it could prove that the enable signals of a read and write port are mutually exclusive.
However, this analysis is pessimistic in nature, i.e. there are cases when the mutual exculsivity can't be proved by static analysis of the enable signals.
We would like to avoid these problems in the new HDL frontend.

On the other hand, we we could implement a fully structural SRAM implementation like below:

```scala
// Declare a 2 read, 2 write, 2 read-write ported SRAM with 8-bit UInt data members
val mem = SRAM(1024, UInt(8.W), 2, 2, 2)

// Whenever we want to read from the first read port
mem.readPorts(0).address := 100.U
mem.readPorts(0).enable := true.B

// Read data is returned one cycle after enable is driven
val foo = WireInit(UInt(8.W), mem.readPorts(0).data)

// Whenever we want to write to the second write port
mem.writePorts(1).address := 5.U
mem.writePorts(1).enable := true.B
mem.writePorts(1).data := 12.U

// Whenever we want to read or write to the third read-write port
// Write:
mem.readwritePorts(2).address := 5.U
mem.readwritePorts(2).enable := true.B
mem.readwritePorts(2).isWrite := true.B
mem.readwritePorts(2).writeData := 100.U

// Read:
mem.readwritePorts(2).address := 5.U
mem.readwritePorts(2).enable := true.B
mem.readwritePorts(2).isWrite := false.B
val bar = WireInit(UInt(8.W), mem.readwritePorts(2).readData)
```

This makes everything explicit at the cost of some ergonomics.
Also, under the hood, `SRAM`s are just a wrapper around `SyncReadMemory`.
So it's more of a hacked on solution rather than a clean one.

I think we can do better than both approaches:

```scala
class SRAM[T <: HWData](x: T, entries: Int)(portInfo: ???)


val sram = SRAM(UInt(3.W), 4)(num_read_ports: Int, num_write_ports: Int, num_readwrite_ports: Int)

when (???)
    sram.readport(0).read(addr)
otherwise
    sram.readport(0).read(addr)

sram.readport(0).read(addr)

sram.readport(1).read(addr)

sram.write(2).write(addr, data)

when (read)
    sram.readwrite(3).read(addr)
otherwise
    sram.readwrite(3).write(addr)


val port_id = Reg(UInt(...))
... some logic to update port_id

sram.readwrite(port_id).read(addr)

```

If we have APIs that look something like the above, we can use `read`, `write` functions to implicitly set the `enable`, `address`, `writedata` signals.
Furthermore, we can add port arbitration logic by enabling us to index into ports by hardware constructs (`port_id` in the above example).
Finally, the SRAMs are still structural in nature.
