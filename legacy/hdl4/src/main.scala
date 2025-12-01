package hdl4

import scala.compiletime.*

// ---------- Roles & Direction ----------
sealed trait Role
enum Direction:
  case In, Out, Unspecified

final case class IO(dir: Direction) extends Role
object Wire extends Role
object Reg extends Role
object Literal extends Role

// ---------- Core Data hierarchy ----------
sealed trait Data

final case class Bool() extends Data

// UInt with type-level width W (an Int singleton type)
final class UInt[W <: Int & Singleton] extends Data
object UInt:
  def of[W <: Int & Singleton]: UInt[W] = new UInt[W]

// Directional wrappers (used for IO descriptions)
final case class In[T <: Data](value: T) extends Data
final case class Out[T <: Data](value: T) extends Data

// Role-attached wrappers (used for internal uses)
final case class WireOf[T <: Data](value: T) extends Data
type Wire[T <: Data] = WireOf[T]

final case class RegOf[T <: Data](value: T) extends Data
type Reg[T <: Data] = RegOf[T]

final case class LitOf[T <: Data](value: T) extends Data
type Lit[T <: Data] = LitOf[T]

// ---------- Bundle base ----------
abstract class Bundle[R <: Role] extends Data:
  // Subclasses must define how to re-apply themselves to a different Role
  type Self[F <: Role] <: Bundle[F]

  // Field declaration that materializes a value appropriate for the Role
  inline def field[D <: Data]: Of[D, R] = FieldBuilder.make[R, D]

// Builders for base Data values used by FieldCtor implementations
object Builders:
  inline def base[D <: Data]: D = inline erasedValue[D] match
    case _: Bool               => Bool().asInstanceOf[D]
    case _: UInt[w]            => new UInt[w].asInstanceOf[D]
    case _: In[u]              => In(base[u]).asInstanceOf[D]
    case _: Out[u]             => Out(base[u]).asInstanceOf[D]
    case _: ValidReady[d, r]   => new ValidReady[d, r](base[d]).asInstanceOf[D]
    
object FieldBuilder:
  inline def make[R <: Role, D <: Data]: Of[D, R] = inline erasedValue[R] match
    case _: IO           => buildIO[D].asInstanceOf[Of[D, R]]
    case _: Wire.type    => buildWire[D].asInstanceOf[Of[D, R]]
    case _: Reg.type     => buildReg[D].asInstanceOf[Of[D, R]]
    case _: Literal.type => buildLit[D].asInstanceOf[Of[D, R]]

  private inline def buildIO[D <: Data]: Of[D, IO] = inline erasedValue[D] match
    case _: In[u]              => In(Builders.base[u]).asInstanceOf[Of[D, IO]]
    case _: Out[u]             => Out(Builders.base[u]).asInstanceOf[Of[D, IO]]
    case _: Bool               => Bool().asInstanceOf[Of[D, IO]]
    case _: UInt[w]            => new UInt[w].asInstanceOf[Of[D, IO]]
    case _: ValidReady[d, r]   => new ValidReady[d, IO](Builders.base[d]).asInstanceOf[Of[D, IO]]

  private inline def buildWire[D <: Data]: Of[D, Wire.type] = inline erasedValue[D] match
    case _: In[u]              => WireOf(Builders.base[u]).asInstanceOf[Of[D, Wire.type]]
    case _: Out[u]             => WireOf(Builders.base[u]).asInstanceOf[Of[D, Wire.type]]
    case _: Bool               => WireOf(Bool()).asInstanceOf[Of[D, Wire.type]]
    case _: UInt[w]            => WireOf(new UInt[w]).asInstanceOf[Of[D, Wire.type]]
    case _: ValidReady[d, r]   => new ValidReady[d, Wire.type](Builders.base[d]).asInstanceOf[Of[D, Wire.type]]

  private inline def buildReg[D <: Data]: Of[D, Reg.type] = inline erasedValue[D] match
    case _: In[u]              => RegOf(Builders.base[u]).asInstanceOf[Of[D, Reg.type]]
    case _: Out[u]             => RegOf(Builders.base[u]).asInstanceOf[Of[D, Reg.type]]
    case _: Bool               => RegOf(Bool()).asInstanceOf[Of[D, Reg.type]]
    case _: UInt[w]            => RegOf(new UInt[w]).asInstanceOf[Of[D, Reg.type]]
    case _: ValidReady[d, r]   => new ValidReady[d, Reg.type](Builders.base[d]).asInstanceOf[Of[D, Reg.type]]

  private inline def buildLit[D <: Data]: Of[D, Literal.type] = inline erasedValue[D] match
    case _: In[u]              => LitOf(Builders.base[u]).asInstanceOf[Of[D, Literal.type]]
    case _: Out[u]             => LitOf(Builders.base[u]).asInstanceOf[Of[D, Literal.type]]
    case _: Bool               => LitOf(Bool()).asInstanceOf[Of[D, Literal.type]]
    case _: UInt[w]            => LitOf(new UInt[w]).asInstanceOf[Of[D, Literal.type]]
    case _: ValidReady[d, r]   => new ValidReady[d, Literal.type](Builders.base[d]).asInstanceOf[Of[D, Literal.type]]

// ---------- Type-level utilities ----------
type StripDir[T <: Data] <: Data = T match
  case In[u]  => u
  case Out[u] => u
  case _      => T

// Compute the concrete field type given a base Data type D and a Role R
// Note: We avoid subtype patterns in match types; handle specific bundles explicitly for now.
type Of[D <: Data, R <: Role] <: Data = (D, R) match
  // IO role: preserve In/Out wrappers and bundle structure
  case (In[u], IO)                => In[u]
  case (Out[u], IO)               => Out[u]
  case (Bool, IO)                 => Bool
  case (UInt[w], IO)              => UInt[w]
  case (ValidReady[d, r], IO)     => ValidReady[d, IO]

  // Wire role: strip direction, wrap leaves in Wire, keep bundle structure
  case (In[u], Wire.type)         => Wire[StripDir[u]]
  case (Out[u], Wire.type)        => Wire[StripDir[u]]
  case (Bool, Wire.type)          => Wire[Bool]
  case (UInt[w], Wire.type)       => Wire[UInt[w]]
  case (ValidReady[d, r], Wire.type) => ValidReady[d, Wire.type]

  // Reg role
  case (In[u], Reg.type)          => Reg[StripDir[u]]
  case (Out[u], Reg.type)         => Reg[StripDir[u]]
  case (Bool, Reg.type)           => Reg[Bool]
  case (UInt[w], Reg.type)        => Reg[UInt[w]]
  case (ValidReady[d, r], Reg.type) => ValidReady[d, Reg.type]

  // Literal role
  case (In[u], Literal.type)      => Lit[StripDir[u]]
  case (Out[u], Literal.type)     => Lit[StripDir[u]]
  case (Bool, Literal.type)       => Lit[Bool]
  case (UInt[w], Literal.type)    => Lit[UInt[w]]
  case (ValidReady[d, r], Literal.type) => ValidReady[d, Literal.type]

// ---------- Flipped direction ----------
type Flipped[T <: Data] <: Data = T match
  case In[u]        => Out[u]
  case Out[u]       => In[u]
  case _            => T

// ---------- Example Bundle: ValidReady ----------
final class ValidReady[D <: Data, R <: Role](val dataType: D) extends Bundle[R]:
  type Self[F <: Role] = ValidReady[D, F]
  val valid = field[Out[Bool]]
  val ready = field[In[Bool]]
  val bits  = field[D]

// ---------- Minimal demo with type-level checks ----------
@main def run(): Unit =
  // Core Of[...] mapping checks
  summon[Of[Bool, Wire.type] =:= Wire[Bool]]
  summon[Of[UInt[32], Reg.type] =:= Reg[UInt[32]]]
  summon[Of[In[UInt[32]], IO] =:= In[UInt[32]]]
  summon[Flipped[In[UInt[32]]] =:= Out[UInt[32]]]

  // Nested bundle role promotion check
  summon[Of[ValidReady[UInt[32], IO], Wire.type] =:= ValidReady[UInt[32], Wire.type]]

  // Constructing instances (values inside fields are placeholders)
  val ioBundle: ValidReady[UInt[32], IO] = new ValidReady[UInt[32], IO](UInt.of[32])
  val wireBundle: ValidReady[UInt[32], Wire.type] = new ValidReady[UInt[32], Wire.type](UInt.of[32])
  println(wireBundle.bits)
  println(wireBundle.valid)
  println(wireBundle.ready)

  class MyBundle[R <: Role, W <: Int & Singleton](x: Int, y: Int) extends Bundle[R]:
    val a = field[Out[UInt[W]]]
    val b = field[In[UInt[W]]]

  val mybundle = new MyBundle[Reg.type, 16](2, 3)
  println(s"${mybundle} ${mybundle.a} ${mybundle.b}")



  println("Bundle API core type-level checks passed.")
