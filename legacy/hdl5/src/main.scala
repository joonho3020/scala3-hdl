package hdl5

import scala.deriving.*
import scala.compiletime.*

// ----------------- Leaves -----------------
object Width:
  opaque type Width = Int
  def apply(w: Int): Width = w
  extension (x: Width)
    def toInt: Int = x
    def asString: String = s"Width($x)"
import Width.*

final case class UInt(w: Width)
final case class Reg[A](under: A)
final case class Lit[A](under: A, value: BigInt)

type Id[A] = A

// ----------------- Make / ParamOf (leaf-only rules) -----------------
trait Make[F[_], A]:
  def make(p: ParamOf[F, A]): F[A]

// Compute parameter type for building F[A]
type ParamOf[F[_], A] = F[Any] match
  case Reg[Any] => A match
    case UInt => Int
    case _    => Nothing
  case Lit[Any] => A match
    case UInt => (Int, BigInt)
    case _    => Nothing
  case _ => Nothing

given Make[Reg, UInt] with
  def make(bits: ParamOf[Reg, UInt]): Reg[UInt] = Reg(UInt(Width(bits)))

given Make[Lit, UInt] with
  def make(p: ParamOf[Lit, UInt]): Lit[UInt] = Lit(UInt(Width(p._1)), p._2)

// ----------------- HKD (auto-derived) -----------------
trait HKD[B[_[_]]]:
  def mapK[F[_], G[_]](b: B[F])(nat: [A] => F[A] => G[A]): B[G]

// Minimal HKD instances for our example bundles
given hkdInner: HKD[Inner] with
  def mapK[F[_], G[_]](b: Inner[F])(nat: [A] => F[A] => G[A]): Inner[G] =
    Inner[G](
      flag = nat[UInt](b.flag),
      data = nat[UInt](b.data)
    )

given hkdOuter(using HKD[Inner]): HKD[Outer] with
  def mapK[F[_], G[_]](b: Outer[F])(nat: [A] => F[A] => G[A]): Outer[G] =
    Outer[G](
      left = summon[HKD[Inner]].mapK(b.left)(nat),
      right = summon[HKD[Inner]].mapK(b.right)(nat)
    )

// ----------------- Generic builder -----------------
object Build:
  def regInner(ps: Inner[[A] =>> ParamOf[Reg, A]]): Inner[Reg] =
    Inner[Reg](
      flag = summon[Make[Reg, UInt]].make(ps.flag),
      data = summon[Make[Reg, UInt]].make(ps.data)
    )

  def regOuter(ps: Outer[[A] =>> ParamOf[Reg, A]]): Outer[Reg] =
    Outer[Reg](
      left = regInner(ps.left),
      right = regInner(ps.right)
    )

  def litInner(ps: Inner[[A] =>> ParamOf[Lit, A]]): Inner[Lit] =
    Inner[Lit](
      flag = summon[Make[Lit, UInt]].make(ps.flag),
      data = summon[Make[Lit, UInt]].make(ps.data)
    )

  def litOuter(ps: Outer[[A] =>> ParamOf[Lit, A]]): Outer[Lit] =
    Outer[Lit](
      left = litInner(ps.left),
      right = litInner(ps.right)
    )

// nice sugar
type Params[B[_[_]], F[_]] = B[[A] =>> ParamOf[F, A]]
extension (ps: Params[Outer, Reg]) inline def reg: Outer[Reg] = Build.regOuter(ps)
extension (ps: Params[Outer, Lit]) inline def lit: Outer[Lit] = Build.litOuter(ps)

// ----------------- Example bundles (users write ONLY these) -----------------
final case class Inner[F[_]](flag: F[UInt], data: F[UInt])
final case class Outer[F[_]](left: Inner[F], right: Inner[F])

// ----------------- Demo -----------------
object Main:
  def main(args: Array[String]): Unit =
    // 1) Build REGs: param leaves are just Int widths
    type PReg[A] = ParamOf[Reg, A]       // UInt -> Int
    val regParams: Params[Outer, Reg] =
      Outer[PReg](
        left  = Inner[PReg](flag = 1,  data = 32),
        right = Inner[PReg](flag = 1,  data = 64)
      )
    val regs: Outer[Reg] = regParams.reg

    // 2) Build LITs: param leaves are (width, value)
    type PLit[A] = ParamOf[Lit, A]       // UInt -> (Int, BigInt)
    val litParams: Params[Outer, Lit] =
      Outer[PLit](
        left  = Inner[PLit](flag = (1, 1),  data = (32, BigInt(0x10))),
        right = Inner[PLit](flag = (1, 0),  data = (64, BigInt("deadbeef", 16)))
      )
    val lits: Outer[Lit] = litParams.lit

    println(s"regs.right.data.width = ${regs.right.data.under.w}")
    println(s"lits.right.data = width=${lits.right.data.under.w}, value=${lits.right.data.value}")
