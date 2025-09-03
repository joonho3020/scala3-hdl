package hdl5

import scala.compiletime.*

object Width:
  opaque type Width = Int

  def apply(w: Int): Width = w

  extension (x: Width)
    def toInt: Int = x
    def + (y: Width): Width = x + y
    def - (y: Width): Width = x - y
    def * (y: Width): Width = x * y

    def show: String = s"Width($x)"

import Width.Width

final class UInt(val w: Width):
  override def toString(): String =
    s"UInt<$w>"

final class UIntLit(val w: Width, val v: Int):
  override def toString(): String =
    s"UInt<$w>($v)"

// type Id[A] = A
final case class Reg[A](under: A)

final case class Lit[A](under: A, value: Int)

// Build[F] knows how to make leaves for a given “view” F[_]
trait Build[F[_]]:
  def uint(bits: Int): F[UInt]

trait BuildInit[F[_]]:
  def uint(bits: Int, value: Int): F[UInt]

// given Build[Id] with
// def uint(bits: Int): Id[UInt] = UInt(Width(bits))

given Build[Reg] with
  def uint(bits: Int): Reg[UInt] = Reg(UInt(Width(bits)))

given BuildInit[Lit] with
  def uint(bits: Int, value: Int): Lit[UInt] = Lit(UInt(Width(bits)), value)

// case class Decoupled[T](valid: UInt, ready: UInt, bits: T)

object Main:
  def main(args: Array[String]): Unit =
    println("Hello World")

    final case class Params(flagL: Int, dataL: Int, flagR: Int, dataR: Int)

    final case class Inner[F[_]](
      flag: F[UInt],
      data: F[UInt]
    )

    object Inner:
      def apply[F[_]: Build](flagBits: Int, dataBits: Int): Inner[F] =
        Inner(flag = summon[Build[F]].uint(flagBits),
              data = summon[Build[F]].uint(dataBits))

      def apply[F[_]: BuildInit](
        flagBits: Int, dataBits: Int
      )(
        flagInit: Int, dataInit: Int
      ): Inner[F] =
        Inner(flag = summon[BuildInit[F]].uint(flagBits, flagInit),
              data = summon[BuildInit[F]].uint(dataBits, dataInit))

    val inner_reg = Inner[Reg](2, 3)
    val inner_lit = Inner[Lit](5, 6)(2, 3)
    println(s"flag: ${inner_lit.flag}, data ${inner_lit.data}")

    final case class Outer[F[_]](
      left : Inner[F],
      right: Inner[F])

    object Outer:
      def apply[F[_]: Build](p: Params): Outer[F] =
        Outer(
          left  = Inner[F](p.flagL,  p.dataL),
          right = Inner[F](p.flagR,  p.dataR))

      def apply[F[_]: BuildInit](p: Params)(q: Params): Outer[F] =
        Outer(
          left  = Inner[F](p.flagL,  p.dataL)(q.flagL, q.dataL),
          right = Inner[F](p.flagR,  p.dataR)(q.flagR, q.dataR))

    val p = Params(1, 32, 1, 64)

    // Short, uniform constructors:
// val v: Outer[Id]  = Outer[Id](p)
// println(s"v.left.flat ${v.left.flag}")
// println(s"v.right.flat ${v.right.flag}")

    val r: Outer[Reg] = Outer[Reg](p)
    println(s"r.right.data ${r.right.data}")
    println(s"r.right ${r.right}")

    val uint_reg = Reg(UInt(Width(2)))
    println(s"uint_reg ${uint_reg}")

    val outer_reg_2 = Outer[Reg](Params(2, 3, 4, 5))
    println(s"${outer_reg_2.right}")

    val outer_lit = Outer[Lit](Params(5, 6, 7, 8))(Params(7, 8, 9, 10))
    println(s"left data ${outer_lit.left.data} right flag ${outer_lit.right.flag}")
    println(s"left ${outer_lit.left} right ${outer_lit.right}")

// NOTES
// - Need to be able to derive the HKD typeclass for product types. This should be doable
// - Once again, the constructor syntax being verbose is an ergonomic issue that cannot be easily ignored
// - Instantiating registers using the `HKD[Outer].mapK(v)([A] => (a: A) => mkReg(a))` syntax is kind of crazy.
//   Considering that the average hardware engineer is like Mr. Anderson who complains about not being able to
//   understand Chisel, this is impossible for them