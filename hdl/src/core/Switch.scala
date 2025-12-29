package hdl.core

import hdl.util.BitPat

extension [S, C](selector: S)
  /** Start a switch builder for the selector. */
  inline infix def switch(inline body: SwitchBuilder[S, C] ?=> Unit)(using m: Module, c: SwitchCond[S, C]): Unit =
    given SwitchBuilder[S, C] = new SwitchBuilder(selector, summon[Module])
    body

/** Add a case to the current switch builder. */
def is[S, C](value: C)(block: => Unit)(using builder: SwitchBuilder[S, C]): Unit =
  builder.addCase(value)(block)

/** Add a default case to the current switch builder. */
def default(block: => Unit)(using builder: SwitchBuilder[?, ?]): Unit =
  builder.addDefault(block)

trait SwitchCond[S, C]:
  def apply(lhs: S, rhs: C)(using Module): Bool

object SwitchCond:
  given SwitchCond[UInt, UInt] with
    def apply(lhs: UInt, rhs: UInt)(using Module): Bool =
      ModuleOps.prim2Op(Bool(), IR.PrimOp.Eq, lhs, rhs, summon[Module])

  given SwitchCond[Bool, Bool] with
    def apply(lhs: Bool, rhs: Bool)(using Module): Bool =
      ModuleOps.prim2Op(Bool(), IR.PrimOp.Eq, lhs, rhs, summon[Module])

  given SwitchCond[SInt, SInt] with
    def apply(lhs: SInt, rhs: SInt)(using Module): Bool =
      ModuleOps.prim2Op(Bool(), IR.PrimOp.Eq, lhs, rhs, summon[Module])

  given SwitchCond[OneHot, OneHot] with
    def apply(lhs: OneHot, rhs: OneHot)(using Module): Bool =
      ModuleOps.prim2Op(Bool(), IR.PrimOp.Eq, lhs, rhs, summon[Module])

  given SwitchCond[UInt, BitPat] with
    def apply(lhs: UInt, rhs: BitPat)(using Module): Bool =
      rhs === lhs

  given [E <: scala.reflect.Enum]: SwitchCond[HWEnum[E], HWEnum[E]] with
    def apply(lhs: HWEnum[E], rhs: HWEnum[E])(using Module): Bool =
      if lhs.enumObj != rhs.enumObj then
        throw new IllegalArgumentException("Enum type mismatch")
      ModuleOps.prim2Op(Bool(), IR.PrimOp.Eq, lhs, rhs, summon[Module])

  given SwitchCond[EmptyTuple, EmptyTuple] with
    def apply(lhs: EmptyTuple, rhs: EmptyTuple)(using Module): Bool =
      true.B

  given [H, T <: Tuple](using h: SwitchCond[H, H], t: SwitchCond[T, T]): SwitchCond[H *: T, H *: T] with
    def apply(lhs: H *: T, rhs: H *: T)(using Module): Bool =
      h(lhs.head, rhs.head) && t(lhs.tail, rhs.tail)

final class SwitchBuilder[S, C](selector: S, mod: Module)(using SwitchCond[S, C]):
  private var current: Option[WhenDSL] = None

  def addCase(value: C)(block: => Unit): Unit =
    given Module = mod
    val cond = summon[SwitchCond[S, C]].apply(selector, value)
    val next = current match
      case None => WhenOps.when(cond, mod) {
        block
      }
      case Some(prev) => prev.elsewhen(cond) {
        block
      }
    current = Some(next)

  def addDefault(block: => Unit): Unit =
    current match
      case Some(prev) => prev.otherwise {
        block
      }
      case None => WhenOps.when(true.B, mod) {
        block
      }
