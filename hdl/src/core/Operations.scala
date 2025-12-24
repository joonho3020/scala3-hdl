package hdl

import scala.collection.mutable

extension (lhs: UInt)
  def +&(rhs: UInt)(using m: Module): UInt =
    val w = lhs.getWidth.max(rhs.getWidth) + Width(1)
    ModuleOps.prim2Op(UInt(w), IR.PrimOp.Add, lhs, rhs, m)

  def +(rhs: UInt)(using m: Module): UInt =
    (lhs +& rhs).tail(1)

  def -(rhs: UInt)(using m: Module): UInt =
    (lhs -& rhs).tail(1)

  def -&(rhs: UInt)(using m: Module): UInt =
    val w = lhs.getWidth.max(rhs.getWidth) + Width(1)
    ModuleOps.prim2Op(UInt(w), IR.PrimOp.Sub, lhs, rhs, m)

  def *(rhs: UInt)(using m: Module): UInt =
    val w = lhs.getWidth + rhs.getWidth
    ModuleOps.prim2Op(UInt(w), IR.PrimOp.Mul, lhs, rhs, m)

  def /(rhs: UInt)(using m: Module): UInt =
    ModuleOps.prim2Op(UInt(lhs.getWidth), IR.PrimOp.Div, lhs, rhs, m)

  def %(rhs: UInt)(using m: Module): UInt =
    val w = lhs.getWidth.min(rhs.getWidth)
    ModuleOps.prim2Op(UInt(w), IR.PrimOp.Rem, lhs, rhs, m)

  def <(rhs: UInt)(using m: Module): Bool =
    ModuleOps.prim2Op(Bool(), IR.PrimOp.Lt, lhs, rhs, m)

  def <=(rhs: UInt)(using m: Module): Bool =
    ModuleOps.prim2Op(Bool(), IR.PrimOp.Leq, lhs, rhs, m)

  def >(rhs: UInt)(using m: Module): Bool =
    ModuleOps.prim2Op(Bool(), IR.PrimOp.Gt, lhs, rhs, m)

  def >=(rhs: UInt)(using m: Module): Bool =
    ModuleOps.prim2Op(Bool(), IR.PrimOp.Geq, lhs, rhs, m)

  def ===(rhs: UInt)(using m: Module): Bool =
    ModuleOps.prim2Op(Bool(), IR.PrimOp.Eq, lhs, rhs, m)

  def =/=(rhs: UInt)(using m: Module): Bool =
    ModuleOps.prim2Op(Bool(), IR.PrimOp.Neq, lhs, rhs, m)

  def <<(rhs: UInt)(using m: Module): UInt =
    val w = lhs.getWidth.dynamicShiftLeft(rhs.getWidth)
    ModuleOps.prim2Op(UInt(w), IR.PrimOp.DShl, lhs, rhs, m)

  def >>(rhs: UInt)(using m: Module): UInt =
    ModuleOps.prim2Op(UInt(lhs.getWidth), IR.PrimOp.DShr, lhs, rhs, m)

  def <<(rhs: Int)(using m: Module): UInt =
    val w = lhs.getWidth + rhs
    ModuleOps.prim1Op1Const(UInt(w), IR.PrimOp.Shl, lhs, rhs, m)

  def >>(rhs: Int)(using m: Module): UInt =
    val w = lhs.getWidth.shiftRight(rhs)
    ModuleOps.prim1Op1Const(UInt(w), IR.PrimOp.Shr, lhs, rhs, m)

  def &(rhs: UInt)(using m: Module): UInt =
    val w = lhs.getWidth.max(rhs.getWidth)
    ModuleOps.prim2Op(UInt(w), IR.PrimOp.And, lhs, rhs, m)

  def |(rhs: UInt)(using m: Module): UInt =
    val w = lhs.getWidth.max(rhs.getWidth)
    ModuleOps.prim2Op(UInt(w), IR.PrimOp.Or, lhs, rhs, m)

  def ^(rhs: UInt)(using m: Module): UInt =
    val w = lhs.getWidth.max(rhs.getWidth)
    ModuleOps.prim2Op(UInt(w), IR.PrimOp.Xor, lhs, rhs, m)

// def pad(n: Int)(using m: Module): UInt =
// ModuleOps.pad(lhs, n, m)
  def head(n: Int)(using m: Module): UInt =
    ModuleOps.prim1Op1Const(UInt(Width(n)), IR.PrimOp.Head, lhs, n, m)

  def tail(n: Int)(using m: Module): UInt =
    val w = lhs.getWidth.shiftRight(n)
    ModuleOps.prim1Op1Const(UInt(w), IR.PrimOp.Tail, lhs, n, m)

  def apply(hi: Int, lo: Int)(using m: Module): UInt =
    val w = Width(hi - lo + 1)
    ModuleOps.prim1Op2Const(UInt(w), IR.PrimOp.Bits, lhs, hi, lo, m)

  def apply(idx: Int)(using m: Module): UInt =
    ModuleOps.prim1Op2Const(UInt(Width(1)), IR.PrimOp.Bits, lhs, idx, idx, m)

  def apply(idx: UInt)(using m: Module): UInt =
    val shifted = ModuleOps.prim2Op(UInt(lhs.getWidth), IR.PrimOp.DShr, lhs, idx, m)
    ModuleOps.prim1Op2Const(UInt(Width(1)), IR.PrimOp.Bits, shifted, 0, 0, m)

  def orR(using m: Module): Bool =
    val width = lhs.getWidth match
      case KnownWidth(v) => v
      case _ => throw new IllegalArgumentException("orR requires UInt with known width")
    if width < 1 then
      throw new IllegalArgumentException("orR requires UInt width >= 1")
    val bits = (0 until width).map(i => lhs(i).asBool)
    bits.reduce(_ || _)

  def andR(using m: Module): Bool =
    val width = lhs.getWidth match
      case KnownWidth(v) => v
      case _ => throw new IllegalArgumentException("andR requires UInt with known width")
    if width < 1 then
      throw new IllegalArgumentException("andR requires UInt width >= 1")
    val bits = (0 until width).map(i => lhs(i).asBool)
    bits.reduce(_ && _)

  def asBool(using m: Module): Bool =
    ModuleOps.prim1Op(Bool(), IR.PrimOp.AsBool, lhs, m)

  def unary_~(using m: Module): UInt =
    ModuleOps.prim1Op(UInt(lhs.getWidth), IR.PrimOp.Not, lhs, m)

  def asSInt(using m: Module): SInt =
    ModuleOps.prim1Op(SInt(lhs.getWidth), IR.PrimOp.AsSInt, lhs, m)

extension (lhs: SInt)
  def +&(rhs: SInt)(using m: Module): SInt =
    val w = lhs.getWidth.max(rhs.getWidth) + Width(1)
    ModuleOps.prim2Op(SInt(w), IR.PrimOp.Add, lhs, rhs, m)

  def +(rhs: SInt)(using m: Module): SInt =
    val w = lhs.getWidth.max(rhs.getWidth)
    val result = ModuleOps.prim2Op(SInt(w + Width(1)), IR.PrimOp.Add, lhs, rhs, m)
    ModuleOps.prim1Op(SInt(w), IR.PrimOp.AsSInt, result, m)

  def -(rhs: SInt)(using m: Module): SInt =
    val w = lhs.getWidth.max(rhs.getWidth)
    val result = ModuleOps.prim2Op(SInt(w + Width(1)), IR.PrimOp.Sub, lhs, rhs, m)
    ModuleOps.prim1Op(SInt(w), IR.PrimOp.AsSInt, result, m)

  def -&(rhs: SInt)(using m: Module): SInt =
    val w = lhs.getWidth.max(rhs.getWidth) + Width(1)
    ModuleOps.prim2Op(SInt(w), IR.PrimOp.Sub, lhs, rhs, m)

  def *(rhs: SInt)(using m: Module): SInt =
    val w = lhs.getWidth + rhs.getWidth
    ModuleOps.prim2Op(SInt(w), IR.PrimOp.Mul, lhs, rhs, m)

  def /(rhs: SInt)(using m: Module): SInt =
    val w = lhs.getWidth + Width(1)
    ModuleOps.prim2Op(SInt(w), IR.PrimOp.Div, lhs, rhs, m)

  def %(rhs: SInt)(using m: Module): SInt =
    val w = lhs.getWidth.min(rhs.getWidth)
    ModuleOps.prim2Op(SInt(w), IR.PrimOp.Rem, lhs, rhs, m)

  def <(rhs: SInt)(using m: Module): Bool =
    ModuleOps.prim2Op(Bool(), IR.PrimOp.Lt, lhs, rhs, m)

  def <=(rhs: SInt)(using m: Module): Bool =
    ModuleOps.prim2Op(Bool(), IR.PrimOp.Leq, lhs, rhs, m)

  def >(rhs: SInt)(using m: Module): Bool =
    ModuleOps.prim2Op(Bool(), IR.PrimOp.Gt, lhs, rhs, m)

  def >=(rhs: SInt)(using m: Module): Bool =
    ModuleOps.prim2Op(Bool(), IR.PrimOp.Geq, lhs, rhs, m)

  def ===(rhs: SInt)(using m: Module): Bool =
    ModuleOps.prim2Op(Bool(), IR.PrimOp.Eq, lhs, rhs, m)

  def =/=(rhs: SInt)(using m: Module): Bool =
    ModuleOps.prim2Op(Bool(), IR.PrimOp.Neq, lhs, rhs, m)

  def <<(rhs: UInt)(using m: Module): SInt =
    val w = lhs.getWidth.dynamicShiftLeft(rhs.getWidth)
    ModuleOps.prim2Op(SInt(w), IR.PrimOp.DShl, lhs, rhs, m)

  def >>(rhs: UInt)(using m: Module): SInt =
    ModuleOps.prim2Op(SInt(lhs.getWidth), IR.PrimOp.DShr, lhs, rhs, m)

  def <<(rhs: Int)(using m: Module): SInt =
    val w = lhs.getWidth + rhs
    ModuleOps.prim1Op1Const(SInt(w), IR.PrimOp.Shl, lhs, rhs, m)

  def >>(rhs: Int)(using m: Module): SInt =
    val w = lhs.getWidth.shiftRight(rhs)
    ModuleOps.prim1Op1Const(SInt(w), IR.PrimOp.Shr, lhs, rhs, m)

  def &(rhs: SInt)(using m: Module): SInt =
    val w = lhs.getWidth.max(rhs.getWidth)
    ModuleOps.prim2Op(SInt(w), IR.PrimOp.And, lhs, rhs, m)

  def |(rhs: SInt)(using m: Module): SInt =
    val w = lhs.getWidth.max(rhs.getWidth)
    ModuleOps.prim2Op(SInt(w), IR.PrimOp.Or, lhs, rhs, m)

  def ^(rhs: SInt)(using m: Module): SInt =
    val w = lhs.getWidth.max(rhs.getWidth)
    ModuleOps.prim2Op(SInt(w), IR.PrimOp.Xor, lhs, rhs, m)

  def head(n: Int)(using m: Module): UInt =
    ModuleOps.prim1Op1Const(UInt(Width(n)), IR.PrimOp.Head, lhs, n, m)

  def tail(n: Int)(using m: Module): SInt =
    val w = lhs.getWidth.shiftRight(n)
    ModuleOps.prim1Op1Const(SInt(w), IR.PrimOp.Tail, lhs, n, m)

  def apply(hi: Int, lo: Int)(using m: Module): UInt =
    val w = Width(hi - lo + 1)
    ModuleOps.prim1Op2Const(UInt(w), IR.PrimOp.Bits, lhs, hi, lo, m)

  def apply(idx: Int)(using m: Module): UInt =
    ModuleOps.prim1Op2Const(UInt(Width(1)), IR.PrimOp.Bits, lhs, idx, idx, m)

  def unary_~(using m: Module): SInt =
    ModuleOps.prim1Op(SInt(lhs.getWidth), IR.PrimOp.Not, lhs, m)

  def unary_-(using m: Module): SInt =
    ModuleOps.prim1Op(SInt(lhs.getWidth + Width(1)), IR.PrimOp.Neg, lhs, m)

  def asUInt(using m: Module): UInt =
    ModuleOps.prim1Op(UInt(lhs.getWidth), IR.PrimOp.AsUInt, lhs, m)

  def abs(using m: Module): UInt =
    Mux(lhs < 0.S, (-lhs).asUInt, lhs.asUInt)

extension (lhs: Bool)
  def ===(rhs: Bool)(using m: Module): Bool =
    ModuleOps.prim2Op(Bool(), IR.PrimOp.Eq, lhs, rhs, m)

  def =/=(rhs: Bool)(using m: Module): Bool =
    ModuleOps.prim2Op(Bool(), IR.PrimOp.Neq, lhs, rhs, m)

  def &&(rhs: Bool)(using m: Module): Bool =
    ModuleOps.prim2Op(Bool(), IR.PrimOp.And, lhs, rhs, m)

  def ||(rhs: Bool)(using m: Module): Bool =
    ModuleOps.prim2Op(Bool(), IR.PrimOp.Or, lhs, rhs, m)

  def ^(rhs: Bool)(using m: Module): Bool =
    ModuleOps.prim2Op(Bool(), IR.PrimOp.Xor, lhs, rhs, m)

  def unary_!(using m: Module): Bool =
    ModuleOps.prim1Op(Bool(), IR.PrimOp.Not, lhs, m)

  def asUInt(using m: Module): UInt =
    ModuleOps.asUInt(lhs, m)

  def asSInt(using m: Module): SInt =
    ModuleOps.prim1Op(SInt(Width(1)), IR.PrimOp.AsSInt, lhs, m)

extension [E <: scala.reflect.Enum] (lhs: HWEnum[E])
  def ===(rhs: HWEnum[E])(using m: Module): Bool =
    if lhs.enumObj != rhs.enumObj then
      throw new IllegalArgumentException("Enum type mismatch")
    ModuleOps.prim2Op(Bool(), IR.PrimOp.Eq, lhs, rhs, m)

  def =/=(rhs: HWEnum[E])(using m: Module): Bool =
    if lhs.enumObj != rhs.enumObj then
      throw new IllegalArgumentException("Enum type mismatch")
    ModuleOps.prim2Op(Bool(), IR.PrimOp.Neq, lhs, rhs, m)

  def asUInt(using m: Module): UInt =
    ModuleOps.asUInt(lhs, m)

extension (lhs: Reset)
  def asUInt(using m: Module): UInt =
    ModuleOps.prim1Op(UInt(1.W), IR.PrimOp.AsUInt, lhs, m)

  def asBool(using m: Module): Bool =
    ModuleOps.prim1Op(Bool(), IR.PrimOp.AsUInt, lhs, m)

def clock(using m: Module): Clock =
  m.getImplicitClock

def reset(using m: Module): Reset =
  m.getImplicitReset

def Mux[T <: HWData](cond: Bool, tval: T, fval: T)(using m: Module): T =
  ModuleOps.mux(cond, tval, fval, m)

def dontTouch[T <: HWData](data: T)(using m: Module): T =
  ModuleOps.dontTouch(data, m)

extension (xs: Seq[HWData])
  def Cat(using m: Module): UInt =
    ModuleOps.concatBits(xs, m)

extension (v: Vec[?])
  def Cat(using m: Module): UInt =
    ModuleOps.concatBits(v.elems.asInstanceOf[Seq[HWData]], m)

extension (h: HWData)
  def asUInt(using m: Module): UInt =
    ModuleOps.asUInt(h, m)

extension (dc: DontCare.type)
  inline def as[T <: HWData]: T = dc.asInstanceOf[T]

extension (lhs: UInt)
  def asOH(using m: Module): OneHot =
    ModuleOps.uintToOH(lhs, m)

extension (lhs: OneHot)
  def asUInt(using m: Module): UInt =
    ModuleOps.ohToUInt(lhs, m)

  def ===(rhs: OneHot)(using m: Module): Bool =
    ModuleOps.prim2Op(Bool(), IR.PrimOp.Eq, lhs, rhs, m)

  def =/=(rhs: OneHot)(using m: Module): Bool =
    ModuleOps.prim2Op(Bool(), IR.PrimOp.Neq, lhs, rhs, m)
