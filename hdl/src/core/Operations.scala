package hdl

import scala.quoted.*

extension (lhs: Bits)
  def orR(using m: Module): Bool =
    val width = lhs.requireKnownWidth("orR")
    if width < 1 then
      throw new IllegalArgumentException("orR requires width >= 1")
    val u = lhs match
      case u: UInt => u
      case other => ModuleOps.prim1Op(UInt(other.getWidth), IR.PrimOp.AsUInt, other, m)
    val bits = (0 until width).map { i =>
      val bit = ModuleOps.prim1Op2Const(UInt(Width(1)), IR.PrimOp.Bits, u, i, i, m)
      ModuleOps.prim1Op(Bool(), IR.PrimOp.AsBool, bit, m)
    }
    bits.reduce((a, b) => ModuleOps.prim2Op(Bool(), IR.PrimOp.Or, a, b, m))

  def andR(using m: Module): Bool =
    val width = lhs.requireKnownWidth("andR")
    if width < 1 then
      throw new IllegalArgumentException("andR requires width >= 1")
    val u = lhs match
      case u: UInt => u
      case other => ModuleOps.prim1Op(UInt(other.getWidth), IR.PrimOp.AsUInt, other, m)
    val bits = (0 until width).map { i =>
      val bit = ModuleOps.prim1Op2Const(UInt(Width(1)), IR.PrimOp.Bits, u, i, i, m)
      ModuleOps.prim1Op(Bool(), IR.PrimOp.AsBool, bit, m)
    }
    bits.reduce((a, b) => ModuleOps.prim2Op(Bool(), IR.PrimOp.And, a, b, m))

extension (lhs: UInt)
  inline def +&(rhs: UInt)(using inline m: Module): UInt =
    ${ OperationMacros.uint2OpMaxWidthPlus1('lhs, 'rhs, '{ IR.PrimOp.Add }, 'm) }

  inline def +(rhs: UInt)(using inline m: Module): UInt =
    (lhs +& rhs).tail(1)

  inline def -(rhs: UInt)(using inline m: Module): UInt =
    (lhs -& rhs).tail(1)

  inline def -&(rhs: UInt)(using inline m: Module): UInt =
    ${ OperationMacros.uint2OpMaxWidthPlus1('lhs, 'rhs, '{ IR.PrimOp.Sub }, 'm) }

  inline def *(rhs: UInt)(using inline m: Module): UInt =
    ${ OperationMacros.uint2OpAddWidth('lhs, 'rhs, '{ IR.PrimOp.Mul }, 'm) }

  inline def /(rhs: UInt)(using inline m: Module): UInt =
    ${ OperationMacros.uint2OpLhsWidth('lhs, 'rhs, '{ IR.PrimOp.Div }, 'm) }

  inline def %(rhs: UInt)(using inline m: Module): UInt =
    ${ OperationMacros.uint2OpMinWidth('lhs, 'rhs, '{ IR.PrimOp.Rem }, 'm) }

  inline def <(rhs: UInt)(using inline m: Module): Bool =
    ${ OperationMacros.uint2OpToBool('lhs, 'rhs, '{ IR.PrimOp.Lt }, 'm) }

  inline def <=(rhs: UInt)(using inline m: Module): Bool =
    ${ OperationMacros.uint2OpToBool('lhs, 'rhs, '{ IR.PrimOp.Leq }, 'm) }

  inline def >(rhs: UInt)(using inline m: Module): Bool =
    ${ OperationMacros.uint2OpToBool('lhs, 'rhs, '{ IR.PrimOp.Gt }, 'm) }

  inline def >=(rhs: UInt)(using inline m: Module): Bool =
    ${ OperationMacros.uint2OpToBool('lhs, 'rhs, '{ IR.PrimOp.Geq }, 'm) }

  inline def ===(rhs: UInt)(using inline m: Module): Bool =
    ${ OperationMacros.uint2OpToBool('lhs, 'rhs, '{ IR.PrimOp.Eq }, 'm) }

  inline def =/=(rhs: UInt)(using inline m: Module): Bool =
    ${ OperationMacros.uint2OpToBool('lhs, 'rhs, '{ IR.PrimOp.Neq }, 'm) }

  inline def <<(rhs: UInt)(using inline m: Module): UInt =
    ${ OperationMacros.uint2OpDynShl('lhs, 'rhs, '{ IR.PrimOp.DShl }, 'm) }

  inline def >>(rhs: UInt)(using inline m: Module): UInt =
    ${ OperationMacros.uint2OpLhsWidth('lhs, 'rhs, '{ IR.PrimOp.DShr }, 'm) }

  inline def <<(rhs: Int)(using inline m: Module): UInt =
    ${ OperationMacros.uint1Op1ConstAddWidth('lhs, 'rhs, '{ IR.PrimOp.Shl }, 'm) }

  inline def >>(rhs: Int)(using inline m: Module): UInt =
    ${ OperationMacros.uint1Op1ConstShrWidth('lhs, 'rhs, '{ IR.PrimOp.Shr }, 'm) }

  inline def &(rhs: UInt)(using inline m: Module): UInt =
    ${ OperationMacros.uint2OpMaxWidth('lhs, 'rhs, '{ IR.PrimOp.And }, 'm) }

  inline def |(rhs: UInt)(using inline m: Module): UInt =
    ${ OperationMacros.uint2OpMaxWidth('lhs, 'rhs, '{ IR.PrimOp.Or }, 'm) }

  inline def ^(rhs: UInt)(using inline m: Module): UInt =
    ${ OperationMacros.uint2OpMaxWidth('lhs, 'rhs, '{ IR.PrimOp.Xor }, 'm) }

  inline def head(n: Int)(using inline m: Module): UInt =
    ${ OperationMacros.uint1Op1ConstFixedWidth('lhs, 'n, '{ IR.PrimOp.Head }, 'm) }

  inline def tail(n: Int)(using inline m: Module): UInt =
    ${ OperationMacros.uint1Op1ConstShrWidth('lhs, 'n, '{ IR.PrimOp.Tail }, 'm) }

  inline def apply(hi: Int, lo: Int)(using inline m: Module): UInt =
    ${ OperationMacros.uint1Op2ConstBits('lhs, 'hi, 'lo, '{ IR.PrimOp.Bits }, 'm) }

  inline def apply(idx: Int)(using inline m: Module): UInt =
    ${ OperationMacros.uint1Op2ConstSingleBit('lhs, 'idx, '{ IR.PrimOp.Bits }, 'm) }

  def apply(idx: UInt)(using m: Module): UInt =
    val shifted = ModuleOps.prim2Op(UInt(lhs.getWidth), IR.PrimOp.DShr, lhs, idx, m)
    ModuleOps.prim1Op2Const(UInt(Width(1)), IR.PrimOp.Bits, shifted, 0, 0, m)

  inline def asBool(using inline m: Module): Bool =
    ${ OperationMacros.uint1OpToBool('lhs, '{ IR.PrimOp.AsBool }, 'm) }

  inline def unary_~(using inline m: Module): UInt =
    ${ OperationMacros.uint1OpSameWidth('lhs, '{ IR.PrimOp.Not }, 'm) }

  inline def asSInt(using inline m: Module): SInt =
    ${ OperationMacros.uint1OpToSInt('lhs, '{ IR.PrimOp.AsSInt }, 'm) }

extension (lhs: SInt)
  inline def +&(rhs: SInt)(using inline m: Module): SInt =
    ${ OperationMacros.sint2OpMaxWidthPlus1('lhs, 'rhs, '{ IR.PrimOp.Add }, 'm) }

  inline def +(rhs: SInt)(using inline m: Module): SInt =
    (lhs +& rhs).tail(1).asSInt

  inline def -(rhs: SInt)(using inline m: Module): SInt =
    (lhs -& rhs).tail(1).asSInt

  inline def -&(rhs: SInt)(using inline m: Module): SInt =
    ${ OperationMacros.sint2OpMaxWidthPlus1('lhs, 'rhs, '{ IR.PrimOp.Sub }, 'm) }

  inline def *(rhs: SInt)(using inline m: Module): SInt =
    ${ OperationMacros.sint2OpAddWidth('lhs, 'rhs, '{ IR.PrimOp.Mul }, 'm) }

  inline def /(rhs: SInt)(using inline m: Module): SInt =
    ${ OperationMacros.sint2OpLhsWidthPlus1('lhs, 'rhs, '{ IR.PrimOp.Div }, 'm) }

  inline def %(rhs: SInt)(using inline m: Module): SInt =
    ${ OperationMacros.sint2OpMinWidth('lhs, 'rhs, '{ IR.PrimOp.Rem }, 'm) }

  inline def <(rhs: SInt)(using inline m: Module): Bool =
    ${ OperationMacros.sint2OpToBool('lhs, 'rhs, '{ IR.PrimOp.Lt }, 'm) }

  inline def <=(rhs: SInt)(using inline m: Module): Bool =
    ${ OperationMacros.sint2OpToBool('lhs, 'rhs, '{ IR.PrimOp.Leq }, 'm) }

  inline def >(rhs: SInt)(using inline m: Module): Bool =
    ${ OperationMacros.sint2OpToBool('lhs, 'rhs, '{ IR.PrimOp.Gt }, 'm) }

  inline def >=(rhs: SInt)(using inline m: Module): Bool =
    ${ OperationMacros.sint2OpToBool('lhs, 'rhs, '{ IR.PrimOp.Geq }, 'm) }

  inline def ===(rhs: SInt)(using inline m: Module): Bool =
    ${ OperationMacros.sint2OpToBool('lhs, 'rhs, '{ IR.PrimOp.Eq }, 'm) }

  inline def =/=(rhs: SInt)(using inline m: Module): Bool =
    ${ OperationMacros.sint2OpToBool('lhs, 'rhs, '{ IR.PrimOp.Neq }, 'm) }

  inline def <<(rhs: UInt)(using inline m: Module): SInt =
    ${ OperationMacros.sintUint2OpDynShl('lhs, 'rhs, '{ IR.PrimOp.DShl }, 'm) }

  inline def >>(rhs: UInt)(using inline m: Module): SInt =
    ${ OperationMacros.sintUint2OpLhsWidth('lhs, 'rhs, '{ IR.PrimOp.DShr }, 'm) }

  inline def <<(rhs: Int)(using inline m: Module): SInt =
    ${ OperationMacros.sint1Op1ConstAddWidth('lhs, 'rhs, '{ IR.PrimOp.Shl }, 'm) }

  inline def >>(rhs: Int)(using inline m: Module): SInt =
    ${ OperationMacros.sint1Op1ConstShrWidth('lhs, 'rhs, '{ IR.PrimOp.Shr }, 'm) }

  inline def &(rhs: SInt)(using inline m: Module): SInt =
    ${ OperationMacros.sint2OpMaxWidth('lhs, 'rhs, '{ IR.PrimOp.And }, 'm) }

  inline def |(rhs: SInt)(using inline m: Module): SInt =
    ${ OperationMacros.sint2OpMaxWidth('lhs, 'rhs, '{ IR.PrimOp.Or }, 'm) }

  inline def ^(rhs: SInt)(using inline m: Module): SInt =
    ${ OperationMacros.sint2OpMaxWidth('lhs, 'rhs, '{ IR.PrimOp.Xor }, 'm) }

  inline def head(n: Int)(using inline m: Module): UInt =
    ${ OperationMacros.sint1Op1ConstToUIntFixedWidth('lhs, 'n, '{ IR.PrimOp.Head }, 'm) }

  inline def tail(n: Int)(using inline m: Module): UInt =
    ${ OperationMacros.sint1Op1ConstToUIntShrWidth('lhs, 'n, '{ IR.PrimOp.Tail }, 'm) }

  inline def apply(hi: Int, lo: Int)(using inline m: Module): UInt =
    ${ OperationMacros.sint1Op2ConstBits('lhs, 'hi, 'lo, '{ IR.PrimOp.Bits }, 'm) }

  inline def apply(idx: Int)(using inline m: Module): UInt =
    ${ OperationMacros.sint1Op2ConstSingleBit('lhs, 'idx, '{ IR.PrimOp.Bits }, 'm) }

  inline def unary_~(using inline m: Module): SInt =
    ${ OperationMacros.sint1OpSameWidth('lhs, '{ IR.PrimOp.Not }, 'm) }

  inline def unary_-(using inline m: Module): SInt =
    ${ OperationMacros.sint1OpWidthPlus1('lhs, '{ IR.PrimOp.Neg }, 'm) }

  inline def asUInt(using inline m: Module): UInt =
    ${ OperationMacros.sint1OpToUInt('lhs, '{ IR.PrimOp.AsUInt }, 'm) }

  inline def abs(using inline m: Module): UInt =
    Mux(lhs < 0.S, (-lhs).asUInt, lhs.asUInt)

extension (lhs: Bool)
  inline def ===(rhs: Bool)(using inline m: Module): Bool =
    ${ OperationMacros.bool2Op('lhs, 'rhs, '{ IR.PrimOp.Eq }, 'm) }

  inline def =/=(rhs: Bool)(using inline m: Module): Bool =
    ${ OperationMacros.bool2Op('lhs, 'rhs, '{ IR.PrimOp.Neq }, 'm) }

  inline def &&(rhs: Bool)(using inline m: Module): Bool =
    ${ OperationMacros.bool2Op('lhs, 'rhs, '{ IR.PrimOp.And }, 'm) }

  inline def ||(rhs: Bool)(using inline m: Module): Bool =
    ${ OperationMacros.bool2Op('lhs, 'rhs, '{ IR.PrimOp.Or }, 'm) }

  inline def ^(rhs: Bool)(using inline m: Module): Bool =
    ${ OperationMacros.bool2Op('lhs, 'rhs, '{ IR.PrimOp.Xor }, 'm) }

  inline def unary_!(using inline m: Module): Bool =
    ${ OperationMacros.bool1Op('lhs, '{ IR.PrimOp.Not }, 'm) }

  inline def asUInt(using inline m: Module): UInt =
    ${ OperationMacros.hwdata1OpToUInt('lhs, 'm) }

  inline def asSInt(using inline m: Module): SInt =
    ${ OperationMacros.bool1OpToSInt('lhs, '{ IR.PrimOp.AsSInt }, 'm) }

extension [E <: scala.reflect.Enum] (lhs: HWEnum[E])
  inline def ===(rhs: HWEnum[E])(using inline m: Module): Bool =
    ${ OperationMacros.hwenum2OpToBool('lhs, 'rhs, '{ IR.PrimOp.Eq }, 'm) }

  inline def =/=(rhs: HWEnum[E])(using inline m: Module): Bool =
    ${ OperationMacros.hwenum2OpToBool('lhs, 'rhs, '{ IR.PrimOp.Neq }, 'm) }

  inline def asUInt(using inline m: Module): UInt =
    ${ OperationMacros.hwdata1OpToUInt('lhs, 'm) }

extension (lhs: Reset)
  inline def asUInt(using inline m: Module): UInt =
    ${ OperationMacros.reset1OpToUInt('lhs, '{ IR.PrimOp.AsUInt }, 'm) }

  inline def asBool(using inline m: Module): Bool =
    ${ OperationMacros.reset1OpToBool('lhs, '{ IR.PrimOp.AsUInt }, 'm) }

def clock(using m: Module): Clock =
  m.getImplicitClock

def reset(using m: Module): Reset =
  m.getImplicitReset

inline def Mux[T <: HWData](cond: Bool, tval: T, fval: T)(using inline m: Module): T =
  ${ OperationMacros.muxImpl[T]('cond, 'tval, 'fval, 'm) }

def dontTouch[T <: HWData](data: T)(using m: Module): T =
  ModuleOps.dontTouch(data, m)

extension (xs: Seq[HWData])
  inline def Cat(using inline m: Module): UInt =
    ${ OperationMacros.catSeqImpl('xs, 'm) }

extension (v: Vec[?])
  inline def Cat(using inline m: Module): UInt =
    ${ OperationMacros.catVecImpl('v, 'm) }

extension (h: HWData)
  inline def asUInt(using inline m: Module): UInt =
    ${ OperationMacros.hwdata1OpToUInt('h, 'm) }

extension (dc: DontCare.type)
  inline def as[T <: HWData]: T = dc.asInstanceOf[T]

extension (lhs: UInt)
  def asOH(using m: Module): OneHot =
    ModuleOps.uintToOH(lhs, m)

extension (lhs: OneHot)
  def asUInt(using m: Module): UInt =
    ModuleOps.ohToUInt(lhs, m)

  inline def ===(rhs: OneHot)(using inline m: Module): Bool =
    ${ OperationMacros.onehot2OpToBool('lhs, 'rhs, '{ IR.PrimOp.Eq }, 'm) }

  inline def =/=(rhs: OneHot)(using inline m: Module): Bool =
    ${ OperationMacros.onehot2OpToBool('lhs, 'rhs, '{ IR.PrimOp.Neq }, 'm) }

object OperationMacros:
  def uint2OpMaxWidthPlus1(lhs: Expr[UInt], rhs: Expr[UInt], op: Expr[IR.PrimOp], mod: Expr[Module])(using Quotes): Expr[UInt] =
    val nameOpt = MacroUtils.findEnclosingValName
    '{
      val w = $lhs.getWidth.max($rhs.getWidth) + Width(1)
      ModuleOps.prim2Op(UInt(w), $op, $lhs, $rhs, $mod, ${Expr(nameOpt)})
    }

  def uint2OpAddWidth(lhs: Expr[UInt], rhs: Expr[UInt], op: Expr[IR.PrimOp], mod: Expr[Module])(using Quotes): Expr[UInt] =
    val nameOpt = MacroUtils.findEnclosingValName
    '{
      val w = $lhs.getWidth + $rhs.getWidth
      ModuleOps.prim2Op(UInt(w), $op, $lhs, $rhs, $mod, ${Expr(nameOpt)})
    }

  def uint2OpLhsWidth(lhs: Expr[UInt], rhs: Expr[UInt], op: Expr[IR.PrimOp], mod: Expr[Module])(using Quotes): Expr[UInt] =
    val nameOpt = MacroUtils.findEnclosingValName
    '{
      ModuleOps.prim2Op(UInt($lhs.getWidth), $op, $lhs, $rhs, $mod, ${Expr(nameOpt)})
    }

  def uint2OpMinWidth(lhs: Expr[UInt], rhs: Expr[UInt], op: Expr[IR.PrimOp], mod: Expr[Module])(using Quotes): Expr[UInt] =
    val nameOpt = MacroUtils.findEnclosingValName
    '{
      val w = $lhs.getWidth.min($rhs.getWidth)
      ModuleOps.prim2Op(UInt(w), $op, $lhs, $rhs, $mod, ${Expr(nameOpt)})
    }

  def uint2OpMaxWidth(lhs: Expr[UInt], rhs: Expr[UInt], op: Expr[IR.PrimOp], mod: Expr[Module])(using Quotes): Expr[UInt] =
    val nameOpt = MacroUtils.findEnclosingValName
    '{
      val w = $lhs.getWidth.max($rhs.getWidth)
      ModuleOps.prim2Op(UInt(w), $op, $lhs, $rhs, $mod, ${Expr(nameOpt)})
    }

  def uint2OpToBool(lhs: Expr[UInt], rhs: Expr[UInt], op: Expr[IR.PrimOp], mod: Expr[Module])(using Quotes): Expr[Bool] =
    val nameOpt = MacroUtils.findEnclosingValName
    '{
      ModuleOps.prim2Op(Bool(), $op, $lhs, $rhs, $mod, ${Expr(nameOpt)})
    }

  def uint2OpDynShl(lhs: Expr[UInt], rhs: Expr[UInt], op: Expr[IR.PrimOp], mod: Expr[Module])(using Quotes): Expr[UInt] =
    val nameOpt = MacroUtils.findEnclosingValName
    '{
      val w = $lhs.getWidth.dynamicShiftLeft($rhs.getWidth)
      ModuleOps.prim2Op(UInt(w), $op, $lhs, $rhs, $mod, ${Expr(nameOpt)})
    }

  def uint1Op1ConstAddWidth(lhs: Expr[UInt], const: Expr[Int], op: Expr[IR.PrimOp], mod: Expr[Module])(using Quotes): Expr[UInt] =
    val nameOpt = MacroUtils.findEnclosingValName
    '{
      val w = $lhs.getWidth + $const
      ModuleOps.prim1Op1Const(UInt(w), $op, $lhs, $const, $mod, ${Expr(nameOpt)})
    }

  def uint1Op1ConstShrWidth(lhs: Expr[UInt], const: Expr[Int], op: Expr[IR.PrimOp], mod: Expr[Module])(using Quotes): Expr[UInt] =
    val nameOpt = MacroUtils.findEnclosingValName
    '{
      val w = $lhs.getWidth.shiftRight($const)
      ModuleOps.prim1Op1Const(UInt(w), $op, $lhs, $const, $mod, ${Expr(nameOpt)})
    }

  def uint1Op1ConstFixedWidth(lhs: Expr[UInt], const: Expr[Int], op: Expr[IR.PrimOp], mod: Expr[Module])(using Quotes): Expr[UInt] =
    val nameOpt = MacroUtils.findEnclosingValName
    '{
      ModuleOps.prim1Op1Const(UInt(Width($const)), $op, $lhs, $const, $mod, ${Expr(nameOpt)})
    }

  def uint1Op2ConstBits(lhs: Expr[UInt], hi: Expr[Int], lo: Expr[Int], op: Expr[IR.PrimOp], mod: Expr[Module])(using Quotes): Expr[UInt] =
    val nameOpt = MacroUtils.findEnclosingValName
    '{
      val w = Width($hi - $lo + 1)
      ModuleOps.prim1Op2Const(UInt(w), $op, $lhs, $hi, $lo, $mod, ${Expr(nameOpt)})
    }

  def uint1Op2ConstSingleBit(lhs: Expr[UInt], idx: Expr[Int], op: Expr[IR.PrimOp], mod: Expr[Module])(using Quotes): Expr[UInt] =
    val nameOpt = MacroUtils.findEnclosingValName
    '{
      ModuleOps.prim1Op2Const(UInt(Width(1)), $op, $lhs, $idx, $idx, $mod, ${Expr(nameOpt)})
    }

  def uint1OpToBool(lhs: Expr[UInt], op: Expr[IR.PrimOp], mod: Expr[Module])(using Quotes): Expr[Bool] =
    val nameOpt = MacroUtils.findEnclosingValName
    '{
      ModuleOps.prim1Op(Bool(), $op, $lhs, $mod, ${Expr(nameOpt)})
    }

  def uint1OpSameWidth(lhs: Expr[UInt], op: Expr[IR.PrimOp], mod: Expr[Module])(using Quotes): Expr[UInt] =
    val nameOpt = MacroUtils.findEnclosingValName
    '{
      ModuleOps.prim1Op(UInt($lhs.getWidth), $op, $lhs, $mod, ${Expr(nameOpt)})
    }

  def uint1OpToSInt(lhs: Expr[UInt], op: Expr[IR.PrimOp], mod: Expr[Module])(using Quotes): Expr[SInt] =
    val nameOpt = MacroUtils.findEnclosingValName
    '{
      ModuleOps.prim1Op(SInt($lhs.getWidth), $op, $lhs, $mod, ${Expr(nameOpt)})
    }

  def sint2OpMaxWidthPlus1(lhs: Expr[SInt], rhs: Expr[SInt], op: Expr[IR.PrimOp], mod: Expr[Module])(using Quotes): Expr[SInt] =
    val nameOpt = MacroUtils.findEnclosingValName
    '{
      val w = $lhs.getWidth.max($rhs.getWidth) + Width(1)
      ModuleOps.prim2Op(SInt(w), $op, $lhs, $rhs, $mod, ${Expr(nameOpt)})
    }

  def sint2OpAddWidth(lhs: Expr[SInt], rhs: Expr[SInt], op: Expr[IR.PrimOp], mod: Expr[Module])(using Quotes): Expr[SInt] =
    val nameOpt = MacroUtils.findEnclosingValName
    '{
      val w = $lhs.getWidth + $rhs.getWidth
      ModuleOps.prim2Op(SInt(w), $op, $lhs, $rhs, $mod, ${Expr(nameOpt)})
    }

  def sint2OpLhsWidthPlus1(lhs: Expr[SInt], rhs: Expr[SInt], op: Expr[IR.PrimOp], mod: Expr[Module])(using Quotes): Expr[SInt] =
    val nameOpt = MacroUtils.findEnclosingValName
    '{
      val w = $lhs.getWidth + Width(1)
      ModuleOps.prim2Op(SInt(w), $op, $lhs, $rhs, $mod, ${Expr(nameOpt)})
    }

  def sint2OpMinWidth(lhs: Expr[SInt], rhs: Expr[SInt], op: Expr[IR.PrimOp], mod: Expr[Module])(using Quotes): Expr[SInt] =
    val nameOpt = MacroUtils.findEnclosingValName
    '{
      val w = $lhs.getWidth.min($rhs.getWidth)
      ModuleOps.prim2Op(SInt(w), $op, $lhs, $rhs, $mod, ${Expr(nameOpt)})
    }

  def sint2OpMaxWidth(lhs: Expr[SInt], rhs: Expr[SInt], op: Expr[IR.PrimOp], mod: Expr[Module])(using Quotes): Expr[SInt] =
    val nameOpt = MacroUtils.findEnclosingValName
    '{
      val w = $lhs.getWidth.max($rhs.getWidth)
      ModuleOps.prim2Op(SInt(w), $op, $lhs, $rhs, $mod, ${Expr(nameOpt)})
    }

  def sint2OpToBool(lhs: Expr[SInt], rhs: Expr[SInt], op: Expr[IR.PrimOp], mod: Expr[Module])(using Quotes): Expr[Bool] =
    val nameOpt = MacroUtils.findEnclosingValName
    '{
      ModuleOps.prim2Op(Bool(), $op, $lhs, $rhs, $mod, ${Expr(nameOpt)})
    }

  def sintUint2OpDynShl(lhs: Expr[SInt], rhs: Expr[UInt], op: Expr[IR.PrimOp], mod: Expr[Module])(using Quotes): Expr[SInt] =
    val nameOpt = MacroUtils.findEnclosingValName
    '{
      val w = $lhs.getWidth.dynamicShiftLeft($rhs.getWidth)
      ModuleOps.prim2Op(SInt(w), $op, $lhs, $rhs, $mod, ${Expr(nameOpt)})
    }

  def sintUint2OpLhsWidth(lhs: Expr[SInt], rhs: Expr[UInt], op: Expr[IR.PrimOp], mod: Expr[Module])(using Quotes): Expr[SInt] =
    val nameOpt = MacroUtils.findEnclosingValName
    '{
      ModuleOps.prim2Op(SInt($lhs.getWidth), $op, $lhs, $rhs, $mod, ${Expr(nameOpt)})
    }

  def sint1Op1ConstAddWidth(lhs: Expr[SInt], const: Expr[Int], op: Expr[IR.PrimOp], mod: Expr[Module])(using Quotes): Expr[SInt] =
    val nameOpt = MacroUtils.findEnclosingValName
    '{
      val w = $lhs.getWidth + $const
      ModuleOps.prim1Op1Const(SInt(w), $op, $lhs, $const, $mod, ${Expr(nameOpt)})
    }

  def sint1Op1ConstShrWidth(lhs: Expr[SInt], const: Expr[Int], op: Expr[IR.PrimOp], mod: Expr[Module])(using Quotes): Expr[SInt] =
    val nameOpt = MacroUtils.findEnclosingValName
    '{
      val w = $lhs.getWidth.shiftRight($const)
      ModuleOps.prim1Op1Const(SInt(w), $op, $lhs, $const, $mod, ${Expr(nameOpt)})
    }

  def sint1Op1ConstToUIntFixedWidth(lhs: Expr[SInt], const: Expr[Int], op: Expr[IR.PrimOp], mod: Expr[Module])(using Quotes): Expr[UInt] =
    val nameOpt = MacroUtils.findEnclosingValName
    '{
      ModuleOps.prim1Op1Const(UInt(Width($const)), $op, $lhs, $const, $mod, ${Expr(nameOpt)})
    }

  def sint1Op1ConstToUIntShrWidth(lhs: Expr[SInt], const: Expr[Int], op: Expr[IR.PrimOp], mod: Expr[Module])(using Quotes): Expr[UInt] =
    val nameOpt = MacroUtils.findEnclosingValName
    '{
      val w = $lhs.getWidth.shiftRight($const)
      ModuleOps.prim1Op1Const(UInt(w), $op, $lhs, $const, $mod, ${Expr(nameOpt)})
    }

  def sint1Op2ConstBits(lhs: Expr[SInt], hi: Expr[Int], lo: Expr[Int], op: Expr[IR.PrimOp], mod: Expr[Module])(using Quotes): Expr[UInt] =
    val nameOpt = MacroUtils.findEnclosingValName
    '{
      val w = Width($hi - $lo + 1)
      ModuleOps.prim1Op2Const(UInt(w), $op, $lhs, $hi, $lo, $mod, ${Expr(nameOpt)})
    }

  def sint1Op2ConstSingleBit(lhs: Expr[SInt], idx: Expr[Int], op: Expr[IR.PrimOp], mod: Expr[Module])(using Quotes): Expr[UInt] =
    val nameOpt = MacroUtils.findEnclosingValName
    '{
      ModuleOps.prim1Op2Const(UInt(Width(1)), $op, $lhs, $idx, $idx, $mod, ${Expr(nameOpt)})
    }

  def sint1OpSameWidth(lhs: Expr[SInt], op: Expr[IR.PrimOp], mod: Expr[Module])(using Quotes): Expr[SInt] =
    val nameOpt = MacroUtils.findEnclosingValName
    '{
      ModuleOps.prim1Op(SInt($lhs.getWidth), $op, $lhs, $mod, ${Expr(nameOpt)})
    }

  def sint1OpWidthPlus1(lhs: Expr[SInt], op: Expr[IR.PrimOp], mod: Expr[Module])(using Quotes): Expr[SInt] =
    val nameOpt = MacroUtils.findEnclosingValName
    '{
      ModuleOps.prim1Op(SInt($lhs.getWidth + Width(1)), $op, $lhs, $mod, ${Expr(nameOpt)})
    }

  def sint1OpToUInt(lhs: Expr[SInt], op: Expr[IR.PrimOp], mod: Expr[Module])(using Quotes): Expr[UInt] =
    val nameOpt = MacroUtils.findEnclosingValName
    '{
      ModuleOps.prim1Op(UInt($lhs.getWidth), $op, $lhs, $mod, ${Expr(nameOpt)})
    }

  def bool2Op(lhs: Expr[Bool], rhs: Expr[Bool], op: Expr[IR.PrimOp], mod: Expr[Module])(using Quotes): Expr[Bool] =
    val nameOpt = MacroUtils.findEnclosingValName
    '{
      ModuleOps.prim2Op(Bool(), $op, $lhs, $rhs, $mod, ${Expr(nameOpt)})
    }

  def bool1Op(lhs: Expr[Bool], op: Expr[IR.PrimOp], mod: Expr[Module])(using Quotes): Expr[Bool] =
    val nameOpt = MacroUtils.findEnclosingValName
    '{
      ModuleOps.prim1Op(Bool(), $op, $lhs, $mod, ${Expr(nameOpt)})
    }

  def bool1OpToSInt(lhs: Expr[Bool], op: Expr[IR.PrimOp], mod: Expr[Module])(using Quotes): Expr[SInt] =
    val nameOpt = MacroUtils.findEnclosingValName
    '{
      ModuleOps.prim1Op(SInt(Width(1)), $op, $lhs, $mod, ${Expr(nameOpt)})
    }

  def hwenum2OpToBool[E <: scala.reflect.Enum: Type](lhs: Expr[HWEnum[E]], rhs: Expr[HWEnum[E]], op: Expr[IR.PrimOp], mod: Expr[Module])(using Quotes): Expr[Bool] =
    val nameOpt = MacroUtils.findEnclosingValName
    '{
      if $lhs.enumObj != $rhs.enumObj then
        throw new IllegalArgumentException("Enum type mismatch")
      ModuleOps.prim2Op(Bool(), $op, $lhs, $rhs, $mod, ${Expr(nameOpt)})
    }

  def hwdata1OpToUInt(h: Expr[HWData], mod: Expr[Module])(using Quotes): Expr[UInt] =
    val nameOpt = MacroUtils.findEnclosingValName
    '{
      ModuleOps.prim1Op(UInt($h.getWidth), IR.PrimOp.AsUInt, $h, $mod, ${Expr(nameOpt)})
    }

  def reset1OpToUInt(lhs: Expr[Reset], op: Expr[IR.PrimOp], mod: Expr[Module])(using Quotes): Expr[UInt] =
    val nameOpt = MacroUtils.findEnclosingValName
    '{
      ModuleOps.prim1Op(UInt(1.W), $op, $lhs, $mod, ${Expr(nameOpt)})
    }

  def reset1OpToBool(lhs: Expr[Reset], op: Expr[IR.PrimOp], mod: Expr[Module])(using Quotes): Expr[Bool] =
    val nameOpt = MacroUtils.findEnclosingValName
    '{
      ModuleOps.prim1Op(Bool(), $op, $lhs, $mod, ${Expr(nameOpt)})
    }

  def muxImpl[T <: HWData: Type](cond: Expr[Bool], tval: Expr[T], fval: Expr[T], mod: Expr[Module])(using Quotes): Expr[T] =
    val nameOpt = MacroUtils.findEnclosingValName
    '{
      ModuleOps.mux($cond, $tval, $fval, $mod, ${Expr(nameOpt)})
    }

  def catSeqImpl(xs: Expr[Seq[HWData]], mod: Expr[Module])(using Quotes): Expr[UInt] =
    val nameOpt = MacroUtils.findEnclosingValName
    '{
      ModuleOps.concatBits($xs, $mod, ${Expr(nameOpt)})
    }

  def catVecImpl(v: Expr[Vec[?]], mod: Expr[Module])(using Quotes): Expr[UInt] =
    val nameOpt = MacroUtils.findEnclosingValName
    '{
      ModuleOps.concatBits($v.elems.asInstanceOf[Seq[HWData]], $mod, ${Expr(nameOpt)})
    }

  def onehot2OpToBool(lhs: Expr[OneHot], rhs: Expr[OneHot], op: Expr[IR.PrimOp], mod: Expr[Module])(using Quotes): Expr[Bool] =
    val nameOpt = MacroUtils.findEnclosingValName
    '{
      ModuleOps.prim2Op(Bool(), $op, $lhs, $rhs, $mod, ${Expr(nameOpt)})
    }
