package hdl

import scala.collection.mutable

private[hdl] object ModuleOps:
  private[hdl] def irTypeOf(tpe: HWData): IR.Type = tpe match
    case u: UInt => IR.UIntType(u.getWidth)
    case _: Bool => IR.BoolType
    case _: Clock => IR.ClockType
    case _: Reset => IR.ResetType
    case oh: OneHot => IR.OneHotType(oh.getWidth)
    case e: HWEnum[?] => IR.UIntType(e.getWidth)
    case v: Vec[?] =>
      val elemType = v.elems.headOption.map(irTypeOf).getOrElse(IR.BoolType)
      IR.VecType(v.length, elemType)
    case bundle: Bundle[?] =>
      val p = bundle.asInstanceOf[Product]
      val fields = (0 until p.productArity).flatMap { i =>
        val name = p.productElementName(i)
        fieldInfo(p.productElement(i)).map { case (dir, tpe) =>
          IR.BundleField(IR.Identifier(name), dir == Direction.In, tpe)
        }
      }
      IR.BundleType(fields.toSeq)
    case _: DontCare.type => throw new IllegalArgumentException("DontCare has no concrete type")

  private def fieldInfo(value: Any): Option[(Direction, IR.Type)] = value match
    case h: HWData => Some((h.dir, irTypeOf(h)))
    case opt: Option[?] => opt.flatMap(fieldInfo)
    case it: Iterable[?] =>
      val elems = it.iterator.toSeq
      elems.headOption.flatMap(fieldInfo).map { case (dir, elemType) =>
        (dir, IR.VecType(elems.length, elemType))
      }
    case _ => None

  def io[T <: HWData](t: T, name: Option[String], mod: Module): T =
    val baseName = mod.getBuilder.allocateName(name, "io")
    val inst = HWAggregate.cloneData(t)
    inst.setNodeKind(NodeKind.IO)
    mod.register(inst, Some(baseName))
    emitPortDecl(baseName, inst)
      .foreach(mod.getBuilder.addPort)
    inst

  def wire[T <: HWData](t: T, name: Option[String], mod: Module): T =
    val wireName = mod.getBuilder.allocateName(name, "wire")
    val inst = HWAggregate.cloneData(t)
    inst.setNodeKind(NodeKind.Wire)
    mod.register(inst, Some(wireName))
    mod.getBuilder.addStmt(IR.Wire(IR.Identifier(wireName), irTypeOf(inst)))
    inst

  def reg[T <: HWData](t: T, name: Option[String], mod: Module): T =
    val regName = mod.getBuilder.allocateName(name, "reg")
    val inst = HWAggregate.cloneData(t)
    inst.setNodeKind(NodeKind.Reg)
    mod.register(inst, Some(regName))
    val clockExpr = exprFor(mod.getImplicitClock, mod)
    mod.getBuilder.addStmt(IR.Reg(IR.Identifier(regName), irTypeOf(inst), clockExpr))
    inst

  def regNext[T <: HWData](next: T, name: Option[String], mod: Module): T =
    val regNode = reg(next, name, mod)
    connect(regNode, next, mod)
    regNode

  def regInit[T <: HWData](t: T, name: Option[String], mod: Module): T =
    val initExpr = exprFor(t, mod)
    val inst = HWAggregate.cloneData(t)
    val regName = mod.getBuilder.allocateName(name, "reginit")
    inst.setNodeKind(NodeKind.Reg)
    mod.register(inst, Some(regName))

    val clockExpr = exprFor(mod.getImplicitClock, mod)
    val resetExpr = exprFor(mod.getImplicitReset, mod)
    mod.getBuilder.addStmt(IR.RegReset(IR.Identifier(regName), irTypeOf(inst), clockExpr, resetExpr, initExpr))
    inst

  def lit[T <: HWData](t: T, payload: HostTypeOf[T], mod: Module): T =
    t.setNodeKind(NodeKind.Lit)
    t.setLitVal(payload)
    mod.register(t, None)
    t

  def wireInit[T <: HWData](t: T, name: Option[String], mod: Module): T =
    val initExpr = exprFor(t, mod)
    val inst = HWAggregate.cloneData(t)
    val wireName = mod.getBuilder.allocateName(name, "wireinit")
    inst.setNodeKind(NodeKind.Wire)
    mod.register(inst, Some(wireName))

    val clockExpr = exprFor(mod.getImplicitClock, mod)
    val resetExpr = exprFor(mod.getImplicitReset, mod)
    mod.getBuilder.addStmt(IR.WireInit(IR.Identifier(wireName), irTypeOf(inst), clockExpr, resetExpr, initExpr))
    inst

  def connect[T <: HWData](dst: T, src: T, mod: Module): Unit =
    if src eq DontCare then
      val lhs = locFor(dst, mod)
      mod.getBuilder.addStmt(IR.Invalid(lhs))
    else
      (dst, src) match
        case (de: HWEnum[?], se: HWEnum[?]) =>
          ensureEnumCompat(de, se)
          val lhs = locFor(de, mod)
          val rhs = exprFor(se, mod)
          mod.getBuilder.addStmt(IR.Connect(lhs, rhs))
        case (dv: Vec[?], sv: Vec[?]) =>
          val dElems = dv.elems
          val sElems = sv.elems
          assert(dElems.length == sElems.length)
          val len = dElems.length
          var i = 0
          while i < len do
            connect(dElems(i).asInstanceOf[HWData], sElems(i).asInstanceOf[HWData], mod)
            i += 1
        case _ =>
          val lhs = locFor(dst, mod)
          val rhs = exprFor(src, mod)
          mod.getBuilder.addStmt(IR.Connect(lhs, rhs))

  def when(cond: Bool, mod: Module)(block: => Unit): WhenDSL =
    val condExpr = exprFor(cond, mod)
    val conseq = mod.getBuilder.captureBody {
      block
    }
    val raw = RawWhen(condExpr, conseq, Seq.empty)
    mod.getBuilder.addRaw(raw)
    new WhenDSL(mod, raw)

  private[hdl] def printf(name: Option[String], format: String, args: Seq[HWData], mod: Module): Unit =
    val printfName = mod.getBuilder.allocateName(name, "printf")
    val clockExpr = exprFor(mod.getImplicitClock, mod)
    val enableExpr = IR.DoPrim(IR.PrimOp.Not, Seq(exprFor(mod.getImplicitReset, mod)), Seq.empty)
    val argExprs = args.map(exprFor(_, mod))
    mod.getBuilder.addStmt(IR.Printf(IR.Identifier(printfName), clockExpr, enableExpr, format, argExprs))

  private[hdl] def hwAssert(name: Option[String], cond: Bool, message: String, mod: Module): Unit =
    val assertName = mod.getBuilder.allocateName(name, "assert")
    val clockExpr = exprFor(mod.getImplicitClock, mod)
    val enableExpr = IR.DoPrim(IR.PrimOp.Not, Seq(exprFor(mod.getImplicitReset, mod)), Seq.empty)
    val predExpr = exprFor(cond, mod)
    mod.getBuilder.addStmt(IR.Assert(IR.Identifier(assertName), clockExpr, enableExpr, predExpr, message))

  private def primOp[R <: HWData](result: R, op: IR.PrimOp, args: Seq[HWData], consts: Seq[Int], mod: Module): R =
    result.setNodeKind(NodeKind.PrimOp)
    val name = mod.getBuilder.allocateName(None, op.opName)
    mod.register(result, Some(name))
    val exprArgs = args.map(exprFor(_, mod))
    val expr = IR.DoPrim(op, exprArgs, consts)
    result.setIRExpr(expr)
    result

  def prim2Op[R <: HWData, T <: HWData](
    result: R, op: IR.PrimOp, lhs: T, rhs: T, mod: Module
  ): R =
    primOp(result, op, Seq(lhs, rhs), Seq(), mod)

  def prim1Op1Const[R <: HWData, T <: HWData](
    result: R, op: IR.PrimOp, lhs: T, const: Int, mod: Module
  ): R =
    primOp(result, op, Seq(lhs), Seq(const), mod)

  def prim1Op2Const[R <: HWData, T <: HWData](
    result: R, op: IR.PrimOp, lhs: T, a: Int, b: Int, mod: Module
  ): R =
    primOp(result, op, Seq(lhs), Seq(a, b), mod)

  def prim1Op[R <: HWData, T <: HWData](
    result: R, op: IR.PrimOp, lhs: T, mod: Module
  ): R =
    primOp(result, op, Seq(lhs), Seq(), mod)

  def mux[T <: HWData](cond: Bool, tval: T, fval: T, mod: Module): T =
    primOp(HWAggregate.cloneData(tval), IR.PrimOp.Mux, Seq(cond, tval, fval), Seq.empty, mod)

  def asUInt(value: HWData, mod: Module): UInt =
    prim1Op(UInt(value.getWidth), IR.PrimOp.AsUInt, value, mod)

  def ohToUInt(value: OneHot, mod: Module): UInt =
    prim1Op(UInt(value.getWidth), IR.PrimOp.AsUInt, value, mod)

  def uintToOH(value: UInt, mod: Module): OneHot =
    prim1Op(OneHot(value.getWidth), IR.PrimOp.AsUInt, value, mod)

  def concatBits(values: Seq[HWData], mod: Module): UInt =
    val flat = values.flatMap {
      case u: UInt      => Seq(u)
      case b: Bool      => Seq(asUInt(b, mod))
      case c: Clock     => Seq(asUInt(c, mod))
      case r: Reset     => Seq(asUInt(r, mod))
      case oh: OneHot   => Seq(ohToUInt(oh, mod))
      case v: Vec[?]    => Seq(asUInt(v, mod))
      case e: HWEnum[?] => Seq(asUInt(e, mod))
      case b: Bundle[?] => Seq(asUInt(b, mod))
      case _: DontCare.type => Seq.empty
    }
    if flat.isEmpty then
      throw new IllegalArgumentException("Cannot concat empty sequence")

    val width = values.map(_.getWidth).reduce(_ + _)
    primOp(UInt(width), IR.PrimOp.Cat, flat, Seq.empty, mod)

// def pad(lhs: UInt, width: Int, mod: Module): UInt =
// primUInt(IR.PrimOp.Pad, Seq(lhs), Seq(width), mod)

  private def refFor(data: HWData, current: Module): IR.Identifier =
    val base = data.getRef.map(_.value).getOrElse(data.toString)
    val name = data.getOwner match
      case Some(owner) if owner.ne(current) =>
        owner.instanceName.map(prefix => s"$prefix.$base").getOrElse(base)
      case _ => base
    IR.Identifier(name)

  private def locFor(data: HWData, current: Module): IR.Expr =
    data.getIRExpr.getOrElse(IR.Ref(refFor(data, current)))

  def exprFor(data: HWData, current: Module): IR.Expr =
    if data eq DontCare then IR.DontCare
    else if data.kind == NodeKind.Lit then IR.Literal(formatLiteral(data, data.literal.getOrElse("")))
    else data.getIRExpr.getOrElse(IR.Ref(refFor(data, current)))

  def emitPortDecl[T <: HWData](name: String, tpe: T): Seq[IR.Port] =
    val dir = if tpe.dir == Direction.In then Direction.In else Direction.Out
    Seq(IR.Port(IR.Identifier(name), dir, irTypeOf(tpe)))

  def formatLiteral(tpe: HWData, value: Any): IR.Identifier = tpe match
    case u: UInt =>
      val w = u.getWidth
      if w.known then IR.Identifier(s"UInt<${w.get}>($value)") else IR.Identifier(s"UInt($value)")
    case _: Bool => IR.Identifier(s"Bool($value)")
    case _: Clock => IR.Identifier(s"Clock($value)")
    case _: Reset => IR.Identifier(s"Reset($value)")
    case oh: OneHot =>
      val w = oh.getWidth
      if w.known then IR.Identifier(s"OneHot<${w.get}>($value)") else IR.Identifier(s"OneHot($value)")
    case e: HWEnum[?] =>
      val ord = value.asInstanceOf[scala.reflect.Enum].ordinal
      val w = e.getWidth
      if w.known then IR.Identifier(s"UInt<${w.get}>($ord)") else IR.Identifier(s"UInt($ord)")
    case _ => IR.Identifier(value.toString)

  private def dirPrefixOf(h: HWData): String =
    if h.dir == Direction.In then "flip " else ""

  def assignOwner[T](value: T, mod: Module): Unit =
    HWAggregate.foreach(value, "")((h, _) => h.setOwner(mod))

  def assignRefs[T](value: T, base: String): Unit =
    HWAggregate.foreach(value, base)((h, path) => h.setRef(IR.Identifier(path)))

  private def ensureEnumCompat(a: HWEnum[?], b: HWEnum[?]): Unit =
    if a.enumObj != b.enumObj then
      throw new IllegalArgumentException("Enum type mismatch")

extension [T <: HWData](dst: T)
  def :=(src: T)(using m: Module): Unit =
    ModuleOps.connect(dst, src, m)

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

  def asBool(using m: Module): Bool =
    ModuleOps.prim1Op(Bool(), IR.PrimOp.AsBool, lhs, m)

  def unary_~(using m: Module): UInt =
    ModuleOps.prim1Op(UInt(lhs.getWidth), IR.PrimOp.Not, lhs, m)

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

def Mux[T <: HWData](cond: Bool, tval: T, fval: T)(using m: Module): T =
  ModuleOps.mux(cond, tval, fval, m)

extension (xs: Seq[HWData])
  def Cat(using m: Module): UInt =
    ModuleOps.concatBits(xs, m)

extension (v: Vec[?])
  def Cat(using m: Module): UInt =
    ModuleOps.concatBits(v.elems.asInstanceOf[Seq[HWData]], m)

extension (h: HWData)
  def asUInt(using m: Module): UInt =
    ModuleOps.asUInt(h, m)

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

extension [T <: HWData](selector: T)
  inline infix def switch(inline body: SwitchBuilder[T] ?=> Unit)(using m: Module): Unit =
    given SwitchBuilder[T] = new SwitchBuilder(selector, summon[Module])
    body

def is[T <: HWData](value: T)(block: => Unit)(using builder: SwitchBuilder[T]): Unit =
  builder.addCase(value)(block)

def default(block: => Unit)(using builder: SwitchBuilder[?]): Unit =
  builder.addDefault(block)

final class WhenDSL(private val mod: Module, private val current: RawWhen):
  def elsewhen(cond: Bool)(block: => Unit): WhenDSL =
    val body = mod.getBuilder.captureBody {
      block
    }
    val nested = RawWhen(ModuleOps.exprFor(cond, mod), body, Seq.empty)
    current.alt = Seq(nested)
    new WhenDSL(mod, nested)

  def otherwise(block: => Unit): Unit =
    current.alt = mod.getBuilder.captureBody {
      block
    }

final class SwitchBuilder[T <: HWData](selector: T, mod: Module):
  private var current: Option[WhenDSL] = None

  def addCase(value: T)(block: => Unit): Unit =
    val cond = SwitchBuilder.condition(selector, value, mod)
    val next = current match
      case None => ModuleOps.when(cond, mod) {
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
      case None => ModuleOps.when(true.B, mod) {
        block
      }

object SwitchBuilder:
  def condition(selector: HWData, value: HWData, mod: Module): Bool = (selector, value) match
    case (l: UInt, r: UInt) => ModuleOps.prim2Op(Bool(), IR.PrimOp.Eq, l, r, mod)
    case (l: Bool, r: Bool) => ModuleOps.prim2Op(Bool(), IR.PrimOp.Eq, l, r, mod)
    case (l: HWEnum[?], r: HWEnum[?]) =>
      if l.enumObj != r.enumObj then throw new IllegalArgumentException("Enum type mismatch")
      ModuleOps.prim2Op(Bool(), IR.PrimOp.Eq, l, r, mod)
    case (l: OneHot, r: OneHot) =>
      ModuleOps.prim2Op(Bool(), IR.PrimOp.Eq, l, r, mod)
    case _ => throw new IllegalArgumentException("Switch only supports UInt, Bool, Enum, and OneHot types")
