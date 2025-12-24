package hdl

import scala.collection.mutable

private[hdl] object ModuleOps:
  private[hdl] def irTypeOf(tpe: HWData): IR.Type = tpe match
    case u: UInt => IR.UIntType(u.getWidth)
    case s: SInt => IR.SIntType(s.getWidth)
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

  private def resolveName(
    explicit: Option[String],
    data: HWData,
    fallback: String,
    mod: Module
  ): String =
    mod.getBuilder.allocateName(explicit, fallback)

  def io[T <: HWData](t: T, name: Option[String], mod: Module): T =
    val inst = HWAggregate.cloneData(t)
    val baseName = resolveName(name, inst, "io", mod)
    inst.setNodeKind(NodeKind.IO)
    mod.register(inst, Some(baseName))
    emitPortDecl(baseName, inst)
      .foreach(mod.getBuilder.addPort)
    inst

  def wire[T <: HWData](t: T, name: Option[String], mod: Module): T =
    val inst = HWAggregate.cloneData(t)
    val wireName = resolveName(name, inst, "wire", mod)
    inst.setNodeKind(NodeKind.Wire)
    mod.register(inst, Some(wireName))
    mod.getBuilder.addStmt(IR.Wire(IR.Identifier(wireName), irTypeOf(inst)))
    inst

  def reg[T <: HWData](t: T, name: Option[String], mod: Module): T =
    val inst = HWAggregate.cloneData(t)
    val regName = resolveName(name, inst, "reg", mod)
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
    val regName = resolveName(name, inst, "reginit", mod)
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
    val wireName = resolveName(name, inst, "wireinit", mod)
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
          assert(dElems.length == sElems.length, s"d ${dElems.length} s ${sElems.length}")
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
    WhenOps.when(cond, mod)(block)

  private[hdl] def printf(name: Option[String], format: String, args: Seq[HWData], mod: Module): Unit =
    val printfName = mod.getBuilder.allocateName(name, "printf")
    val clockExpr = exprFor(mod.getImplicitClock, mod)
    val enableExpr = IR.DoPrim(IR.PrimOp.Not, Seq(exprFor(mod.getImplicitReset, mod)), Seq.empty)
    val argExprs = args.map(exprFor(_, mod))
    mod.getBuilder.addStmt(IR.Printf(IR.Identifier(printfName), clockExpr, enableExpr, format, argExprs))

  private[hdl] def hwAssert(name: Option[String], cond: Bool, message: String, mod: Module): Unit =
    val assertName = mod.getBuilder.allocateName(name, "assert")
    val clockExpr = exprFor(mod.getImplicitClock, mod)
    val enableExpr = IR.DoPrim(
      IR.PrimOp.Not,
      Seq(
        IR.DoPrim(IR.PrimOp.AsUInt, Seq(exprFor(mod.getImplicitReset, mod)))
      )
    )
    val predExpr = exprFor(cond, mod)
    mod.getBuilder.addStmt(IR.Assert(IR.Identifier(assertName), clockExpr, enableExpr, predExpr, message))

  private def primOp[R <: HWData](
    result: R,
    op: IR.PrimOp,
    args: Seq[HWData],
    consts: Seq[Int],
    mod: Module,
    nameHint: Option[String]
  ): R =
    result.setNodeKind(NodeKind.PrimOp)
    val exprArgs = args.map(exprFor(_, mod))
    val expr = IR.DoPrim(op, exprArgs, consts)
    nameHint match
      case Some(hint) =>
        val nodeName = mod.getBuilder.allocateName(Some(hint), hint)
        mod.register(result, Some(nodeName))
        mod.getBuilder.addStmt(IR.DefNode(IR.Identifier(nodeName), expr))
        result.setIRExpr(IR.Ref(IR.Identifier(nodeName)))
      case None =>
        val name = mod.getBuilder.allocateName(None, op.opName)
        mod.register(result, Some(name))
        result.setIRExpr(expr)
    result

  def prim2Op[R <: HWData, T <: HWData](
    result: R, op: IR.PrimOp, lhs: T, rhs: T, mod: Module, nameHint: Option[String] = None
  ): R =
    primOp(result, op, Seq(lhs, rhs), Seq(), mod, nameHint)

  def prim1Op1Const[R <: HWData, T <: HWData](
    result: R, op: IR.PrimOp, lhs: T, const: Int, mod: Module, nameHint: Option[String] = None
  ): R =
    primOp(result, op, Seq(lhs), Seq(const), mod, nameHint)

  def prim1Op2Const[R <: HWData, T <: HWData](
    result: R, op: IR.PrimOp, lhs: T, a: Int, b: Int, mod: Module, nameHint: Option[String] = None
  ): R =
    primOp(result, op, Seq(lhs), Seq(a, b), mod, nameHint)

  def prim1Op[R <: HWData, T <: HWData](
    result: R, op: IR.PrimOp, lhs: T, mod: Module, nameHint: Option[String] = None
  ): R =
    primOp(result, op, Seq(lhs), Seq(), mod, nameHint)

  def prim3Op[R <: HWData](
    result: R, op: IR.PrimOp, a: HWData, b: HWData, c: HWData, mod: Module, nameHint: Option[String] = None
  ): R =
    primOp(result, op, Seq(a, b, c), Seq(), mod, nameHint)

  def mux[T <: HWData](cond: Bool, tval: T, fval: T, mod: Module, nameHint: Option[String] = None): T =
    primOp(HWAggregate.cloneData(tval), IR.PrimOp.Mux, Seq(cond, tval, fval), Seq.empty, mod, nameHint)

  def asUInt(value: HWData, mod: Module): UInt =
    prim1Op(UInt(value.getWidth), IR.PrimOp.AsUInt, value, mod)

  def ohToUInt(value: OneHot, mod: Module): UInt =
    prim1Op(UInt(value.getWidth), IR.PrimOp.AsUInt, value, mod)

  def uintToOH(value: UInt, mod: Module): OneHot =
    prim1Op(OneHot(value.getWidth), IR.PrimOp.AsUInt, value, mod)

  def concatBits(values: Seq[HWData], mod: Module, nameHint: Option[String] = None): UInt =
    val flat = values.flatMap {
      case u: UInt      => Seq(u)
      case s: SInt      => Seq(asUInt(s, mod))
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

    val bits = flat.map {
      case u: UInt => u
      case other => asUInt(other, mod)
    }

    if bits.length == 1 then
      bits.head
    else
      val numCats = bits.length - 1
      bits.tail.zipWithIndex.foldLeft(bits.head) { case (acc, (next, idx)) =>
        val width = acc.getWidth + next.getWidth
        val isLast = idx == numCats - 1
        val catName = if isLast then nameHint else None
        prim2Op(UInt(width), IR.PrimOp.Cat, acc, next, mod, catName)
      }

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
    case s: SInt =>
      val w = s.getWidth
      if w.known then IR.Identifier(s"SInt<${w.get}>($value)") else IR.Identifier(s"SInt($value)")
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

  private def pathFromExpr(expr: IR.Expr): Option[String] = expr match
    case IR.Ref(name) => Some(name.value)
    case IR.SubField(inner, field) => pathFromExpr(inner).map(base => s"$base.${field.value}")
    case IR.SubIndex(inner, value) => pathFromExpr(inner).map(base => s"$base[$value]")
    case _ => None

  def dontTouch[T <: HWData](data: T, mod: Module): T =
    if data eq DontCare then
      throw new IllegalArgumentException("Cannot annotate DontCare")
    val base = pathFromExpr(locFor(data, mod)).getOrElse {
      throw new IllegalArgumentException("dontTouch requires a static hardware reference")
    }
    HWAggregate.foreach(data, base) { (h, path) =>
      h match
        case _: Vec[?] | _: Bundle[?] =>
          ()
        case _ =>
          mod.getBuilder.addDontTouch(path)
    }
    data
