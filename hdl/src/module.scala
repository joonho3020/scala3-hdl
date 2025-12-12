package hdl

import scala.collection.mutable
import scala.compiletime.summonInline

final case class ElaboratedDesign(name: String, ir: IR.Module) extends Serializable

private sealed trait RawStmt
private final case class RawInst(instName: String, mod: Module, baseModule: String) extends RawStmt
private final case class RawWhen(cond: IR.Expr, conseq: Seq[RawStmt], var alt: Seq[RawStmt]) extends RawStmt
private final case class RawLeaf(stmt: IR.Stmt) extends RawStmt

final class ModuleBuilder(val moduleBaseName: String):
  private val ports = mutable.ArrayBuffer.empty[IR.Port]
  private val usedNames = mutable.Set.empty[String]
  private val bodyStack = mutable.Stack[mutable.ArrayBuffer[RawStmt]]()
  bodyStack.push(mutable.ArrayBuffer.empty[RawStmt])
  private var tempCounter = 0

  def freshName(prefix: String): String =
    tempCounter += 1
    val candidate = s"${prefix}_$tempCounter"
    if usedNames.contains(candidate) then freshName(prefix)
    else
      usedNames += candidate
      candidate

  private def isSaneName(n: String): Boolean =
    n.nonEmpty && n.matches("^[A-Za-z_][A-Za-z0-9_]*$")

  def allocateName(name: Option[String], prefix: String): String =
    name.filter(n => isSaneName(n) && !usedNames.contains(n)).map { n =>
      usedNames += n
      n
    }.getOrElse(freshName(prefix))

  def addPort(port: IR.Port): Unit = ports += port
  private def currentBody: mutable.ArrayBuffer[RawStmt] = bodyStack.head

  def addRaw(stmt: RawStmt): Unit = currentBody += stmt
  def addStmt(stmt: IR.Stmt): Unit = addRaw(RawLeaf(stmt))
  def addInst(name: String, mod: Module, base: String): Unit =
    addRaw(RawInst(name, mod, base))

  def captureBody(thunk: => Unit): Seq[RawStmt] =
    val buf = mutable.ArrayBuffer.empty[RawStmt]
    bodyStack.push(buf)
    try
      thunk
      buf.toSeq
    finally
      bodyStack.pop()

  def snapshot(label: String, instLabels: Map[Module, String]): ElaboratedDesign =
    val body = currentBody.toSeq.map(rawToIR(_, instLabels))
    val mod = IR.Module(label, ports.toSeq, body)
    ElaboratedDesign(label, mod)

  private def rawToIR(rs: RawStmt, instLabels: Map[Module, String]): IR.Stmt = rs match
    case RawLeaf(s) => s
    case RawInst(name, m, base) =>
      val target = instLabels.getOrElse(m, base)
      IR.Inst(name, target)
    case w: RawWhen =>
      val conseq = w.conseq.map(rawToIR(_, instLabels))
      val alt = w.alt.map(rawToIR(_, instLabels))
      IR.When(w.cond, conseq, alt)

abstract class Module:
  private val _builder = new ModuleBuilder(moduleName)
  private val _children = mutable.ArrayBuffer.empty[Module]
  private var _instanceName: Option[String] = None
  private var _bodyFn: Option[Module ?=> Unit] = None
  private var _bodyRan = false

  private var _implicitClock: Clock =
    val name = this.getBuilder.allocateName(Some("clock"), "clock")
    val clk = Input(Clock())
    clk.setNodeKind(NodeKind.IO)
    this.register(clk, Some(name))
    this.getBuilder.addPort(IR.Port(name, Direction.In, IR.ResetType))
    clk

  private var _implicitReset: Reset =
    val name = this.getBuilder.allocateName(Some("reset"), "reset")
    val reset = Input(Reset())
    reset.setNodeKind(NodeKind.IO)
    this.register(reset, Some(name))
    this.getBuilder.addPort(IR.Port(name, Direction.In, IR.ResetType))
    reset

  def moduleName: String = getClass.getSimpleName.stripSuffix("$")
  private[hdl] def setInstanceName(n: String): Unit = _instanceName = Some(n)
  private[hdl] def instanceName: Option[String] = _instanceName
  private[hdl] def addChild(m: Module): Unit = _children += m
  private[hdl] def children: Seq[Module] = _children.toSeq
  private[hdl] def getBuilder: ModuleBuilder = _builder
  private[hdl] def register[T](data: T, ref: Option[String])(using WalkHW[T]): Unit =
    ModuleOps.assignOwner(data, this)
    ref.foreach(r => ModuleOps.assignRefs(data, r))
  private[hdl] def getImplicitClock: Clock = _implicitClock
  private[hdl] def getImplicitReset: Reset = _implicitReset

  // Storing the module body as a thunk is required in order to achieve
  // lazy elaboration.
  // If the module body is elaborated eagerly, there is no point in
  // incremental elaboration and caching.
  protected final def body(f: Module ?=> Unit): Unit =
    _bodyFn = Some(f)

  private[hdl] def runBody(): Unit =
    if !_bodyRan then
      given Module = this
      _bodyFn.foreach(fn => fn(using summon[Module]))
      _bodyRan = true

  protected inline def IO[T <: HWData](inline t: T)(using WalkHW[T]): T =
    ${ ModuleMacros.ioImpl('t, 'this) }

  protected inline def Wire[T <: HWData](inline t: T)(using WalkHW[T]): T =
    ${ ModuleMacros.wireImpl('t, 'this) }

  protected inline def Reg[T <: HWData](inline t: T)(using WalkHW[T]): T =
    ${ ModuleMacros.regImpl('t, 'this) }

  protected inline def RegInit[T <: HWData](inline t: T)(using WalkHW[T]): T =
    ${ ModuleMacros.regInitImpl('t, 'this) }

  protected inline def WireInit[T <: HWData](inline t: T)(inline init: T)(using WalkHW[T]): T =
    ${ ModuleMacros.wireInitImpl('t, 'init, 'this) }

  protected inline def Lit[T <: HWData](inline t: T)(inline payload: HostTypeOf[T])(using WalkHW[T]): T =
    ${ ModuleMacros.litImpl('t, 'payload, 'this) }

  protected inline def when(cond: Bool)(block: => Unit)(using m: Module): WhenDSL =
    ModuleOps.when(cond, summon[Module]) {
      block
    }

// protected inline def Cat[T <: HWData](inline t: Seq[T])(using WalkHW[T]): T =
// ModuleOps.cat

object Module:
  inline def apply[M <: hdl.Module](inline gen: M)(using inline parent: hdl.Module): M =
    ${ ModuleMacros.moduleInstImpl('gen, 'parent) }

  def instantiate[M <: hdl.Module](gen: => M, parent: hdl.Module, name: Option[String]): M =
    val sub = gen
    val instName = parent.getBuilder.allocateName(name, "inst")
    sub.setInstanceName(instName)
    parent.addChild(sub)
    parent.getBuilder.addInst(instName, sub, sub.moduleName)
    sub

private[hdl] object ModuleOps:
  private def irTypeOf(tpe: HWData): IR.Type = tpe match
    case u: UInt => IR.UIntType(u.w)
    case _: Bool => IR.BoolType
    case _: Clock => IR.ClockType
    case _: Reset => IR.ResetType
    case v: Vec[?] =>
      val elemType = v.elems.headOption.map(irTypeOf).getOrElse(IR.BoolType)
      IR.VecType(v.length, elemType)
    case bundle: Bundle[?] =>
      val p = bundle.asInstanceOf[Product]
      val fields = (0 until p.productArity).flatMap { i =>
        p.productElement(i) match
          case hd: HWData =>
            val dirFlip = hd.dir == Direction.In
            Some(IR.BundleField(p.productElementName(i), dirFlip, irTypeOf(hd)))
          case _ => Nil
      }
      IR.BundleType(fields.toSeq)

  def io[T <: HWData](t: T, name: Option[String], mod: Module)(using WalkHW[T]): T =
    val baseName = mod.getBuilder.allocateName(name, "io")
    t.setNodeKind(NodeKind.IO)
    mod.register(t, Some(baseName))
    emitPortDecl(baseName, t)
      .foreach(mod.getBuilder.addPort)
    t

  def wire[T <: HWData](t: T, name: Option[String], mod: Module)(using WalkHW[T]): T =
    val wireName = mod.getBuilder.allocateName(name, "wire")
    t.setNodeKind(NodeKind.Wire)
    mod.register(t, Some(wireName))
    mod.getBuilder.addStmt(IR.Wire(wireName, irTypeOf(t)))
    t

  def reg[T <: HWData](t: T, name: Option[String], mod: Module)(using WalkHW[T]): T =
    val regName = mod.getBuilder.allocateName(name, "reg")
    t.setNodeKind(NodeKind.Reg)
    mod.register(t, Some(regName))
    val clockExpr = exprFor(mod.getImplicitClock, mod)
    mod.getBuilder.addStmt(IR.Reg(regName, irTypeOf(t), clockExpr))
    t

  def regInit[T <: HWData](t: T, name: Option[String], mod: Module)(using WalkHW[T]): T =
    val initValue = t
    val regName = mod.getBuilder.allocateName(name, "reginit")
    t.setNodeKind(NodeKind.Reg)
    mod.register(t, Some(regName))

    val clockExpr = exprFor(mod.getImplicitClock, mod)
    val resetExpr = exprFor(mod.getImplicitReset, mod)
    val initExpr = exprFor(initValue, mod)
    mod.getBuilder.addStmt(IR.RegInit(regName, irTypeOf(t), clockExpr, resetExpr, initExpr))
    t

  def lit[T <: HWData](t: T, payload: HostTypeOf[T], mod: Module)(using WalkHW[T]): T =
    t.setNodeKind(NodeKind.Lit)
    t.setLitVal(payload)
    mod.register(t, None)
    t

  def wireInit[T <: HWData](t: T, init: T, name: Option[String], mod: Module)(using WalkHW[T]): T =
    val wireName = mod.getBuilder.allocateName(name, "wireinit")
    t.setNodeKind(NodeKind.Wire)
    mod.register(t, Some(wireName))

    val clockExpr = exprFor(mod.getImplicitClock, mod)
    val resetExpr = exprFor(mod.getImplicitReset, mod)
    val initExpr = exprFor(init, mod)
    mod.getBuilder.addStmt(IR.WireInit(wireName, irTypeOf(t), clockExpr, resetExpr, initExpr))
    t

  def connect[T <: HWData](dst: T, src: T, mod: Module): Unit =
    (dst, src) match
      case (dv: Vec[?], sv: Vec[?]) =>
        val dElems = dv.elems
        val sElems = sv.elems
        val len = math.min(dElems.length, sElems.length)
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

  private def primOp[R <: HWData, T <: HWData](result: R, op: IR.PrimOp, args: Seq[T], consts: Seq[Int], mod: Module): R =
    result.setNodeKind(NodeKind.PrimOp)
    val name = mod.getBuilder.allocateName(None, op.opName)
    mod.register(result, Some(name))
    val exprArgs = args.map(exprFor(_, mod))
    val expr = IR.DoPrim(op, exprArgs, consts)
    result.setIRExpr(expr)
    result

  def prim2Op[R <: HWData, T <: HWData](result: R, op: IR.PrimOp, lhs: T, rhs: T, mod: Module): R =
    primOp(result, op, Seq(lhs, rhs), Seq(), mod)

  def prim1Op1Const[R <: HWData, T <: HWData](result: R, op: IR.PrimOp, lhs: T, const: Int, mod: Module): R =
    primOp(result, op, Seq(lhs), Seq(const), mod)

  def prim1Op2Const[R <: HWData, T <: HWData](result: R, op: IR.PrimOp, lhs: T, a: Int, b: Int, mod: Module): R =
    primOp(result, op, Seq(lhs), Seq(a, b), mod)

  def prim1Op[R <: HWData, T <: HWData](result: R, op: IR.PrimOp, lhs: T, mod: Module): R =
    primOp(result, op, Seq(lhs), Seq(), mod)

// def cat[T <: HWData](x: Seq[T], mod: Module): T =
// primUInt(IR.PrimOp.Cat, x, Seq.empty, mod)

// def pad(lhs: UInt, width: Int, mod: Module): UInt =
// primUInt(IR.PrimOp.Pad, Seq(lhs), Seq(width), mod)

  private def refFor(data: HWData, current: Module): String =
    val base = data.getRef.getOrElse(data.toString)
    data.getOwner match
      case Some(owner) if owner.ne(current) =>
        owner.instanceName.map(prefix => s"$prefix.$base").getOrElse(base)
      case _ => base

  private def locFor(data: HWData, current: Module): IR.Expr =
    data.getIRExpr.getOrElse(IR.Ref(refFor(data, current)))

  def exprFor(data: HWData, current: Module): IR.Expr =
    if data.kind == NodeKind.Lit then IR.Literal(formatLiteral(data, data.literal.getOrElse("")))
    else data.getIRExpr.getOrElse(IR.Ref(refFor(data, current)))

  def emitPortDecl[T <: HWData](name: String, tpe: T)(using w: WalkHW[T]): Seq[IR.Port] =
    val dir = if tpe.dir == Direction.In then Direction.In else Direction.Out
    Seq(IR.Port(name, dir, irTypeOf(tpe)))

  def formatType(tpe: HWData): String = tpe match
    case u: UInt => u.w.map(v => s"UInt<$v>").getOrElse("UInt")
    case _: Bool => "Bool"
    case _: Clock => "Clock"
    case _: Reset => "Reset"
    case v: Vec[?] =>
      val elemStr = v.elems.headOption.map(e => formatType(e)).getOrElse("?")
      s"Vec<${v.length}, $elemStr>"
    case bundle: Bundle[?] =>
      val p = bundle.asInstanceOf[Product]
      val fields = (0 until p.productArity).flatMap { i =>
        val fieldName = p.productElementName(i)
        p.productElement(i) match
          case hd: HWData =>
            val dirPrefix = dirPrefixOf(hd)
            Some(s"$dirPrefix$fieldName : ${formatType(hd)}")
          case _ => Nil
      }
      s"{ ${fields.mkString(", ")} }"
    case _ => tpe.toString

  def formatLiteral(tpe: HWData, value: Any): String = tpe match
    case u: UInt => u.w.map(w => s"UInt<$w>($value)").getOrElse(s"UInt($value)")
    case _: Bool => s"Bool($value)"
    case _: Clock => s"Clock($value)"
    case _: Reset => s"Reset($value)"
    case _ => value.toString

  private def dirPrefixOf(h: HWData): String =
    if h.dir == Direction.In then "flip " else ""

  def assignOwner[T](value: T, mod: Module)(using w: WalkHW[T]): Unit =
    w(value, "")((h, _) => h.setOwner(mod))

  def assignRefs[T](value: T, base: String)(using w: WalkHW[T]): Unit =
    w(value, base)((h, path) => h.setRef(path))

extension [T <: HWData](dst: T)
  def :=(src: T)(using m: Module): Unit =
    ModuleOps.connect(dst, src, m)

extension (lhs: UInt)
  def +(rhs: UInt)(using m: Module): UInt =
    ModuleOps.prim2Op(UInt(), IR.PrimOp.Add, lhs, rhs, m)

  def -(rhs: UInt)(using m: Module): UInt =
    ModuleOps.prim2Op(UInt(), IR.PrimOp.Sub, lhs, rhs, m)

  def *(rhs: UInt)(using m: Module): UInt =
    ModuleOps.prim2Op(UInt(), IR.PrimOp.Mul, lhs, rhs, m)

  def /(rhs: UInt)(using m: Module): UInt =
    ModuleOps.prim2Op(UInt(), IR.PrimOp.Div, lhs, rhs, m)

  def %(rhs: UInt)(using m: Module): UInt =
    ModuleOps.prim2Op(UInt(), IR.PrimOp.Rem, lhs, rhs, m)

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
    ModuleOps.prim2Op(UInt(), IR.PrimOp.DShl, lhs, rhs, m)

  def >>(rhs: UInt)(using m: Module): UInt =
    ModuleOps.prim2Op(UInt(), IR.PrimOp.DShr, lhs, rhs, m)

  def <<(rhs: Int)(using m: Module): UInt =
    ModuleOps.prim1Op1Const(UInt(), IR.PrimOp.Shl, lhs, rhs, m)

  def >>(rhs: Int)(using m: Module): UInt =
    ModuleOps.prim1Op1Const(UInt(), IR.PrimOp.Shr, lhs, rhs, m)

  def &(rhs: UInt)(using m: Module): UInt =
    ModuleOps.prim2Op(UInt(), IR.PrimOp.And, lhs, rhs, m)

  def |(rhs: UInt)(using m: Module): UInt =
    ModuleOps.prim2Op(UInt(), IR.PrimOp.Or, lhs, rhs, m)

  def ^(rhs: UInt)(using m: Module): UInt =
    ModuleOps.prim2Op(UInt(), IR.PrimOp.Xor, lhs, rhs, m)

// def pad(n: Int)(using m: Module): UInt =
// ModuleOps.pad(lhs, n, m)
  def head(n: Int)(using m: Module): UInt =
    ModuleOps.prim1Op1Const(UInt(), IR.PrimOp.Head, lhs, n, m)

  def tail(n: Int)(using m: Module): UInt =
    ModuleOps.prim1Op1Const(UInt(), IR.PrimOp.Tail, lhs, n, m)

  def bits(hi: Int, lo: Int)(using m: Module): UInt =
    ModuleOps.prim1Op2Const(UInt(), IR.PrimOp.Tail, lhs, lo, hi, m)

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

object ModuleMacros:
  import scala.quoted.*

  private def findEnclosingValName(using Quotes): Option[String] =
    import quotes.reflect.*
    def loop(sym: Symbol): Option[String] =
      if sym.isNoSymbol then None
      else if sym.isValDef && !sym.flags.is(Flags.Synthetic) && !sym.flags.is(Flags.Artifact) then Some(sym.name)
      else loop(sym.owner)
    loop(Symbol.spliceOwner)

  def ioImpl[T <: HWData: Type](t: Expr[T], mod: Expr[Module])(using Quotes): Expr[T] =
    val nameOpt = findEnclosingValName
    '{
      ModuleOps.io($t, ${Expr(nameOpt)}, $mod)(using summonInline[WalkHW[T]])
    }

  def wireImpl[T <: HWData: Type](t: Expr[T], mod: Expr[Module])(using Quotes): Expr[T] =
    val nameOpt = findEnclosingValName
    '{
      ModuleOps.wire($t, ${Expr(nameOpt)}, $mod)(using summonInline[WalkHW[T]])
    }

  def regImpl[T <: HWData: Type](t: Expr[T], mod: Expr[Module])(using Quotes): Expr[T] =
    val nameOpt = findEnclosingValName
    '{
      ModuleOps.reg($t, ${Expr(nameOpt)}, $mod)(using summonInline[WalkHW[T]])
    }

  def regInitImpl[T <: HWData: Type](t: Expr[T], mod: Expr[Module])(using Quotes): Expr[T] =
    val nameOpt = findEnclosingValName
    '{
      ModuleOps.regInit($t, ${Expr(nameOpt)}, $mod)(using summonInline[WalkHW[T]])
    }

  def wireInitImpl[T <: HWData: Type](t: Expr[T], init: Expr[T], mod: Expr[Module])(using Quotes): Expr[T] =
    val nameOpt = findEnclosingValName
    '{
      ModuleOps.wireInit($t, $init, ${Expr(nameOpt)}, $mod)(using summonInline[WalkHW[T]])
    }

  def litImpl[T <: HWData: Type](t: Expr[T], payload: Expr[HostTypeOf[T]], mod: Expr[Module])(using Quotes): Expr[T] =
    '{
      ModuleOps.lit($t, $payload, $mod)(using summonInline[WalkHW[T]])
    }

  def moduleInstImpl[M <: Module: Type](gen: Expr[M], parent: Expr[Module])(using Quotes): Expr[M] =
    val nameOpt = findEnclosingValName
    '{ Module.instantiate($gen, $parent, ${Expr(nameOpt)}) }
