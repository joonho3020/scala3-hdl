package hdl

import scala.collection.mutable
import scala.collection.concurrent.TrieMap
import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext
import scala.concurrent.duration.Duration
import scala.compiletime.summonInline

final case class ElaboratedDesign(name: String, ports: Seq[String], body: Seq[String])

private sealed trait BodyEntry:
  def indent: Int
private final case class InstEntry(instName: String, mod: Module, baseModule: String, indent: Int) extends BodyEntry
private final case class LineEntry(text: String, indent: Int) extends BodyEntry

final class ModuleBuilder(val moduleBaseName: String):
  private val ports = mutable.ArrayBuffer.empty[String]
  private val body = mutable.ArrayBuffer.empty[BodyEntry]
  private val usedNames = mutable.Set.empty[String]
  private var tempCounter = 0
  private var indentLevel = 0

  private def currentIndentLevel: Int = indentLevel
  private def indentString(n: Int): String = "  " * n

  def withIndent[T](thunk: => T): T =
    indentLevel += 1
    try thunk
    finally indentLevel -= 1

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

  def addPort(stmt: String): Unit = ports += stmt
  def addBody(stmt: String): Unit = body += LineEntry(stmt, currentIndentLevel)
  def addInst(name: String, mod: Module, base: String): Unit =
    body += InstEntry(name, mod, base, currentIndentLevel)

  def snapshot(label: String, instLabels: Map[Module, String]): ElaboratedDesign =
    val renderedBody = body.map {
      case LineEntry(t, ind) => s"${indentString(ind)}$t"
      case InstEntry(n, m, b, ind) => s"${indentString(ind)}inst $n of ${instLabels.getOrElse(m, b)}"
    }
    ElaboratedDesign(label, ports.toSeq, renderedBody.toSeq)

abstract class Module:
  private val _builder = new ModuleBuilder(moduleName)
  private val _children = mutable.ArrayBuffer.empty[Module]
  private var _instanceName: Option[String] = None
  private var _bodyFn: Option[Module ?=> Unit] = None
  private var _bodyRan = false

  def moduleName: String = getClass.getSimpleName.stripSuffix("$")
  private[hdl] def setInstanceName(n: String): Unit = _instanceName = Some(n)
  private[hdl] def instanceName: Option[String] = _instanceName
  private[hdl] def addChild(m: Module): Unit = _children += m
  private[hdl] def children: Seq[Module] = _children.toSeq
  private[hdl] def getBuilder: ModuleBuilder = _builder
  private[hdl] def register[T](data: T, ref: Option[String])(using WalkHW[T]): Unit =
    ModuleOps.assignOwner(data, this)
    ref.foreach(r => ModuleOps.assignRefs(data, r))

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

  protected inline def Lit[T <: HWData](inline t: T)(inline payload: HostTypeOf[T])(using WalkHW[T]): T =
    ${ ModuleMacros.litImpl('t, 'payload, 'this) }

  protected inline def when(cond: Bool)(block: => Unit)(using m: Module): WhenDSL =
    ModuleOps.when(cond, summon[Module]) {
      block
    }

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
  def io[T <: HWData](t: T, name: Option[String], mod: Module)(using WalkHW[T]): T =
    println(s"ModuleOps.io ${t} ${name} ${mod}")
    val baseName = mod.getBuilder.allocateName(name, "io")
    t.setNodeKind(NodeKind.IO)
    mod.register(t, Some(baseName))
    emitPortDecl(baseName, t)
      .foreach(mod.getBuilder.addPort)
    t

  def wire[T <: HWData](t: T, name: Option[String], mod: Module)(using WalkHW[T]): T =
    println(s"ModuleOps.wire ${t} ${name} ${mod}")
    val wireName = mod.getBuilder.allocateName(name, "wire")
    t.setNodeKind(NodeKind.Wire)
    mod.register(t, Some(wireName))
    mod.getBuilder.addBody(s"wire $wireName : ${formatType(t)}")
    t

  def reg[T <: HWData](t: T, name: Option[String], mod: Module)(using WalkHW[T]): T =
    println(s"ModuleOps.reg ${t} ${name} ${mod}")
    val regName = mod.getBuilder.allocateName(name, "reg")
    t.setNodeKind(NodeKind.Reg)
    mod.register(t, Some(regName))
    mod.getBuilder.addBody(s"reg $regName : ${formatType(t)}, clock")
    t

  def lit[T <: HWData](t: T, payload: HostTypeOf[T], mod: Module)(using WalkHW[T]): T =
    println(s"ModuleOps.lit ${t} ${payload} ${mod}")
    t.setNodeKind(NodeKind.Lit)
    t.setLitVal(payload)
    mod.register(t, None)
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
        val lhs = refFor(dst, mod)
        val rhs = refFor(src, mod)
        mod.getBuilder.addBody(s"connect $lhs, $rhs")

  def when(cond: Bool, mod: Module)(block: => Unit): WhenDSL =
    val condRef = refFor(cond, mod)
    mod.getBuilder.addBody(s"when $condRef:")
    mod.getBuilder.withIndent {
      block
    }
    new WhenDSL(mod)

  def add(lhs: UInt, rhs: UInt, mod: Module): UInt =
    val resWidth = math.max(lhs.w.value, rhs.w.value)
    val result = UInt(Width(resWidth))
    result.setNodeKind(NodeKind.PrimOp)
    val name = mod.getBuilder.allocateName(None, "add")
    mod.register(result, Some(name))
    val lhsRef = refFor(lhs, mod)
    val rhsRef = refFor(rhs, mod)
    mod.getBuilder.addBody(s"node $name = add($lhsRef, $rhsRef)")
    result

  private def cmp[T <: HWData](op: String, lhs: T, rhs: T, mod: Module): Bool =
    val result = Bool()
    result.setNodeKind(NodeKind.PrimOp)
    val name = mod.getBuilder.allocateName(None, op)
    mod.register(result, Some(name))
    val lhsRef = refFor(lhs, mod)
    val rhsRef = refFor(rhs, mod)
    mod.getBuilder.addBody(s"node $name = $op($lhsRef, $rhsRef)")
    result

  def eq[T <: HWData](lhs: T, rhs: T, mod: Module): Bool =
    cmp("eq", lhs, rhs, mod)

  def neq[T <: HWData](lhs: T, rhs: T, mod: Module): Bool =
    cmp("neq", lhs, rhs, mod)

  def refFor(data: HWData, current: Module): String =
    if data.kind == NodeKind.Lit then formatLiteral(data, data.literal.getOrElse(""))
    else
      val base = data.getRef.getOrElse(data.toString)
      data.getOwner match
        case Some(owner) if owner.ne(current) =>
          owner.instanceName.map(prefix => s"$prefix.$base").getOrElse(base)
        case _ => base

  def emitPortDecl[T <: HWData](name: String, tpe: T)(using w: WalkHW[T]): Seq[String] =
    val dirStr = if tpe.dir == Direction.In then "input" else "output"
    Seq(s"$dirStr $name : ${formatType(tpe)}")

  def formatType(tpe: HWData): String = tpe match
    case u: UInt => s"UInt<${u.w.value}>"
    case _: Bool => "Bool"
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
    case u: UInt => s"UInt<${u.w.value}>($value)"
    case _: Bool => s"Bool($value)"
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
    ModuleOps.add(lhs, rhs, m)
  def ===(rhs: UInt)(using m: Module): Bool =
    ModuleOps.eq(lhs, rhs, m)
  def =/=(rhs: UInt)(using m: Module): Bool =
    ModuleOps.neq(lhs, rhs, m)

extension (lhs: Bool)
  def ===(rhs: Bool)(using m: Module): Bool =
    ModuleOps.eq(lhs, rhs, m)
  def =/=(rhs: Bool)(using m: Module): Bool =
    ModuleOps.neq(lhs, rhs, m)

final class WhenDSL(private val mod: Module):
  def elsewhen(cond: Bool)(block: => Unit): WhenDSL =
    mod.getBuilder.addBody(s"else when ${ModuleOps.refFor(cond, mod)}:")
    mod.getBuilder.withIndent {
      block
    }
    this

  def otherwise(block: => Unit): Unit =
    mod.getBuilder.addBody("otherwise:")
    mod.getBuilder.withIndent {
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

  def litImpl[T <: HWData: Type](t: Expr[T], payload: Expr[HostTypeOf[T]], mod: Expr[Module])(using Quotes): Expr[T] =
    '{
      ModuleOps.lit($t, $payload, $mod)(using summonInline[WalkHW[T]])
    }

  def moduleInstImpl[M <: Module: Type](gen: Expr[M], parent: Expr[Module])(using Quotes): Expr[M] =
    val nameOpt = findEnclosingValName
    '{ Module.instantiate($gen, $parent, ${Expr(nameOpt)}) }

final class Elaborator:
  private implicit val ec: ExecutionContext = ExecutionContext.global
  private val nameCounters = TrieMap.empty[String, Int]
  private val labels = TrieMap.empty[Module, String]

  def elaborate(top: Module): Seq[ElaboratedDesign] =
    Await.result(elaborateModule(top), Duration.Inf)

  private def nextModuleLabel(base: String): String =
    nameCounters.synchronized:
      val n = nameCounters.getOrElse(base, 0) + 1
      nameCounters.update(base, n)
      if n == 1 then base else s"${base}_$n"

  private def assignLabel(mod: Module): String =
    labels.synchronized:
      labels.getOrElseUpdate(mod, nextModuleLabel(mod.moduleName))

  private def elaborateModule(mod: Module): Future[Seq[ElaboratedDesign]] =
    Future(mod.runBody()).flatMap { _ =>
      val label = assignLabel(mod)
      val childFutures = mod.children.map(elaborateModule)
      Future.sequence(childFutures).map(_.flatten).map { childDesigns =>
        val instLabelMap = labels.synchronized { labels.toMap }
        childDesigns :+ mod.getBuilder.snapshot(label, instLabelMap)
      }
    }

  def emit(design: ElaboratedDesign): String =
    val sb = new StringBuilder
    sb.append(s"module ${design.name}:\n")
    design.ports.foreach(p => sb.append(s"  $p\n"))
    if design.body.nonEmpty then sb.append("\n")
    design.body.foreach(s => sb.append(s"  $s\n"))
    sb.toString

  def emitAll(designs: Seq[ElaboratedDesign]): String =
    designs.map(emit).mkString("\n")
