package hdl

import java.nio.charset.StandardCharsets
import java.security.MessageDigest
import java.util.UUID
import scala.collection.mutable
import scala.collection.concurrent.TrieMap
import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext
import scala.concurrent.duration.Duration
import scala.compiletime.summonInline

final case class ElaboratedDesign(name: String, ports: Seq[String], body: Seq[String])

private sealed trait BodyEntry:
  def indent: Int
private final case class InstEntry(instName: String, elabKey: String, baseModule: String, indent: Int) extends BodyEntry
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
  def addInst(name: String, elabKey: String, base: String): Unit =
    body += InstEntry(name, elabKey, base, currentIndentLevel)

  def hashSignature: String =
    val sb = new StringBuilder
    ports.foreach(p => sb.append("p:").append(p).append(";"))
    body.foreach {
      case LineEntry(t, ind) => sb.append("l:").append(ind).append(":").append(t).append(";")
      case InstEntry(n, k, b, ind) => sb.append("i:").append(ind).append(":").append(n).append("@").append(k).append(":").append(b).append(";")
    }
    sb.toString

  def getPorts: Seq[String] = ports.toSeq

  def getBodyLines: Seq[String] =
    val indentStr = (n: Int) => "  " * n
    body.map {
      case LineEntry(t, ind) => s"${indentStr(ind)}$t"
      case InstEntry(n, k, b, ind) => s"${indentStr(ind)}inst $n of $k"
    }.toSeq

  def getInstEntries: Seq[(String, String, String)] =
    body.collect { case InstEntry(n, k, b, _) => (n, k, b) }.toSeq

trait WalkHW[T]:
  def apply(x: T, path: String)(f: (HWData, String) => Unit): Unit

object WalkHW:
  private def walkAny(x: Any, path: String, f: (HWData, String) => Unit): Unit =
    x match
      case h: HWData =>
        f(h, path)
        x match
          case p: Product    => walkProduct(p, path, f)
          case it: IterableOnce[?] => walkIterable(it, path, f)
          case _ => ()
      case it: IterableOnce[?] =>
        walkIterable(it, path, f)
      case p: Product =>
        walkProduct(p, path, f)
      case _ => ()

  private def walkIterable(it: IterableOnce[?], path: String, f: (HWData, String) => Unit): Unit =
    val iter = it.iterator
    var idx = 0
    while iter.hasNext do
      val n = if path.isEmpty then s"[$idx]" else s"$path[$idx]"
      walkAny(iter.next(), n, f)
      idx += 1

  private def walkProduct(p: Product, path: String, f: (HWData, String) => Unit): Unit =
    val arity = p.productArity
    var i = 0
    while i < arity do
      val name =
        try p.productElementName(i)
        catch case _: Throwable => i.toString
      val childPath = if path.isEmpty then name else s"$path.$name"
      walkAny(p.productElement(i), childPath, f)
      i += 1

  given universal[T]: WalkHW[T] with
    def apply(x: T, path: String)(f: (HWData, String) => Unit): Unit =
      walkAny(x, path, f)

abstract class Module:
  private val _builder = new ModuleBuilder(moduleName)
  private val _children = mutable.ArrayBuffer.empty[Module]
  private var _elabKey: Option[String] = None
  private var _instanceName: Option[String] = None

  def moduleName: String = getClass.getSimpleName.stripSuffix("$")
  private[hdl] def setElabKey(k: String): Unit = _elabKey = Some(k)
  private[hdl] def setInstanceName(n: String): Unit = _instanceName = Some(n)
  private[hdl] def instanceName: Option[String] = _instanceName
  private[hdl] def addChild(m: Module): Unit = _children += m
  private[hdl] def children: Seq[Module] = _children.toSeq
  private[hdl] def getBuilder: ModuleBuilder = _builder
  private[hdl] def register[T](data: T, ref: Option[String])(using WalkHW[T]): Unit =
    ModuleOps.assignOwner(data, this)
    ref.foreach(r => ModuleOps.assignRefs(data, r))

  def elaborationKey: String = _elabKey.getOrElse(s"${moduleName}_${elaborationKeyForCache.take(8)}")

  private[hdl] def elaborationKeyForCache: String = this match
    case cm: CacheableModule[?] =>
      cm.cacheKey.paramHash
    case _ =>
      java.util.UUID.randomUUID().toString

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

trait CacheableModule[P]:
  this: Module =>
  given stableHashElabParams: StableHash[P]
  def elabParams: P
  def cacheKey: ModuleKey =
    val paramHash = StableHash.hex(StableHash.digest(elabParams))
    ModuleKey.withCodeHash(this.getClass, paramHash)

object Module:
  inline def apply[M <: hdl.Module](inline gen: M)(using inline parent: hdl.Module): M =
    ${ ModuleMacros.moduleInstImpl('gen, 'parent) }

  def instantiate[M <: hdl.Module](gen: => M, parent: hdl.Module, name: Option[String]): M =
    println(s"instantiate ${name}")
    val sub = gen
    val instName = parent.getBuilder.allocateName(name, "inst")
    sub.setInstanceName(instName)
    parent.addChild(sub)
    parent.getBuilder.addInst(instName, sub.elaborationKey, sub.moduleName)
    sub

private[hdl] object ModuleOps:
  def io[T <: HWData](t: T, name: Option[String], mod: Module)(using WalkHW[T]): T =
    println(s"ModuleOps io ${name} ${mod}")
    val baseName = mod.getBuilder.allocateName(name, "io")
    t.setNodeKind(NodeKind.IO)
    mod.register(t, Some(baseName))
    emitPortDecl(baseName, t)
      .foreach(mod.getBuilder.addPort)
    t

  def wire[T <: HWData](t: T, name: Option[String], mod: Module)(using WalkHW[T]): T =
    println(s"ModuleOps wire ${name} ${mod}")
    val wireName = mod.getBuilder.allocateName(name, "wire")
    t.setNodeKind(NodeKind.Wire)
    mod.register(t, Some(wireName))
    mod.getBuilder.addBody(s"wire $wireName : ${formatType(t)}")
    t

  def reg[T <: HWData](t: T, name: Option[String], mod: Module)(using WalkHW[T]): T =
    println(s"ModuleOps reg ${name} ${mod}")
    val regName = mod.getBuilder.allocateName(name, "reg")
    t.setNodeKind(NodeKind.Reg)
    mod.register(t, Some(regName))
    mod.getBuilder.addBody(s"reg $regName : ${formatType(t)}, clock")
    t

  def lit[T <: HWData](t: T, payload: HostTypeOf[T], mod: Module)(using WalkHW[T]): T =
    println(s"ModuleOps lit ${payload} ${mod}")
    t.setNodeKind(NodeKind.Lit)
    t.setLitVal(payload)
    mod.register(t, None)
    t

  def connect[T <: HWData](dst: T, src: T, mod: Module): Unit =
    println(s"ModuleOps connect ${dst} := ${src}")
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
    println(s"ModuleOps when ${cond}")
    val condRef = refFor(cond, mod)
    mod.getBuilder.addBody(s"when $condRef:")
    mod.getBuilder.withIndent {
      block
    }
    new WhenDSL(mod)

  def add(lhs: UInt, rhs: UInt, mod: Module): UInt =
    println(s"ModuleOps add ${lhs} ${rhs}")
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
    println(s"ModuleOps cmp ${lhs} ${rhs}")
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
