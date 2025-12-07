package hdl

import scala.collection.mutable
import scala.collection.concurrent.TrieMap
import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext
import scala.concurrent.duration.Duration
import scala.util.hashing.MurmurHash3
import scala.compiletime.summonInline

final case class ElaboratedDesign(name: String, ports: Seq[String], body: Seq[String])

private sealed trait BodyEntry
private final case class InstEntry(instName: String, elabKey: String, baseModule: String) extends BodyEntry
private final case class LineEntry(text: String) extends BodyEntry

final class ModuleBuilder(val moduleBaseName: String):
  private val ports = mutable.ArrayBuffer.empty[String]
  private val body = mutable.ArrayBuffer.empty[BodyEntry]
  private val usedNames = mutable.Set.empty[String]
  private var tempCounter = 0

  def freshName(prefix: String): String =
    tempCounter += 1
    val candidate = s"${prefix}_$tempCounter"
    if usedNames.contains(candidate) then freshName(prefix)
    else
      usedNames += candidate
      candidate

  def allocateName(name: Option[String], prefix: String): String =
    name.filter(n => n.nonEmpty && !usedNames.contains(n)).map { n =>
      usedNames += n
      n
    }.getOrElse(freshName(prefix))

  def addPort(stmt: String): Unit = ports += stmt
  def addBody(stmt: String): Unit = body += LineEntry(stmt)
  def addInst(name: String, elabKey: String, base: String): Unit =
    body += InstEntry(name, elabKey, base)

  def snapshot(label: String, instLabels: Map[String, String]): ElaboratedDesign =
    val renderedBody = body.map {
      case LineEntry(t)     => t
      case InstEntry(n, k, b) => s"inst $n of ${instLabels.getOrElse(k, b)}"
    }
    ElaboratedDesign(label, ports.toSeq, renderedBody.toSeq)

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
  protected def elaborationParamHash: String = Module.paramHash(this)
  def elaborationKey: String = _elabKey.getOrElse(Module.autoElaborationKey(this))
  private[hdl] def setElabKey(k: String): Unit = _elabKey = Some(k)
  private[hdl] def setInstanceName(n: String): Unit = _instanceName = Some(n)
  private[hdl] def instanceName: Option[String] = _instanceName
  private[hdl] def addChild(m: Module): Unit = _children += m
  private[hdl] def children: Seq[Module] = _children.toSeq
  private[hdl] def getBuilder: ModuleBuilder = _builder
  private[hdl] def register[T](data: T, ref: Option[String])(using WalkHW[T]): Unit =
    ModuleOps.assignOwner(data, this)
    ref.foreach(r => ModuleOps.assignRefs(data, r))

  protected inline def IO[T <: HWData](inline t: T)(using WalkHW[T]): T =
    ${ ModuleMacros.ioImpl('t, 'this) }

  protected inline def Wire[T <: HWData](inline t: T)(using WalkHW[T]): T =
    ${ ModuleMacros.wireImpl('t, 'this) }

  protected inline def Reg[T <: HWData](inline t: T)(using WalkHW[T]): T =
    ${ ModuleMacros.regImpl('t, 'this) }

  protected inline def Lit[T <: HWData](inline t: T)(inline payload: HostTypeOf[T])(using WalkHW[T]): T =
    ${ ModuleMacros.litImpl('t, 'payload, 'this) }

object Module:
  inline def apply[M <: hdl.Module](inline gen: M)(using inline parent: hdl.Module): M =
    ${ ModuleMacros.moduleInstImpl('gen, 'parent) }

  def instantiate[M <: hdl.Module](gen: => M, parent: hdl.Module, name: Option[String]): M =
    val sub = gen
    val instName = parent.getBuilder.allocateName(name, "inst")
    sub.setInstanceName(instName)
    sub.setElabKey(autoElaborationKey(sub))
    parent.addChild(sub)
    parent.getBuilder.addInst(instName, sub.elaborationKey, sub.moduleName)
    sub

  private def autoElaborationKey(mod: Module): String =
    s"${mod.moduleName}#${mod.elaborationParamHash}"

  private def paramHash(mod: Module): String =
    val cls = mod.getClass
    val fields = cls.getDeclaredFields
      .filter(f => !java.lang.reflect.Modifier.isStatic(f.getModifiers))
      .filter(f => !f.isSynthetic)
    val sb = new StringBuilder
    fields.sortBy(_.getName).foreach { f =>
      f.setAccessible(true)
      val v = try f.get(mod) catch case _: Throwable => "<?>"
      sb.append(f.getName).append("=").append(Option(v).map(_.toString).getOrElse("null")).append(";")
    }
    MurmurHash3.stringHash(sb.toString).toString

private[hdl] object ModuleOps:
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
    mod.getBuilder.addBody(s"wire $wireName : ${formatType(t)}")
    t

  def reg[T <: HWData](t: T, name: Option[String], mod: Module)(using WalkHW[T]): T =
    val regName = mod.getBuilder.allocateName(name, "reg")
    t.setNodeKind(NodeKind.Reg)
    mod.register(t, Some(regName))
    mod.getBuilder.addBody(s"reg $regName : ${formatType(t)}, clock")
    t

  def lit[T <: HWData](t: T, payload: HostTypeOf[T], mod: Module)(using WalkHW[T]): T =
    t.setNodeKind(NodeKind.Lit)
    t.setLitVal(payload)
    mod.register(t, None)
    t

  def connect[T <: HWData](dst: T, src: T, mod: Module): Unit =
    val lhs = refFor(dst, mod)
    val rhs = refFor(src, mod)
    mod.getBuilder.addBody(s"connect $lhs, $rhs")

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

  def refFor(data: HWData, current: Module): String =
    if data.kind == NodeKind.Lit then formatLiteral(data, data.literal.getOrElse(""))
    else
      val base = data.getRef.getOrElse(data.toString)
      data.getOwner match
        case Some(owner) if owner.ne(current) =>
          owner.instanceName.map(prefix => s"$prefix.$base").getOrElse(base)
        case _ => base

  def emitPortDecl[T <: HWData](name: String, tpe: T)(using w: WalkHW[T]): Seq[String] =
    val buf = mutable.ArrayBuffer.empty[String]
    w(tpe, name) { (h, path) =>
      h match
        case u: UInt =>
          val dirStr = if u.dir == Direction.In then "input" else "output"
          buf += s"$dirStr $path : UInt<${u.w.value}>"
        case b: Bool =>
          val dirStr = if b.dir == Direction.In then "input" else "output"
          buf += s"$dirStr $path : Bool"
        case _ =>
    }
    buf.toSeq

  def formatType(tpe: HWData): String = tpe match
    case u: UInt => s"UInt<${u.w.value}>"
    case _: Bool => "Bool"
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
  private val designs = TrieMap.empty[String, Future[ElaboratedDesign]]
  private val labels = TrieMap.empty[String, String]
  private val nameCounters = TrieMap.empty[String, Int]

  def elaborate(top: Module): Seq[ElaboratedDesign] =
    val _ = Await.result(elaborateAsync(top), Duration.Inf)
    designs.values.map(f => Await.result(f, Duration.Inf)).toSeq

  private def nextModuleLabel(base: String): String =
    val n = nameCounters.getOrElse(base, 0) + 1
    nameCounters.update(base, n)
    if n == 1 then base else s"${base}_$n"

  private def elaborateAsync(mod: Module): Future[ElaboratedDesign] =
    val key = mod.elaborationKey
    val label = labels.getOrElseUpdate(key, nextModuleLabel(mod.moduleName))
    designs.getOrElseUpdate(key, Future:
      mod.children.foreach(child => Await.result(elaborateAsync(child), Duration.Inf))
      val instLabelMap = labels.toMap
      mod.getBuilder.snapshot(label, instLabelMap)
    )

  def emit(design: ElaboratedDesign): String =
    val sb = new StringBuilder
    sb.append(s"module ${design.name}:\n")
    design.ports.foreach(p => sb.append(s"  $p\n"))
    if design.body.nonEmpty then sb.append("\n")
    design.body.foreach(s => sb.append(s"  $s\n"))
    sb.toString

  def emitAll(designs: Seq[ElaboratedDesign]): String =
    designs.map(emit).mkString("\n")
