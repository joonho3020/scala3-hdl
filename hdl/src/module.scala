package hdl

import scala.collection.mutable
import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext
import scala.concurrent.duration.Duration

final case class ElaboratedDesign(name: String, ports: Seq[String], body: Seq[String])

final class ModuleBuilder(val moduleName: String):
  private val ports = mutable.ArrayBuffer.empty[String]
  private val body = mutable.ArrayBuffer.empty[String]
  private val usedNames = mutable.Set.empty[String]
  private var tempCounter = 0

  def freshName(prefix: String): String =
    tempCounter += 1
    val candidate = s"${prefix}_$tempCounter"
    if usedNames.contains(candidate) then freshName(prefix) else
      usedNames += candidate
      candidate

  def allocateName(name: Option[String], prefix: String): String =
    name.filter(n => n.nonEmpty && !usedNames.contains(n)).map { n =>
      usedNames += n
      n
    }.getOrElse(freshName(prefix))

  def addPort(stmt: String): Unit = ports += stmt
  def addBody(stmt: String): Unit = body += stmt
  def snapshot: ElaboratedDesign = ElaboratedDesign(moduleName, ports.toSeq, body.toSeq)

object ModuleBuilder:
  def instantiate[M <: hdl.Module](gen: => M, parent: hdl.Module, name: Option[String]): M =
    val sub = gen
    val instName = parent.getBuilder.allocateName(name, "inst")
    sub.setInstanceName(instName)
    sub.setElabKey(ModuleBuilder.autoElaborationKey(sub))
    parent.addChild(sub)
    parent.getBuilder.addBody(s"inst $instName of ${sub.moduleName}")
    sub

  def io[T <: HWData](t: T, name: Option[String], mod: Module): T =
    val baseName = mod.getBuilder.allocateName(name, "io")
    t.setNodeKind(NodeKind.IO)
    mod.register(t, Some(baseName))
    emitPortDecl(baseName, t).foreach(mod.getBuilder.addPort)
    t

  def wire[T <: HWData](t: T, name: Option[String], mod: Module): T =
    val wireName = mod.getBuilder.allocateName(name, "wire")
    t.setNodeKind(NodeKind.Wire)
    mod.register(t, Some(wireName))
    mod.getBuilder.addBody(s"wire $wireName : ${formatType(t)}")
    t

  def reg[T <: HWData](t: T, name: Option[String], mod: Module): T =
    val regName = mod.getBuilder.allocateName(name, "reg")
    t.setNodeKind(NodeKind.Reg)
    mod.register(t, Some(regName))
    mod.getBuilder.addBody(s"reg $regName : ${formatType(t)}, clock")
    t

  def lit[T <: HWData](t: T, payload: HostTypeOf[T], name: Option[String], mod: Module): T =
    val litName = mod.getBuilder.allocateName(name, "lit")
    t.setNodeKind(NodeKind.Lit)
    t.setLitVal(payload)
    mod.register(t, Some(litName))
    mod.getBuilder.addBody(s"node $litName = ${formatLiteral(t, payload)}")
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

  def assignOwner(value: Any, mod: Module): Unit =
    value match
      case h: HWData =>
        h.setOwner(mod)
        h match
          case b: Bundle[?] =>
            val p = b.asInstanceOf[Product]
            (0 until p.productArity).foreach(i => assignOwner(p.productElement(i), mod))
          case _ =>
      case Some(v) =>
        assignOwner(v, mod)
      case seq: Seq[?] =>
        seq.foreach(elem => assignOwner(elem, mod))
      case _ =>

  def assignRefs(value: Any, base: String): Unit =
    value match
      case h: HWData =>
        h.setRef(base)
        h match
          case b: Bundle[?] =>
            val p = b.asInstanceOf[Product]
            (0 until p.productArity).foreach { i =>
              val fieldName = p.productElementName(i)
              p.productElement(i) match
                case hd: HWData => assignRefs(hd, s"$base.$fieldName")
                case Some(hd: HWData) => assignRefs(hd, s"$base.$fieldName")
                case seq: Seq[?] =>
                  seq.zipWithIndex.foreach {
                    case (hd: HWData, idx) => assignRefs(hd, s"$base.$fieldName[$idx]")
                    case (Some(hd: HWData), idx) => assignRefs(hd, s"$base.$fieldName[$idx]")
                    case _ =>
                  }
                case _ =>
            }
          case _ =>
      case _ =>

  def refFor(data: HWData, current: Module): String =
    val base = data.getRef
      .orElse(data.literal.map(v => formatLiteral(data, v)))
      .getOrElse(data.toString)
    data.getOwner match
      case Some(owner) if owner.ne(current) =>
        owner.instanceName.map(prefix => s"$prefix.$base").getOrElse(base)
      case _ => base

  def emitPortDecl(name: String, tpe: HWData, prefix: String = ""): Seq[String] =
    val fullName = if prefix.isEmpty then name else s"$prefix.$name"
    tpe match
      case u: UInt =>
        val dirStr = if u.dir == Direction.In then "input" else "output"
        Seq(s"$dirStr $fullName : UInt<${u.w.value}>")
      case b: Bool =>
        val dirStr = if b.dir == Direction.In then "input" else "output"
        Seq(s"$dirStr $fullName : Bool")
      case bundle: Bundle[?] =>
        val p = bundle.asInstanceOf[Product]
        (0 until p.productArity).flatMap { i =>
          val fieldName = p.productElementName(i)
          p.productElement(i) match
            case hd: HWData => emitPortDecl(fieldName, hd, fullName)
            case Some(hd: HWData) => emitPortDecl(fieldName, hd, fullName)
            case seq: Seq[?] =>
              seq.zipWithIndex.flatMap {
                case (hd: HWData, idx) => emitPortDecl(s"$fieldName[$idx]", hd, fullName)
                case (Some(hd: HWData), idx) => emitPortDecl(s"$fieldName[$idx]", hd, fullName)
                case _ => Seq.empty
              }
            case _ => Seq.empty
        }
      case _ => Seq.empty

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
          case Some(hd: HWData) =>
            val dirPrefix = dirPrefixOf(hd)
            Some(s"$dirPrefix$fieldName : ${formatType(hd)}")
          case seq: Seq[?] =>
            seq.zipWithIndex.collect {
              case (hd: HWData, idx) =>
                val dirPrefix = dirPrefixOf(hd)
                s"$dirPrefix$fieldName[$idx] : ${formatType(hd)}"
              case (Some(hd: HWData), idx) =>
                val dirPrefix = dirPrefixOf(hd)
                s"$dirPrefix$fieldName[$idx] : ${formatType(hd)}"
            }
          case _ => Nil
      }
      s"{ ${fields.mkString(", ")} }"
    case _ => tpe.toString

  def formatLiteral(tpe: HWData, value: Any): String = tpe match
    case u: UInt => s"UInt<${u.w.value}>($value)"
    case _: Bool => s"Bool($value)"
    case _ => value.toString

  def dirPrefixOf(h: HWData): String =
    if (h.dir == Direction.In) "flip " else ""

  def autoElaborationKey(mod: Module): String =
    s"${mod.moduleName}@${System.identityHashCode(mod)}"

abstract class Module:
  private val _builder = new ModuleBuilder(moduleName)
  private val _children = mutable.ArrayBuffer.empty[Module]
  private var _elabKey: Option[String] = None
  private var _instanceName: Option[String] = None

  def moduleName: String = getClass.getSimpleName.stripSuffix("$")
  def elaborationKey: String = _elabKey.getOrElse(ModuleBuilder.autoElaborationKey(this))
  private[hdl] def setElabKey(k: String): Unit = _elabKey = Some(k)
  private[hdl] def setInstanceName(n: String): Unit = _instanceName = Some(n)
  private[hdl] def instanceName: Option[String] = _instanceName
  private[hdl] def addChild(m: Module): Unit = _children += m
  private[hdl] def children: Seq[Module] = _children.toSeq
  private[hdl] def getBuilder: ModuleBuilder = _builder
  private[hdl] def register(data: HWData, ref: Option[String]): Unit =
    ModuleBuilder.assignOwner(data, this)
    ref.foreach(r => ModuleBuilder.assignRefs(data, r))

  protected inline def IO[T <: HWData](inline t: T): T =
    ${ ModuleMacros.ioImpl('t, 'this) }

  protected inline def Wire[T <: HWData](inline t: T): T =
    ${ ModuleMacros.wireImpl('t, 'this) }

  protected inline def Reg[T <: HWData](inline t: T): T =
    ${ ModuleMacros.regImpl('t, 'this) }

  protected inline def Lit[T <: HWData](inline t: T)(inline payload: HostTypeOf[T]): T =
    ${ ModuleMacros.litImpl('t, 'payload, 'this) }


object Module:
  inline def apply[M <: hdl.Module](inline gen: M)(using inline parent: hdl.Module): M =
    ${ ModuleMacros.moduleInstImpl('gen, 'parent) }

extension [T <: HWData](dst: T)
  def :=(src: T)(using m: Module): Unit =
    ModuleBuilder.connect(dst, src, m)

extension (lhs: UInt)
  def +(rhs: UInt)(using m: Module): UInt =
    ModuleBuilder.add(lhs, rhs, m)

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
    '{ ModuleBuilder.io($t, ${Expr(nameOpt)}, $mod) }

  def wireImpl[T <: HWData: Type](t: Expr[T], mod: Expr[Module])(using Quotes): Expr[T] =
    val nameOpt = findEnclosingValName
    '{ ModuleBuilder.wire($t, ${Expr(nameOpt)}, $mod) }

  def regImpl[T <: HWData: Type](t: Expr[T], mod: Expr[Module])(using Quotes): Expr[T] =
    val nameOpt = findEnclosingValName
    '{ ModuleBuilder.reg($t, ${Expr(nameOpt)}, $mod) }

  def litImpl[T <: HWData: Type](t: Expr[T], payload: Expr[HostTypeOf[T]], mod: Expr[Module])(using Quotes): Expr[T] =
    val nameOpt = findEnclosingValName
    '{ ModuleBuilder.lit($t, $payload, ${Expr(nameOpt)}, $mod) }

  def moduleInstImpl[M <: Module: Type](gen: Expr[M], parent: Expr[Module])(using Quotes): Expr[M] =
    val nameOpt = findEnclosingValName
    '{ ModuleBuilder.instantiate($gen, $parent, ${Expr(nameOpt)}) }

final class Elaborator:
  private implicit val ec: ExecutionContext = ExecutionContext.global
  private val elaborated = collection.concurrent.TrieMap.empty[String, Future[ElaboratedDesign]]

  def elaborate(top: Module): ElaboratedDesign =
    Await.result(elaborateAsync(top), Duration.Inf)

  private def elaborateAsync(mod: Module): Future[ElaboratedDesign] =
    val key = mod.elaborationKey
    elaborated.getOrElseUpdate(key, Future:
      mod.children.foreach(child => Await.result(elaborateAsync(child), Duration.Inf))
      mod.getBuilder.snapshot
    )

  def emit(design: ElaboratedDesign): String =
    val sb = new StringBuilder
    sb.append(s"module ${design.name}:\n")
    design.ports.foreach(p => sb.append(s"  $p\n"))
    if design.body.nonEmpty then sb.append("\n")
    design.body.foreach(s => sb.append(s"  $s\n"))
    sb.toString

  def emitAll: String =
    elaborated.values.map(f => emit(Await.result(f, Duration.Inf))).mkString("\n")
