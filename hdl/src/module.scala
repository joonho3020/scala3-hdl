package hdl

import scala.collection.mutable
import scala.compiletime.summonInline
import scala.compiletime.summonFrom
import scala.compiletime.uninitialized

final case class ElaboratedDesign(name: Option[String], ports: Seq[String], body: Seq[String])

class ElabContext:
  private var design: ElaboratedDesign = ElaboratedDesign(None, Seq(), Seq())
  private val designs = mutable.Map.empty[Module, ElaboratedDesign]
  private val ports = mutable.ArrayBuffer.empty[String]
  private val body = mutable.ArrayBuffer.empty[String]
  private var current: Option[Module] = None
  private var currentName: Option[String] = None

  private def isLeaf(node: HWData): Boolean =
    node match
      case _: Bundle[?] => false
      case _: Vec[?] => false
      case _ => true

  private def fallbackName(node: HWData, path: String): String =
    if path.nonEmpty then path else node.getRef.getOrElse(node.getClass.getSimpleName)

  def begin(module: Module): Unit =
    ports.clear()
    body.clear()
    current = Some(module)
    currentName = module.instName.orElse(Some(module.getClass.getSimpleName))
    design = ElaboratedDesign(currentName, Seq(), Seq())

  def addPortStmt(path: String, node: HWData): Unit =
    val key = fallbackName(node, path)
    ports += s"$key: ${node.toString}"

  def addBodyStmt(path: String, node: HWData): Unit =
    val key = fallbackName(node, path)
    body += s"$key: ${node.toString}"

  def addInstStmt(name: Option[String], child: Module): Unit =
    val inst = name.getOrElse(child.getClass.getSimpleName)
    body += s"inst $inst: ${child.getClass.getSimpleName}"

  def finish(module: Module): ElaboratedDesign =
    val d = ElaboratedDesign(currentName, ports.toSeq, body.toSeq)
    design = d
    designs.update(module, d)
    current = None
    currentName = None
    d

  def designFor(module: Module): Option[ElaboratedDesign] = designs.get(module)
  def lastDesign: ElaboratedDesign = design

abstract class Module:
  private var _instName: Option[String] = None
  private var _bodyFn: Module ?=> ElabContext ?=> Unit = uninitialized

  protected final def body(f: Module ?=> ElabContext ?=> Unit): Unit =
    _bodyFn = f

  private[hdl] def instName: Option[String] = _instName
  private[hdl] def setInstName(name: Option[String]): Unit = _instName = name

  private[hdl] def runBody(using ctx: ElabContext): ElaboratedDesign =
    given Module = this
    ctx.begin(this)
    _bodyFn(using summon[Module])(using ctx)
    ctx.finish(this)

  inline def IO[T <: HWData](inline t: T)(using WalkHW[T], ElabContext): T =
    ${ ModuleMacros.ioImpl('t) }

  inline def Wire[T <: HWData](inline t: T)(using WalkHW[T], Module, ElabContext): T =
    ${ ModuleMacros.wireImpl('t) }

  inline def Lit[T <: HWData](inline t: T)(inline payload: HostTypeOf[T])(using WalkHW[T]): T =
    ${ ModuleMacros.litImpl('t)('payload) }

object Module:
  inline def apply[M <: hdl.Module](
    inline gen: M
  )(using inline parent: hdl.Module): M =
    ${ ModuleMacros.moduleInstImpl('gen, 'parent) }

  def instantiate[M <: hdl.Module](
    gen: => M,
    parent: hdl.Module,
    name: Option[String]
  )(using ctx: ElabContext): M =
    val sub = gen
    sub.setInstName(name)
    ctx.addInstStmt(name, sub)
    sub

private[hdl] object ModuleOps:
  def io[T <: HWData](
    t: T, name: Option[String]
  )(using WalkHW[T], ElabContext)
  : T =
    t.setNodeKind(NodeKind.IO)
    if (name.isDefined)
      summon[ElabContext].addPortStmt(name.get, t)
    else
      ???
    t

  def wire[T <: HWData](
    t: T, name: Option[String]
  )(using WalkHW[T], Module, ElabContext): T =
    t.setNodeKind(NodeKind.Wire)
    if (name.isDefined)
      summon[ElabContext].addBodyStmt(name.get, t)
    else
      ???
    t

  def lit[T <: HWData](t: T, payload: HostTypeOf[T])(using WalkHW[T]): T =
    t.setNodeKind(NodeKind.Lit)
    t.setLitVal(payload)
    t

object ModuleMacros:
  import scala.quoted.*

  private def findEnclosingValName(using Quotes): Option[String] =
    import quotes.reflect.*
    def loop(sym: Symbol): Option[String] =
      if sym.isNoSymbol then None
      else if sym.isValDef && !sym.flags.is(Flags.Synthetic) && !sym.flags.is(Flags.Artifact) then Some(sym.name)
      else loop(sym.owner)
    loop(Symbol.spliceOwner)

  def ioImpl[T <: HWData: Type](t: Expr[T])(using Quotes): Expr[T] =
    val nameOpt = findEnclosingValName
    '{
      ModuleOps.io($t, ${Expr(nameOpt)})(
        using summonInline[WalkHW[T]],
        summonInline[ElabContext])
    }

  def wireImpl[T <: HWData: Type](t: Expr[T])(using Quotes): Expr[T] =
    val nameOpt = findEnclosingValName
    '{
      ModuleOps.wire($t, ${Expr(nameOpt)})(
        using summonInline[WalkHW[T]],
        summonInline[Module],
        summonInline[ElabContext])
    }

  def litImpl[T <: HWData: Type](
    t: Expr[T]
  )(
    payload: Expr[HostTypeOf[T]]
  )(using Quotes): Expr[T] =
    '{
      ModuleOps.lit($t, $payload)(using summonInline[WalkHW[T]])
    }

  def moduleInstImpl[M <: Module: Type](gen: Expr[M], parent: Expr[Module])(using Quotes): Expr[M] =
    val nameOpt = findEnclosingValName
    '{ Module.instantiate($gen, $parent, ${Expr(nameOpt)})(using summonInline[ElabContext]) }


//   private val classHashCache = TrieMap.empty[Class[?], Array[Byte]]
// 
//   private def codeIdentity(cls: Class[?]): Array[Byte] =
//     classHashCache.getOrElseUpdate(cls, sha256(classBytes(cls)))
// 
//   private def classBytes(cls: Class[?]): Array[Byte] =
//     val path = cls.getName.replace('.', '/') + ".class"
//     val stream =
//       Option(cls.getClassLoader).flatMap(cl => Option(cl.getResourceAsStream(path)))
//         .orElse(Option(ClassLoader.getSystemResourceAsStream(path)))
//     stream match
//       case Some(is) =>
//         try is.readAllBytes()
//         finally is.close()
//       case None =>
//         cls.getName.getBytes(StandardCharsets.UTF_8)
// 
//   private def sha256(bytes: Array[Byte]): Array[Byte] =
//     val md = MessageDigest.getInstance("SHA-256")
//     md.update(bytes)
//     md.digest()
