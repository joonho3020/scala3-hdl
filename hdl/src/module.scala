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

abstract class Module:
  private val _children = mutable.ArrayBuffer.empty[Module]
  private var _elabKey: Option[String] = None
  private var _instanceName: Option[String] = None

  private[hdl] def register[T](data: T, ref: Option[String])(using WalkHW[T]): Unit =
    ModuleOps.assignOwner(data, this)
    ref.foreach(r => ModuleOps.assignRefs(data, r))

  protected inline def IO[T <: HWData](inline t: T)(using WalkHW[T]): T =
    ${ ModuleMacros.ioImpl('t, 'this) }

  protected inline def Wire[T <: HWData](inline t: T)(using WalkHW[T]): T =
    ${ ModuleMacros.wireImpl('t, 'this) }

  protected inline def Lit[T <: HWData](inline t: T)(inline payload: HostTypeOf[T])(using WalkHW[T]): T =
    ${ ModuleMacros.litImpl('t, 'payload, 'this) }

object Module:
  inline def apply[M <: hdl.Module](inline gen: M)(using inline parent: hdl.Module): M =
    ${ ModuleMacros.moduleInstImpl('gen, 'parent) }

  def instantiate[M <: hdl.Module](gen: => M, parent: hdl.Module, name: Option[String]): M =
    println(s"instantiate ${name}")
    val sub = gen
    sub

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

private[hdl] object ModuleOps:
  def io[T <: HWData](t: T, name: Option[String], mod: Module)(using WalkHW[T]): T =
    println(s"io ${t} ${name} ${mod}")
    t.setNodeKind(NodeKind.IO)
    t

  def wire[T <: HWData](t: T, name: Option[String], mod: Module)(using WalkHW[T]): T =
    println(s"wire ${t} ${name} ${mod}")
    t.setNodeKind(NodeKind.Wire)
    t

  def lit[T <: HWData](t: T, payload: HostTypeOf[T], mod: Module)(using WalkHW[T]): T =
    println(s"lit ${t} ${payload} ${mod}")
    t.setNodeKind(NodeKind.Lit)
    t.setLitVal(payload)
    mod.register(t, None)
    t

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

  def litImpl[T <: HWData: Type](t: Expr[T], payload: Expr[HostTypeOf[T]], mod: Expr[Module])(using Quotes): Expr[T] =
    '{
      ModuleOps.lit($t, $payload, $mod)(using summonInline[WalkHW[T]])
    }

  def moduleInstImpl[M <: Module: Type](gen: Expr[M], parent: Expr[Module])(using Quotes): Expr[M] =
    val nameOpt = findEnclosingValName
    '{ Module.instantiate($gen, $parent, ${Expr(nameOpt)}) }
