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
import scala.compiletime.uninitialized

final case class ElaboratedDesign(name: Option[String], ports: Seq[String], body: Seq[String])

class ElabContext:
  private var design: ElaboratedDesign = ElaboratedDesign(
    name = None,
    ports = Seq(),
    body = Seq())

abstract class Module:
  private var _bodyFn: Module ?=> ElabContext ?=> Unit = uninitialized

  protected final def body(f: Module ?=> ElabContext ?=> Unit): Unit =
    _bodyFn = f

  private[hdl] def runBody(using ctx: ElabContext): Unit =
    given Module = this
    _bodyFn

object Module:
  inline def apply[M <: hdl.Module](inline gen: M)(using inline parent: hdl.Module): M =
    ${ ModuleMacros.moduleInstImpl('gen, 'parent) }

  def instantiate[M <: hdl.Module](gen: => M, parent: hdl.Module, name: Option[String]): M =
    println(s"instantiate ${name}")
    val sub = gen
    sub

inline def IO[T <: HWData](inline t: T)(using WalkHW[T]): T =
  ${ ModuleMacros.ioImpl('t) }

inline def Wire[T <: HWData](inline t: T)(using WalkHW[T]): T =
  ${ ModuleMacros.wireImpl('t) }

inline def Lit[T <: HWData](inline t: T)(inline payload: HostTypeOf[T])(using WalkHW[T]): T =
  ${ ModuleMacros.litImpl('t)('payload) }

private[hdl] object ModuleOps:
  def io[T <: HWData](t: T, name: Option[String])(using WalkHW[T]): T =
    println(s"io ${t} ${name}")
    t.setNodeKind(NodeKind.IO)
    t

  def wire[T <: HWData](t: T, name: Option[String])(using WalkHW[T]): T =
    println(s"wire ${t} ${name}")
    t.setNodeKind(NodeKind.Wire)
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
      ModuleOps.io($t, ${Expr(nameOpt)})(using summonInline[WalkHW[T]])
    }

  def wireImpl[T <: HWData: Type](t: Expr[T])(using Quotes): Expr[T] =
    val nameOpt = findEnclosingValName
    '{
      ModuleOps.wire($t, ${Expr(nameOpt)})(using summonInline[WalkHW[T]])
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
    '{ Module.instantiate($gen, $parent, ${Expr(nameOpt)}) }




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

