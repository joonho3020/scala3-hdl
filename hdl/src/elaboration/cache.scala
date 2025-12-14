package hdl

import java.io.{ObjectInputStream, ObjectOutputStream}
import java.nio.file.{Files, Path, Paths}
import java.security.MessageDigest
import scala.collection.concurrent.TrieMap
import scala.util.control.NonFatal

final case class CachedArtifact(design: ElaboratedDesign) extends Serializable

final case class ModuleKey(value: String, cacheable: Boolean, label: String)

trait CacheableModule:
  this: Module =>
  type ElabParams
  given stableHashElabParams: StableHash[ElabParams]
  def elabParams: ElabParams

object ModuleKey:
  private def updateInt(md: MessageDigest, i: Int): Unit =
    md.update(((i >>> 24) & 0xff).toByte)
    md.update(((i >>> 16) & 0xff).toByte)
    md.update(((i >>> 8) & 0xff).toByte)
    md.update((i & 0xff).toByte)

  private def merge(parts: Seq[Array[Byte]]): Array[Byte] =
    val md = MessageDigest.getInstance("SHA-256")
    parts.foreach { p =>
      updateInt(md, p.length)
      md.update(p)
    }
    md.digest()

  private def bytecodeHash(cls: Class[?]): Option[Array[Byte]] =
    val resourcePath = "/" + cls.getName.replace('.', '/') + ".class"
    val stream =
      Option(cls.getResourceAsStream(resourcePath))
        .orElse(Option(cls.getResourceAsStream(cls.getSimpleName + ".class")))
    stream match
      case Some(s) =>
        val md = MessageDigest.getInstance("SHA-256")
        val buf = new Array[Byte](4096)
        try
          var read = s.read(buf)
          while read != -1 do
            md.update(buf, 0, read)
            read = s.read(buf)
          Some(md.digest())
        finally
          s.close()
      case None =>
        None

  def apply(mod: Module): ModuleKey =
    mod match
      case c: CacheableModule =>
        val codeHashOpt = bytecodeHash(mod.getClass)
        val paramHash = StableHash.digest(c.elabParams)(using c.stableHashElabParams)
        codeHashOpt match
          case Some(codeHash) =>
            val merged = merge(Seq(codeHash, paramHash))
            val hex = StableHash.hex(merged)
            val label = s"${mod.moduleName}_${hex.take(8)}"
            ModuleKey(s"cached:${mod.getClass.getName}:$hex", true, label)
          case None =>
            val cls = mod.getClass
            throw new IllegalStateException(
              s"Unable to locate classfile for ${cls.getName} (loader=${Option(cls.getClassLoader).getOrElse("<bootstrap>")})"
            )
      case _ =>
        // `identityHashCode` provides a different hash per class instance.
        // The hash value is different regardless of the parameters passed
        // on to a Module
        val salt = s"${mod.getClass.getName}:${System.identityHashCode(mod)}"
        val hex = StableHash.hex(StableHash.digest(salt))
        ModuleKey(s"uncached:${mod.getClass.getName}:$hex", false, "")

final class BuildCache private (path: Path):
  private val data: TrieMap[String, CachedArtifact] = load()
  private val lock = new Object

  private def load(): TrieMap[String, CachedArtifact] =
    if Files.exists(path) then
      try
        val ois = new ObjectInputStream(Files.newInputStream(path))
        try
          ois.readObject() match
            case m: Map[?, ?] =>
              TrieMap.from(m.asInstanceOf[Map[String, CachedArtifact]])
            case _ =>
              TrieMap.empty[String, CachedArtifact]
        finally ois.close()
      catch
        case NonFatal(_) => TrieMap.empty[String, CachedArtifact]
    else TrieMap.empty[String, CachedArtifact]

  def get(key: String): Option[CachedArtifact] = data.get(key)

  def put(key: String, artifact: CachedArtifact): Unit =
    // Need to hold a lock to prevent race conditions.
    // Otherwise can lead corrupted buildcache files.
    lock.synchronized:
      data.update(key, CachedArtifact(artifact.design))
      persist()

  private def persist(): Unit =
    val parent = path.getParent
    if parent != null && !Files.exists(parent) then Files.createDirectories(parent)
    val oos = new ObjectOutputStream(Files.newOutputStream(path))
    try oos.writeObject(data.readOnlySnapshot().toMap)
    finally oos.close()

object BuildCache:
  private val defaultPath = Paths.get(".hdl-cache", "elab-cache.bin")
  def at(path: Path): BuildCache = new BuildCache(path)
  lazy val default: BuildCache = new BuildCache(defaultPath)
