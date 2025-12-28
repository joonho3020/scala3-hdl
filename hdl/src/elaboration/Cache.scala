package hdl

import java.io.{ByteArrayOutputStream, ObjectInputStream, ObjectOutputStream}
import java.nio.file.{Files, Path, Paths}
import java.security.MessageDigest
import org.objectweb.asm.{ClassReader, ClassVisitor, MethodVisitor, Opcodes, Type}
import scala.collection.concurrent.TrieMap
import scala.collection.mutable
import scala.util.control.NonFatal

final case class CachedArtifact(design: ElaboratedDesign) extends Serializable

final case class ModuleKey(value: String, cacheable: Boolean, label: String)

trait CacheableModule:
  this: Module =>
  type ElabParams
  given stableHashElabParams: StableHash[ElabParams]
  def elabParams: ElabParams

// =============================================================================
// Bytecode-Based Dependency Tracking for Cache Invalidation
// =============================================================================
//
// This module computes a stable hash of a Module's code and all its transitive dependencies.
// When any dependent class changes, the hash changes, causing a cache miss and re-elaboration.
//
// Background: JVM Classfile Structure
// ------------------------------------
// A compiled .class file contains:
//   1. Magic number (0xCAFEBABE) and version info
//   2. Constant Pool - a table of constants including:
//      - Class references (CONSTANT_Class, tag 7)
//      - Field/Method references with type descriptors
//      - String literals, numbers, etc.
//   3. Class metadata (access flags, superclass, interfaces)
//   4. Fields and Methods with their bytecode
//
// Why ASM Instead of Manual Constant Pool Parsing?
// -------------------------------------------------
// The constant pool alone is insufficient because of JVM type erasure:
//   - Generic types like Vec[BPUUpdate] become just Vec in the constant pool
//   - The actual type parameter (BPUUpdate) only appears in method bodies
//     where the type is instantiated or cast
//
// ASM's ClassVisitor/MethodVisitor pattern lets us scan:
//   - Class-level references (superclass, interfaces, field types)
//   - Method body instructions (NEW, INVOKEVIRTUAL, CHECKCAST, etc.)
//
// This captures dependencies like:
// ```
// case class FrontendIf(
//   mem: MagicMemIf,
//   redirect: RedirectIf,
//   uops: Vec[Decoupled[UOp]],
//   bpu_update: Valid[BPUUpdate]
// ) extends Bundle[FrontendIf]
// ```
//   - FrontendIf.apply() calls BPUUpdate.apply() -> FrontendIf depends on BPUUpdate
//
// =============================================================================

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

  // Exclude standard library classes - they don't change between builds
  // and including them would bloat the dependency graph unnecessarily.
  // Uses JVM internal name format: "java/lang/String" not "java.lang.String"
  private val excludedPrefixes = Set(
    "java/", "javax/", "jdk/", "sun/",
    "scala/", "dotty/",
    "kotlin/"
  )

  private def shouldIncludeClass(internalName: String): Boolean =
    !excludedPrefixes.exists(internalName.startsWith)

  private def readClassBytes(cls: Class[?]): Option[Array[Byte]] =
    val resourcePath = "/" + cls.getName.replace('.', '/') + ".class"
    val stream =
      Option(cls.getResourceAsStream(resourcePath))
        .orElse(Option(cls.getResourceAsStream(cls.getSimpleName + ".class")))
    stream.map { s =>
      try
        val baos = new ByteArrayOutputStream()
        val buf = new Array[Byte](4096)
        var read = s.read(buf)
        while read != -1 do
          baos.write(buf, 0, read)
          read = s.read(buf)
        baos.toByteArray
      finally
        s.close()
    }

  // Extract class references from ASM Type descriptors.
  // Type descriptors use a compact format:
  //   - "Lcom/example/MyClass;" for object types
  //   - "[Lcom/example/MyClass;" for array of objects
  //   - "(Lcom/example/Arg;)Lcom/example/Return;" for methods
  private def addTypeRef(refs: mutable.Set[String], t: Type): Unit =
    t.getSort match
      case Type.OBJECT => refs += t.getInternalName
      case Type.ARRAY => addTypeRef(refs, t.getElementType)
      case Type.METHOD =>
        addTypeRef(refs, t.getReturnType)
        t.getArgumentTypes.foreach(addTypeRef(refs, _))
      case _ => () // primitives (int, boolean, etc.) have no class refs

  // Parse all class references from a classfile using ASM.
  // This uses the Visitor pattern: ASM calls our methods as it parses.
  private def parseClassReferences(classBytes: Array[Byte]): Set[String] =
    val refs = mutable.Set[String]()
    try
      val reader = new ClassReader(classBytes)
      reader.accept(new ClassVisitor(Opcodes.ASM9) {

        // Called once at the start with class-level info
        // Captures: superclass (e.g., "hdl/Module") and interfaces
        override def visit(version: Int, access: Int, name: String,
                          signature: String, superName: String,
                          interfaces: Array[String]): Unit =
          if superName != null then refs += superName
          if interfaces != null then interfaces.foreach(refs += _)

        // Called for each field declaration
        // Captures: field types (e.g., "val io: MyBundle" -> MyBundle)
        override def visitField(access: Int, name: String, descriptor: String,
                               signature: String, value: Any) =
          // println(s"visitField ${access} ${name} ${descriptor} ${signature} ${value}")
          addTypeRef(refs, Type.getType(descriptor))
          null

        // Called for each method declaration
        // Returns a MethodVisitor to scan the method body for more references
        override def visitMethod(access: Int, name: String, descriptor: String,
                                signature: String, exceptions: Array[String]) =
          // println(s"visitMethod ${access} ${name} ${descriptor} ${signature}")
          // Capture parameter and return types from method signature
          addTypeRef(refs, Type.getMethodType(descriptor))
          if exceptions != null then exceptions.foreach(refs += _)

          // Return a MethodVisitor to scan bytecode instructions
          // This is critical for capturing types used inside method bodies
          new MethodVisitor(Opcodes.ASM9) {

            // NEW, ANEWARRAY, CHECKCAST, INSTANCEOF instructions
            // e.g., "new BusyTable()" generates NEW riscv_ooo/BusyTable
            override def visitTypeInsn(opcode: Int, tpe: String): Unit =
              // println(s"visitTypeInsn ${opcode} ${tpe}")
              refs += tpe

            // GETFIELD, PUTFIELD, GETSTATIC, PUTSTATIC instructions
            // Captures both the owner class and field type
            override def visitFieldInsn(opcode: Int, owner: String, name: String, descriptor: String): Unit =
              // println(s"visitFieldInsn ${opcode} ${owner} ${name} ${descriptor}")
              refs += owner
              addTypeRef(refs, Type.getType(descriptor))

            // INVOKEVIRTUAL, INVOKESPECIAL, INVOKESTATIC, INVOKEINTERFACE
            // e.g., "BPUUpdate.apply(p)" generates INVOKEVIRTUAL riscv_ooo/BPUUpdate$.apply
            override def visitMethodInsn(opcode: Int, owner: String, name: String, descriptor: String, isInterface: Boolean): Unit =
              // println(s"visitMethodInsn ${opcode} ${owner} ${name} ${descriptor} ${isInterface}")
              refs += owner
              addTypeRef(refs, Type.getMethodType(descriptor))

            // LDC instruction - loads constants including Class objects
            // e.g., "classOf[MyClass]" loads MyClass.class
            override def visitLdcInsn(value: Any): Unit =
              // println(s"visitLdcInsn ${value}")
              value match
                case t: Type => addTypeRef(refs, t)
                case _ => ()

            // MULTIANEWARRAY instruction - multi-dimensional array creation
            override def visitMultiANewArrayInsn(descriptor: String, numDimensions: Int): Unit =
              // println(s"visitMultiANewArrayInsn ${descriptor} ${numDimensions}")
              addTypeRef(refs, Type.getType(descriptor))
          }

        // Called for inner/nested class references
        // Scala companion objects appear as inner classes (e.g., MyClass$)
        override def visitInnerClass(name: String, outerName: String,
                                     innerName: String, access: Int): Unit =
          // println(s"visitInnerClass ${name} ${outerName} ${innerName} ${access}")
          refs += name
      }, 0)
    catch case _: Exception => ()
    refs.toSet

  // Recursively collect all transitive dependencies starting from a class.
  // Uses BFS/DFS to walk the dependency graph, loading and parsing each class.
  private def collectTransitiveDependencies(
    cls: Class[?],
    visited: mutable.Set[String],
    classBytes: mutable.Map[String, Array[Byte]]
  ): Unit =
    val className = cls.getName.replace('.', '/')
    if visited.contains(className) then return
    visited += className

    readClassBytes(cls) match
      case Some(bytes) =>
        classBytes(className) = bytes
        // println(s"============= ${className} ======================")
        val refs = parseClassReferences(bytes)
        for ref <- refs if shouldIncludeClass(ref) && !visited.contains(ref) do
          val refClassName = ref.replace('/', '.')
          try
            val refCls = cls.getClassLoader.loadClass(refClassName)
            collectTransitiveDependencies(refCls, visited, classBytes)
          catch
            case _: ClassNotFoundException | _: NoClassDefFoundError => ()
      case None => ()

  // Compute a SHA-256 hash over all transitive dependencies.
  // The hash includes both class names and their bytecode, ensuring
  // any change to any dependency produces a different hash.
  private def transitiveCodeHash(cls: Class[?]): Option[Array[Byte]] =
    val visited = mutable.Set[String]()
    val classBytes = mutable.Map[String, Array[Byte]]()
    collectTransitiveDependencies(cls, visited, classBytes)

    // println(s"= ${cls.getName()}")
    // println(s"  - classBytes.keySet: ${classBytes.keySet}")
    // println(s"  - visisted: ${visited}")

    if classBytes.isEmpty then return None

    // Sort by class name for deterministic ordering
    val md = MessageDigest.getInstance("SHA-256")
    for (name, bytes) <- classBytes.toSeq.sortBy(_._1) do
      md.update(name.getBytes("UTF-8"))
      updateInt(md, bytes.length)
      md.update(bytes)
    Some(md.digest())

  def apply(mod: Module): ModuleKey =
    mod match
      case c: CacheableModule =>
        val codeHashOpt = transitiveCodeHash(mod.getClass)
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
