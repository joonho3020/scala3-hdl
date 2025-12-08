package hdl

import java.security.MessageDigest
import java.util.concurrent.{ConcurrentHashMap, Executors, Future as JFuture}
import scala.collection.concurrent.TrieMap
import scala.collection.mutable
import scala.concurrent.{Await, ExecutionContext, Future, Promise}
import scala.concurrent.duration.Duration

final case class ModuleKey(moduleClass: String, paramHash: String, codeHash: String):
  override def toString: String = s"${moduleClass}_${paramHash.take(8)}"
  def fullKey: String = s"${moduleClass}_${paramHash}_${codeHash}"

object ModuleKey:
  def apply(moduleClass: String, paramHash: String): ModuleKey =
    ModuleKey(moduleClass, paramHash, "")

  def withCodeHash(clazz: Class[?], paramHash: String): ModuleKey =
    val codeHash = computeClassHash(clazz)
    ModuleKey(clazz.getSimpleName.stripSuffix("$"), paramHash, codeHash)

  private def computeClassHash(clazz: Class[?]): String =
    try
      val classLoader = clazz.getClassLoader
      val resourceName = clazz.getName.replace('.', '/') + ".class"
      val stream = classLoader.getResourceAsStream(resourceName)
      if stream == null then return ""
      try
        val bytes = stream.readAllBytes()
        val digest = MessageDigest.getInstance("SHA-256")
        val hash = digest.digest(bytes)
        hash.take(8).map(b => f"$b%02x").mkString
      finally
        stream.close()
    catch
      case _: Throwable => ""

sealed trait ElaborationResult:
  def name: String
  def key: ModuleKey

final case class ElaboratedModule(
  key: ModuleKey,
  name: String,
  ports: Seq[String],
  body: Seq[String],
  childKeys: Seq[(String, ModuleKey)]
) extends ElaborationResult

trait ModuleQuery[M <: Module]:
  def key: ModuleKey
  def construct(): M
  def isCacheable: Boolean = true

object ModuleQuery:
  def apply[P, M <: Module](
    clsName: String,
    params: P
  )(thunk: => M)(using sh: StableHash[P]): ModuleQuery[M] =
    val hash = StableHash.hex(StableHash.digest(params))
    new ModuleQuery[M]:
      def key: ModuleKey = ModuleKey(clsName, hash)
      def construct(): M = thunk

  def uncached[M <: Module](thunk: => M): ModuleQuery[M] =
    new ModuleQuery[M]:
      def key: ModuleKey = ModuleKey(thunk.getClass.getSimpleName, java.util.UUID.randomUUID().toString)
      def construct(): M = thunk
      override def isCacheable: Boolean = false

trait ModuleFactory[M <: Module]:
  def moduleClass: String
  def paramHash: String
  def key: ModuleKey = ModuleKey(moduleClass, paramHash)
  def construct(): M

object ModuleFactory:
  def create[M <: Module](clsName: String, hash: String)(thunk: => M): ModuleFactory[M] =
    new ModuleFactory[M]:
      def moduleClass: String = clsName
      def paramHash: String = hash
      def construct(): M = thunk

trait CacheableModuleCompanion[P, M <: Module]:
  def moduleName: String
  def moduleClass: Class[?]
  def hashParams(p: P)(using sh: StableHash[P]): String =
    StableHash.hex(StableHash.digest(p))
  def construct(p: P): M
  def query(p: P)(using StableHash[P]): ModuleQuery[M] =
    val hash = hashParams(p)
    val codeHash = ModuleKey.withCodeHash(moduleClass, hash)
    new ModuleQuery[M]:
      def key: ModuleKey = codeHash
      def construct(): M = CacheableModuleCompanion.this.construct(p)
  def factory(p: P)(using StableHash[P]): ModuleFactory[M] =
    val hash = hashParams(p)
    ModuleFactory.create(moduleName, hash)(construct(p))

class ElaborationDatabase:
  private val cache = new ConcurrentHashMap[ModuleKey, ElaboratedModule]()
  private val inProgress = new ConcurrentHashMap[ModuleKey, Promise[ElaboratedModule]]()
  private val executorService = Executors.newWorkStealingPool()
  given ExecutionContext = ExecutionContext.fromExecutor(executorService)

  private val allModules = new ConcurrentHashMap[ModuleKey, ElaboratedModule]()

  private var cacheHits = 0
  private var cacheMisses = 0

  def stats: (Int, Int) = (cacheHits, cacheMisses)

  def query[M <: Module](q: ModuleQuery[M]): ElaboratedModule =
    val key = q.key
    if q.isCacheable then
      val cached = cache.get(key)
      if cached != null then
        cacheHits += 1
        cached
      else
        cacheMisses += 1
        val promise = Promise[ElaboratedModule]()
        val existing = inProgress.putIfAbsent(key, promise)
        if existing != null then
          Await.result(existing.future, Duration.Inf)
        else
          try
            val module = q.construct()
            val result = elaborateModule(module, key)
            cache.put(key, result)
            allModules.put(key, result)
            promise.success(result)
            result
          catch
            case e: Throwable =>
              inProgress.remove(key)
              promise.failure(e)
              throw e
          finally
            inProgress.remove(key)
    else
      cacheMisses += 1
      val module = q.construct()
      val result = elaborateModule(module, key)
      allModules.put(key, result)
      result

  def query[M <: Module](factory: ModuleFactory[M]): ElaboratedModule =
    val q = new ModuleQuery[M]:
      def key: ModuleKey = factory.key
      def construct(): M = factory.construct()
    query(q)

  def queryUncached[M <: Module](module: M, keyOverride: Option[ModuleKey] = None): ElaboratedModule =
    cacheMisses += 1
    val key = keyOverride.getOrElse(ModuleKey(module.moduleName, java.util.UUID.randomUUID().toString))
    val result = elaborateModule(module, key)
    allModules.put(key, result)
    result

  private def elaborateModule(module: Module, key: ModuleKey): ElaboratedModule =
    module.setElabKey(key.toString)
    val builder = module.getBuilder
    val childKeys = mutable.ArrayBuffer.empty[(String, ModuleKey)]
    module.children.foreach { child =>
      child.instanceName.foreach { instName =>
        val childKey = ModuleKey(child.moduleName, child.elaborationKeyForCache)
        childKeys += ((instName, childKey))
        val childFactory = ModuleFactory.create(child.moduleName, child.elaborationKeyForCache)(child)
        val childResult = query(childFactory)
      }
    }
    ElaboratedModule(
      key = key,
      name = s"${builder.moduleBaseName}_${key.paramHash.take(8)}",
      ports = builder.getPorts,
      body = builder.getBodyLines,
      childKeys = childKeys.toSeq
    )

  def getAllModules: Seq[ElaboratedModule] = {
    import scala.jdk.CollectionConverters.*
    allModules.values().asScala.toSeq
  }

  def shutdown(): Unit = executorService.shutdown()

class Elaborator:
  private val db = new ElaborationDatabase

  def stats: (Int, Int) = db.stats

  def elaborate[M <: Module](module: M): Seq[ElaboratedModule] =
    println(s"Elaborate ${module}")
    val result = db.queryUncached(module)
    collectAll(result)

  def elaborate[M <: Module](factory: ModuleFactory[M]): Seq[ElaboratedModule] =
    println(s"Elaborate ${factory}")
    val result = db.query(factory)
    collectAll(result)

  def elaborate[M <: Module](query: ModuleQuery[M]): Seq[ElaboratedModule] =
    println(s"Elaborate ${query}")
    val result = db.query(query)
    collectAll(result)

  private def collectAll(root: ElaboratedModule): Seq[ElaboratedModule] =
    db.getAllModules

  def emitAll(designs: Seq[ElaboratedModule]): String =
    val sb = new StringBuilder
    sb.append("circuit ").append(designs.headOption.map(_.name).getOrElse("Top")).append(" :\n")
    designs.foreach { d =>
      sb.append("  module ").append(d.name).append(" :\n")
      d.ports.foreach(p => sb.append("    ").append(p).append("\n"))
      d.body.foreach(b => sb.append("    ").append(b).append("\n"))
      sb.append("\n")
    }
    sb.toString

  def shutdown(): Unit = db.shutdown()

