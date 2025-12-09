package hdl

import scala.collection.concurrent.TrieMap
import scala.concurrent.ExecutionContext
import scala.concurrent.{Await, Future}
import scala.concurrent.duration.Duration

final class Elaborator(buildCache: BuildCache = BuildCache.default):
  private implicit val ec: ExecutionContext = ExecutionContext.global

  private val labels = TrieMap.empty[Module, String]
  private val nameCounters = TrieMap.empty[String, Int]
  private val memoized = TrieMap.empty[String, Seq[ElaboratedDesign]]
  private val inProgress = TrieMap.empty[String, Future[Seq[ElaboratedDesign]]]

  private def nextModuleLabel(base: String): String =
    nameCounters.synchronized:
      val n = nameCounters.getOrElse(base, 0) + 1
      nameCounters.update(base, n)
      if n == 1 then base else s"${base}_$n"

  private def assignLabel(mod: Module, key: ModuleKey): String =
    labels.synchronized:
      if key.cacheable then labels.getOrElseUpdate(mod, key.label)
      else labels.getOrElseUpdate(mod, nextModuleLabel(mod.moduleName))

  def elaborate(top: Module): Seq[ElaboratedDesign] =
    Await.result(elaborateModule(top), Duration.Inf).distinctBy(_.name)

  private def elaborateModule(mod: Module): Future[Seq[ElaboratedDesign]] =
    val key = ModuleKey(mod)
    val label = assignLabel(mod, key)
    memoized.get(key.value) match
      case Some(designs) =>
        // Found identical instance, reuse it
        Future.successful(designs)
      case None =>
        if key.cacheable then
          buildCache.get(key.value) match
            case Some(hit) =>
              println(s"Cache Hit ${mod.getClass.getName} ${key} ${label}")
              val designs = hit.designs.distinctBy(_.name)
              memoized.putIfAbsent(key.value, designs)
              Future.successful(designs)
            case None =>
              println(s"Cache Miss ${mod.getClass.getName} ${key} ${label}")
              startElaboration(mod, key, label)
        else
          println(s"NonCacheable ${mod.getClass.getName} ${key} ${label}")
          startElaboration(mod, key, label)

  private def startElaboration(mod: Module, key: ModuleKey, label: String): Future[Seq[ElaboratedDesign]] =
    inProgress.getOrElseUpdate(key.value,
      // Submit `mod.runBody` to the execution pool
      Future(mod.runBody()).flatMap { _ =>
        val childFutures = mod.children.map(elaborateModule)

        // Submit all child `elaborateModule` to the execution pool
        Future.sequence(childFutures).map(_.flatten).map { childDesigns =>
          val instLabelMap = labels.synchronized { labels.toMap }
          val design = mod.getBuilder.snapshot(label, instLabelMap)
          val result = (childDesigns :+ design).distinctBy(_.name)
          memoized.putIfAbsent(key.value, result)
          if key.cacheable then buildCache.put(key.value, CachedArtifact(result.toVector))
          result
        }
      }
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
