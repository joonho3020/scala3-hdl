package hdl

import scala.collection.concurrent.TrieMap
import scala.concurrent.ExecutionContext
import scala.concurrent.{Await, Future}
import scala.concurrent.duration.Duration

final class Elaborator(buildCache: BuildCache = BuildCache.default, log: String => Unit = println):
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
        log(s"Cache Hit ${mod.getClass.getName} ${key} ${label}")
        Future.successful(designs)
      case None =>
        if key.cacheable then
          buildCache.get(key.value) match
            case Some(hit) =>
              log(s"Cache Hit ${mod.getClass.getName} ${key} ${label}")
              val designs = hit.designs.distinctBy(_.name)
              memoized.putIfAbsent(key.value, designs)
              Future.successful(designs)
            case None =>
              log(s"Cache Miss ${mod.getClass.getName} ${key} ${label}")
              startElaboration(mod, key, label)
        else
          log(s"NonCacheable ${mod.getClass.getName} ${key} ${label}")
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
    val m = design.ir
    val sb = new StringBuilder
    sb.append(s"module ${m.name}:\n")
    m.ports.foreach(p => sb.append(s"  ${emitPort(p)}\n"))
    if m.body.nonEmpty then sb.append("\n")
    m.body.foreach(stmt => emitStmt(stmt, indent = 1, sb))
    sb.toString

  def emitAll(designs: Seq[ElaboratedDesign]): String =
    designs.map(emit).mkString("\n")

  private def emitPort(p: IR.Port): String =
    val dirStr = if p.direction == Direction.In then "input" else "output"
    s"$dirStr ${p.name} : ${emitType(p.tpe)}"

  private def emitType(t: IR.Type): String = t match
    case IR.UIntType(w) => s"UInt<$w>"
    case IR.BoolType    => "Bool"
    case IR.VecType(len, elem) => s"Vec<$len, ${emitType(elem)}>"
    case IR.BundleType(fields) =>
      val inner = fields.map { f =>
        val dirPrefix = if f.flipped then "flip " else ""
        s"$dirPrefix${f.name} : ${emitType(f.tpe)}"
      }.mkString(", ")
      s"{ $inner }"

  private def emitExpr(e: IR.Expr): String = e match
    case IR.Ref(name)       => name
    case IR.Literal(value)  => value
    case IR.SubIndex(expr, value) => s"${emitExpr(expr)}[$value]"
    case IR.SubAccess(expr, index) => s"${emitExpr(expr)}[${emitExpr(index)}]"
    case IR.SubField(expr, field) => s"${emitExpr(expr)}.$field"
    case IR.DoPrim(op, args) => s"${op.opName}(${args.map(emitExpr).mkString(", ")})"

  private def emitStmt(stmt: IR.Stmt, indent: Int, sb: StringBuilder): Unit =
    val prefix = "  " * indent
    stmt match
      case IR.Wire(name, tpe) =>
        sb.append(s"${prefix}wire $name : ${emitType(tpe)}\n")
      case IR.Reg(name, tpe) =>
        sb.append(s"${prefix}reg $name : ${emitType(tpe)}, clock\n")
      case IR.DefNode(name, value) =>
        sb.append(s"${prefix}node $name = ${emitExpr(value)}\n")
      case IR.Connect(loc, expr) =>
        sb.append(s"${prefix}connect ${emitExpr(loc)}, ${emitExpr(expr)}\n")
      case IR.When(cond, conseq, alt) =>
        sb.append(s"${prefix}when ${emitExpr(cond)}:\n")
        conseq.foreach(s => emitStmt(s, indent + 1, sb))
        if alt.nonEmpty then
          sb.append(s"${prefix}otherwise:\n")
          alt.foreach(s => emitStmt(s, indent + 1, sb))
      case IR.Inst(name, module) =>
        sb.append(s"${prefix}inst $name of $module\n")
