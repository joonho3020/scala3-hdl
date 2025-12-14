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
        log(s"Memoized Hit ${mod.getClass.getName} ${key} ${label}")
        Future.successful(designs)
      case None =>
        startElaboration(mod, key, label)

  private def startElaboration(mod: Module, key: ModuleKey, label: String): Future[Seq[ElaboratedDesign]] =
    inProgress.getOrElseUpdate(key.value,
      // Submit `mod.runBody` to the execution pool
      Future(mod.runBody()).flatMap { _ =>
        val childFutures = mod.children.map(elaborateModule)

        // Submit all child `elaborateModule` to the execution pool
        Future.sequence(childFutures).map(_.flatten).map { childDesigns =>
          val childKeys = mod.children.map(m => ModuleKey(m).value)

          val cachedDesign: Option[ElaboratedDesign] =
            if key.cacheable then
              buildCache.get(key.value) match
                case Some(hit) =>
                  log(s"Cache Hit ${mod.getClass.getName} ${key} ${label}")
                  Some(hit.design)
                case None =>
                  log(s"Cache Miss ${mod.getClass.getName} ${key} ${label}")
                  None
            else
              log(s"NonCacheable ${mod.getClass.getName} ${key} ${label}")
              None

          val design = cachedDesign.getOrElse {
            val instLabelMap = labels.synchronized { labels.toMap }
            mod.getBuilder.snapshot(label, instLabelMap)
          }

          val result = (childDesigns :+ design).distinctBy(_.name)
          memoized.putIfAbsent(key.value, result)
          if key.cacheable && cachedDesign.isEmpty then buildCache.put(key.value, CachedArtifact(design))
          result
        }
      }
    )

  def emit(design: ElaboratedDesign): String =
    val m = design.ir
    val sb = new StringBuilder
    sb.append(s"module ${m.name.value}:\n")
    m.ports.foreach(p => sb.append(s"  ${emitPort(p)}\n"))
    if m.body.nonEmpty then sb.append("\n")
    m.body.foreach(stmt => emitStmt(stmt, indent = 1, sb))
    sb.toString

  def emitAll(designs: Seq[ElaboratedDesign]): String =
    designs.map(emit).mkString("\n")

  def emitChirrtl(designs: Seq[ElaboratedDesign], topName: String): String =
    val sb = new StringBuilder
    sb.append("FIRRTL version 3.3.0\n")
    sb.append(s"circuit $topName :\n")
    designs.foreach { design =>
      val isTop = design.ir.name.value == topName
      emitChirrtlModule(design.ir, isTop, sb)
    }
    sb.toString

  private def emitChirrtlModule(m: IR.Module, isTop: Boolean, sb: StringBuilder): Unit =
    sb.append(s"  module ${m.name.value} :\n")
    m.ports.foreach(p => sb.append(s"    ${emitChirrtlPort(p, isTop)}\n"))
    sb.append("\n")
    m.body.foreach(stmt => emitChirrtlStmt(stmt, indent = 2, sb))
    sb.append("\n")

  private def emitPort(p: IR.Port): String =
    val dirStr = if p.direction == Direction.In then "input" else "output"
    s"$dirStr ${p.name.value} : ${emitType(p.tpe)}"

  private def emitType(t: IR.Type): String = t match
    case IR.UIntType(w) => w.map(v => s"UInt<$v>").getOrElse("UInt")
    case IR.BoolType    => "Bool"
    case IR.ClockType   => "Clock"
    case IR.ResetType   => "Reset"
    case IR.VecType(len, elem) => s"Vec<$len, ${emitType(elem)}>"
    case IR.BundleType(fields) =>
      val inner = fields.map { f =>
        val dirPrefix = if f.flipped then "flip " else ""
        s"$dirPrefix${f.name} : ${emitType(f.tpe)}"
      }.mkString(", ")
      s"{ $inner }"

  private def emitExpr(e: IR.Expr): String = e match
    case IR.Ref(name)       => name.value
    case IR.Literal(value)  => value.value
    case IR.DontCare        => "DontCare"
    case IR.SubIndex(expr, value) => s"${emitExpr(expr)}[$value]"
    case IR.SubAccess(expr, index) => s"${emitExpr(expr)}[${emitExpr(index)}]"
    case IR.SubField(expr, field) => s"${emitExpr(expr)}.${field.value}"
    case IR.DoPrim(op, args, consts) =>
      val parts = args.map(emitExpr) ++ consts.map(_.toString)
      s"${op.opName}(${parts.mkString(", ")})"

  private def emitStmt(stmt: IR.Stmt, indent: Int, sb: StringBuilder): Unit =
    val prefix = "  " * indent
    stmt match
      case IR.Wire(name, tpe) =>
        sb.append(s"${prefix}wire ${name.value} : ${emitType(tpe)}\n")
      case IR.WireInit(name, tpe, clock, reset, init) =>
        sb.append(s"${prefix}wire ${name.value} : ${emitType(tpe)} with reset : ${emitExpr(reset)} init : ${emitExpr(init)}\n")
      case IR.Reg(name, tpe, clock) =>
        sb.append(s"${prefix}reg ${name.value} : ${emitType(tpe)}, ${emitExpr(clock)}\n")
      case IR.RegReset(name, tpe, clock, reset, init) =>
        sb.append(s"${prefix}reg ${name.value} : ${emitType(tpe)}, ${emitExpr(clock)} with reset : ${emitExpr(reset)} init : ${emitExpr(init)}\n")
      case IR.DefNode(name, value) =>
        sb.append(s"${prefix}node ${name.value} = ${emitExpr(value)}\n")
      case IR.Connect(loc, expr) =>
        sb.append(s"${prefix}connect ${emitExpr(loc)}, ${emitExpr(expr)}\n")
      case IR.Invalid(expr) =>
        sb.append(s"${prefix}invalidate ${emitExpr(expr)}\n")
      case IR.When(cond, conseq, alt) =>
        sb.append(s"${prefix}when ${emitExpr(cond)}:\n")
        conseq.foreach(s => emitStmt(s, indent + 1, sb))
        if alt.nonEmpty then
          sb.append(s"${prefix}otherwise:\n")
          alt.foreach(s => emitStmt(s, indent + 1, sb))
      case IR.Inst(name, module) =>
        sb.append(s"${prefix}inst ${name.value} of ${module.value}\n")
      case IR.Mem(name, tpe, depth, rlat, wlat, readers, writers, readwriters, ruw) =>
        sb.append(s"${prefix}mem ${name.value}:\n")
        sb.append(s"${prefix}  data-type => ${emitType(tpe)}\n")
        sb.append(s"${prefix}  depth => $depth\n")
        sb.append(s"${prefix}  read-latency => $rlat\n")
        sb.append(s"${prefix}  write-latency => $wlat\n")
        readers.foreach(r => sb.append(s"${prefix}  reader => ${r.value}\n"))
        writers.foreach(w => sb.append(s"${prefix}  writer => ${w.value}\n"))
        readwriters.foreach(rw => sb.append(s"${prefix}  readwriter => ${rw.value}\n"))
        val ruwStr = ruw match
          case IR.ReadUnderWrite.Undefined => "undefined"
          case IR.ReadUnderWrite.Old => "old"
          case IR.ReadUnderWrite.New => "new"
        sb.append(s"${prefix}  read-under-write => $ruwStr\n")

  private def emitChirrtlPort(p: IR.Port, isTop: Boolean): String =
    val dirStr = if p.direction == Direction.In then "input" else "output"
    s"$dirStr ${p.name.value} : ${emitChirrtlType(p.tpe, isTop)}"

  private def emitChirrtlType(t: IR.Type, isTop: Boolean): String = t match
    case IR.UIntType(w) => w.map(v => s"UInt<$v>").getOrElse("UInt")
    case IR.BoolType    => "UInt<1>"
    case IR.ClockType   => "Clock"
    case IR.ResetType   => if isTop then "UInt<1>" else "Reset"
    case IR.VecType(len, elem) => s"${emitChirrtlType(elem, isTop)}[$len]"
    case IR.BundleType(fields) =>
      val inner = fields.map { f =>
        val dirPrefix = if f.flipped then "flip " else ""
        s"$dirPrefix${f.name.value} : ${emitChirrtlType(f.tpe, isTop)}"
      }.mkString(", ")
      s"{ $inner }"

  private def emitChirrtlExpr(e: IR.Expr): String = e match
    case IR.Ref(name)       => name.value
    case IR.Literal(value)  => value.value
    case IR.DontCare        => "invalidate"
    case IR.SubIndex(expr, value) => s"${emitChirrtlExpr(expr)}[$value]"
    case IR.SubAccess(expr, index) => s"${emitChirrtlExpr(expr)}[${emitChirrtlExpr(index)}]"
    case IR.SubField(expr, field) => s"${emitChirrtlExpr(expr)}.${field.value}"
    case IR.DoPrim(op, args, consts) =>
      val parts = args.map(emitChirrtlExpr) ++ consts.map(_.toString)
      s"${op.opName}(${parts.mkString(", ")})"

  private def emitChirrtlStmt(stmt: IR.Stmt, indent: Int, sb: StringBuilder): Unit =
    val prefix = "    " * indent
    stmt match
      case IR.Wire(name, tpe) =>
        sb.append(s"${prefix}wire ${name.value} : ${emitChirrtlType(tpe, false)}\n")
      case IR.WireInit(name, tpe, clock, reset, init) =>
        sb.append(s"${prefix}wire ${name.value} : ${emitChirrtlType(tpe, false)}\n")
        sb.append(s"${prefix}connect ${name.value}, ${emitChirrtlExpr(init)}\n")
      case IR.Reg(name, tpe, clock) =>
        sb.append(s"${prefix}reg ${name.value} : ${emitChirrtlType(tpe, false)}, ${emitChirrtlExpr(clock)}\n")
      case IR.RegReset(name, tpe, clock, reset, init) =>
        sb.append(s"${prefix}regreset ${name.value} : ${emitChirrtlType(tpe, false)}, ${emitChirrtlExpr(clock)}, ${emitChirrtlExpr(reset)}, ${emitChirrtlExpr(init)}\n")
      case IR.DefNode(name, value) =>
        sb.append(s"${prefix}node ${name.value} = ${emitChirrtlExpr(value)}\n")
      case IR.Connect(loc, expr) =>
        sb.append(s"${prefix}connect ${emitChirrtlExpr(loc)}, ${emitChirrtlExpr(expr)}\n")
      case IR.Invalid(expr) =>
        sb.append(s"${prefix}invalidate ${emitChirrtlExpr(expr)}\n")
      case IR.When(cond, conseq, alt) =>
        sb.append(s"${prefix}when ${emitChirrtlExpr(cond)} :\n")
        conseq.foreach(s => emitChirrtlStmt(s, indent + 1, sb))
        if alt.nonEmpty then
          sb.append(s"${prefix}else :\n")
          alt.foreach(s => emitChirrtlStmt(s, indent + 1, sb))
      case IR.Inst(name, module) =>
        sb.append(s"${prefix}inst ${name.value} of ${module.value}\n")
        sb.append(s"${prefix}connect ${name.value}.clock, clock\n")
        sb.append(s"${prefix}connect ${name.value}.reset, reset\n")
      case IR.Mem(name, tpe, depth, rlat, wlat, readers, writers, readwriters, ruw) =>
        sb.append(s"${prefix}mem ${name.value} :\n")
        sb.append(s"${prefix}  data-type => ${emitChirrtlType(tpe, false)}\n")
        sb.append(s"${prefix}  depth => $depth\n")
        sb.append(s"${prefix}  read-latency => $rlat\n")
        sb.append(s"${prefix}  write-latency => $wlat\n")
        readers.foreach(r => sb.append(s"${prefix}  reader => ${r.value}\n"))
        writers.foreach(w => sb.append(s"${prefix}  writer => ${w.value}\n"))
        readwriters.foreach(rw => sb.append(s"${prefix}  readwriter => ${rw.value}\n"))
        val ruwStr = ruw match
          case IR.ReadUnderWrite.Undefined => "undefined"
          case IR.ReadUnderWrite.Old => "old"
          case IR.ReadUnderWrite.New => "new"
        sb.append(s"${prefix}  read-under-write => $ruwStr\n")
