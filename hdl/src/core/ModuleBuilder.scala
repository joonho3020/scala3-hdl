package hdl

import scala.collection.mutable

final case class ElaboratedDesign(name: IR.Identifier, ir: IR.Module) extends Serializable

private sealed trait RawStmt
private final case class RawInst(instName: String, mod: Module, baseModule: String) extends RawStmt
private final case class RawWhen(cond: IR.Expr, conseq: Seq[RawStmt], var alt: Seq[RawStmt]) extends RawStmt
private final case class RawLeaf(stmt: IR.Stmt) extends RawStmt

private[hdl] final class ModuleBuilder(val moduleBaseName: String):
  private val ports = mutable.ArrayBuffer.empty[IR.Port]
  private val usedNames = mutable.Set.empty[String]
  private val bodyStack = mutable.Stack[mutable.ArrayBuffer[RawStmt]]()
  bodyStack.push(mutable.ArrayBuffer.empty[RawStmt])
  private var tempCounter = 0
  private val annotations = mutable.ArrayBuffer.empty[String]

  def freshName(prefix: String): String =
    tempCounter += 1
    val candidate = s"${prefix}_$tempCounter"
    if usedNames.contains(candidate) then freshName(prefix)
    else
      usedNames += candidate
      candidate

  private def isSaneName(n: String): Boolean =
    n.nonEmpty && n.matches("^[A-Za-z_][A-Za-z0-9_]*$")

  def allocateName(name: Option[String], prefix: String): String =
    name.filter(n => isSaneName(n) && !usedNames.contains(n)).map { n =>
      usedNames += n
      n
    }.getOrElse(freshName(prefix))

  def addPort(port: IR.Port): Unit = ports += port
  private def currentBody: mutable.ArrayBuffer[RawStmt] = bodyStack.head

  def addRaw(stmt: RawStmt): Unit = currentBody += stmt
  def addStmt(stmt: IR.Stmt): Unit = addRaw(RawLeaf(stmt))
  def addInst(name: String, mod: Module, base: String): Unit =
    addRaw(RawInst(name, mod, base))
  def addDontTouch(target: String): Unit = annotations += target

  def captureBody(thunk: => Unit): Seq[RawStmt] =
    val buf = mutable.ArrayBuffer.empty[RawStmt]
    bodyStack.push(buf)
    try
      thunk
      val body = buf.toSeq
      if body.isEmpty then Seq(RawLeaf(IR.Skip)) else body
    finally
      bodyStack.pop()

  def snapshot(label: String, instLabels: Map[Module, String]): ElaboratedDesign =
    val body = currentBody.toSeq.map(rawToIR(_, instLabels))
    val id = IR.Identifier(label)
    val annos = annotations.toSeq.map { target =>
      IR.Annotation(IR.Identifier("firrtl.transforms.DontTouchAnnotation"), IR.AnnotationTarget(id, IR.Identifier(target)))
    }
    val mod = IR.Module(id, ports.toSeq, body, annos)
    ElaboratedDesign(id, mod)

  private def rawToIR(rs: RawStmt, instLabels: Map[Module, String]): IR.Stmt = rs match
    case RawLeaf(s) => s
    case RawInst(name, m, base) =>
      val target = instLabels.getOrElse(m, base)
      IR.Inst(IR.Identifier(name), IR.Identifier(target))
    case w: RawWhen =>
      val conseq = w.conseq.map(rawToIR(_, instLabels))
      val alt = w.alt.map(rawToIR(_, instLabels))
      IR.When(w.cond, conseq, alt)
