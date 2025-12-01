package hdl

import scala.collection.mutable

trait Module:
  def moduleName: String = getClass.getSimpleName.stripSuffix("$")

  def io: Bundle

  def body(using ctx: ElabContext): Unit

class ElabContext private[hdl] (
  private val moduleName: String,
  private val elaborator: Elaborator
):
  private val statements = mutable.ListBuffer[StmtIR]()
  private val ports = mutable.ListBuffer[PortIR]()
  private var nameCounter = 0

  // Implicit clock and reset
  val clock: ExprIR = ExprIR.Ref("clock")
  val reset: ExprIR = ExprIR.Ref("reset")

  private def freshName(prefix: String): String =
    nameCounter += 1
    s"${prefix}_$nameCounter"

  def emit(stmt: StmtIR): Unit =
    statements += stmt

  def addPort(port: PortIR): Unit =
    ports += port

  def wire(tpe: TypeIR, name: String = ""): ExprIR =
    val n = if name.nonEmpty then name else freshName("wire")
    emit(StmtIR.WireDecl(n, tpe))
    ExprIR.Ref(n)

  def reg(tpe: TypeIR, name: String = ""): ExprIR =
    val n = if name.nonEmpty then name else freshName("reg")
    emit(StmtIR.RegDecl(n, tpe))
    ExprIR.Ref(n)

  def connect(lhs: ExprIR, rhs: ExprIR): Unit =
    emit(StmtIR.Connect(lhs, rhs))

  def instantiate(child: Module, name: String = ""): InstanceHandle =
    val childIR = elaborator.elaborate(child)
    val n = if name.nonEmpty then name else freshName(child.moduleName)
    emit(StmtIR.Instance(n, child.moduleName))
    InstanceHandle(n, childIR)

  private[hdl] def build(): ModuleIR =
    ModuleIR(moduleName, ports.toSeq, statements.toSeq)

case class InstanceHandle(name: String, moduleIR: ModuleIR):
  def io(portName: String): ExprIR = 
    ExprIR.SubField(ExprIR.Ref(name), portName)

class Elaborator:
  def elaborate(module: Module): ModuleIR =
    val ctx = new ElabContext(module.moduleName, this)
    registerPorts(ctx, module.io)
    module.body(using ctx)
    ctx.build()

  private def registerPorts(ctx: ElabContext, io: Bundle): Unit =
    val tpeIR = valueTypeToIR(io.asInstanceOf[ValueType])
    tpeIR match
      case TypeIR.BundleIR(_, fields) =>
        fields.foreach { (name, fieldTpe) =>
          val dir = extractDirection(io, name)
          ctx.addPort(PortIR(name, fieldTpe, dir))
        }
      case _ =>
        ctx.addPort(PortIR("io", tpeIR, Direction.Out))

  private def extractDirection(io: Bundle, fieldName: String): Direction =
    val p = io.asInstanceOf[Product]
    val idx = p.productElementNames.indexOf(fieldName)
    if idx >= 0 then
      p.productElement(idx) match
        case u: UInt => u.dir
        case b: Bool => b.dir
        case _ => Direction.Out
    else Direction.Out

private def valueTypeToIR(v: ValueType): TypeIR = v match
  case u: UInt => TypeIR.UIntIR(u.w.value)
  case b: Bool => TypeIR.BoolIR()
  case vec: Vec[?] => TypeIR.VecIR(valueTypeToIR(vec.elem), vec.len)
  case b: Bundle =>
    val p = b.asInstanceOf[Product]
    val fields = (0 until p.productArity).map { i =>
      val name = p.productElementName(i)
      val value = p.productElement(i).asInstanceOf[ValueType]
      (name, valueTypeToIR(value))
    }
    TypeIR.BundleIR(b.getClass.getSimpleName, fields.toSeq)
