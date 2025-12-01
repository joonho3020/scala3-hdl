package hdl

import scala.collection.mutable
import scala.language.dynamics

trait Module:
  def moduleName: String = getClass.getSimpleName.stripSuffix("$")

  def body(using ctx: ElabContext): Unit

class ElabContext private[hdl] (
  private val modName: String,
  private val elaborator: Elaborator
):
  private val statements = mutable.ListBuffer[StmtIR]()
  private val ports = mutable.ListBuffer[PortIR]()
  private var nameCounter = 0

  // Implicit clock and reset
  val clock: ExprIR = ExprIR.Ref("clock")
  val reset: ExprIR = ExprIR.Ref("reset")

  private[hdl] def freshName(prefix: String): String =
    nameCounter += 1
    s"${prefix}_$nameCounter"

  def emit(stmt: StmtIR): Unit =
    statements += stmt

  def addPort(port: PortIR): Unit =
    ports += port

  def wire[T <: ValueType](tpe: T, name: String = ""): Wire[T] =
    val n = if name.nonEmpty then name else freshName("wire")
    emit(StmtIR.WireDecl(n, valueTypeToIR(tpe)))
    new Wire(tpe, n)

  def reg[T <: ValueType](tpe: T, name: String = ""): Reg[T] =
    val n = if name.nonEmpty then name else freshName("reg")
    emit(StmtIR.RegDecl(n, valueTypeToIR(tpe)))
    new Reg(tpe, n)

  def instantiate[M <: Module](child: M, name: String = ""): Instance[M] =
    val childIR = elaborator.elaborate(child)
    val n = if name.nonEmpty then name else freshName(child.moduleName)
    emit(StmtIR.Instance(n, child.moduleName))
    new Instance[M](n, child, childIR)

  private[hdl] def build(): ModuleIR =
    ModuleIR(modName, ports.toSeq, statements.toSeq)

final class Instance[M <: Module](val name: String, val module: M, val moduleIR: ModuleIR):
  def io: InstanceIO[M] = new InstanceIO[M](name, module)

  override def toString(): String = s"Instance($name, ${module.moduleName})"

final class InstanceIO[M <: Module](val instanceName: String, val module: M) extends Selectable with Dynamic:
  private lazy val ioField: IO[?] =
    val clazz = module.getClass
    var result: IO[?] = null
    for field <- clazz.getDeclaredFields if result == null do
      field.setAccessible(true)
      field.get(module) match
        case io: IO[?] => result = io
        case _ => ()
    if result == null then
      throw new NoSuchElementException(s"Module ${module.moduleName} has no IO field")
    result

  def selectDynamic(fieldName: String): InstancePort[?] =
    val ioVal = ioField
    val ioT = ioVal.t
    ioT match
      case p: Product =>
        val labels = p.productElementNames.toArray
        val idx = labels.indexOf(fieldName)
        if idx >= 0 then
          val child = p.productElement(idx).asInstanceOf[ValueType]
          // Use the IO's name as prefix (e.g., "io") plus the field name
          val portPath = if ioVal.name.isEmpty then fieldName else s"${ioVal.name}_$fieldName"
          new InstancePort(instanceName, child, portPath)
        else
          throw new NoSuchElementException(s"IO has no field '$fieldName'")
      case _ =>
        throw new NoSuchElementException(s"IO is not a product type")

object dsl:
  def Wire[T <: ValueType](tpe: T)(using ctx: ElabContext): hdl.Wire[T] =
    ctx.wire(tpe)

  def Wire[T <: ValueType](tpe: T, name: String)(using ctx: ElabContext): hdl.Wire[T] =
    ctx.wire(tpe, name)

  def Reg[T <: ValueType](tpe: T)(using ctx: ElabContext): hdl.Reg[T] =
    ctx.reg(tpe)

  def Reg[T <: ValueType](tpe: T, name: String)(using ctx: ElabContext): hdl.Reg[T] =
    ctx.reg(tpe, name)

  def Module[M <: hdl.Module](child: M)(using ctx: ElabContext): Instance[M] =
    ctx.instantiate(child)

  def Module[M <: hdl.Module](child: M, name: String)(using ctx: ElabContext): Instance[M] =
    ctx.instantiate(child, name)

class Elaborator:
  def elaborate(module: Module): ModuleIR =
    val ctx = new ElabContext(module.moduleName, this)
    discoverAndRegisterPorts(ctx, module)
    module.body(using ctx)
    ctx.build()

  private def discoverAndRegisterPorts(ctx: ElabContext, module: Module): Unit =
    // Find all IO[_] fields via reflection and set their names
    val clazz = module.getClass
    for field <- clazz.getDeclaredFields do
      field.setAccessible(true)
      field.get(module) match
        case io: IO[?] =>
          // Set the IO's name from the field name if not already set
          if io.name.isEmpty then
            io.setName(field.getName)
          registerIO(ctx, io.name, io.t.asInstanceOf[ValueType])
        case _ => ()

  private def registerIO(ctx: ElabContext, baseName: String, v: ValueType): Unit =
    v match
      case b: Bundle =>
        // Flatten bundle fields into individual ports
        val p = b.asInstanceOf[Product]
        for i <- 0 until p.productArity do
          val fieldName = p.productElementName(i)
          val fieldValue = p.productElement(i).asInstanceOf[ValueType]
          val fullName = s"${baseName}_$fieldName"
          registerIO(ctx, fullName, fieldValue)
      case u: UInt =>
        ctx.addPort(PortIR(baseName, TypeIR.UIntIR(u.w.value), u.dir))
      case b: Bool =>
        ctx.addPort(PortIR(baseName, TypeIR.BoolIR(), b.dir))
      case vec: Vec[?] =>
        ctx.addPort(PortIR(baseName, valueTypeToIR(vec), Direction.Out))

def valueTypeToIR(v: ValueType): TypeIR = v match
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
