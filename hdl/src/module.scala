package hdl

import scala.collection.mutable

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

  private[hdl] def build(): ModuleIR =
    ModuleIR(modName, ports.toSeq, statements.toSeq)

object dsl:
  // Wire with auto-captured name from enclosing val
  inline def Wire[T <: ValueType](tpe: T)(using ctx: ElabContext): hdl.Wire[T] =
    NameMacros.wireWithName(tpe)

  // Reg with auto-captured name from enclosing val
  inline def Reg[T <: ValueType](tpe: T)(using ctx: ElabContext): hdl.Reg[T] =
    NameMacros.regWithName(tpe)

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
