package hdl

import scala.collection.mutable

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

  def instantiate[M <: Module](mod: M, name: String = ""): M =
    println(s"instantiate ${name}")
    elaborator.elaborateSubmodule(mod)
    val instName = if name.nonEmpty then name else freshName("inst")
    emit(StmtIR.Instance(instName, mod.moduleName))
    mod

  private[hdl] def build(): ModuleIR =
    ModuleIR(modName, ports.toSeq, statements.toSeq)

class Elaborator:
  private val elaborated = mutable.LinkedHashMap[Module, ModuleIR]()
  private val elaborating = mutable.Set[Module]()

  def elaborate(module: Module): ModuleIR =
    println(s"elaborate ${module.moduleName}")
    elaborateModule(module)

  private[hdl] def elaborateSubmodule(module: Module): ModuleIR =
    println(s"elaborateSubmodule ${module.moduleName}")
    elaborateModule(module)

  private def elaborateModule(module: Module): ModuleIR =
    elaborated.getOrElseUpdate(module, {
      if elaborating.contains(module) then
        throw new IllegalStateException(s"Circular elaboration detected for ${module.moduleName}")
      elaborating += module
      val ctx = new ElabContext(module.moduleName, this)
      discoverAndRegisterPorts(ctx, module)
      module.body(using ctx)
      val ir = ctx.build()
      elaborating -= module
      ir
    })

  def modules: Seq[ModuleIR] =
    elaborated.values.toSeq

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
