package hdl

import scala.collection.mutable
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.CompletableFuture
import java.util.function.Function
import scala.jdk.CollectionConverters.*
import java.lang.ThreadLocal

class ElabContext private[hdl] (
  private var modName: String,
  private val elaborator: Elaborator
):
  private val statements = mutable.ListBuffer[StmtIR]()
  private val ports = mutable.ListBuffer[PortIR]()
  private var nameCounter = 0

  // Implicit clock and reset
  val clock: ExprIR = ExprIR.Ref("clock")
  val reset: ExprIR = ExprIR.Ref("reset")

  private[hdl] def setModuleName(n: String): Unit =
    modName = n

  private[hdl] def freshName(prefix: String): String =
    nameCounter += 1
    s"${prefix}_$nameCounter"

  def emit(stmt: StmtIR): Unit =
    statements += stmt

  def addPort(port: PortIR): Unit =
    ports += port

  def wire[T <: ValueType](tpe: T, name: String = ""): Node[T] =
    val n = if name.nonEmpty then name else freshName("wire")
    emit(StmtIR.WireDecl(n, valueTypeToIR(tpe)))
    new RefNode(n, tpe, NodeKind.Wire)

  def reg[T <: ValueType](tpe: T, name: String = ""): Node[T] =
    val n = if name.nonEmpty then name else freshName("reg")
    emit(StmtIR.RegDecl(n, valueTypeToIR(tpe)))
    new RefNode(n, tpe, NodeKind.Reg)

  def instantiate[M <: Module](mod: ElabContext ?=> M, name: String = ""): M =
    val childCtx = new ElabContext("", elaborator)
    val m = mod(using childCtx)
    childCtx.setModuleName(m.moduleName)
    elaborator.elaborateSubmodule(m, childCtx)
    val instName = if name.nonEmpty then name else freshName("inst")
    setInstanceIOs(m, instName)
    emit(StmtIR.Instance(instName, m.moduleName))
    m

  // Required for IR emission
  // Otherwise, accessing IO ports of submodules won't have a reference to the
  // module instance
  private def setInstanceIOs(mod: Module, prefix: String): Unit =
    val clazz = mod.getClass
    for field <- clazz.getDeclaredFields do
      field.setAccessible(true)
      field.get(mod) match
        case io: RefNode[?] if io.kind == NodeKind.IO =>
          val baseName = if io.refName.isEmpty then field.getName else io.refName
          io.setName(s"$prefix.$baseName")
        case _ => ()

  private[hdl] def build(): ModuleIR =
    ModuleIR(modName, ports.toSeq, statements.toSeq)

class Elaborator:
  private val elaborated = new ConcurrentHashMap[Module, CompletableFuture[ModuleIR]]()
  private val elaboratingStack = new ThreadLocal[mutable.Set[Module]]:
    override def initialValue(): mutable.Set[Module] = mutable.Set.empty[Module]

  def elaborate[M <: Module](factory: ElabContext ?=> M): ModuleIR =
    val ctx = new ElabContext("", this)
    val module = factory(using ctx)
    ctx.setModuleName(module.moduleName)
    elaborateModule(module, ctx)

  private[hdl] def elaborateSubmodule[M <: Module](module: M, ctx: ElabContext): ModuleIR =
    elaborateModule(module, ctx)

  private def elaborateModule[M <: Module](module: M, ctx: ElabContext): ModuleIR =
    val stack = elaboratingStack.get()
    val existing = elaborated.get(module)
    if existing != null then
      if stack.contains(module) then
        throw new IllegalStateException(s"Circular elaboration detected for ${module.moduleName}")
      return existing.join()
    val future = new CompletableFuture[ModuleIR]()
    val prev = elaborated.putIfAbsent(module, future)
    if prev != null then
      if stack.contains(module) then
        throw new IllegalStateException(s"Circular elaboration detected for ${module.moduleName}")
      return prev.join()
    if stack.contains(module) then
      val ex = new IllegalStateException(s"Circular elaboration detected for ${module.moduleName}")
      future.completeExceptionally(ex)
      throw ex
    stack += module
    try
      println(s"[${Thread.currentThread().getName}] start ${module.moduleName}")
      ctx.setModuleName(module.moduleName)
      discoverAndRegisterPorts(ctx, module)
      module.body()
      val ir = ctx.build()
      println(s"[${Thread.currentThread().getName}] done ${module.moduleName}")
      future.complete(ir)
    catch
      case e: Throwable =>
        future.completeExceptionally(e)
        throw e
    finally
      stack -= module
    future.join()

  def modules: Seq[ModuleIR] =
    elaborated.values.asScala.map(_.join()).toSeq

  private def discoverAndRegisterPorts(ctx: ElabContext, module: Module): Unit =
    val clazz = module.getClass
    for field <- clazz.getDeclaredFields do
      field.setAccessible(true)
      field.get(module) match
        case io: RefNode[?] if io.kind == NodeKind.IO =>
          if io.refName.isEmpty then
            io.setName(field.getName)
          registerIO(ctx, io.refName, io.t.asInstanceOf[ValueType])
        case _ => ()


  private def registerIO(ctx: ElabContext, baseName: String, v: ValueType): Unit =
    v match
      case u: UInt =>
        ctx.addPort(PortIR(baseName, TypeIR.UIntIR(u.w.value), u.dir))
      case b: Bool =>
        ctx.addPort(PortIR(baseName, TypeIR.BoolIR(), b.dir))
      case other =>
        ctx.addPort(PortIR(baseName, valueTypeToIR(other), Direction.Out))

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
