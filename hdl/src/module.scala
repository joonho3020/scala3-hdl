package hdl

abstract class Module(using protected val ctx: ElabContext):
  def moduleName: String = getClass.getSimpleName.stripSuffix("$")

  protected def Wire[T <: ValueType](tpe: T, name: String = ""): Node[T] = ctx.wire(tpe, name)
  protected def Reg[T <: ValueType](tpe: T, name: String = ""): Node[T] = ctx.reg(tpe, name)
  protected def Instantiate[M <: Module](factory: ElabContext ?=> M, name: String = ""): M = ctx.instantiate(factory, name)
  protected def IO[T <: ValueType](t: T): Node[T] = hdl.IO(t)

  def body(): Unit
