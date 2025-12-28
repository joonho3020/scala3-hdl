package hdl

import scala.collection.mutable

/** Base class for all hardware modules. */
abstract class Module:
  private val _builder = new ModuleBuilder(moduleName)
  private val _children = mutable.ArrayBuffer.empty[Module]
  private var _instanceName: Option[String] = None
  private var _bodyFn: Option[Module ?=> Unit] = None
  private var _bodyRan = false

  private var _implicitClock: Clock =
    val name = this.getBuilder.allocateName(Some("clock"), "clock")
    val clk = Input(Clock())
    clk.setNodeKind(NodeKind.IO)
    this.register(clk, Some(name))
    this.getBuilder.addPort(IR.Port(IR.Identifier(name), Direction.In, IR.ClockType))
    clk

  private var _implicitReset: Reset =
    val name = this.getBuilder.allocateName(Some("reset"), "reset")
    val reset = Input(Reset())
    reset.setNodeKind(NodeKind.IO)
    this.register(reset, Some(name))
    this.getBuilder.addPort(IR.Port(IR.Identifier(name), Direction.In, IR.ResetType))
    reset

  /** Name used for module definition in emitted IR. */
  def moduleName: String = getClass.getSimpleName.stripSuffix("$")
  private[hdl] def setInstanceName(n: String): Unit = _instanceName = Some(n)
  private[hdl] def instanceName: Option[String] = _instanceName
  private[hdl] def addChild(m: Module): Unit = _children += m
  private[hdl] def children: Seq[Module] = _children.toSeq
  private[hdl] def getBuilder: ModuleBuilder = _builder
  private[hdl] def register[T](data: T, ref: Option[String]): Unit =
    ModuleOps.assignOwner(data, this)
    ref.foreach(r => ModuleOps.assignRefs(data, r))
  private[hdl] def getImplicitClock: Clock = _implicitClock
  private[hdl] def getImplicitReset: Reset = _implicitReset

  /** Registers the module body for lazy elaboration.
   * Storing the module body as a thunk is required in order to achieve lazy elaboration.
   * If the module body is elaborated eagerly, there is no point in incremental elaboration and caching. */
  protected final def body(f: Module ?=> Unit): Unit =
    _bodyFn = Some(f)

  private[hdl] def runBody(): Unit =
    if !_bodyRan then
      given Module = this
      _bodyFn.foreach(fn => fn(using summon[Module]))
      _bodyRan = true

  /** Declare a module IO port. */
  protected inline def IO[T <: HWData](inline t: T): T =
    ${ ModuleMacros.ioImpl('t, 'this) }

  /** Declare a wire. */
  protected inline def Wire[T <: HWData](inline t: T): T =
    ${ ModuleMacros.wireImpl('t, 'this) }

  /** Declare a register. */
  protected inline def Reg[T <: HWData](inline t: T): T =
    ${ ModuleMacros.regImpl('t, 'this) }

  /** Declare a register connected to the next value. */
  protected inline def RegNext[T <: HWData](inline t: T): T =
    ${ ModuleMacros.regNextImpl('t, 'this) }

  /** Declare a register with an explicit reset value. */
  protected inline def RegInit[T <: HWData](inline t: T): T =
    ${ ModuleMacros.regInitImpl('t, 'this) }

  /** Declare a wire with an explicit default value. */
  protected inline def WireInit[T <: HWData](inline t: T): T =
    ${ ModuleMacros.wireInitImpl('t, 'this) }

  /** Create a literal hardware value. */
  protected inline def Lit[T <: HWData](inline t: T)(inline payload: HostTypeOf[T]): T =
    ${ ModuleMacros.litImpl('t, 'payload, 'this) }

  /** Conditional hardware construction. */
  protected inline def when(cond: Bool)(block: => Unit)(using m: Module): WhenDSL =
    ModuleOps.when(cond, summon[Module]) {
      block
    }

  /** Emit a formatted print statement. */
  protected inline def Printf(inline format: String, inline args: HWData*): Unit =
    ${ ModuleMacros.printfImpl('format, 'args, 'this) }

  /** Emit a runtime assertion. */
  protected inline def Assert(inline cond: Bool, inline message: String = ""): Unit =
    ${ ModuleMacros.assertImpl('cond, 'message, 'this) }

object Module:
  /** Instantiate a child module with a name inferred from the enclosing val. */
  inline def apply[M <: hdl.Module](inline gen: M)(using inline parent: hdl.Module): M =
    ${ ModuleMacros.moduleInstImpl('gen, 'parent) }

  /** Instantiate a child module with an optional explicit name hint. */
  def instantiate[M <: hdl.Module](gen: => M, parent: hdl.Module, name: Option[String]): M =
    val sub = gen
    val instName = parent.getBuilder.allocateName(name, "inst")
    sub.setInstanceName(instName)
    parent.addChild(sub)
    parent.getBuilder.addInst(instName, sub, sub.moduleName)
    sub

object ModuleMacros:
  import scala.quoted.*

  def ioImpl[T <: HWData: Type](t: Expr[T], mod: Expr[Module])(using Quotes): Expr[T] =
    val nameOpt = MacroUtils.findEnclosingValName
    '{
      ModuleOps.io($t, ${Expr(nameOpt)}, $mod)
    }

  def wireImpl[T <: HWData: Type](t: Expr[T], mod: Expr[Module])(using Quotes): Expr[T] =
    val nameOpt = MacroUtils.findEnclosingValName
    '{
      ModuleOps.wire($t, ${Expr(nameOpt)}, $mod)
    }

  def regImpl[T <: HWData: Type](t: Expr[T], mod: Expr[Module])(using Quotes): Expr[T] =
    val nameOpt = MacroUtils.findEnclosingValName
    '{
      ModuleOps.reg($t, ${Expr(nameOpt)}, $mod)
    }

  def regInitImpl[T <: HWData: Type](t: Expr[T], mod: Expr[Module])(using Quotes): Expr[T] =
    val nameOpt = MacroUtils.findEnclosingValName
    '{
      ModuleOps.regInit($t, ${Expr(nameOpt)}, $mod)
    }

  def regNextImpl[T <: HWData: Type](t: Expr[T], mod: Expr[Module])(using Quotes): Expr[T] =
    val nameOpt = MacroUtils.findEnclosingValName
    '{
      ModuleOps.regNext($t, ${Expr(nameOpt)}, $mod)
    }

  def wireInitImpl[T <: HWData: Type](t: Expr[T], mod: Expr[Module])(using Quotes): Expr[T] =
    val nameOpt = MacroUtils.findEnclosingValName
    '{
      ModuleOps.wireInit($t, ${Expr(nameOpt)}, $mod)
    }

  def litImpl[T <: HWData: Type](t: Expr[T], payload: Expr[HostTypeOf[T]], mod: Expr[Module])(using Quotes): Expr[T] =
    '{
      ModuleOps.lit($t, $payload, $mod)
    }

  def moduleInstImpl[M <: Module: Type](gen: Expr[M], parent: Expr[Module])(using Quotes): Expr[M] =
    val nameOpt = MacroUtils.findEnclosingValName
    '{ Module.instantiate($gen, $parent, ${Expr(nameOpt)}) }

  def printfImpl(format: Expr[String], args: Expr[Seq[HWData]], mod: Expr[Module])(using Quotes): Expr[Unit] =
    val nameOpt = MacroUtils.findEnclosingValName
    '{
      ModuleOps.printf(${Expr(nameOpt)}, $format, $args, $mod)
    }

  def assertImpl(cond: Expr[Bool], message: Expr[String], mod: Expr[Module])(using Quotes): Expr[Unit] =
    val nameOpt = MacroUtils.findEnclosingValName
    '{
      ModuleOps.hwAssert(${Expr(nameOpt)}, $cond, $message, $mod)
    }
