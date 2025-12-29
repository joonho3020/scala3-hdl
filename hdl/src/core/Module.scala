package hdl.core

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

  /**
   * Declare a module IO port with automatic name inference.
   *
   * Creates a port on the module boundary. The port name is automatically inferred
   * from the enclosing `val` declaration. Direction (Input/Output/Flipped) must be
   * specified in the hardware type construction.
   *
   * @example
   * {{{
   * class MyModule extends Module:
   *   given Module = this
   *   val io = IO(MyIO(
   *     dataIn = Input(UInt(Width(8))),
   *     dataOut = Output(UInt(Width(8)))
   *   ))
   * }}}
   *
   * @tparam T the hardware data type of the port
   * @param t the port template with direction annotations
   * @return the declared IO port
   */
  protected inline def IO[T <: HWData](inline t: T): T =
    ${ ModuleMacros.ioImpl('t, 'this) }

  /**
   * Declare a combinational wire with automatic name inference.
   *
   * Wires represent combinational logic. They must be assigned exactly once using `:=`.
   * The wire name is automatically inferred from the enclosing `val` declaration.
   *
   * @example
   * {{{
   * val sum = Wire(UInt(Width(9)))
   * sum := a +& b  // Assign once
   * }}}
   *
   * @tparam T the hardware data type
   * @param t the wire type template
   * @return the declared wire
   * @see [[WireInit]] for wires with initialization
   */
  protected inline def Wire[T <: HWData](inline t: T): T =
    ${ ModuleMacros.wireImpl('t, 'this) }

  /**
   * Declare a register with automatic name inference.
   *
   * Registers capture their input on the rising edge of the implicit clock signal.
   * They hold state across clock cycles. Must be assigned to establish the next value.
   *
   * @example
   * {{{
   * val counter = Reg(UInt(Width(32)))
   * counter := counter + 1.U
   * }}}
   *
   * @tparam T the hardware data type
   * @param t the register type template
   * @return the declared register
   * @see [[RegInit]] for registers with reset values
   * @see [[RegNext]] for direct signal delay
   */
  protected inline def Reg[T <: HWData](inline t: T): T =
    ${ ModuleMacros.regImpl('t, 'this) }

  /**
   * Declare a register directly connected to a signal.
   *
   * Creates a register whose next value is the provided signal, equivalent to
   * creating a Reg and immediately assigning it. Useful for pipeline delays.
   *
   * @example
   * {{{
   * val delayed = RegNext(dataIn)  // Delays dataIn by one cycle
   * val pipeline = RegNext(RegNext(dataIn))  // Two-cycle delay
   * }}}
   *
   * @tparam T the hardware data type
   * @param t the signal to register
   * @return the declared register with input connected
   * @see [[Reg]] for explicit assignment
   */
  protected inline def RegNext[T <: HWData](inline t: T): T =
    ${ ModuleMacros.regNextImpl('t, 'this) }

  /**
   * Declare a register with a reset value.
   *
   * Creates a register that initializes to the specified value when the implicit
   * reset signal is asserted. The reset value must be a hardware literal.
   *
   * @example
   * {{{
   * val state = RegInit(0.U(3.W))      // Resets to 0
   * val valid = RegInit(false.B)       // Resets to false
   * }}}
   *
   * @tparam T the hardware data type
   * @param t the reset value (hardware literal)
   * @return the declared register with reset
   * @see [[Reg]] for registers without reset
   */
  protected inline def RegInit[T <: HWData](inline t: T): T =
    ${ ModuleMacros.regInitImpl('t, 'this) }

  /**
   * Declare a wire with an initialization value.
   *
   * Creates a wire and immediately assigns it to the provided value. Equivalent
   * to declaring a Wire and then using `:=` to assign it.
   *
   * @example
   * {{{
   * val defaultValue = WireInit(42.U(8.W))
   * }}}
   *
   * @tparam T the hardware data type
   * @param t the initialization value
   * @return the declared and initialized wire
   * @see [[Wire]] for uninitialized wires
   */
  protected inline def WireInit[T <: HWData](inline t: T): T =
    ${ ModuleMacros.wireInitImpl('t, 'this) }

  /**
   * Create a hardware literal value.
   *
   * Converts a Scala value to a hardware literal. The payload type must match
   * the hardware type's host type.
   *
   * @example
   * {{{
   * val myBundle = Lit(MyBundle(UInt(Width(4)), Bool()))(4, true)
   * }}}
   *
   * @tparam T the hardware data type
   * @param t the hardware type template
   * @param payload the Scala value to convert
   * @return a hardware literal
   */
  protected inline def Lit[T <: HWData](inline t: T)(inline payload: HostTypeOf[T]): T =
    ${ ModuleMacros.litImpl('t, 'payload, 'this) }

  /**
   * Conditional hardware construction using when/elsewhen/otherwise.
   *
   * Creates conditional logic blocks. Hardware inside the block is only active
   * when the condition is true. Can be chained with `.elsewhen` and `.otherwise`.
   *
   * @example
   * {{{
   * when(enable) {
   *   counter := counter + 1.U
   * }.elsewhen(reset) {
   *   counter := 0.U
   * }.otherwise {
   *   counter := counter
   * }
   * }}}
   *
   * @param cond the condition (Bool signal)
   * @param block the hardware to construct when condition is true
   * @return WhenDSL for chaining elsewhen/otherwise
   */
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
  inline def apply[M <: Module](inline gen: M)(using inline parent: Module): M =
    ${ ModuleMacros.moduleInstImpl('gen, 'parent) }

  /** Instantiate a child module with an optional explicit name hint. */
  def instantiate[M <: Module](gen: => M, parent: Module, name: Option[String]): M =
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
