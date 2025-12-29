/**
 * Core hardware description language primitives.
 *
 * This package contains the fundamental building blocks for describing hardware:
 *
 * ==Module System==
 * - [[Module]] - Base class for hardware modules
 * - [[ModuleBuilder]] - Internal builder for module construction
 * - [[ModuleOps]] - Operations for module manipulation
 *
 * ==Hardware Types==
 * - [[HWData]] - Base trait for all hardware data types
 * - [[UInt]], [[SInt]] - Unsigned and signed integers
 * - [[Bool]] - Boolean signals
 * - [[Clock]], [[Reset]] - Timing and reset signals
 * - [[Vec]] - Hardware vectors
 * - [[Bundle]] - Aggregate hardware types
 *
 * ==Hardware Construction==
 * - [[IO]], [[Wire]], [[Reg]] - Hardware declaration macros
 * - [[RegNext]], [[RegInit]], [[WireInit]] - Specialized constructors
 * - [[when]], [[switch]] - Conditional logic
 * - [[SRAM]] - Memory primitives
 *
 * ==Intermediate Representation==
 * - [[IR]] - FIRRTL-like intermediate representation
 *
 * @example
 * {{{
 * import hdl.core._
 *
 * class Adder extends Module:
 *   given Module = this
 *   val io = IO(AdderIO(
 *     a = Input(UInt(Width(8))),
 *     b = Input(UInt(Width(8))),
 *     sum = Output(UInt(Width(9)))
 *   ))
 *   io.sum := io.a +& io.b
 * }}}
 */
package object core
