/**
 * Hardware utility functions and common patterns.
 *
 * This package provides reusable hardware utilities and interfaces:
 *
 * ==Bit Manipulation==
 * - [[Fill]] - Bit replication for masks and patterns
 * - [[Reverse]] - Bit order reversal
 * - [[PopCount]] - Population count (Hamming weight)
 * - [[Splice]] - Split values into slices
 *
 * ==Encoding and Selection==
 * - [[PriorityEncoder]] - Find highest set bit (binary output)
 * - [[PriorityEncoderOH]] - Find highest set bit (one-hot output)
 * - [[UIntToOH]] - Binary to one-hot conversion
 * - [[MuxOneHot]] - One-hot multiplexer
 *
 * ==Masking==
 * - [[MaskLower]] - Lower inclusive masks
 * - [[MaskUpper]] - Upper inclusive masks
 *
 * ==Interface Patterns==
 * - [[Decoupled]] - Ready-valid handshaking interface
 * - [[Valid]] - Valid signal wrapper
 * - [[BitPat]] - Bit pattern matching for decoding
 *
 * @example
 * {{{
 * import hdl.util._
 *
 * val requests = Wire(UInt(Width(8)))
 * val grant = PriorityEncoderOH(requests)  // One-hot grant
 * val grantIdx = PriorityEncoder(requests) // Binary index
 * }}}
 */
package object util
