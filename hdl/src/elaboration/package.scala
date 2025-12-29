/**
 * Module elaboration, parallel execution, and caching.
 *
 * This package provides the elaboration system that converts Module definitions
 * into intermediate representation (IR) with support for:
 *
 * ==Elaboration==
 * - [[Elaborator]] - Main elaboration engine with parallel execution
 * - [[BuildCache]] - Persistent caching of elaboration results
 *
 * ==Features==
 * - '''Parallel Elaboration''': Uses multiple threads to elaborate independent modules concurrently
 * - '''Module Memoization''': Modules with identical parameters are elaborated once and reused
 * - '''Incremental Builds''': Cached results are invalidated only when source code changes
 * - '''Classfile Tracking''': Detects changes by hashing module classfiles and dependencies
 *
 * ==Hashing==
 * - [[StableHash]] - Typeclass for computing stable hashes
 *
 * @example
 * {{{
 * import hdl.elaboration._
 *
 * val elaborator = new Elaborator
 * val module = new MyModule
 * val designs = elaborator.elaborate(module)
 * val firrtl = elaborator.emitAll(designs)
 * }}}
 *
 * @see [[BuildCache]] for cache configuration
 */
package object elaboration
