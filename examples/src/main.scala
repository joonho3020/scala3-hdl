package example

import java.io.{ByteArrayOutputStream, PrintStream}
import java.nio.file.{Files, Path}
import hdl.CacheableModule
import hdl.StableHash
import hdl.Elaborator
import hdl.Module

final case class CacheHierarchyParams(width: Int, offset: Int) derives StableHash

private abstract class CacheHierarchyModule(p: CacheHierarchyParams) extends Module with CacheableModule:
  type ElabParams = CacheHierarchyParams
  given stableHashElabParams: StableHash[CacheHierarchyParams] = summon[StableHash[CacheHierarchyParams]]
  def elabParams: CacheHierarchyParams = p
  protected val params: CacheHierarchyParams = p

@main def cachetest(): Unit =
    val params = CacheHierarchyParams(width = 8, offset = 1)

    val cold = new CacheableTopLevel(params)
    val elaborator = new Elaborator
    println(elaborator.emitAll(elaborator.elaborate(cold)))
