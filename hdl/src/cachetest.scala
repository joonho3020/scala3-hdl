package hdl

import java.io.{ByteArrayOutputStream, PrintStream}
import java.nio.file.{Files, Path}

final case class CacheHierarchyParams(width: Int, offset: Int) derives StableHash

private abstract class CacheHierarchyModule(p: CacheHierarchyParams) extends Module with CacheableModule:
  type ElabParams = CacheHierarchyParams
  given stableHashElabParams: StableHash[CacheHierarchyParams] = summon[StableHash[CacheHierarchyParams]]
  def elabParams: CacheHierarchyParams = p
  protected val params: CacheHierarchyParams = p

final class CacheableLevelFour(p: CacheHierarchyParams) extends CacheHierarchyModule(p):
  def codeHash = ModuleCodeHash.astHash[this.type]

  val io = IO(SimpleIO(Input(UInt(p.width.W)), Output(UInt(p.width.W))))
  body:
    io.out := io.in + Lit(UInt(p.width.W))(p.offset)

final class CacheableLevelThree(p: CacheHierarchyParams) extends CacheHierarchyModule(p):
  def codeHash = ModuleCodeHash.astHash[this.type]

  val io = IO(SimpleIO(Input(UInt(p.width.W)), Output(UInt(p.width.W))))
  body:
    val leaf = Module(new CacheableLevelFour(p.copy(offset = p.offset + 1)))
    leaf.io.in := io.in
    io.out := leaf.io.out + leaf.io.out

final class CacheableLevelTwo(p: CacheHierarchyParams) extends CacheHierarchyModule(p):
  def codeHash = ModuleCodeHash.astHash[this.type]

  val io = IO(SimpleIO(Input(UInt(p.width.W)), Output(UInt(p.width.W))))
  body:
    val branch = Module(new CacheableLevelThree(p.copy(offset = p.offset + 2)))
    branch.io.in := io.in
    io.out := branch.io.out

final class CacheableTopLevel(p: CacheHierarchyParams) extends CacheHierarchyModule(p):
  def codeHash = ModuleCodeHash.astHash[this.type]

  val io = IO(SimpleIO(Input(UInt(p.width.W)), Output(UInt(p.width.W))))
  body:
    val mid = Module(new CacheableLevelTwo(p.copy(offset = p.offset + 3)))
    mid.io.in := io.in
    io.out := mid.io.out

@main def cachetest(): Unit =
    val params = CacheHierarchyParams(width = 8, offset = 1)

    val cold = new CacheableTopLevel(params)
    val elaborator = new Elaborator
    println(elaborator.emitAll(elaborator.elaborate(cold)))
