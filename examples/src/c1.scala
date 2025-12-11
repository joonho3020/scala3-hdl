package example

import hdl._

final class CacheableTopLevel(p: CacheHierarchyParams) extends CacheHierarchyModule(p):
  val io = IO(SimpleIO(Input(UInt(p.width.W)), Output(UInt(p.width.W))))
  body:
    val mid = Module(new CacheableLevelTwo(p.copy(offset = p.offset + 3)))
    mid.io.in := io.in
    io.out := mid.io.out
