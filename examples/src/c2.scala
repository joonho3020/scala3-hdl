
package example

import hdl._

final class CacheableLevelTwo(p: CacheHierarchyParams) extends CacheHierarchyModule(p):
  val io = IO(SimpleIO(Input(UInt(p.width.W)), Output(UInt(p.width.W))))
  body:
    val branch = Module(new CacheableLevelThree(p.copy(offset = p.offset + 2)))
    branch.io.in := io.in
    io.out := branch.io.out

