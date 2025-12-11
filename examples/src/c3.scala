
package example

import hdl._

final class CacheableLevelThree(p: CacheHierarchyParams) extends CacheHierarchyModule(p):
  val io = IO(SimpleIO(Input(UInt(p.width.W)), Output(UInt(p.width.W))))
  body:
    val leaf = Module(new CacheableLevelFour(p.copy(offset = p.offset + 1)))
    leaf.io.in := io.in
    io.out := leaf.io.out + leaf.io.out
    io.out := leaf.io.out + leaf.io.out

