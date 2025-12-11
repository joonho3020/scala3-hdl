
package example

import hdl.CacheableModule
import hdl.Module
import hdl._




final class CacheableLevelFour(p: CacheHierarchyParams) extends CacheHierarchyModule(p):
  val io = IO(SimpleIO(Input(UInt(p.width.W)), Output(UInt(p.width.W))))
  body:
    io.out := io.in + Lit(UInt(p.width.W))(p.offset)

