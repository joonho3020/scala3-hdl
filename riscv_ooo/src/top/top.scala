package riscv_ooo

import hdl.core._
import hdl.util._
import hdl.elaboration._


case class TileIO(
  mem: MagicMemIf,
  retire_info: Vec[RetireTraceIf],
  cosim_info: Vec[CoSimInfoIf]
) extends Bundle[TileIO]

object TileIO:
  def apply(p: CoreParams): TileIO =
    TileIO(
      mem = MagicMemIf(p),
      retire_info = Vec.fill(p.coreWidth)(RetireTraceIf(p)),
      cosim_info = Vec.fill(p.coreWidth)(CoSimInfoIf(p))
    )

class Tile(p: CoreParams) extends Module:
  val io = IO(TileIO(p))
  body {
    dontTouch(io)

    val frontend = Module(new Frontend(p))
    val core = Module(new Core(p))
    val memarb = Module(new MemArbiter(p))

    memarb.io.icache <> frontend.io.mem
    memarb.io.dcache <> core.io.mem
    io.mem <> memarb.io.mem

    for (i <- 0 until p.coreWidth) {
      core.io.ifu.fetch_uops(i).valid := frontend.io.core.fetch_uops(i).valid
      core.io.ifu.fetch_uops(i).bits := frontend.io.core.fetch_uops(i).bits
      frontend.io.core.fetch_uops(i).ready := core.io.ifu.fetch_uops(i).ready
    }

    frontend.io.core.redirect.valid := core.io.ifu.redirect.valid
    frontend.io.core.redirect.target := core.io.ifu.redirect.target
    frontend.io.core.redirect.ftq_idx := core.io.ifu.redirect.ftq_idx
    frontend.io.core.redirect.taken := core.io.ifu.redirect.taken

    frontend.io.core.commit.valid := core.io.ifu.commit.valid
    frontend.io.core.commit.ftq_idx := core.io.ifu.commit.ftq_idx
    io.retire_info <> core.io.retire_info
    io.cosim_info <> core.io.cosim_info
  }
