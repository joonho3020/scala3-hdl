package riscv_ooo

import hdl._

case class TileIO(
  mem: MagicMemIf,
  retire_info: Vec[RetireInfoIf],
  rn2_uops: Option[Vec[Valid[UOp]]]
) extends Bundle[TileIO]

object TileIO:
  def apply(p: CoreParams): TileIO =
    TileIO(
      mem = MagicMemIf(p),
      retire_info = Vec.fill(p.coreWidth)(RetireInfoIf(p)),
      rn2_uops = if p.debug then Some(Output(Vec.fill(p.coreWidth)(Valid(UOp(p))))) else None
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

    core.io.fetch_uops <> frontend.io.uops
    frontend.io.redirect <> core.io.redirect
    frontend.io.bpu_update <> core.io.bpu_update

    io.retire_info <> core.io.retire_info
    io.rn2_uops.zip(core.io.debug_rn2_uops).foreach(_ <> _)
  }
