package riscv

import hdl._

case class TileIO(
  mem: MagicMemIf,
  retire_info: Vec[RetireInfoIf]
) extends Bundle[TileIO]

object TileIO:
  def apply(p: CoreParams): TileIO =
    TileIO(
      mem = MagicMemIf(p),
      retire_info = Vec.fill(p.coreWidth)(RetireInfoIf(p))
    )

class Tile(p: CoreParams) extends Module:
  val io = IO(TileIO(p))
  body {
    dontTouch(io)

    val frontend = Module(new Frontend(p))
    val core = Module(new Core(p))
    val memarb = Module(new MemArbiter(p))

    memarb.io.icache.req.valid := frontend.io.mem.req.valid
    memarb.io.icache.req.bits := frontend.io.mem.req.bits
    frontend.io.mem.req.ready := memarb.io.icache.req.ready

    frontend.io.mem.resp := memarb.io.icache.resp

    memarb.io.dcache.req.valid := core.io.mem.req.valid
    memarb.io.dcache.req.bits := core.io.mem.req.bits
    core.io.mem.req.ready := memarb.io.dcache.req.ready

    core.io.mem.resp := memarb.io.dcache.resp

    io.mem.req.valid := memarb.io.mem.req.valid
    io.mem.req.bits := memarb.io.mem.req.bits
    memarb.io.mem.req.ready := io.mem.req.ready

    memarb.io.mem.resp := io.mem.resp


    core.io.fetch_uops := frontend.io.uops
    frontend.io.redirect := core.io.redirect
    frontend.io.bpu_update := core.io.bpu_update

    io.retire_info := core.io.retire_info
  }
