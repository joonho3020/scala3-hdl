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

    io.mem := frontend.io.mem

    val core = Module(new Core(p))
    core.io.fetch_uops := frontend.io.uops
    frontend.io.redirect := core.io.redirect
    frontend.io.bpu_update := core.io.bpu_update

    io.retire_info := core.io.retire_info
  }
