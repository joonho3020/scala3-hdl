package riscv

import hdl._

case class MemArbIO(
  icache: MagicMemIf,
  dcache: MagicMemIf,
  mem: MagicMemIf
) extends Bundle[MemArbIO]

object MemArbIO:
  def apply(p: CoreParams): MemArbIO =
    MemArbIO(
      icache = Flipped(MagicMemIf(p)),
      dcache = Flipped(MagicMemIf(p)),
      mem = MagicMemIf(p)
    )

class MemArbiter(p: CoreParams) extends Module with CoreCacheable(p):
  val io = IO(MemArbIO(p))
  body {
    val busy = RegInit(false.B)
    val owner = Reg(Bool())

    val chooseD = io.dcache.req.valid && !busy
    val chooseI = io.icache.req.valid && !busy && !io.dcache.req.valid

    io.mem.req.valid := (chooseD || chooseI) && !busy
    io.mem.req.bits := Mux(chooseD, io.dcache.req.bits, io.icache.req.bits)

    io.dcache.req.ready := chooseD && io.mem.req.ready && !busy
    io.icache.req.ready := chooseI && io.mem.req.ready && !busy

    when (io.mem.req.fire) {
      busy := true.B
      owner := chooseD
    }

    when (io.mem.resp.valid) {
      busy := false.B
    }

    io.dcache.resp.valid := io.mem.resp.valid && busy && owner
    io.icache.resp.valid := io.mem.resp.valid && busy && !owner
    io.dcache.resp.bits  := io.mem.resp.bits
    io.icache.resp.bits  := io.mem.resp.bits
  }
