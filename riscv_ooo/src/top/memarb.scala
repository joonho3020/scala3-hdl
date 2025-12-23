package riscv_ooo

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
    val ownerD = Reg(Bool())
    val ownerI = !ownerD

    val chooseD = io.dcache.req.valid
    val chooseI = io.icache.req.valid && !io.dcache.req.valid

    val request = DecoupledHelper(
      io.mem.req.ready,
      !busy)

    io.mem.req.valid := request.fire(io.mem.req.ready, chooseD) ||
                        request.fire(io.mem.req.ready, chooseI)
    io.mem.req.bits  := Mux(chooseD, io.dcache.req.bits, io.icache.req.bits)

    io.dcache.req.ready := request.fire() && chooseD
    io.icache.req.ready := request.fire() && chooseI

    when (io.mem.req.fire) {
      busy := true.B
      ownerD := chooseD
    }

    when (io.mem.resp.valid) {
      busy := false.B
    }

    io.dcache.resp.valid := io.mem.resp.valid && busy && ownerD
    io.icache.resp.valid := io.mem.resp.valid && busy && ownerI
    io.dcache.resp.bits := io.mem.resp.bits
    io.icache.resp.bits := io.mem.resp.bits
  }
