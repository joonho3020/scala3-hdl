package riscv_ooo

import hdl._

case class RegfileReadPort(
  addr: UInt,
  data: UInt
) extends Bundle[RegfileReadPort]

object RegfileReadPort:
  def apply(p: CoreParams): RegfileReadPort =
    RegfileReadPort(
      addr = Input(UInt(p.pRegIdxBits.W)),
      data = Output(UInt(p.xlenBits.W))
    )

case class RegfileWritePort(
  valid: Bool,
  addr: UInt,
  data: UInt
) extends Bundle[RegfileWritePort]

object RegfileWritePort:
  def apply(p: CoreParams): RegfileWritePort =
    RegfileWritePort(
      valid = Input(Bool()),
      addr = Input(UInt(p.pRegIdxBits.W)),
      data = Input(UInt(p.xlenBits.W))
    )

case class PhysicalRegfileIO(
  read_ports: Vec[RegfileReadPort],
  write_ports: Vec[RegfileWritePort]
) extends Bundle[PhysicalRegfileIO]

class PhysicalRegfile(p: CoreParams) extends Module with CoreCacheable(p):
  given Module = this

  val io = IO(PhysicalRegfileIO(
    read_ports  = Vec.fill(p.prfReadPorts )(RegfileReadPort(p)),
    write_ports = Vec.fill(p.prfWritePorts)(RegfileWritePort(p))
  ))

  body {
    val regfile = Reg(Vec.fill(p.nPhysicalRegs)(UInt(p.xlenBits.W)))

    for (rp <- io.read_ports) {
      val bypass_hit = io.write_ports.map(wp => wp.valid && wp.addr === rp.addr)
      val bypass_data = io.write_ports.map(_.data)
      val bypass_en = bypass_hit.reduce(_ || _)
      val bypass_oh = Cat(bypass_hit.reverse).asOH

      rp.data := Mux(bypass_en,
        MuxOneHot(bypass_oh, bypass_data.toSeq),
        regfile(rp.addr))
    }

    for (wp <- io.write_ports) {
      when (wp.valid && wp.addr =/= 0.U) {
        regfile(wp.addr) := wp.data
      }
    }

    when (reset.asBool) {
      for (i <- 0 until p.nPhysicalRegs) {
        regfile(i) := 0.U
      }
    }

    dontTouch(io)
    dontTouch(regfile)
  }
