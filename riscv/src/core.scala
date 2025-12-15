package riscv

import hdl._

case class CoreIf(
  fetch_uops: Vec[Decoupled[UOp]],


  alu_valid: Bool,
  alu_out: UInt,
  alu_adder_out: UInt,
  alu_cmp_out: Bool
) extends Bundle[CoreIf]

object CoreIf:
  def apply(p: CoreParams): CoreIf =
    CoreIf(
      fetch_uops    = Flipped(Vec.fill(p.coreWidth)(Decoupled(UOp(p)))),


      alu_valid     = Output(Bool()),
      alu_out       = Output(UInt(p.xlenBits.W)),
      alu_adder_out = Output(UInt(p.xlenBits.W)),
      alu_cmp_out   = Output(Bool()),
    )

class Core(p: CoreParams) extends Module with CoreCacheable(p):
  val io = IO(CoreIf(p))

  body {
    val dec = Module(new Decoder(p))
    dec.io.enq.zip(io.fetch_uops).foreach((d, f) => {
      d.valid := f.valid
      d.bits  := f.bits
      f.ready := d.ready
    })

    dec.io.deq.foreach(x => x.ready := false.B)

    val pipe0 = Module(new ALU(ALUParams(xlen = p.xlenBits)))
    dec.io.deq(0).ready := true.B
    pipe0.io.fn  := dec.io.deq(0).bits.aluOp.asUInt
    pipe0.io.in1 := dec.io.deq(0).bits.rs1
    pipe0.io.in2 := dec.io.deq(0).bits.rs2

    // FIXME...
    io.alu_valid := dec.io.deq(0).valid
    io.alu_out   := pipe0.io.out
    io.alu_adder_out := pipe0.io.adder_out
    io.alu_cmp_out := pipe0.io.cmp_out
  }

case class CoreTopIO(
  icache: ICacheIf
) extends Bundle[CoreTopIO]

object CoreTopIO:
  def apply(p: CoreParams): CoreTopIO =
    CoreTopIO(
      icache = ICacheIf(p),
    )

class CoreTop(p: CoreParams) extends Module with CoreCacheable(p):
  val io = IO(CoreTopIO(p))
  body {
    val frontend = Module(new Frontend(p))
    val core = Module(new Core(p))
    core.io.fetch_uops := frontend.io.uops
    io.icache.req := frontend.io.icache.req
    frontend.io.icache.resp := io.icache.resp
  }
