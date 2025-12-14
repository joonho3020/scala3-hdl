package riscv

import hdl._

case class CoreIf(fetch: FetchIf, decoded: Decoupled[RTypeDecode]) extends Bundle[CoreIf]

object CoreIf:
  def apply(p: CoreParams): CoreIf =
    CoreIf(
      fetch = Flipped(FetchIf(p)),
      decoded = Decoupled(RTypeDecode(p))
    )

class Core(p: CoreParams) extends Module with CoreCacheable(p):
  val io = IO(CoreIf(p))
  body {
    val fetchBuffer = Module(new FetchBuffer(p, entries = p.coreWidth * 4))
    fetchBuffer.io.in := io.fetch
    val decoder = Module(new RTypeDecoder(p))
    decoder.io.in.valid := fetchBuffer.io.out.valid
    decoder.io.in.bits := fetchBuffer.io.out.bits
    fetchBuffer.io.out.ready := decoder.io.in.ready
    io.decoded.valid := decoder.io.out.valid
    io.decoded.bits := decoder.io.out.bits
    decoder.io.out.ready := io.decoded.ready
  }

case class CoreTopIO(icache: ICacheIf, decoded: Decoupled[RTypeDecode]) extends Bundle[CoreTopIO]

object CoreTopIO:
  def apply(p: CoreParams): CoreTopIO =
    CoreTopIO(
      icache = ICacheIf(p),
      decoded = Decoupled(RTypeDecode(p))
    )

class CoreTop(p: CoreParams) extends Module with CoreCacheable(p):
  val io = IO(CoreTopIO(p))
  body {
    val frontend = Module(new Frontend(p))
    val core = Module(new Core(p))
    core.io.fetch := frontend.io.core
    io.icache.req := frontend.io.icache.req
    frontend.io.icache.resp := io.icache.resp
    io.decoded.valid := core.io.decoded.valid
    io.decoded.bits := core.io.decoded.bits
    core.io.decoded.ready := io.decoded.ready
  }
