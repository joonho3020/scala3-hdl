package hdl

import hdl.core._
import hdl.util._
import hdl.elaboration._

import utest.*
import java.io.{File, PrintWriter}
import scala.sys.process.*

case class AdderIO(a: UInt, b: UInt, c: UInt) extends Bundle[AdderIO]

class Adder(length: Int) extends Module:
  given Module = this
  val io = IO(AdderIO(
    Input(UInt(Width(length))),
    Input(UInt(Width(length))),
    Output(UInt(Width(length)))
  ))
  val reg = Reg(UInt(Width(length)))
  reg := io.a + io.b
  io.c := reg

case class GCDIO(
  value1: UInt,
  value2: UInt,
  loadingValues: Bool,
  outputGCD: UInt,
  outputValid: Bool
) extends Bundle[GCDIO]

class GCD extends Module:
  given Module = this
  val io = IO(GCDIO(
    Input(UInt(Width(16))),
    Input(UInt(Width(16))),
    Input(Bool()),
    Output(UInt(Width(16))),
    Output(Bool())
  ))

  val x = Reg(UInt(Width(16)))
  val y = Reg(UInt(Width(16)))

  when(x > y) {
    x := x - y
  }.otherwise {
    y := y - x
  }

  when(io.loadingValues) {
    x := io.value1
    y := io.value2
  }

  io.outputGCD := x
  io.outputValid := y === 0.U

case class BitSel1IO(in: UInt, out_hi: UInt, out_lo: UInt) extends Bundle[BitSel1IO]

class BitSel1 extends Module:
  given Module = this
  val io = IO(BitSel1IO(
    Input(UInt(Width(8))),
    Output(UInt(Width(4))),
    Output(UInt(Width(4)))
  ))
  io.out_hi := io.in(7, 4)
  io.out_lo := io.in(3, 0)

case class BitSel2IO(in: UInt, bit3: Bool) extends Bundle[BitSel2IO]

class BitSel2 extends Module:
  given Module = this
  val io = IO(BitSel2IO(
    Input(UInt(Width(8))),
    Output(Bool())
  ))
  io.bit3 := io.in(3).asBool

case class DataMemIO(
  writeEnable: Bool,
  writeAddr: UInt,
  writeData: UInt,
  readAddr: UInt,
  readData: UInt
) extends Bundle[DataMemIO]

class DataMem(depth: Int, width: Int) extends Module:
  given Module = this
  val io = IO(DataMemIO(
    Input(Bool()),
    Input(UInt(Width(log2Ceil(depth)))),
    Input(UInt(Width(width))),
    Input(UInt(Width(log2Ceil(depth)))),
    Output(UInt(Width(width)))
  ))
  val mem = SRAM(UInt(Width(width)), depth)(1, 1, 0)
  io.readData := mem.readPorts(0).read(io.readAddr, !io.writeEnable)
  when(io.writeEnable) {
    mem.writePorts(0).write(io.writeAddr, io.writeData)
  }

case class CacheIO(
  reqAddr: UInt,
  reqRead: Bool,
  reqWrite: Bool,
  reqData: UInt,

  respData: UInt,
  respValid: Bool,

  memAddr: UInt,
  memRead: Bool,
  memWrite: Bool,
  memDataOut: UInt,
  memDataIn: UInt
) extends Bundle[CacheIO]

class Cache(addrWidth: Int = 8, dataWidth: Int = 8, cacheSize: Int = 8) extends Module:
  given Module = this
  private val indexWidth = log2Ceil(cacheSize)
  private val tagWidth = addrWidth - indexWidth - 2
  val io = IO(CacheIO(
    Input(UInt(Width(addrWidth))),
    Input(Bool()),
    Input(Bool()),
    Input(UInt(Width(dataWidth))),
    Output(UInt(Width(dataWidth))),
    Output(Bool()),
    Output(UInt(Width(addrWidth))),
    Output(Bool()),
    Output(Bool()),
    Output(UInt(Width(dataWidth))),
    Input(UInt(Width(dataWidth)))
  ))
  val tagMem = SRAM(UInt(Width(tagWidth)), cacheSize)(1, 1, 0)
  val dataMem = Module(new DataMem(cacheSize, dataWidth))

  val sIdle = 0.U(Width(2))
  val sMemRead = 1.U(Width(2))
  val sWait = 2.U(Width(2))
  val state = RegInit(sIdle)

  val reqTag = io.reqAddr(addrWidth - 1, indexWidth + 2)
  val reqIndex = io.reqAddr(indexWidth + 1, 2)
  val tagRead = tagMem.readPorts(0)
  val tagWrite = tagMem.writePorts(0)

  val reqTagReg    = Reg(UInt(tagWidth.W))
  val reqIndexReg  = Reg(UInt(indexWidth.W))
  val reqDataReg   = Reg(UInt(dataWidth.W))
  val writeBackTag = Reg(UInt(tagWidth.W))
  val writeBackData = Reg(UInt(dataWidth.W))

  val storedTag = tagRead.read(reqIndex, state === sIdle || state === sWait)
  val hit = (storedTag === reqTag) && (storedTag =/= 0.U)

  dataMem.io.readAddr := reqIndex
  dataMem.io.writeEnable := false.B
  dataMem.io.writeAddr := 0.U
  dataMem.io.writeData := 0.U
  io.respValid := false.B
  io.respData := 0.U(dataWidth.W)

  io.memAddr := 0.U(addrWidth.W)
  io.memRead := false.B
  io.memWrite := false.B
  io.memDataOut := 0.U(dataWidth.W)

  when (state === sIdle) {
    reqTagReg := reqTag
    reqIndexReg := reqIndex
    reqDataReg := io.reqData

    when (hit) {
      when (io.reqRead) {
        io.respData := dataMem.io.readData
        io.respValid := true.B
      } .elsewhen (io.reqWrite) {
        dataMem.io.writeEnable := true.B
        dataMem.io.writeAddr := reqIndex
        dataMem.io.writeData := io.reqData
      }
    } .otherwise {
      state := sMemRead
      io.memAddr := io.reqAddr
      io.memRead := true.B
    }
  } .elsewhen (state === sMemRead) {
    state := sWait
    io.memRead := false.B
  } .elsewhen (state === sWait) {
    tagWrite.write(reqIndex, reqTag)
    dataMem.io.writeEnable := true.B
    dataMem.io.writeAddr := reqIndex
    dataMem.io.writeData := io.memDataIn

    io.respData := io.memDataIn
    io.respValid := true.B
    state := sIdle
  }

case class ConstIO(
  input: UInt,
  outSum: UInt,
  outAnd: UInt,
  outOr: UInt,
  outXor: UInt
) extends Bundle[ConstIO]

class Const(length: Int) extends Module:
  given Module = this
  val io = IO(ConstIO(
    Input(UInt(Width(8))),
    Output(UInt(Width(8))),
    Output(UInt(Width(8))),
    Output(UInt(Width(8))),
    Output(UInt(Width(8)))
  ))
  val constant1 = 42.U(Width(8))
  val constant2 = 15.U(Width(8))
  val constant3 = Lit(UInt(Width(8)))(BigInt("10101010", 2))
  val a = io.input + constant1 + constant2
  val b = (io.input + constant1) & constant3
  val c = (io.input + constant1) | constant3
  val d = (io.input + constant1) ^ constant3
  io.outSum := RegNext(a)
  io.outAnd := RegNext(b)
  io.outOr := RegNext(c)
  io.outXor := RegNext(d)

case class CounterIO(start: Bool, out: UInt) extends Bundle[CounterIO]

class Counter(length: Int) extends Module:
  given Module = this
  val io = IO(CounterIO(
    Input(Bool()),
    Output(UInt(Width(length)))
  ))
  val cntr = RegInit(0.U(Width(length)))
  cntr := cntr + 1.U
  io.out := cntr

case class Agg(x: UInt, y: UInt) extends Bundle[Agg]
case class DecoupledMuxIO(
  a: Decoupled[Agg],
  b: Decoupled[Agg],
  c: Decoupled[Agg]
) extends Bundle[DecoupledMuxIO]

object DecoupledMuxIO:
  def apply(): DecoupledMuxIO =
    val agg = Agg(UInt(Width(2)), UInt(Width(2)))
    DecoupledMuxIO(
      Flipped(Decoupled(agg)),
      Flipped(Decoupled(agg)),
      Decoupled(agg)
    )

class DecoupledMux extends Module:
  given Module = this
  val io = IO(DecoupledMuxIO())
  io.c.bits := io.b.bits
  when(io.a.valid) {
    io.c.bits := io.a.bits
  }
  io.c.valid := io.a.valid || io.b.valid
  io.a.ready := io.c.ready
  io.b.ready := io.c.ready && !io.a.valid

case class OutputBundle(x: UInt, y: UInt) extends Bundle[OutputBundle]

case class DynamicIndexingIO(addr: UInt, out: OutputBundle) extends Bundle[DynamicIndexingIO]

class DynamicIndexing extends Module:
  given Module = this
  val io = IO(DynamicIndexingIO(
    Input(UInt(Width(2))),
    Output(OutputBundle(UInt(Width(2)), UInt(Width(3))))
  ))
  val arr_a = Reg(Vec.fill(4)(UInt(Width(3))))
  val arr_b = Reg(Vec.fill(8)(OutputBundle(UInt(Width(2)), UInt(Width(3)))))
  io.out := arr_b(arr_a(io.addr))

case class FirIO(in: UInt, valid: Bool, out: UInt, consts: Vec[UInt]) extends Bundle[FirIO]

object FirIO:
  def apply(length: Int): FirIO =
    FirIO(
      Input(UInt(Width(4))),
      Input(Bool()),
      Output(UInt(Width(4))),
      Input(Vec.fill(length)(UInt(Width(4))))
    )

class Fir(length: Int) extends Module:
  given Module = this
  val io = IO(FirIO(length))
  val taps = Seq(io.in) ++ Seq.fill(io.consts.length - 1)(RegInit(0.U(Width(4))))
  taps.zip(taps.tail).foreach { case (a, b) => when(io.valid) { b := a } }
  io.out := taps.zip(io.consts.elems).map { case (a, b) => a * b }.reduce(_ + _)

case class MyBundle(in: UInt, out: UInt) extends Bundle[MyBundle]

class A extends Module:
  val io = IO(MyBundle(Input(UInt(Width(2))), Output(UInt(Width(2)))))

  body:
    io.out := RegNext(io.in)

class C extends Module:
  val io = IO(MyBundle(Input(UInt(Width(2))), Output(UInt(Width(2)))))

  body:
    io.out := RegNext(io.in)

class B extends Module:
  given Module = this
  val io = IO(MyBundle(Input(UInt(Width(2))), Output(UInt(Width(2)))))

  body:
    val c = Module(new C)
    c.io.in := io.in
    io.out := RegNext(c.io.out)

class Top extends Module:
  val io = IO(MyBundle(Input(UInt(Width(2))), Output(UInt(Width(2)))))

  body:
    val a = Module(new A)
    a.io.in := io.in
    val b = Module(new B)
    b.io.in := a.io.out
    io.out := b.io.out

case class NestedWhenIO(a: UInt, b: UInt, c: UInt, sel: UInt, output: UInt) extends Bundle[NestedWhenIO]

class NestedWhen extends Module:
  given Module = this
  val io = IO(NestedWhenIO(
    Input(UInt(Width(2))),
    Input(UInt(Width(2))),
    Input(UInt(Width(2))),
    Input(UInt(Width(2))),
    Output(UInt(Width(2)))
  ))
  when(io.sel === 0.U(Width(2))) {
    io.output := io.a
  }.elsewhen(io.sel === 1.U(Width(2))) {
    io.output := io.b
  }.otherwise {
    io.output := io.c
  }

case class LCS1IO(out: UInt) extends Bundle[LCS1IO]

class LCS1 extends Module:
  given Module = this
  val io = IO(LCS1IO(
    Output(UInt(Width(8)))
  ))
  val w = Wire(UInt(Width(8)))
  w := 1.U(Width(8))
  w := 2.U(Width(8))
  io.out := w

case class LCS2IO(cond: Bool, out: UInt) extends Bundle[LCS2IO]

class LCS2 extends Module:
  given Module = this
  val io = IO(LCS2IO(
    Input(Bool()),
    Output(UInt(Width(8)))
  ))
  val reg = RegInit(0.U(Width(8)))
  when(io.cond) {
    reg := 42.U(Width(8))
  }
  reg := 99.U(Width(8))
  io.out := reg

case class LCS3IO(in: UInt, out: UInt) extends Bundle[LCS3IO]

class LCS3 extends Module:
  given Module = this
  val io = IO(LCS3IO(
    Input(UInt(Width(8))),
    Output(UInt(Width(8)))
  ))
  val result = Wire(UInt(Width(8)))
  result := 0.U(Width(8))
  when(io.in === 1.U(Width(8))) {
    result := 1.U(Width(8))
    when(io.in === 1.U(Width(8))) {
      result := 2.U(Width(8))
    }
  }
  io.out := result

case class LCS4IO(sel: UInt, out: UInt) extends Bundle[LCS4IO]

class LCS4 extends Module:
  given Module = this
  val io = IO(LCS4IO(
    Input(UInt(Width(2))),
    Output(UInt(Width(8)))
  ))
  val out = Wire(UInt(Width(8)))
  out := 0.U(Width(8))
  when(io.sel === 0.U(Width(2))) { out := 10.U(Width(8)) }
  when(io.sel === 1.U(Width(2))) { out := 20.U(Width(8)) }
  when(io.sel === 2.U(Width(2))) { out := 30.U(Width(8)) }
  io.out := out

case class LCS5IO(a: Bool, b: Bool, c: Bool, out: UInt) extends Bundle[LCS5IO]

class LCS5 extends Module:
  given Module = this
  val io = IO(LCS5IO(
    Input(Bool()),
    Input(Bool()),
    Input(Bool()),
    Output(UInt(Width(8)))
  ))
  val w = Wire(UInt(Width(8)))
  w := 0.U(Width(8))
  when(io.a) {
    w := 1.U(Width(8))
    when(io.b) {
      w := 2.U(Width(8))
      when(io.c) {
        w := 3.U(Width(8))
      }
    }
  }
  io.out := w

case class XY(x: UInt, y: UInt) extends Bundle[XY]
case class LCS6IO(sel: UInt, out: XY) extends Bundle[LCS6IO]

class LCS6 extends Module:
  given Module = this
  val io = IO(LCS6IO(
    Input(UInt(Width(2))),
    Output(XY(UInt(Width(8)), UInt(Width(8))))
  ))
  val temp = Wire(XY(UInt(Width(8)), UInt(Width(8))))
  temp.x := 0.U(Width(8))
  temp.y := 0.U(Width(8))
  when(io.sel === 0.U(Width(2))) {
    temp.x := 10.U(Width(8))
  }.elsewhen(io.sel === 1.U(Width(2))) {
    temp.y := 20.U(Width(8))
  }.otherwise {
    temp.x := 30.U(Width(8))
    temp.y := 40.U(Width(8))
  }
  temp.x := 55.U(Width(8))
  io.out := temp

case class LCS7Inner(a: UInt, b: UInt) extends Bundle[LCS7Inner]
case class LCS7IO(sel: Bool, out: LCS7Inner) extends Bundle[LCS7IO]

class LCS7 extends Module:
  given Module = this
  val io = IO(LCS7IO(
    Input(Bool()),
    Output(LCS7Inner(UInt(Width(8)), UInt(Width(8))))
  ))
  val my_wire = Wire(LCS7Inner(UInt(Width(8)), UInt(Width(8))))
  my_wire.a := 0.U(Width(8))
  my_wire.b := 0.U(Width(8))
  when(io.sel) {
    my_wire.a := 10.U(Width(8))
    my_wire.b := 20.U(Width(8))
  }
  my_wire.b := 99.U(Width(8))
  io.out := my_wire

case class LCS8IO(
  a: UInt,
  b: UInt,
  c: UInt,
  d: UInt,
  sel: UInt,
  output: UInt,
  output_2: UInt
) extends Bundle[LCS8IO]

class LCS8 extends Module:
  given Module = this
  val io = IO(LCS8IO(
    Input(UInt(Width(2))),
    Input(UInt(Width(2))),
    Input(UInt(Width(2))),
    Input(UInt(Width(2))),
    Input(UInt(Width(2))),
    Output(UInt(Width(2))),
    Output(UInt(Width(2)))
  ))
  io.output := DontCare
  when(io.sel === 0.U(Width(2))) {
    io.output := io.b
    io.output := io.a
  }.elsewhen(io.sel === 1.U(Width(2))) {
    io.output := io.a
    io.output := io.b
  }.otherwise {
    when(io.sel === 2.U(Width(2))) {
      io.output := io.d
      io.output := io.c
    }
    io.output := io.c
    io.output := io.d
  }
  when(io.sel === 3.U(Width(2))) {
    io.output_2 := io.c
    io.output_2 := io.d
    io.output_2 := io.b
    io.output_2 := io.a
  }
  io.output_2 := io.c
  io.output_2 := io.d

case class LastConnectSemantics2IO(
  a: UInt,
  b: UInt,
  c: UInt,
  d: UInt,
  sel: UInt,
  output: UInt,
  output_2: UInt
) extends Bundle[LastConnectSemantics2IO]

class LastConnectSemantics2 extends Module:
  given Module = this
  val io = IO(LastConnectSemantics2IO(
    Input(UInt(Width(2))),
    Input(UInt(Width(2))),
    Input(UInt(Width(2))),
    Input(UInt(Width(2))),
    Input(UInt(Width(2))),
    Output(UInt(Width(2))),
    Output(UInt(Width(2)))
  ))
  io.output := DontCare
  when(io.sel === 0.U(Width(2))) {
    io.output := io.a
  }.elsewhen(io.sel === 1.U(Width(2))) {
    io.output := io.b
  }.otherwise {
    when(io.sel === 2.U(Width(2))) {
      io.output := io.c
    }
    io.output := io.d
  }
  when(io.sel === 3.U(Width(2))) {
    io.output_2 := io.a
  }
  io.output_2 := io.c

case class WireRegInsideWhenIO(a: UInt, b: UInt, out: UInt) extends Bundle[WireRegInsideWhenIO]

class WireRegInsideWhen extends Module:
  given Module = this
  val io = IO(WireRegInsideWhenIO(
    Input(UInt(Width(2))),
    Input(UInt(Width(2))),
    Output(UInt(Width(2)))
  ))
  when(io.a =/= io.b) {
    val out = Wire(UInt(Width(2)))
    out := io.a + io.b
    val nxt = RegNext(out)
    io.out := nxt
  }.otherwise {
    val out = Wire(UInt(Width(2)))
    out := io.a - io.b
    val nxt = RegInit(0.U(Width(2)))
    when(io.b === 2.U(Width(2))) {
      nxt := out
    }
    io.out := nxt
  }

case class MultiWhenIO(update: Bool, a: UInt, b: UInt, out: UInt, out_2: UInt) extends Bundle[MultiWhenIO]

class MultiWhen extends Module:
  given Module = this
  val io = IO(MultiWhenIO(
    Input(Bool()),
    Input(UInt(Width(3))),
    Input(UInt(Width(3))),
    Output(UInt(Width(3))),
    Output(UInt(Width(3)))
  ))
  io.out := DontCare
  io.out_2 := DontCare
  when(io.update) {
    val out = Wire(UInt(Width(3)))
    out := io.a + io.b
    val nxt = RegNext(out)
    io.out := nxt
  }.otherwise {
    io.out := io.a + io.b
  }
  when(io.update) {
    val out = Wire(UInt(Width(3)))
    out := io.a + io.b
    val nxt = RegNext(out)
    io.out_2 := nxt
  }.otherwise {
    io.out_2 := io.a - io.b
  }

case class SRAMModuleIO(
  wr_en: Bool,
  wr_addr: UInt,
  wr_data: UInt,
  rd_addr: UInt,
  rd_data: UInt
) extends Bundle[SRAMModuleIO]

class SRAMModule(depth: Int, width: Int) extends Module:
  given Module = this
  val io = IO(SRAMModuleIO(
    Input(Bool()),
    Input(UInt(Width(log2Ceil(depth)))),
    Input(UInt(Width(width))),
    Input(UInt(Width(log2Ceil(depth)))),
    Output(UInt(Width(width)))
  ))
  val mem = SRAM(UInt(Width(width)), depth)(1, 1, 0)
  when(io.wr_en) {
    mem.writePorts(0).write(io.wr_addr, io.wr_data)
  }
  io.rd_data := mem.readPorts(0).read(io.rd_addr, true.B)

case class ChaserIO(
  start: Bool,
  done: Bool,
  sram_rd_addr: UInt,
  sram_rd_data: UInt,
  final_addr: UInt
) extends Bundle[ChaserIO]

class Chaser(addrWidth: Int, steps: Int) extends Module:
  given Module = this
  val io = IO(ChaserIO(
    Input(Bool()),
    Output(Bool()),
    Output(UInt(Width(log2Ceil(steps)))),
    Input(UInt(Width(addrWidth))),
    Output(UInt(Width(addrWidth)))
  ))
  val sIdle = 0.U(Width(2))
  val sChasing = 1.U(Width(2))
  val sDone = 2.U(Width(2))
  val state = RegInit(sIdle)
  val currentAddr = Reg(UInt(Width(addrWidth)))
  val step = RegInit(0.U(Width(log2Ceil(steps + 1))))
  io.done := state === sDone
  io.sram_rd_addr := 0.U(Width(log2Ceil(steps)))
  io.final_addr := currentAddr
  when(state === sIdle) {
    when(io.start) {
      currentAddr := 0.U(Width(addrWidth))
      step := 0.U(Width(log2Ceil(steps + 1)))
      state := sChasing
    }
  }.elsewhen(state === sChasing) {
    when(step < steps.U(Width(log2Ceil(steps + 1)))) {
      io.sram_rd_addr := currentAddr
      currentAddr := io.sram_rd_data
      step := step + 1.U(Width(log2Ceil(steps + 1)))
    }.otherwise {
      state := sDone
    }
  }

case class PointerChasingIO(start: Bool, done: Bool, final_addr: UInt) extends Bundle[PointerChasingIO]

class PointerChasing extends Module:
  given Module = this
  val addrWidth = 4
  val sramDepth = 8
  val chasingSteps = 4
  val io = IO(PointerChasingIO(
    Input(Bool()),
    Output(Bool()),
    Output(UInt(Width(addrWidth)))
  ))
  val sram = SRAM(UInt(Width(addrWidth)), sramDepth)(1, 1, 0)
  val initDone = RegInit(false.B)
  val initCntr = RegInit(0.U(Width(log2Ceil(sramDepth + 1))))
  sram.writePorts(0).enable := false.B
  sram.writePorts(0).address := 0.U(Width(log2Ceil(sramDepth)))
  sram.writePorts(0).data := 0.U(Width(addrWidth))
  when(!initDone) {
    sram.writePorts(0).enable := true.B
    sram.writePorts(0).address := initCntr
    sram.writePorts(0).data := initCntr + 1.U(Width(addrWidth))
    initCntr := initCntr + 1.U(Width(log2Ceil(sramDepth + 1)))
    when(initCntr === (sramDepth - 1).U(Width(log2Ceil(sramDepth)))) {
      initDone := true.B
    }
  }
  val chaser = Module(new Chaser(addrWidth, chasingSteps))
  chaser.io.start := io.start && initDone
  io.done := chaser.io.done
  io.final_addr := chaser.io.final_addr
  sram.readPorts(0).enable := true.B
  sram.readPorts(0).address := chaser.io.sram_rd_addr
  chaser.io.sram_rd_data := sram.readPorts(0).data

case class BundleQueue(a: UInt, b: UInt) extends Bundle[BundleQueue]

case class MyQueueIO(in: Decoupled[BundleQueue], out: Decoupled[BundleQueue]) extends Bundle[MyQueueIO]

class MyQueue(length: Int) extends Module:
  given Module = this
  val io = IO(MyQueueIO(
    Flipped(Decoupled(BundleQueue(UInt(Width(3)), UInt(Width(2))))),
    Decoupled(BundleQueue(UInt(Width(3)), UInt(Width(2))))
  ))
  val q = Module(new Queue(BundleQueue(UInt(Width(3)), UInt(Width(2))), length))
  q.io.enq.valid := io.in.valid
  q.io.enq.bits := io.in.bits
  io.in.ready := q.io.enq.ready
  io.out.valid := q.io.deq.valid
  io.out.bits := q.io.deq.bits
  q.io.deq.ready := io.out.ready

case class MyCustomQueueIO[T <: HWData](enq: Decoupled[T], deq: Decoupled[T]) extends Bundle[MyCustomQueueIO[T]]

class MyCustomQueue[T <: HWData](data: T, entries: Int) extends Module:
  given Module = this
  val io = IO(MyCustomQueueIO(
    Flipped(Decoupled(data)),
    Decoupled(data)
  ))
  val addrBits = log2Ceil(entries + 1)
  val mem = Reg(Vec.fill(entries)(data))
  val enq_ptr = RegInit(0.U(Width(addrBits)))
  val deq_ptr = RegInit(0.U(Width(addrBits)))
  val full = RegInit(false.B)
  val empty = (enq_ptr === deq_ptr) && !full
  io.enq.ready := !full
  io.deq.valid := !empty
  io.deq.bits := mem(deq_ptr)
  val enq_fire = io.enq.valid && io.enq.ready
  val deq_fire = io.deq.valid && io.deq.ready
  val almost_full = (enq_ptr + 1.U(Width(addrBits))) % entries.U(Width(addrBits)) === deq_ptr
  when(enq_fire) {
    enq_ptr := (enq_ptr + 1.U(Width(addrBits))) % entries.U(Width(addrBits))
    mem(enq_ptr) := io.enq.bits
  }
  when(deq_fire) {
    deq_ptr := (deq_ptr + 1.U(Width(addrBits))) % entries.U(Width(addrBits))
  }
  when(enq_fire && deq_fire) {
  }.elsewhen(enq_fire && almost_full) {
    full := true.B
  }.elsewhen(deq_fire) {
    full := false.B
  }

case class PrintfTestIO(in: UInt, out: UInt) extends Bundle[PrintfTestIO]

class PrintfTest extends Module:
  given Module = this
  val io = IO(PrintfTestIO(
    Input(UInt(Width(8))),
    Output(UInt(Width(8)))
  ))
  val counter = RegInit(0.U(Width(8)))
  counter := counter + 1.U(Width(8))
  Printf("counter = %d, in = %d\\n", counter, io.in)
  io.out := counter

case class AssertTestIO(a: UInt, b: UInt, out: UInt) extends Bundle[AssertTestIO]

class AssertTest extends Module:
  given Module = this
  val io = IO(AssertTestIO(
    Input(UInt(Width(8))),
    Input(UInt(Width(8))),
    Output(UInt(Width(8)))
  ))
  Assert(io.a =/= io.b, "a and b must be different")
  io.out := io.a + io.b

case class PrintfAssertCombinedIO(in: UInt, enable: Bool, out: UInt) extends Bundle[PrintfAssertCombinedIO]

class PrintfAssertCombined extends Module:
  given Module = this
  val io = IO(PrintfAssertCombinedIO(
    Input(UInt(Width(8))),
    Input(Bool()),
    Output(UInt(Width(8)))
  ))
  val counter = RegInit(0.U(Width(8)))
  when(io.enable) {
    counter := counter + 1.U(Width(8))
    Printf("incrementing counter to %d\\n", counter + 1.U(Width(8)))
  }
  Assert(counter < 100.U(Width(8)), "counter overflow")
  io.out := counter

case class RegFileIO(addr: UInt, data: UInt) extends Bundle[RegFileIO]

class RegFile extends Module:
  given Module = this
  val io = IO(RegFileIO(
    Input(UInt(Width(4))),
    Output(UInt(Width(2)))
  ))
  val regfile = Reg(Vec.fill(16)(UInt(Width(2))))
  io.data := regfile(io.addr)

case class NestedIndexIO(addr: UInt, data: UInt) extends Bundle[NestedIndexIO]

class NestedIndex extends Module:
  given Module = this
  val io = IO(NestedIndexIO(
    Input(UInt(Width(4))),
    Output(UInt(Width(3)))
  ))
  val regfile_1 = Reg(Vec.fill(16)(UInt(Width(2))))
  val regfile_2 = Reg(Vec.fill(4)(UInt(Width(3))))
  io.data := regfile_2(regfile_1(io.addr))

case class ProcessorBundle(in: UInt, out: UInt) extends Bundle[ProcessorBundle]

class Processor(width: Int) extends Module:
  given Module = this
  val io = IO(ProcessorBundle(Input(UInt(Width(width))), Output(UInt(Width(width)))))
  io.out := io.in + 1.U(Width(width))

case class TopModuleIO(in: UInt, out1: UInt, out2: UInt) extends Bundle[TopModuleIO]

class TopModule extends Module:
  given Module = this
  val io = IO(TopModuleIO(
    Input(UInt(Width(8))),
    Output(UInt(Width(8))),
    Output(UInt(Width(8)))
  ))
  val proc0 = Module(new Processor(8))
  val proc1 = Module(new Processor(8))
  proc0.io.in := io.in
  proc1.io.in := io.in
  io.out1 := proc0.io.out
  io.out2 := proc1.io.out

case class OneReadOneWriteIO(
  ren: Bool,
  raddr: UInt,
  rdata: Vec[UInt],
  wen: Bool,
  waddr: UInt,
  wdata: Vec[UInt],
  wmask: Vec[Bool]
) extends Bundle[OneReadOneWriteIO]

object OneReadOneWriteIO:
  def apply(width: Int): OneReadOneWriteIO =
    OneReadOneWriteIO(
      Input(Bool()),
      Input(UInt(Width(3))),
      Output(Vec.fill(4)(UInt(Width(width)))),
      Input(Bool()),
      Input(UInt(Width(3))),
      Input(Vec.fill(4)(UInt(Width(width)))),
      Input(Vec.fill(4)(Bool()))
    )

class OneReadOneWritePortSRAM(width: Int) extends Module:
  given Module = this
  val io = IO(OneReadOneWriteIO(width))
  val mem = SRAM(Vec.fill(4)(UInt(Width(width))), 8)(1, 1, 0, masked = true)
  when(io.wen) {
    mem.writePorts(0).write(io.waddr, io.wdata, io.wmask)
  }
  io.rdata := mem.readPorts(0).read(io.raddr, io.ren)

class SinglePortSRAM(width: Int) extends Module:
  given Module = this
  val io = IO(OneReadOneWriteIO(width))
  val mem = SRAM(Vec.fill(4)(UInt(Width(width))), 8)(1, 1, 0, masked = true)
  when(io.wen) {
    mem.writePorts(0).write(io.waddr, io.wdata, io.wmask)
  }
  io.rdata := mem.readPorts(0).read(io.raddr, !io.wen)

case class AggregateBundle(a: UInt, b: UInt) extends Bundle[AggregateBundle]

object AggregateBundle:
  def apply(): AggregateBundle = AggregateBundle(UInt(Width(2)), UInt(Width(3)))

case class AggregateSRAMIO(
  raddr: UInt,
  rdata: Vec[AggregateBundle],
  wen: Bool,
  waddr: UInt,
  wdata: Vec[AggregateBundle],
  wmask: Vec[Bool]
) extends Bundle[AggregateSRAMIO]

class AggregateSRAM(width: Int) extends Module:
  given Module = this
  val io = IO(AggregateSRAMIO(
    Input(UInt(Width(3))),
    Output(Vec.fill(4)(AggregateBundle())),
    Input(Bool()),
    Input(UInt(Width(3))),
    Input(Vec.fill(4)(AggregateBundle())),
    Input(Vec.fill(4)(Bool()))
  ))
  val mem = SRAM(Vec.fill(4)(AggregateBundle()), 8)(1, 1, 0, masked = true)
  when(io.wen) {
    mem.writePorts(0).write(io.waddr, io.wdata, io.wmask)
  }
  io.rdata := mem.readPorts(0).read(io.raddr, !io.wen)

case class DualReadSingleWritePortSRAMIO(
  raddr_0: UInt,
  raddr_1: UInt,
  rdata_0: Vec[UInt],
  rdata_1: Vec[UInt],
  wen: Bool,
  waddr: UInt,
  wdata: Vec[UInt],
  wmask: Vec[Bool]
) extends Bundle[DualReadSingleWritePortSRAMIO]

class DualReadSingleWritePortSRAM(width: Int) extends Module:
  given Module = this
  val io = IO(DualReadSingleWritePortSRAMIO(
    Input(UInt(Width(3))),
    Input(UInt(Width(3))),
    Output(Vec.fill(4)(UInt(Width(width)))),
    Output(Vec.fill(4)(UInt(Width(width)))),
    Input(Bool()),
    Input(UInt(Width(3))),
    Input(Vec.fill(4)(UInt(Width(width)))),
    Input(Vec.fill(4)(Bool()))
  ))
  val mem = SRAM(Vec.fill(4)(UInt(Width(width))), 8)(2, 1, 0, masked = true)
  when(io.wen) {
    mem.writePorts(0).write(io.waddr, io.wdata, io.wmask)
  }
  io.rdata_0 := mem.readPorts(0).read(io.raddr_0, !io.wen)
  io.rdata_1 := mem.readPorts(1).read(io.raddr_1, !io.wen)

case class OneReadOneReadWritePortSRAMIO(
  raddr_0: UInt,
  raddr_1: UInt,
  rdata_0: Vec[UInt],
  rdata_1: Vec[UInt],
  ren: Bool,
  wen: Bool,
  waddr: UInt,
  wdata: Vec[UInt],
  wmask: Vec[Bool]
) extends Bundle[OneReadOneReadWritePortSRAMIO]

class OneReadOneReadWritePortSRAM(width: Int) extends Module:
  given Module = this
  val io = IO(OneReadOneReadWritePortSRAMIO(
    Input(UInt(Width(3))),
    Input(UInt(Width(3))),
    Output(Vec.fill(4)(UInt(Width(width)))),
    Output(Vec.fill(4)(UInt(Width(width)))),
    Input(Bool()),
    Input(Bool()),
    Input(UInt(Width(3))),
    Input(Vec.fill(4)(UInt(Width(width)))),
    Input(Vec.fill(4)(Bool()))
  ))
  val mem = SRAM(Vec.fill(4)(UInt(Width(width))), 8)(2, 1, 0, masked = true)
  when(io.wen) {
    mem.writePorts(0).write(io.waddr, io.wdata, io.wmask)
  }
  io.rdata_0 := mem.readPorts(0).read(io.raddr_0, !io.wen)
  io.rdata_1 := mem.readPorts(1).read(io.raddr_1, !io.wen && !io.ren)

final case class SwitchIO(sel: UInt, out: UInt) extends Bundle[SwitchIO]
final case class SwitchEnumIO(in: HWEnum[TestEnumOpcode], out: HWEnum[TestEnumOpcode]) extends Bundle[SwitchEnumIO]

class SwitchEnum extends Module:
  given Module = this
  val io = IO(SwitchEnumIO(Input(HWEnum(TestEnumOpcode)), Output(HWEnum(TestEnumOpcode))))
  val reg = RegInit(TestEnumOpcode.Idle.EN)
  reg switch {
    is(TestEnumOpcode.Idle.EN) { reg := TestEnumOpcode.Run.EN }
    is(TestEnumOpcode.Run.EN) { reg := TestEnumOpcode.Wait.EN }
    default { reg := TestEnumOpcode.Idle.EN }
  }
  io.out := reg

case class SIntBasicIO(a: SInt, b: SInt, sum: SInt, diff: SInt, neg: SInt, cmp: Bool) extends Bundle[SIntBasicIO]

class SIntBasic extends Module:
  given Module = this
  val io = IO(SIntBasicIO(
    a = Input(SInt(Width(8))),
    b = Input(SInt(Width(8))),
    sum = Output(SInt(Width(8))),
    diff = Output(SInt(Width(8))),
    neg = Output(SInt(Width(9))),
    cmp = Output(Bool())
  ))
  io.sum := io.a + io.b
  io.diff := io.a - io.b
  io.neg := -io.a
  io.cmp := io.a < io.b

case class SIntConvIO(uIn: UInt, sIn: SInt, uOut: UInt, sOut: SInt) extends Bundle[SIntConvIO]

class SIntConv extends Module:
  given Module = this
  val io = IO(SIntConvIO(
    uIn = Input(UInt(Width(8))),
    sIn = Input(SInt(Width(8))),
    uOut = Output(UInt(Width(8))),
    sOut = Output(SInt(Width(8)))
  ))
  io.uOut := io.sIn.asUInt
  io.sOut := io.uIn.asSInt

case class SIntRegIO(in: SInt, out: SInt) extends Bundle[SIntRegIO]

class SIntReg extends Module:
  given Module = this
  val io = IO(SIntRegIO(
    in = Input(SInt(Width(8))),
    out = Output(SInt(Width(8)))
  ))
  val reg = Reg(SInt(Width(8)))
  reg := io.in
  io.out := reg

case class SIntLitIO(out: SInt) extends Bundle[SIntLitIO]

class SIntLit extends Module:
  given Module = this
  val io = IO(SIntLitIO(out = Output(SInt(Width(8)))))
  io.out := (-42).S(Width(8))

case class SIntMulDivIO(a: SInt, b: SInt, mul: SInt, div: SInt, rem: SInt) extends Bundle[SIntMulDivIO]

class SIntMulDiv extends Module:
  given Module = this
  val io = IO(SIntMulDivIO(
    a = Input(SInt(Width(8))),
    b = Input(SInt(Width(8))),
    mul = Output(SInt(Width(16))),
    div = Output(SInt(Width(9))),
    rem = Output(SInt(Width(8)))
  ))
  io.mul := io.a * io.b
  io.div := io.a / io.b
  io.rem := io.a % io.b

case class SIntShiftIO(a: SInt, amt: UInt, shl: SInt, shr: SInt, shlConst: SInt, shrConst: SInt) extends Bundle[SIntShiftIO]

class SIntShift extends Module:
  given Module = this
  val io = IO(SIntShiftIO(
    a = Input(SInt(Width(8))),
    amt = Input(UInt(Width(3))),
    shl = Output(SInt(Width(16))),
    shr = Output(SInt(Width(8))),
    shlConst = Output(SInt(Width(12))),
    shrConst = Output(SInt(Width(6)))
  ))
  io.shl := io.a << io.amt
  io.shr := io.a >> io.amt
  io.shlConst := io.a << 4
  io.shrConst := io.a >> 2

final case class SwitchTupleIO(
  in_1: UInt,
  in_2: UInt,
  in_3: UInt,
  out_1: UInt,
  out_2: UInt) extends Bundle[SwitchTupleIO]

object SwitchTupleIO:
  def apply(): SwitchTupleIO = SwitchTupleIO(
    Input(UInt(Width(8))),
    Input(UInt(Width(8))),
    Input(UInt(Width(8))),
    Output(UInt(Width(8))),
    Output(UInt(Width(8)))
  )

class SwitchTuple extends Module:
  given Module = this
  val io = IO(SwitchTupleIO())

  switch ((io.in_1, io.in_2, io.in_3)) {
    is((0.U, 0.U, 0.U)) { io.out_1 := 0.U }
    is((0.U, 0.U, 1.U)) { io.out_1 := 1.U }
    is((0.U, 1.U, 0.U)) { io.out_1 := 2.U }
    is((0.U, 1.U, 1.U)) { io.out_1 := 3.U }
    is((1.U, 0.U, 0.U)) { io.out_1 := 4.U }
    is((1.U, 0.U, 1.U)) { io.out_1 := 5.U }
    is((1.U, 1.U, 0.U)) { io.out_1 := 6.U }
    is((1.U, 1.U, 1.U)) { io.out_1 := 7.U }
    default { io.out_1 := 8.U }
  }

  switch (io.in_1 + io.in_2) {
    is(0.U) { io.out_2 := 0.U }
    is(1.U) { io.out_2 := 1.U }
    default { io.out_2 := 2.U }
  }

object ChirrtlEmissionSpec extends TestSuite:
  def writeChirrtl(filename: String, content: String): Unit =
    val dir = new File("test-outputs/chirrtl")
    dir.mkdirs()
    val pw = new PrintWriter(new File(dir, filename))
    pw.write(content)
    pw.close()

  def runFirtool(firFile: String): (Int, String) =
    val cmd = Seq(
      "firtool",
      "--format=fir",
      "--verify-each=true",
      "--split-verilog",
      "-o", "test-outputs/verilog",
      s"test-outputs/chirrtl/$firFile"
    )
    val output = new StringBuilder
    val exitCode = cmd.!(ProcessLogger(s => output.append(s + "\n"), s => output.append(s + "\n")))
    (exitCode, output.toString)

  val tests = Tests {
    test("Adder firtool validation") {
      val elaborator = new Elaborator(log = _ => ())
      val adder = new Adder(2)
      val designs = elaborator.elaborate(adder)
      val chirrtl = elaborator.emitChirrtl(designs, "Adder")
      writeChirrtl("Adder.fir", chirrtl)
      val (exitCode, output) = runFirtool("Adder.fir")
      println("firtool output:")
      println(output)
      if exitCode != 0 then throw new java.lang.AssertionError(s"firtool failed with exit code $exitCode: $output")
    }

    test("GCD firtool validation") {
      val elaborator = new Elaborator(log = _ => ())
      val gcd = new GCD
      val designs = elaborator.elaborate(gcd)
      val chirrtl = elaborator.emitChirrtl(designs, "GCD")
      writeChirrtl("GCD.fir", chirrtl)
      val (exitCode, output) = runFirtool("GCD.fir")
      println("firtool output:")
      println(output)
      if exitCode != 0 then throw new java.lang.AssertionError(s"firtool failed with exit code $exitCode: $output")
    }

    test("BitSel1 CHIRRTL emission") {
      val elaborator = new Elaborator(log = _ => ())
      val mod = new BitSel1
      val designs = elaborator.elaborate(mod)
      val chirrtl = elaborator.emitChirrtl(designs, "BitSel1")
      writeChirrtl("BitSel1.fir", chirrtl)
      val (exitCode, output) = runFirtool("BitSel1.fir")
      if exitCode != 0 then throw new java.lang.AssertionError(s"firtool failed with exit code $exitCode: $output")
    }

    test("BitSel2 CHIRRTL emission") {
      val elaborator = new Elaborator(log = _ => ())
      val mod2 = new BitSel2
      val designs2 = elaborator.elaborate(mod2)
      val chirrtl2 = elaborator.emitChirrtl(designs2, "BitSel2")
      writeChirrtl("BitSel2.fir", chirrtl2)
      val (exitCode, output) = runFirtool("BitSel2.fir")
      if exitCode != 0 then throw new java.lang.AssertionError(s"firtool failed with exit code $exitCode: $output")
    }

    test("Cache CHIRRTL emission") {
      val elaborator = new Elaborator(log = _ => ())
      val cache = new Cache()
      val designs = elaborator.elaborate(cache)
      val chirrtl = elaborator.emitChirrtl(designs, "Cache")
      writeChirrtl("Cache.fir", chirrtl)
      val (exitCode, output) = runFirtool("Cache.fir")
      if exitCode != 0 then throw new java.lang.AssertionError(s"firtool failed with exit code $exitCode: $output")
    }

    test("Const CHIRRTL emission") {
      val elaborator = new Elaborator(log = _ => ())
      val mod = new Const(2)
      val designs = elaborator.elaborate(mod)
      val chirrtl = elaborator.emitChirrtl(designs, "Const")
      writeChirrtl("Const.fir", chirrtl)
      val (exitCode, output) = runFirtool("Const.fir")
      if exitCode != 0 then throw new java.lang.AssertionError(s"firtool failed with exit code $exitCode: $output")
    }

    test("Counter CHIRRTL emission") {
      val elaborator = new Elaborator(log = _ => ())
      val mod = new Counter(2)
      val designs = elaborator.elaborate(mod)
      val chirrtl = elaborator.emitChirrtl(designs, "Counter")
      writeChirrtl("Counter.fir", chirrtl)
      val (exitCode, output) = runFirtool("Counter.fir")
      if exitCode != 0 then throw new java.lang.AssertionError(s"firtool failed with exit code $exitCode: $output")
    }

    test("DecoupledMux CHIRRTL emission") {
      val elaborator = new Elaborator(log = _ => ())
      val mod = new DecoupledMux
      val designs = elaborator.elaborate(mod)
      val chirrtl = elaborator.emitChirrtl(designs, "DecoupledMux")
      writeChirrtl("DecoupledMux.fir", chirrtl)
      val (exitCode, output) = runFirtool("DecoupledMux.fir")
      if exitCode != 0 then throw new java.lang.AssertionError(s"firtool failed with exit code $exitCode: $output")
    }

    test("DynamicIndexing CHIRRTL emission") {
      val elaborator = new Elaborator(log = _ => ())
      val mod = new DynamicIndexing
      val designs = elaborator.elaborate(mod)
      val chirrtl = elaborator.emitChirrtl(designs, "DynamicIndexing")
      writeChirrtl("DynamicIndexing.fir", chirrtl)
      assert(chirrtl.contains("circuit DynamicIndexing :"))
      val (exitCode, output) = runFirtool("DynamicIndexing.fir")
      if exitCode != 0 then throw new java.lang.AssertionError(s"firtool failed with exit code $exitCode: $output")
    }

    test("Fir CHIRRTL emission") {
      val elaborator = new Elaborator(log = _ => ())
      val mod = new Fir(4)
      val designs = elaborator.elaborate(mod)
      val chirrtl = elaborator.emitChirrtl(designs, "Fir")
      writeChirrtl("Fir.fir", chirrtl)
      val (exitCode, output) = runFirtool("Fir.fir")
      if exitCode != 0 then throw new java.lang.AssertionError(s"firtool failed with exit code $exitCode: $output")
    }

    test("Hierarchy CHIRRTL emission") {
      val elaborator = new Elaborator(log = _ => ())
      val mod = new Top
      val designs = elaborator.elaborate(mod)
      val chirrtl = elaborator.emitChirrtl(designs, "Top")
      writeChirrtl("Hierarchy.fir", chirrtl)
      val (exitCode, output) = runFirtool("Hierarchy.fir")
      if exitCode != 0 then throw new java.lang.AssertionError(s"firtool failed with exit code $exitCode: $output")
    }

    test("NestedWhen CHIRRTL emission") {
      val elaborator = new Elaborator(log = _ => ())
      val nested = new NestedWhen
      val designs = elaborator.elaborate(nested)
      val chirrtl = elaborator.emitChirrtl(designs, "NestedWhen")
      writeChirrtl("NestedWhen.fir", chirrtl)
      val (exitCode, output) = runFirtool("NestedWhen.fir")
      if exitCode != 0 then throw new java.lang.AssertionError(s"firtool failed with exit code $exitCode: $output")
    }

    test("NestedWhen auxiliary modules CHIRRTL emission") {
      val elaborator = new Elaborator(log = _ => ())
      val mods = Seq(
        new LCS1,
        new LCS2,
        new LCS3,
        new LCS4,
        new LCS5,
        new LCS6,
        new LCS7,
        new LCS8,
        new LastConnectSemantics2,
        new WireRegInsideWhen,
        new MultiWhen
      )
      mods.foreach { m =>
        val designs = elaborator.elaborate(m)
        val chirrtl = elaborator.emitChirrtl(designs, m.moduleName)
        writeChirrtl(s"${m.moduleName}.fir", chirrtl)
        val (exitCode, output) = runFirtool(s"${m.moduleName}.fir")
        if exitCode != 0 then throw new java.lang.AssertionError(s"firtool failed with exit code $exitCode: $output")
      }
    }

    test("PointerChasing CHIRRTL emission") {
      val elaborator = new Elaborator(log = _ => ())
      val mod = new PointerChasing
      val designs = elaborator.elaborate(mod)
      val chirrtl = elaborator.emitChirrtl(designs, "PointerChasing")
      writeChirrtl("PointerChasing.fir", chirrtl)
      val (exitCode, output) = runFirtool(s"PointerChasing.fir")
      if exitCode != 0 then throw new java.lang.AssertionError(s"firtool failed with exit code $exitCode: $output")
    }

    test("Queue1 CHIRRTL emission") {
      val elaborator = new Elaborator(log = _ => ())
      val mod = new MyQueue(2)
      val designs = elaborator.elaborate(mod)
      val chirrtl = elaborator.emitChirrtl(designs, "MyQueue")
      writeChirrtl("MyQueue.fir", chirrtl)
      val (exitCode, output) = runFirtool(s"MyQueue.fir")
      if exitCode != 0 then throw new java.lang.AssertionError(s"firtool failed with exit code $exitCode: $output")
    }

    test("Queue2 CHIRRTL emission") {
      val elaborator = new Elaborator(log = _ => ())
      val custom = new MyCustomQueue(UInt(Width(3)), 4)
      val designs2 = elaborator.elaborate(custom)
      val chirrtl2 = elaborator.emitChirrtl(designs2, "MyCustomQueue")
      writeChirrtl("MyCustomQueue.fir", chirrtl2)
      val (exitCode, output) = runFirtool(s"MyCustomQueue.fir")
      if exitCode != 0 then throw new java.lang.AssertionError(s"firtool failed with exit code $exitCode: $output")
    }

    test("RegFile CHIRRTL emission") {
      val elaborator = new Elaborator(log = _ => ())
      val mod = new RegFile
      val designs = elaborator.elaborate(mod)
      val chirrtl = elaborator.emitChirrtl(designs, "RegFile")
      writeChirrtl("RegFile.fir", chirrtl)
      val (exitCode, output) = runFirtool(s"RegFile.fir")
      if exitCode != 0 then throw new java.lang.AssertionError(s"firtool failed with exit code $exitCode: $output")
    }

    test("NestedIndex CHIRRTL emission") {
      val elaborator = new Elaborator(log = _ => ())
      val nested = new NestedIndex
      val designs2 = elaborator.elaborate(nested)
      val chirrtl2 = elaborator.emitChirrtl(designs2, "NestedIndex")
      writeChirrtl("NestedIndex.fir", chirrtl2)
      val (exitCode, output) = runFirtool(s"NestedIndex.fir")
      if exitCode != 0 then throw new java.lang.AssertionError(s"firtool failed with exit code $exitCode: $output")
    }

    test("ShiftRegister CHIRRTL emission") {
      val elaborator = new Elaborator(log = _ => ())
      val mod = new TopModule
      val designs = elaborator.elaborate(mod)
      val chirrtl = elaborator.emitChirrtl(designs, "TopModule")
      writeChirrtl("TopModule.fir", chirrtl)
      val (exitCode, output) = runFirtool(s"TopModule.fir")
      if exitCode != 0 then throw new java.lang.AssertionError(s"firtool failed with exit code $exitCode: $output")
    }

    test("SRAM CHIRRTL emission") {
      val elaborator = new Elaborator(log = _ => ())
      val mods = Seq(
        new SinglePortSRAM(2),
        new OneReadOneWritePortSRAM(2),
        new AggregateSRAM(2),
        new DualReadSingleWritePortSRAM(2),
        new OneReadOneReadWritePortSRAM(2)
      )
      mods.foreach { m =>
        val designs = elaborator.elaborate(m)
        val chirrtl = elaborator.emitChirrtl(designs, m.moduleName)
        writeChirrtl(s"${m.moduleName}.fir", chirrtl)
        val (exitCode, output) = runFirtool(s"${m.moduleName}.fir")
        if exitCode != 0 then throw new java.lang.AssertionError(s"firtool failed with exit code $exitCode: $output")
      }
    }

    test("Printf CHIRRTL emission") {
      val elaborator = new Elaborator(log = _ => ())
      val mod = new PrintfTest
      val designs = elaborator.elaborate(mod)
      val chirrtl = elaborator.emitChirrtl(designs, "PrintfTest")
      println("Printf CHIRRTL:")
      println(chirrtl)
      writeChirrtl("PrintfTest.fir", chirrtl)
      assert(chirrtl.contains("printf("))
      val (exitCode, output) = runFirtool("PrintfTest.fir")
      println("firtool output:")
      println(output)
      if exitCode != 0 then throw new java.lang.AssertionError(s"firtool failed with exit code $exitCode: $output")
      val verilogFile = new File("test-outputs/verilog/PrintfTest.sv")
      if !verilogFile.exists() then throw new java.lang.AssertionError("Verilog file should be generated")
      val verilog = scala.io.Source.fromFile(verilogFile).mkString
      if !(verilog.contains("$fwrite") || verilog.contains("fwrite")) then
        throw new java.lang.AssertionError("Verilog should contain $fwrite for printf")
    }

    test("Assert CHIRRTL emission") {
      val elaborator = new Elaborator(log = _ => ())
      val mod = new AssertTest
      val designs = elaborator.elaborate(mod)
      val chirrtl = elaborator.emitChirrtl(designs, "AssertTest")
      println("Assert CHIRRTL:")
      println(chirrtl)
      writeChirrtl("AssertTest.fir", chirrtl)
      assert(chirrtl.contains("assert("))
      val (exitCode, output) = runFirtool("AssertTest.fir")
      println("firtool output:")
      println(output)
      if exitCode != 0 then throw new java.lang.AssertionError(s"firtool failed with exit code $exitCode: $output")
      val verilogFile = new File("test-outputs/verilog/AssertTest.sv")
      if !verilogFile.exists() then throw new java.lang.AssertionError("Verilog file should be generated")
      val verilog = scala.io.Source.fromFile(verilogFile).mkString
      if !(verilog.contains("assert") || verilog.contains("Assert")) then
        throw new java.lang.AssertionError("Verilog should contain assert statement")
    }

    test("Printf and Assert combined CHIRRTL emission") {
      val elaborator = new Elaborator(log = _ => ())
      val mod = new PrintfAssertCombined
      val designs = elaborator.elaborate(mod)
      val chirrtl = elaborator.emitChirrtl(designs, "PrintfAssertCombined")
      println("PrintfAssertCombined CHIRRTL:")
      println(chirrtl)
      writeChirrtl("PrintfAssertCombined.fir", chirrtl)
      assert(chirrtl.contains("printf("))
      assert(chirrtl.contains("assert("))
      val (exitCode, output) = runFirtool("PrintfAssertCombined.fir")
      println("firtool output:")
      println(output)
      if exitCode != 0 then throw new java.lang.AssertionError(s"firtool failed with exit code $exitCode: $output")
      val verilogFile = new File("test-outputs/verilog/PrintfAssertCombined.sv")
      if !verilogFile.exists() then throw new java.lang.AssertionError("Verilog file should be generated")
      val verilog = scala.io.Source.fromFile(verilogFile).mkString
      assert(verilog.contains("assert("))
    }

    test("Switch emission") {
      val elaborator = new Elaborator(log = _ => ())
      val mod = new SwitchEnum
      val designs = elaborator.elaborate(mod)
      val chirrtl = elaborator.emitChirrtl(designs, "SwitchEnum")
      writeChirrtl("SwitchEnum.fir", chirrtl)
      val (exitCode, output) = runFirtool(s"SwitchEnum.fir")
      if exitCode != 0 then throw new java.lang.AssertionError(s"firtool failed with exit code $exitCode: $output")
    }

    test("SwitchTuple emission") {
      val elaborator = new Elaborator(log = _ => ())
      val mod = new SwitchTuple
      val designs = elaborator.elaborate(mod)
      val chirrtl = elaborator.emitChirrtl(designs, "SwitchTuple")
      writeChirrtl("SwitchTuple.fir", chirrtl)
      val (exitCode, output) = runFirtool(s"SwitchTuple.fir")
      if exitCode != 0 then throw new java.lang.AssertionError(s"firtool failed with exit code $exitCode: $output")
    }

    test("SInt basic operations CHIRRTL emission") {
      val elaborator = new Elaborator(log = _ => ())
      val mod = new SIntBasic
      val designs = elaborator.elaborate(mod)
      val chirrtl = elaborator.emitChirrtl(designs, "SIntBasic")
      println("SIntBasic CHIRRTL:")
      println(chirrtl)
      writeChirrtl("SIntBasic.fir", chirrtl)
      assert(chirrtl.contains("SInt<8>"))
      assert(chirrtl.contains("neg("))
      val (exitCode, output) = runFirtool("SIntBasic.fir")
      if exitCode != 0 then throw new java.lang.AssertionError(s"firtool failed with exit code $exitCode: $output")
    }

    test("SInt conversion CHIRRTL emission") {
      val elaborator = new Elaborator(log = _ => ())
      val mod = new SIntConv
      val designs = elaborator.elaborate(mod)
      val chirrtl = elaborator.emitChirrtl(designs, "SIntConv")
      println("SIntConv CHIRRTL:")
      println(chirrtl)
      writeChirrtl("SIntConv.fir", chirrtl)
      assert(chirrtl.contains("asUInt("))
      assert(chirrtl.contains("asSInt("))
      val (exitCode, output) = runFirtool("SIntConv.fir")
      if exitCode != 0 then throw new java.lang.AssertionError(s"firtool failed with exit code $exitCode: $output")
    }

    test("SInt register CHIRRTL emission") {
      val elaborator = new Elaborator(log = _ => ())
      val mod = new SIntReg
      val designs = elaborator.elaborate(mod)
      val chirrtl = elaborator.emitChirrtl(designs, "SIntReg")
      println("SIntReg CHIRRTL:")
      println(chirrtl)
      writeChirrtl("SIntReg.fir", chirrtl)
      assert(chirrtl.contains("reg reg : SInt<8>"))
      val (exitCode, output) = runFirtool("SIntReg.fir")
      if exitCode != 0 then throw new java.lang.AssertionError(s"firtool failed with exit code $exitCode: $output")
    }

    test("SInt literal CHIRRTL emission") {
      val elaborator = new Elaborator(log = _ => ())
      val mod = new SIntLit
      val designs = elaborator.elaborate(mod)
      val chirrtl = elaborator.emitChirrtl(designs, "SIntLit")
      println("SIntLit CHIRRTL:")
      println(chirrtl)
      writeChirrtl("SIntLit.fir", chirrtl)
      assert(chirrtl.contains("SInt<8>(-42)"))
      val (exitCode, output) = runFirtool("SIntLit.fir")
      if exitCode != 0 then throw new java.lang.AssertionError(s"firtool failed with exit code $exitCode: $output")
    }

    test("SInt mul/div CHIRRTL emission") {
      val elaborator = new Elaborator(log = _ => ())
      val mod = new SIntMulDiv
      val designs = elaborator.elaborate(mod)
      val chirrtl = elaborator.emitChirrtl(designs, "SIntMulDiv")
      println("SIntMulDiv CHIRRTL:")
      println(chirrtl)
      writeChirrtl("SIntMulDiv.fir", chirrtl)
      assert(chirrtl.contains("mul("))
      assert(chirrtl.contains("div("))
      assert(chirrtl.contains("rem("))
      val (exitCode, output) = runFirtool("SIntMulDiv.fir")
      if exitCode != 0 then throw new java.lang.AssertionError(s"firtool failed with exit code $exitCode: $output")
    }

    test("SInt shift CHIRRTL emission") {
      val elaborator = new Elaborator(log = _ => ())
      val mod = new SIntShift
      val designs = elaborator.elaborate(mod)
      val chirrtl = elaborator.emitChirrtl(designs, "SIntShift")
      println("SIntShift CHIRRTL:")
      println(chirrtl)
      writeChirrtl("SIntShift.fir", chirrtl)
      assert(chirrtl.contains("dshl("))
      assert(chirrtl.contains("dshr("))
      assert(chirrtl.contains("shl("))
      assert(chirrtl.contains("shr("))
      val (exitCode, output) = runFirtool("SIntShift.fir")
      if exitCode != 0 then throw new java.lang.AssertionError(s"firtool failed with exit code $exitCode: $output")
    }
  }
