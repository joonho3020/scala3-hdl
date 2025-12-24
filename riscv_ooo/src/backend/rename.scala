package riscv_ooo

import hdl._

case class RenameReq(
  lrs1: UInt,
  lrs2: UInt,
  lrd: UInt
) extends Bundle[RenameReq]

object RenameReq:
  def apply(p: CoreParams): RenameReq =
    RenameReq(
      lrs1 = UInt(5.W),
      lrs2 = UInt(5.W),
      lrd = UInt(5.W)
    )

case class RenameResp(
  prs1: UInt,
  prs2: UInt,
  stale_prd: UInt
) extends Bundle[RenameResp]

object RenameResp:
  def apply(p: CoreParams): RenameResp =
    RenameResp(
      prs1 = UInt(p.pRegIdxBits.W),
      prs2 = UInt(p.pRegIdxBits.W),
      stale_prd = UInt(p.pRegIdxBits.W)
    )

case class RenameWrite(
  lrd: UInt,
  prd: UInt
) extends Bundle[RenameWrite]

object RenameWrite:
  def apply(p: CoreParams): RenameWrite =
    RenameWrite(
      lrd = UInt(5.W),
      prd = UInt(p.pRegIdxBits.W)
    )

case class MapTableIO(
  dec_req: Vec[RenameReq],
  dec_resp: Vec[RenameResp],
  rn1_update: Vec[Valid[RenameWrite]]
) extends Bundle[MapTableIO]

class MapTable(p: CoreParams) extends Module with CoreCacheable(p):
  given Module = this
  val io = IO(MapTableIO(
    dec_req    = Input (Vec.fill(p.coreWidth)(RenameReq (p))),
    dec_resp   = Output(Vec.fill(p.coreWidth)(RenameResp(p))),
    rn1_update = Input (Vec.fill(p.coreWidth)(Valid(RenameWrite(p))))
  ))

  body {
    val table = Reg(Vec.fill(32)(UInt(p.xlenBits.W)))

    for (i <- 0 until p.coreWidth) {
      io.dec_resp(i).prs1 := table(io.dec_req(i).lrs1)
      io.dec_resp(i).prs2 := table(io.dec_req(i).lrs2)
      io.dec_resp(i).stale_prd := table(io.dec_req(i).lrd)
    }

    for (i <- 0 until p.coreWidth) {
      when (io.rn1_update(i).valid) {
        table(io.rn1_update(i).bits.lrd) := io.rn1_update(i).bits.prd
      }
    }

    when (reset.asBool) {
      for (i <- 0 until 32) {
        table(i) := i.U
      }
    }
    dontTouch(io)
  }

abstract class BitMaskModule extends Module:
  given Module = this

  class BitMask(n: Int, init: BigInt):
    val initLit = Lit(UInt(n.W))(init)
    val data = RegInit(initLit)

    def clear: Unit =
      data := initLit

    def unset(indices: Vec[UInt], valid: Vec[Bool]): Unit =
      val mask = valid.zip(indices).map((v, idx) => {
        Mux(v, 1.U << idx, 0.U)
      }).reduce(_ | _)
      data := data & ~mask

    def set(indices: Vec[UInt], valid: Vec[Bool]): Unit =
      val mask = valid.zip(indices).map((v, idx) => {
        Mux(v, 1.U << idx, 0.U)
      }).reduce(_ | _)
      data := data | mask

    def count: UInt =
      PopCount(data)

    def get(x: UInt): UInt =
      data(x)

    def getSetIds(cnt: Int): Vec[UInt] =
      val y = Wire(UInt(n.W))
      y := data

      val ret = Wire(Vec.fill(cnt)(UInt(log2Ceil(n+1).W)))
      ret.foreach(_ := DontCare)

      val found = Wire(Vec.fill(n)(UInt(log2Ceil(n+1).W)))
      found(0) := 0.U

      for (i <- 0 until n) {
        if i == 0 then
          when (y(i).asBool) {
            ret(0) := 0.U
            found(i) := 1.U
          }
        else
          when (y(i).asBool && found(i-1) < cnt.U) {
            ret(found(i-1)) := i.U
            found(i) := found(i-1) + 1.U
          } .otherwise {
            found(i) := found(i-1)
          }
      }
      ret

case class FreeListIO(
  alloc_req: Valid[UInt],
  alloc_resp: Vec[UInt],
  count: UInt,
  comm_prds: Vec[Valid[UInt]]
) extends Bundle[FreeListIO]

// Assumes allocation requests happen only when there is enough entries
class FreeList(p: CoreParams) extends BitMaskModule with CoreCacheable(p):
  val io = IO(FreeListIO(
      alloc_req  = Input(Valid(UInt(p.coreWidthBits.W))),
      alloc_resp = Output(Vec.fill(p.coreWidth)(UInt(p.pRegIdxBits.W))),
      count      = Output(UInt(p.pRegIdxBits.W)),
      comm_prds  = Input(Vec.fill(p.retireWidth)(Valid(UInt(p.pRegIdxBits.W))))
    ))

  body {
    val entries = p.nPhysicalRegs
    require(entries > 32)

    // Initial 32 registers should be taken
    val init = ((BigInt(1) << (entries - 32)) - 1) << 32
    val free_list = new BitMask(entries, init)

    io.count := free_list.count

    io.alloc_resp := DontCare
    when (io.alloc_req.valid) {
      io.alloc_resp := free_list.getSetIds(p.coreWidth)
      free_list.unset(io.alloc_resp, Vec((0 until p.coreWidth).map(i => i.U < io.alloc_req.bits)))
      Assert(io.count >= io.alloc_req.bits, "Not enough entries in the free list")
    }


    when (io.comm_prds.map(_.valid).reduce(_ || _)) {
      free_list.set(io.comm_prds.map(_.bits), io.comm_prds.map(_.valid))
    }
    dontTouch(io)
  }

case class BusyTableRN1Req(
  prs1: Valid[UInt],
  prs2: Valid[UInt],
) extends Bundle[BusyTableRN1Req]

case class BusyTableRN1Resp(
  prs1_busy: Bool,
  prs2_busy: Bool,
) extends Bundle[BusyTableRN1Resp]

case class BusyTableWBReq(
  prd: Valid[UInt]
) extends Bundle[BusyTableWBReq]


case class BusyTableIO(
  rn1_req: Vec[BusyTableRN1Req],
  rn1_resp: Vec[BusyTableRN1Resp],

  // Unset busy bit for prd that finished computing
  wb_req: Vec[BusyTableWBReq],

  // Set busy bit for retired stale_rd
  comm_prds: Vec[Valid[UInt]]
) extends Bundle[BusyTableIO]

class BusyTable(p: CoreParams) extends BitMaskModule with CoreCacheable(p):
  val io = IO(BusyTableIO(
    rn1_req = Input(Vec.fill(p.coreWidth)(BusyTableRN1Req(
      prs1 = Valid(UInt(p.pRegIdxBits.W)),
      prs2 = Valid(UInt(p.pRegIdxBits.W)),
    ))),
    rn1_resp = Output(Vec.fill(p.coreWidth)(BusyTableRN1Resp(
      prs1_busy = Bool(),
      prs2_busy = Bool(),
    ))),

    wb_req = Input(Vec.fill(p.coreWidth)(BusyTableWBReq(
      prd = Valid(UInt(p.pRegIdxBits.W))
    ))),
    comm_prds = Input(Vec.fill(p.retireWidth)(Valid(UInt(p.pRegIdxBits.W)))),
  ))

  body {
    val entries = p.nPhysicalRegs

    // Initial 32 registers should be taken
    val init = ((BigInt(1) << (entries - 32)) - 1) << 32
    val busy_table = new BitMask(entries, init)

    for (i <- 0 until p.coreWidth) {
      val req = io.rn1_req(i)

      val prs1_wb_match = io.wb_req.map(wb => wb.prd.valid && wb.prd.bits === req.prs1.bits).reduce(_ || _)
      io.rn1_resp(i).prs1_busy := Mux(prs1_wb_match,
        false.B,
        busy_table.get(req.prs1.bits).asBool)

      val prs2_wb_match = io.wb_req.map(wb => wb.prd.valid && wb.prd.bits === req.prs2.bits).reduce(_ || _)
      io.rn1_resp(i).prs2_busy := Mux(prs2_wb_match,
        false.B,
        busy_table.get(req.prs2.bits).asBool)
    }

    when (io.wb_req.map(_.prd.valid).reduce(_ || _)) {
      busy_table.unset(io.wb_req.map(_.prd.bits), io.wb_req.map(_.prd.valid))
    }
    when (io.comm_prds.map(_.valid).reduce(_ || _)) {
      busy_table.set(io.comm_prds.map(_.bits), io.comm_prds.map(_.valid))
    }
    dontTouch(io)
  }

case class RenamerIO(
  dec_uops: Vec[Valid[UOp]],
  dec_ready: Bool,

  rn2_uops: Vec[Valid[UOp]],

  wb_wakeup: Vec[BusyTableWBReq],

  comm_free_phys: Vec[Valid[UInt]]
) extends Bundle[RenamerIO]

class Renamer(p: CoreParams) extends Module with CoreCacheable(p):
  given Module = this

  val io = IO(RenamerIO(
    dec_uops  = Input(Vec.fill(p.coreWidth)(Valid(UOp(p)))),
    dec_ready = Output(Bool()),

    rn2_uops = Output(Vec.fill(p.coreWidth)(Valid(UOp(p)))),

    wb_wakeup = Input(Vec.fill(p.coreWidth)(BusyTableWBReq(
      prd = Valid(UInt(p.pRegIdxBits.W))
    ))),

    comm_free_phys = Input(Vec.fill(p.retireWidth)(Valid(UInt(p.pRegIdxBits.W)))),
  ))

  body {
    val free_list  = Module(new FreeList(p))
    val map_table  = Module(new MapTable(p))
    val busy_table = Module(new BusyTable(p))

    val rn1_uops_reg = Reg(Vec.fill(p.coreWidth)(Valid(UOp(p))))
    dontTouch(rn1_uops_reg)

    val dec_alloc_reqs = io.dec_uops .map(u => (u.valid && u.bits.lrd_val).asUInt).reduce(_ +& _)
    val rn1_alloc_reqs = rn1_uops_reg.map(u => (u.valid && u.bits.lrd_val).asUInt).reduce(_ +& _)
    io.dec_ready := free_list.io.count > (dec_alloc_reqs +& rn1_alloc_reqs)

    // ------------------------------------------------------------------------
    // Rename 0
    // - Lookup map table
    // ------------------------------------------------------------------------
    for (i <- 0 until p.coreWidth) {
      map_table.io.dec_req(i).lrs1 := io.dec_uops(i).bits.lrs1
      map_table.io.dec_req(i).lrs2 := io.dec_uops(i).bits.lrs2
      map_table.io.dec_req(i).lrd  := io.dec_uops(i).bits.lrd
    }

    for (i <- 0 until p.coreWidth) {
      val dec = io.dec_uops(i).bits
      rn1_uops_reg(i) := io.dec_uops(i)
      rn1_uops_reg(i).valid := io.dec_uops(i).valid && io.dec_ready
      rn1_uops_reg(i).bits.stale_prd := map_table.io.dec_resp(i).stale_prd
      rn1_uops_reg(i).bits.prs1      := map_table.io.dec_resp(i).prs1
      rn1_uops_reg(i).bits.prs2      := map_table.io.dec_resp(i).prs2
      rn1_uops_reg(i).bits.prd       := 0.U
    }

    // ------------------------------------------------------------------------
    // Rename 1
    // - Get prd from freelist
    // - update renaming table & busy bit table
    // - bypass prd & update younger uop's stale_prd & prs1, prs2
    // ------------------------------------------------------------------------
    val rn1_rd_wens = rn1_uops_reg.map(u => u.valid && u.bits.lrd_val)

    free_list.io.alloc_req.bits  := rn1_rd_wens.map(_.asUInt).reduce(_ +& _)
    free_list.io.alloc_req.valid := rn1_rd_wens.reduce(_ || _)

    val alloc_offsets = Wire(Vec.fill(p.coreWidth)(UInt(log2Ceil(p.coreWidth + 1).W)))
    alloc_offsets(0) := 0.U
    for (i <- 1 until p.coreWidth) {
      alloc_offsets(i) := alloc_offsets(i - 1) + rn1_rd_wens(i - 1).asUInt
    }

    val rn1_uops = Wire(Vec.fill(p.coreWidth)(Valid(UOp(p))))
    rn1_uops.zip(rn1_uops_reg).foreach(_ := _)
    dontTouch(rn1_uops)

    for (i <- 0 until p.coreWidth) {
      when (rn1_rd_wens(i)) {
        rn1_uops(i).bits.prd := free_list.io.alloc_resp(alloc_offsets(i))
      }
    }

    for (i <- 0 until p.coreWidth) {
      map_table.io.rn1_update(i).valid := rn1_rd_wens(i)
      map_table.io.rn1_update(i).bits.lrd := rn1_uops(i).bits.lrd
      map_table.io.rn1_update(i).bits.prd := rn1_uops(i).bits.prd
    }

    val rn1_use_rs1 = rn1_uops_reg.map(u => u.valid && u.bits.lrs1_val)
    val rn1_use_rs2 = rn1_uops_reg.map(u => u.valid && u.bits.lrs2_val)

    for (i <- 0 until p.coreWidth) {
      busy_table.io.rn1_req(i).prs1.valid := rn1_use_rs1(i)
      busy_table.io.rn1_req(i).prs1.bits  := rn1_uops(i).bits.prs1

      busy_table.io.rn1_req(i).prs2.valid := rn1_use_rs2(i)
      busy_table.io.rn1_req(i).prs2.bits  := rn1_uops(i).bits.prs2

      rn1_uops(i).bits.prs1_busy := busy_table.io.rn1_resp(i).prs1_busy
      rn1_uops(i).bits.prs2_busy := busy_table.io.rn1_resp(i).prs2_busy
    }

    val rn1_uops_bypass = Wire(Vec.fill(p.coreWidth)(Valid(UOp(p))))
    dontTouch(rn1_uops_bypass)

    def lrd_dep(me: UOp, you: UOp): Bool =
      me.lrd === you.lrd && me.lrd_val && you.lrd_val

    def lrs1_dep(me: UOp, you: UOp): Bool =
      me.lrs1 === you.lrd && me.lrs1_val && you.lrd_val

    def lrs2_dep(me: UOp, you: UOp): Bool =
      me.lrs2 === you.lrd && me.lrs2_val && you.lrd_val

    rn1_uops_bypass(0) := rn1_uops(0)

    // Bypass prd to younger uops
    for (i <- 1 until p.coreWidth) {
      val prev_uops = rn1_uops.take(i)
      val bypass    = rn1_uops_bypass(i)
      bypass := rn1_uops(i)

      val lrd_deps  = prev_uops.map(p => p.valid && bypass.valid &&  lrd_dep(p.bits, bypass.bits))
      val lrs1_deps = prev_uops.map(p => p.valid && bypass.valid && lrs1_dep(p.bits, bypass.bits))
      val lrs2_deps = prev_uops.map(p => p.valid && bypass.valid && lrs2_dep(p.bits, bypass.bits))

      val lrd_dep_oh  = PriorityEncoderOH(Cat(lrd_deps .reverse))
      val lrs1_dep_oh = PriorityEncoderOH(Cat(lrs1_deps.reverse))
      val lrs2_dep_oh = PriorityEncoderOH(Cat(lrs2_deps.reverse))

      when (lrd_deps.reduce(_ || _)) {
        bypass.bits.stale_prd := MuxOneHot(lrd_dep_oh, prev_uops.map(_.bits.prd))
      }
      when (lrs1_deps.reduce(_ || _)) {
        bypass.bits.prs1 := MuxOneHot(lrs1_dep_oh, prev_uops.map(_.bits.prd))
        bypass.bits.prs1_busy := true.B
      }
      when (lrs2_deps.reduce(_ || _)) {
        bypass.bits.prs2 := MuxOneHot(lrs2_dep_oh, prev_uops.map(_.bits.prd))
        bypass.bits.prs2_busy := true.B
      }
    }

    io.rn2_uops <> rn1_uops_bypass

    busy_table.io.wb_req <> io.wb_wakeup

    busy_table.io.comm_prds <> io.comm_free_phys
    free_list.io.comm_prds  <> io.comm_free_phys

    dontTouch(io.dec_uops)
    dontTouch(io.dec_ready)
    dontTouch(io.rn2_uops)

    val debug_bypass_rs1_val = Wire(Vec.fill(p.coreWidth)(Bool()))
    val debug_bypass_rs2_val = Wire(Vec.fill(p.coreWidth)(Bool()))
    val debug_bypass_rd_val  = Wire(Vec.fill(p.coreWidth)(Bool()))

    debug_bypass_rs1_val.zip(rn1_uops_bypass).foreach((d, u) => d := u.valid && u.bits.lrs1_val)
    debug_bypass_rs2_val.zip(rn1_uops_bypass).foreach((d, u) => d := u.valid && u.bits.lrs2_val)
    debug_bypass_rd_val .zip(rn1_uops_bypass).foreach((d, u) => d := u.valid && u.bits.lrd_val)

    dontTouch(debug_bypass_rs1_val)
    dontTouch(debug_bypass_rs2_val)
    dontTouch(debug_bypass_rd_val)

  }
