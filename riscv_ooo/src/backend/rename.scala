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
  }

abstract class BitMaskModule extends Module:
  given Module = this

  class BitMask(n: Int, init: BigInt):
    val initLit = Lit(UInt(n.W))(init)
    val data = RegInit(initLit)

    def clear: Unit =
      data := initLit

    def unset(x: UInt): Unit =
      data(x) := 0.U

    def set(x: UInt): Unit =
      data(x) := 1.U

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
            ret(found(i)) := i.U
            found(i) := 1.U
          }
        else
          when (y(i).asBool && found(i-1) < cnt.U) {
            ret(found(i)) := i.U
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
      comm_prds  = Input(Vec.fill(p.coreWidth)(Valid(UInt(p.pRegIdxBits.W))))
    ))

  body {
    val entries = p.nPhysicalRegs
    require(entries > 32)

    val free_list = new BitMask(entries, (BigInt(1) << entries) - 1)

    io.count := free_list.count

    io.alloc_resp := DontCare
    when (io.alloc_req.valid) {
      io.alloc_resp := free_list.getSetIds(p.coreWidth)
      for (i <- 0 until p.coreWidth) {
        when (i.U < io.alloc_req.bits) {
          free_list.unset(io.alloc_resp(i))
        }
      }
      Assert(io.count >= io.alloc_req.bits, "Not enough entries in the free list")
    }

    for (i <- 0 until p.coreWidth) {
      when (io.comm_prds(i).valid) {
        free_list.set(io.comm_prds(i).bits)
      }
    }
  }

case class BusyTableRN1Req(
  prs1: UInt,
  prs2: UInt,
) extends Bundle[BusyTableRN1Req]

case class BusyTableRN1Resp(
  prs1_busy: Bool,
  prs2_busy: Bool,
) extends Bundle[BusyTableRN1Resp]

case class BusyTableWBReq(
  prd: UInt
) extends Bundle[BusyTableWBReq]


case class BusyTableIO(
  rn1_req: Vec[BusyTableRN1Req],
  rn1_resp: Vec[BusyTableRN1Resp],

  // Unset busy bit for prd that finished computing
  wb_req: Vec[Valid[BusyTableWBReq]],

  // Set busy bit for retired stale_rd
  comm_prds: Vec[Valid[UInt]]
) extends Bundle[BusyTableIO]

class BusyTable(p: CoreParams) extends BitMaskModule with CoreCacheable(p):
  val io = IO(BusyTableIO(
    rn1_req = Input(Vec.fill(p.coreWidth)(BusyTableRN1Req(
      prs1 = UInt(p.pRegIdxBits.W),
      prs2 = UInt(p.pRegIdxBits.W),
    ))),
    rn1_resp = Output(Vec.fill(p.coreWidth)(BusyTableRN1Resp(
      prs1_busy = Bool(),
      prs2_busy = Bool(),
    ))),

    wb_req = Input(Vec.fill(p.coreWidth)(Valid(BusyTableWBReq(
      prd = UInt(p.pRegIdxBits.W)
    )))),
    comm_prds = Input(Vec.fill(p.coreWidth)(Valid(UInt(p.pRegIdxBits.W)))),
  ))

  body {
    val entries = p.nPhysicalRegs
    val busy_table = new BitMask(entries, (BigInt(1) << entries) - 1)

    for (i <- 0 until p.coreWidth) {
      val req = io.rn1_req(i)

      val prs1_wb_match = io.wb_req.map(wb => wb.valid && wb.bits.prd === req.prs1).reduce(_ || _)
      io.rn1_resp(i).prs1_busy := Mux(prs1_wb_match,
        false.B,
        busy_table.get(req.prs1).asBool)

      val prs2_wb_match = io.wb_req.map(wb => wb.valid && wb.bits.prd === req.prs2).reduce(_ || _)
      io.rn1_resp(i).prs2_busy := Mux(prs2_wb_match,
        false.B,
        busy_table.get(req.prs2).asBool)
    }
    for (i <- 0 until p.coreWidth) {
      val wb = io.wb_req(i)
      when (wb.valid) {
        busy_table.unset(wb.bits.prd)
      }
    }
    for (i <- 0 until p.coreWidth) {
      val comm = io.comm_prds(i)
      when (comm.valid) {
        busy_table.set(comm.bits)
      }
    }
  }

case class RenamerIO(
  dec_uops: Vec[Valid[UOp]],
  dec_ready: Bool,

  rn2_uops: Vec[Valid[UOp]],

  wb_done_phys: Vec[Valid[BusyTableWBReq]],

  comm_free_phys: Vec[Valid[UInt]]
) extends Bundle[RenamerIO]

class Renamer(p: CoreParams) extends Module with CoreCacheable(p):
  given Module = this

  val io = IO(RenamerIO(
    dec_uops  = Input(Vec.fill(p.coreWidth)(Valid(UOp(p)))),
    dec_ready = Output(Bool()),

    rn2_uops = Output(Vec.fill(p.coreWidth)(Valid(UOp(p)))),

    wb_done_phys = Input(Vec.fill(p.coreWidth)(Valid(BusyTableWBReq(
      prd = UInt(p.pRegIdxBits.W)
    )))),

    comm_free_phys = Input(Vec.fill(p.coreWidth)(Valid(UInt(p.pRegIdxBits.W)))),
  ))

  body {
    val free_list  = Module(new FreeList(p))
    val map_table  = Module(new MapTable(p))
    val busy_table = Module(new BusyTable(p))

    val rn1_uops = Reg(Vec.fill(p.coreWidth)(Valid(UOp(p))))

    io.dec_ready := free_list.io.count >=
      io.dec_uops.map(_.valid.asUInt).reduce(_ +& _) +
         rn1_uops.map(_.valid.asUInt).reduce(_ +& _)

    // ------------------------------------------------------------------------
    // Rename 0
    // - Lookup map table
    // ------------------------------------------------------------------------
    for (i <- 0 until p.coreWidth) {
      map_table.io.dec_req(i).lrs1 := io.dec_uops(i).bits.lrs1
      map_table.io.dec_req(i).lrs2 := io.dec_uops(i).bits.lrs2
      map_table.io.dec_req(i).lrd  := io.dec_uops(i).bits.lrd
    }


    when (io.dec_ready && io.dec_uops.map(_.valid).reduce(_ || _)) {
      for (i <- 0 until p.coreWidth) {
        rn1_uops(i) := io.dec_uops(i)
        rn1_uops(i).bits.stale_prd := map_table.io.dec_resp(i).stale_prd
        rn1_uops(i).bits.prs1      := map_table.io.dec_resp(i).prs1
        rn1_uops(i).bits.prs2      := map_table.io.dec_resp(i).prs2
      }
    }

    // ------------------------------------------------------------------------
    // Rename 1
    // - Get prd from freelist
    // - bypass prd & update younger uop's stale_prd & prs1, prs2
    // - update renaming table & busy bit table
    // ------------------------------------------------------------------------
    val rn1_valids = rn1_uops.map(_.valid)
    free_list.io.alloc_req.bits := rn1_valids.map(_.asUInt).reduce(_ +& _)
    free_list.io.alloc_req.valid := rn1_valids.reduce(_ || _)

    val rn1_uops_bypass = Wire(Vec.fill(p.coreWidth)(Valid(UOp(p))))
    rn1_uops_bypass.zip(rn1_uops).foreach(_ := _)

    for (i <- 0 until p.coreWidth) {
      when (rn1_uops_bypass(i).valid) {
        rn1_uops_bypass(i).bits.prd := free_list.io.alloc_resp(i)
      }
    }

    for (i <- 1 until p.coreWidth) {
      val prev = rn1_uops_bypass.take(i).reverse
      val found_rs1 = Wire(Vec.fill(i)(Bool()))
      val found_rs2 = Wire(Vec.fill(i)(Bool()))
      val found_rd  = Wire(Vec.fill(i)(Bool()))
      for (j <- 0 until i) {
        val rs1_dep = prev(j).valid && (prev(j).bits.lrd === rn1_uops_bypass(i).bits.lrs1)
        if j == 0 then
          when (rs1_dep) {
            found_rs1(j) := true.B
            rn1_uops_bypass(i).bits.prs1 := prev(j).bits.prd
          } .otherwise {
            found_rs1(j) := false.B
          }
        else
          when (!found_rs1(j-1) && rs1_dep) {
            rn1_uops_bypass(i).bits.prs1 := prev(j).bits.prd
            found_rs1(j) := true.B
          } .otherwise {
            found_rs1(j) := found_rs1(j-1)
          }

        val rs2_dep = prev(j).valid && (prev(j).bits.lrd === rn1_uops_bypass(i).bits.lrs2)
        if j == 0 then
          when (rs2_dep) {
            found_rs2(j) := true.B
            rn1_uops_bypass(i).bits.prs2 := prev(j).bits.prd
          } .otherwise {
            found_rs2(j) := false.B
          }
        else
          when (!found_rs2(j-1) && rs2_dep) {
            rn1_uops_bypass(i).bits.prs2 := prev(j).bits.prd
            found_rs2(j) := true.B
          } .otherwise {
            found_rs2(j) := found_rs2(j-1)
          }

        val rd_match = prev(j).valid && (prev(j).bits.lrd === rn1_uops_bypass(i).bits.lrd)
        if j == 0 then
          when (rd_match) {
            found_rd(j) := true.B
            rn1_uops_bypass(i).bits.stale_prd := prev(j).bits.prd
          } .otherwise {
            found_rd(j) := false.B
          }
        else
          when (!found_rd(j-1) && rd_match) {
            rn1_uops_bypass(i).bits.stale_prd := prev(j).bits.prd
            found_rd(j) := true.B
          } .otherwise {
            found_rd(j) := found_rd(j-1)
          }
      }
    }

    for (i <- 0 until p.coreWidth) {
      map_table.io.rn1_update(i).valid := rn1_uops_bypass(i).valid
      map_table.io.rn1_update(i).bits.lrd := rn1_uops_bypass(i).bits.lrd
      map_table.io.rn1_update(i).bits.prd := rn1_uops_bypass(i).bits.prd
    }

    for (i <- 0 until p.coreWidth) {
      busy_table.io.rn1_req(i).prs1 := rn1_uops_bypass(i).bits.prs1
      busy_table.io.rn1_req(i).prs2 := rn1_uops_bypass(i).bits.prs2

      rn1_uops_bypass(i).bits.prs1_busy := busy_table.io.rn1_resp(i).prs1_busy
      rn1_uops_bypass(i).bits.prs2_busy := busy_table.io.rn1_resp(i).prs2_busy
    }

    io.rn2_uops <> rn1_uops_bypass


    busy_table.io.wb_req <> io.wb_done_phys


    busy_table.io.comm_prds <> io.comm_free_phys
    free_list.io.comm_prds  <> io.comm_free_phys

    dontTouch(io.dec_uops)
    dontTouch(io.dec_ready)
    dontTouch(io.rn2_uops)
  }
