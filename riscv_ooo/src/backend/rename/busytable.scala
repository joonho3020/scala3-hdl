package riscv_ooo

import hdl.core._
import hdl.util._
import hdl.elaboration._


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
  wb_req: Vec[BusyTableWBReq],
  rn1_set: Vec[Valid[UInt]]
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
    rn1_set = Input(Vec.fill(p.coreWidth)(Valid(UInt(p.pRegIdxBits.W)))),
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

    var mask = busy_table.data
    mask = busy_table.unset(mask, io.wb_req.map(_.prd.bits), io.wb_req.map(_.prd.valid))
    mask = busy_table  .set(mask, io.rn1_set.map(_.bits), io.rn1_set.map(_.valid))
    busy_table.data := mask

    val busy_count = PopCount(busy_table.data).asWire
    dontTouch(busy_count)

    dontTouch(io)
  }

