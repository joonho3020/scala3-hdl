package riscv_ooo

import hdl.core._
import hdl.util._
import hdl.elaboration._


case class RenamerIO(
  dec_uops: Vec[Valid[UOp]],
  free_count: UInt,

  dis_stall: Bool,

  rn2_uops: Vec[Valid[UOp]],

  wb_wakeup: Vec[BusyTableWBReq],

  comm_free_phys: Vec[Valid[UInt]],
  br_resolve: BranchResolve
) extends Bundle[RenamerIO]

class Renamer(p: CoreParams) extends Module with CoreCacheable(p):
  given Module = this

  val io = IO(RenamerIO(
    dec_uops  = Input(Vec.fill(p.coreWidth)(Valid(UOp(p)))),
    free_count = Output(UInt(p.pRegIdxBits.W)),

    dis_stall = Input(Bool()),

    rn2_uops = Output(Vec.fill(p.coreWidth)(Valid(UOp(p)))),

    wb_wakeup = Input(Vec.fill(p.coreWidth)(BusyTableWBReq(
      prd = Valid(UInt(p.pRegIdxBits.W))
    ))),

    comm_free_phys = Input(Vec.fill(p.retireWidth)(Valid(UInt(p.pRegIdxBits.W)))),
    br_resolve = Input(BranchResolve(p)),
  ))

  body {
    val free_list  = Module(new FreeList(p))
    val map_table  = Module(new MapTable(p))
    val busy_table = Module(new BusyTable(p))

    val rn1_uops_reg = Reg(Vec.fill(p.coreWidth)(Valid(UOp(p))))
    dontTouch(rn1_uops_reg)

    // TODO: too pessimistic???
    val fc = free_list.io.count
    val uc = rn1_uops_reg.map(u => (u.valid && u.bits.lrd_val).asUInt).reduce(_ + _)
    io.free_count := Mux(fc >= (uc + p.coreWidth.U), fc - uc - p.coreWidth.U, 0.U)

    // ------------------------------------------------------------------------
    // Rename 0
    // - Lookup map table
    // ------------------------------------------------------------------------
    for (i <- 0 until p.coreWidth) {
      map_table.io.dec_req(i).lrs1 := io.dec_uops(i).bits.lrs1
      map_table.io.dec_req(i).lrs2 := io.dec_uops(i).bits.lrs2
      map_table.io.dec_req(i).lrd  := io.dec_uops(i).bits.lrd
    }

    val kill_rename = (io.br_resolve.valid && io.br_resolve.mispredict).asWire
    dontTouch(kill_rename)

    val rn1_fire = !io.dis_stall && !kill_rename
    for (i <- 0 until p.coreWidth) {
      val dec = io.dec_uops(i).bits
      when (!io.dis_stall && !kill_rename) {
        rn1_uops_reg(i) := io.dec_uops(i)
        rn1_uops_reg(i).bits.stale_prd := map_table.io.dec_resp(i).stale_prd
        rn1_uops_reg(i).bits.prs1      := map_table.io.dec_resp(i).prs1
        rn1_uops_reg(i).bits.prs2      := map_table.io.dec_resp(i).prs2
        rn1_uops_reg(i).bits.prd       := 0.U
      }
      when (kill_rename) {
        rn1_uops_reg(i).valid := false.B
      }
    }

    // ------------------------------------------------------------------------
    // Rename 1
    // - Get prd from freelist
    // - update renaming table & busy bit table
    // - bypass prd & update younger uop's stale_prd & prs1, prs2
    // ------------------------------------------------------------------------
    val rn1_rd_wens = rn1_uops_reg.map(u => rn1_fire && u.valid && u.bits.lrd_val)

    for (i <- 0 until p.coreWidth) {
      free_list.io.alloc_reqs(i).valid := rn1_fire && rn1_uops_reg(i).valid
      free_list.io.alloc_reqs(i).bits := rn1_uops_reg(i).bits
    }

    val rn1_uops = Wire(Vec.fill(p.coreWidth)(Valid(UOp(p))))
    rn1_uops.zip(rn1_uops_reg).foreach((w, r) => {
      w.valid := r.valid && !kill_rename
      w.bits := r.bits
    })
    dontTouch(rn1_uops)

    for (i <- 0 until p.coreWidth) {
      when (rn1_rd_wens(i)) {
        rn1_uops(i).bits.prd := free_list.io.alloc_resp(i)
      }
    }

    for (i <- 0 until p.coreWidth) {
      map_table.io.rn1_update(i).valid := rn1_fire && rn1_uops(i).valid
      map_table.io.rn1_update(i).bits.lrd := rn1_uops(i).bits.lrd
      map_table.io.rn1_update(i).bits.prd := rn1_uops(i).bits.prd
      map_table.io.rn1_update(i).bits.rd_wen := rn1_uops(i).bits.ctrl.rd_wen
      map_table.io.rn1_update(i).bits.is_cfi := rn1_uops(i).bits.ctrl.is_cfi
      map_table.io.rn1_update(i).bits.brtag := rn1_uops(i).bits.br_tag
      map_table.io.rn1_update(i).bits.brmask := rn1_uops(i).bits.br_mask
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

      busy_table.io.rn1_set(i).valid := rn1_rd_wens(i)
      busy_table.io.rn1_set(i).bits  := rn1_uops(i).bits.prd
    }

    val rn1_uops_bypass = Wire(Vec.fill(p.coreWidth)(Valid(UOp(p))))
    dontTouch(rn1_uops_bypass)

    rn1_uops_bypass(0) := rn1_uops(0)

    // Bypass prd to younger uops
    for (i <- 1 until p.coreWidth) {
      val prev_uops = rn1_uops.take(i)
      val bypass    = rn1_uops_bypass(i)
      bypass := rn1_uops(i)

      val lrd_deps  = prev_uops.map(p => p.valid && bypass.valid &&  bypass.bits.lrd_dep(p.bits))
      val lrs1_deps = prev_uops.map(p => p.valid && bypass.valid && bypass.bits.lrs1_dep(p.bits))
      val lrs2_deps = prev_uops.map(p => p.valid && bypass.valid && bypass.bits.lrs2_dep(p.bits))

      val lrd_dep_oh  = PriorityEncoderOH(Cat(lrd_deps ))
      val lrs1_dep_oh = PriorityEncoderOH(Cat(lrs1_deps))
      val lrs2_dep_oh = PriorityEncoderOH(Cat(lrs2_deps))

      when (lrd_deps.reduce(_ || _)) {
        bypass.bits.stale_prd := MuxOneHot(lrd_dep_oh, prev_uops.map(_.bits.prd).reverse)
      }
      when (lrs1_deps.reduce(_ || _)) {
        bypass.bits.prs1 := MuxOneHot(lrs1_dep_oh, prev_uops.map(_.bits.prd).reverse)
        bypass.bits.prs1_busy := true.B
      }
      when (lrs2_deps.reduce(_ || _)) {
        bypass.bits.prs2 := MuxOneHot(lrs2_dep_oh, prev_uops.map(_.bits.prd).reverse)
        bypass.bits.prs2_busy := true.B
      }
    }

    io.rn2_uops <> rn1_uops_bypass

    busy_table.io.wb_req <> io.wb_wakeup
    free_list.io.comm_prds  <> io.comm_free_phys
    free_list.io.resolve_tag := io.br_resolve
    map_table.io.resolve_tag := io.br_resolve

    val debug_bypass_rs1_val = Wire(Vec.fill(p.coreWidth)(Bool()))
    val debug_bypass_rs2_val = Wire(Vec.fill(p.coreWidth)(Bool()))
    val debug_bypass_rd_val  = Wire(Vec.fill(p.coreWidth)(Bool()))

    debug_bypass_rs1_val.zip(rn1_uops_bypass).foreach((d, u) => d := u.valid && u.bits.lrs1_val)
    debug_bypass_rs2_val.zip(rn1_uops_bypass).foreach((d, u) => d := u.valid && u.bits.lrs2_val)
    debug_bypass_rd_val .zip(rn1_uops_bypass).foreach((d, u) => d := u.valid && u.bits.lrd_val)


    dontTouch(io.dec_uops)
    dontTouch(io.rn2_uops)

    dontTouch(debug_bypass_rs1_val)
    dontTouch(debug_bypass_rs2_val)
    dontTouch(debug_bypass_rd_val)
  }
