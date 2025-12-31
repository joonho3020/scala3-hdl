package riscv_ooo

import hdl.core._
import hdl.util._
import hdl.elaboration._

import riscv_inorder.CoreConstants._

case class LDQEntry(
  valid: Bool,
  addr: Valid[UInt],               // Physical address (valid after AGU)
  addr_issued: Bool,               // Issued addr gen
  executed: Bool,                  // Sent to D-cache
  succeeded: Bool,                 // D-cache response received

  order_fail: Bool,                // Ordering violation detected
  next_stq_idx: UInt,              // All stores BEFORE this load (older stores)

  uop: UOp
) extends Bundle[LDQEntry]

object LDQEntry:
  def apply(p: CoreParams): LDQEntry =
    LDQEntry(
      valid = Bool(),
      addr = Valid(UInt(p.paddrBits.W)),
      addr_issued = Bool(),

      executed = Bool(),
      succeeded = Bool(),

      order_fail = Bool(),
      next_stq_idx = UInt(log2Ceil(p.lsu.sqEntries + 1).W),

      uop = UOp(p),
    )

case class STQEntry(
  valid: Bool,
  addr: Valid[UInt],               // Physical address (valid after AGU)
  addr_issued: Bool,
  data: Valid[UInt],               // Store data (valid after data ready)
  data_issued: Bool,

  committed: Bool,                 // ROB committed this store
  executed: Bool,                  // Sent request to D-cache
  succeeded: Bool,                 // D-cache acknowledged

  next_ldq_idx: UInt,              // All loads before this store (older loads)
  uop: UOp,
) extends Bundle[STQEntry]

object STQEntry:
  def apply(p: CoreParams): STQEntry =
    STQEntry(
      valid = Bool(),
      addr = Valid(UInt(p.paddrBits.W)),
      addr_issued = Bool(),
      data = Valid(UInt(p.xlenBits.W)),
      data_issued = Bool(),

      committed = Bool(),
      executed = Bool(),
      succeeded = Bool(),

      next_ldq_idx = UInt(log2Ceil(p.lsu.lqEntries + 1).W),

      uop = UOp(p),
    )

case class LSUDispatchReq(
  uop: UOp,
  addr: UInt,
  data: UInt,
  size: HWEnum[MemWidth],
  signed: Bool,
) extends Bundle[LSUDispatchReq]

object LSUDispatchReq:
  def apply(p: CoreParams): LSUDispatchReq =
    LSUDispatchReq(
      uop = UOp(p),
      addr = UInt(p.paddrBits.W),
      data = UInt(p.xlenBits.W),
      size = HWEnum(MemWidth),
      signed = Bool()
    )

case class LSUResp(
  data: UInt,
  prd:  UInt,
) extends Bundle[LSUResp]

object LSUResp:
  def apply(p: CoreParams): LSUResp =
    LSUResp(
      data = UInt(p.xlenBits.W),
      prd = UInt(p.pRegIdxBits.W)
    )

case class LSUExeReq(
  ldq_idx: Valid[UInt],
  stq_idx: Valid[UInt],
  is_data: Bool,
  prs1:    UInt,
  prs2:    UInt,
  inst:    UInt,
) extends Bundle[LSUExeReq]

case class LSUExeResp(
  ldq_idx: Valid[UInt],
  stq_idx: Valid[UInt],
  is_data: Bool,
  output:  UInt,
) extends Bundle[LSUExeResp]

case class LSUBundle(
  // Dispatch interface
  dispatch: Vec[Decoupled[UOp]],
  dispatch_ldq_idx: Vec[UInt],
  dispatch_stq_idx: Vec[UInt],

  // Exe interface
  exe_req:  Vec[LSUExeReq],
  exe_resp: Vec[LSUExeResp],

  // Writeback interface
  ld_resp: Vec[Valid[LSUResp]],

  // Wakeup interface (from writeback stage)
  wakeup: Vec[Valid[UInt]],

  // Commit interface (from ROB)
  commit_store: Vec[Valid[UInt]],

  // Branch resolution interface
  br_resolve: BranchResolve,

  // D-cache interface
  dcache: DCacheIf,
) extends Bundle[LSUBundle]

object LSUBundle:
  def apply(p: CoreParams): LSUBundle =
    LSUBundle(
      dispatch = Flipped(Vec.fill(p.intIssueWidth)(Decoupled(UOp(p)))),
      dispatch_ldq_idx = Output(Vec.fill(p.intIssueWidth)(UInt(p.ldqIdxBits.W))),
      dispatch_stq_idx = Output(Vec.fill(p.intIssueWidth)(UInt(p.stqIdxBits.W))),

      exe_req = Output(Vec.fill(p.intIssueWidth)(LSUExeReq(
        ldq_idx = Valid(UInt(p.ldqIdxBits.W)),
        stq_idx = Valid(UInt(p.stqIdxBits.W)),
        is_data = Bool(),
        prs1    = UInt(p.pRegIdxBits.W),
        prs2    = UInt(p.pRegIdxBits.W),
        inst    = UInt(32.W),
      ))),
      exe_resp = Input(Vec.fill(p.intIssueWidth)(LSUExeResp(
        ldq_idx = Valid(UInt(p.ldqIdxBits.W)),
        stq_idx = Valid(UInt(p.stqIdxBits.W)),
        is_data = Bool(),
        output  = UInt(p.xlenBits.W),
      ))),

      ld_resp = Vec.fill(p.intIssueWidth)(Valid(LSUResp(p))),

      wakeup = Input(Vec.fill(p.retireWidth)(Valid(UInt(p.pRegIdxBits.W)))),

      commit_store = Vec.fill(p.retireWidth)(Valid(UInt(p.robIdxBits.W))),

      br_resolve = Input(BranchResolve(p)),

      dcache = DCacheIf(p, log2Ceil(p.lsu.lqEntries + p.lsu.sqEntries)),
    )

class LSU(p: CoreParams) extends Module:
  given Module = this

  val io = IO(LSUBundle(p))

  val numLdqEntries = p.lsu.lqEntries
  val numStqEntries = p.lsu.sqEntries
  val ldqIdxBits = p.ldqIdxBits
  val stqIdxBits = p.stqIdxBits

  body {
    val ldq = Reg(Vec.fill(numLdqEntries)(LDQEntry(p)))
    val ldq_head = RegInit(0.U(ldqIdxBits.W))
    val ldq_tail = RegInit(0.U(ldqIdxBits.W))

    def ldq_ptr_match = ldq_head(ldqIdxBits-2, 0) === ldq_tail(ldqIdxBits-2, 0)
    def ldq_empty = ldq_ptr_match && (ldq_head(ldqIdxBits-1) === ldq_tail(ldqIdxBits-1))
    def ldq_full = ldq_ptr_match && (ldq_head(ldqIdxBits-1) =/= ldq_tail(ldqIdxBits-1))

    def wrap_incr_ldq(ptr: UInt, amt: UInt): UInt = {
      val incremented = ptr + amt
      Mux(incremented >= numLdqEntries.U,
          incremented - numLdqEntries.U,
          incremented)
    }

    val stq = Reg(Vec.fill(numStqEntries)(STQEntry(p)))
    val stq_head = RegInit(0.U(stqIdxBits.W))      // Oldest store
    val stq_commit_head = RegInit(0.U(stqIdxBits.W)) // Oldest uncommitted
    val stq_tail = RegInit(0.U(stqIdxBits.W))      // Youngest store + 1

    def stq_ptr_match = stq_head(stqIdxBits-2, 0) === stq_tail(stqIdxBits-2, 0)
    def stq_empty = stq_ptr_match && (stq_head(stqIdxBits-1) === stq_tail(stqIdxBits-1))
    def stq_full = stq_ptr_match && (stq_head(stqIdxBits-1) =/= stq_tail(stqIdxBits-1))

    def wrap_incr_stq(ptr: UInt, amt: UInt): UInt = {
      val incremented = ptr + amt
      Mux(incremented >= numStqEntries.U,
          incremented - numStqEntries.U,
          incremented)
    }

    // ------------------------------------------------------------------------
    // Dispatch: Allocate LDQ/STQ entries
    // ------------------------------------------------------------------------
    val num_loads_dispatching = Wire(UInt(log2Ceil(p.intIssueWidth + 1).W))
    val num_stores_dispatching = Wire(UInt(log2Ceil(p.intIssueWidth + 1).W))

    num_loads_dispatching  := io.dispatch.map(d => (d.valid && d.bits.ctrl.is_load ).asUInt).reduce(_ + _)
    num_stores_dispatching := io.dispatch.map(d => (d.valid && d.bits.ctrl.is_store).asUInt).reduce(_ + _)

    val ldq_slots_available = Mux(ldq_full, 0.U,
                              Mux(ldq_tail >= ldq_head,
                                  numLdqEntries.U - (ldq_tail - ldq_head),
                                  ldq_head - ldq_tail)).asWire

    val stq_slots_available = Mux(stq_full, 0.U,
                              Mux(stq_tail >= stq_head,
                                  numStqEntries.U - (stq_tail - stq_head),
                                  stq_head - stq_tail)).asWire

    // Pessimistic backpressure, but ok to keep things simple
    val can_allocate_loads = num_loads_dispatching <= ldq_slots_available
    val can_allocate_stores = num_stores_dispatching <= stq_slots_available
    val can_dispatch = can_allocate_loads && can_allocate_stores

    val ldq_alloc_idx = Wire(Vec.fill(p.intIssueWidth)(UInt(p.ldqIdxBits.W)))
    val stq_alloc_idx = Wire(Vec.fill(p.intIssueWidth)(UInt(p.stqIdxBits.W)))

    io.dispatch_ldq_idx.foreach(_ := DontCare)
    io.dispatch_stq_idx.foreach(_ := DontCare)

    for (w <- 0 until p.intIssueWidth) {
      val is_load  = io.dispatch(w).bits.ctrl.is_load
      val is_store = io.dispatch(w).bits.ctrl.is_store

      if w == 0 then
        ldq_alloc_idx(w) := ldq_tail
        stq_alloc_idx(w) := stq_tail
      else
        ldq_alloc_idx(w) := Mux(io.dispatch(w).fire && is_load,
                              wrap_incr_ldq(ldq_alloc_idx(w-1), 1.U),
                              ldq_alloc_idx(w-1))
        stq_alloc_idx(w) := Mux(io.dispatch(w).fire && is_load,
                              wrap_incr_stq(stq_alloc_idx(w-1), 1.U),
                              stq_alloc_idx(w-1))

      io.dispatch(w).ready := can_dispatch
      when (io.dispatch(w).fire) {
        when (is_load) {
          val lidx = ldq_alloc_idx(w)
          val uop = io.dispatch(w).bits

          ldq(lidx).valid := true.B
          ldq(lidx).addr.valid := false.B
          ldq(lidx).addr.bits := DontCare
          ldq(lidx).executed := false.B
          ldq(lidx).succeeded := false.B
          ldq(lidx).order_fail := false.B
          ldq(lidx).next_stq_idx := stq_alloc_idx(w)  // Stores BEFORE this load
          ldq(lidx).uop := uop

          io.dispatch_ldq_idx(w) := lidx
        }

        when (is_store) {
          val sidx = stq_alloc_idx(w)
          val uop = io.dispatch(w).bits

          stq(sidx).valid := true.B
          stq(sidx).addr.valid := false.B
          stq(sidx).addr.bits := DontCare
          stq(sidx).data.valid := false.B
          stq(sidx).data.bits := DontCare
          stq(sidx).committed := false.B
          stq(sidx).succeeded := false.B
          stq(sidx).next_ldq_idx := ldq_alloc_idx(w)  // Loads BEFORE this store
          stq(sidx).uop := uop

          io.dispatch_stq_idx(w) := sidx
        }
      }
    }

    when (can_dispatch) {
      ldq_tail := wrap_incr_ldq(ldq_tail, num_loads_dispatching)
      stq_tail := wrap_incr_stq(stq_tail, num_stores_dispatching)
    }

    // ------------------------------------------------------------------------
    // Wakeup: Monitor writeback and set ready bits
    // ------------------------------------------------------------------------
    for (w <- 0 until p.retireWidth) {
      when (io.wakeup(w).valid) {
        val wakeup_pdst = io.wakeup(w).bits

        for (i <- 0 until numLdqEntries) {
          when (ldq(i).uop.prs1_busy && (ldq(i).uop.prs1 === wakeup_pdst)) {
            ldq(i).uop.prs1_busy := false.B
          }
        }

        for (i <- 0 until numStqEntries) {
          when (stq(i).uop.prs1_busy && (stq(i).uop.prs1 === wakeup_pdst)) {
            stq(i).uop.prs1_busy := false.B
          }
          when (stq(i).uop.prs2_busy && (stq(i).uop.prs2 === wakeup_pdst)) {
            stq(i).uop.prs2_busy := false.B
          }
        }
      }
    }

    // ------------------------------------------------------------------------
    // Issue AGU & Store data PRF reads
    // ------------------------------------------------------------------------
    io.exe_req.foreach(_ := DontCare)
    io.exe_req.foreach(_.ldq_idx.valid := false.B)
    io.exe_req.foreach(_.stq_idx.valid := false.B)

    var ld_issued  = false.B
    var st_issued  = false.B
    var ld_exe_ptr = ldq_head
    var st_exe_ptr = stq_head

    for (w <- 0 until p.intIssueWidth) {

      // lets keep things simple for now...
      // allocate AGU 0 to loads and AGU 1 to stores
      require(p.intIssueWidth >= 2)

      val ldq_entry = ldq(ld_exe_ptr)
      val issue_this_load = ldq_entry.valid &&
                            !ldq_entry.addr.valid &&
                            !ldq_entry.addr_issued &&
                            !ldq_entry.uop.prs1_busy &&
                            !ld_issued
      when (issue_this_load) {
        ldq_entry.addr_issued := true.B

        io.exe_req(0).ldq_idx.valid := true.B
        io.exe_req(0).ldq_idx.bits := ld_exe_ptr
        io.exe_req(0).is_data := false.B
        io.exe_req(0).prs1 := ldq_entry.uop.prs1
        io.exe_req(0).inst := ldq_entry.uop.inst
      }
      ld_issued = Mux(issue_this_load, true.B, ld_issued)

      val stq_entry = stq(st_exe_ptr)
      val issue_this_store_addr = stq_entry.valid &&
                                  !stq_entry.addr.valid &&
                                  !stq_entry.addr_issued &&
                                  !stq_entry.uop.prs1_busy
      val issue_this_store_data = stq_entry.valid &&
                                  !stq_entry.data.valid &&
                                  !stq_entry.data_issued &&
                                  !stq_entry.uop.prs1_busy

      when (issue_this_store_addr) {
        stq_entry.addr_issued := true.B

        io.exe_req(1).stq_idx.valid := true.B
        io.exe_req(1).stq_idx.bits := st_exe_ptr
        io.exe_req(1).is_data := false.B
        io.exe_req(1).prs1 := stq_entry.uop.prs1
        io.exe_req(1).inst := stq_entry.uop.inst
      } .elsewhen (issue_this_store_data) {
        stq_entry.data_issued := true.B

        io.exe_req(1).stq_idx.valid := true.B
        io.exe_req(1).stq_idx.bits := st_exe_ptr
        io.exe_req(1).is_data := true.B
        io.exe_req(1).prs2 := stq_entry.uop.prs2
      }
      st_issued = Mux(issue_this_store_data || issue_this_store_addr, true.B, st_issued)
    }

    for (w <- 0 until p.intIssueWidth) {
      val exe_resp = io.exe_resp(w)
      when (exe_resp.ldq_idx.valid) {
        ldq(exe_resp.ldq_idx.bits).addr.valid := true.B
        ldq(exe_resp.ldq_idx.bits).addr.bits  := exe_resp.output
      }
      when (exe_resp.stq_idx.valid) {
        when (exe_resp.is_data) {
          stq(exe_resp.stq_idx.bits).data.valid := true.B
          stq(exe_resp.stq_idx.bits).data.bits  := exe_resp.output
        } .otherwise {
          stq(exe_resp.stq_idx.bits).addr.valid := true.B
          stq(exe_resp.stq_idx.bits).addr.bits  := exe_resp.output
        }
      }
    }

    // ------------------------------------------------------------------------
    // Select loads/stores ready for dcache
    // ------------------------------------------------------------------------
    val ldq_execute_idx = Wire(Valid(UInt(ldqIdxBits.W)))
    ldq_execute_idx.valid := false.B
    ldq_execute_idx.bits := DontCare

    // Find oldest AGU-ready, unexecuted load
    var ld_search_ptr = ldq_head
    for (i <- 0 until numLdqEntries) {
      when (ldq(ld_search_ptr).addr.valid &&
            !ldq(ld_search_ptr).executed &&
            (ld_search_ptr =/= ldq_tail)) {
        ldq_execute_idx.valid := true.B
        ldq_execute_idx.bits := ld_search_ptr
      }
      ld_search_ptr = wrap_incr_ldq(ld_search_ptr, 1.U)
    }

    val stq_execute_idx = Wire(Valid(UInt(stqIdxBits.W)))
    stq_execute_idx.valid := false.B
    stq_execute_idx.bits := DontCare

    // Find oldest AGU-ready, committed, unexecuted store
    var st_search_ptr = stq_commit_head
    for (i <- 0 until numStqEntries) {
      when (stq(st_search_ptr).addr.valid &&
            stq(st_search_ptr).data.valid &&
            stq(st_search_ptr).committed &&
            !stq(st_search_ptr).executed &&
            !stq(st_search_ptr).succeeded &&
            (st_search_ptr =/= stq_tail)) {
        stq_execute_idx.valid := true.B
        stq_execute_idx.bits := st_search_ptr
      }
      st_search_ptr = wrap_incr_stq(st_search_ptr, 1.U)
    }

    // Prioritize loads over stores
    val execute_load = ldq_execute_idx.valid
    val execute_store = stq_execute_idx.valid && !execute_load
    io.dcache.s0_req.valid := execute_load || execute_store
    io.dcache.s0_req.bits := DontCare

    when (execute_load) {
      val idx = ldq_execute_idx.bits
      io.dcache.s0_req.bits.vaddr := ldq(idx).addr.bits
      io.dcache.s0_req.bits.data := DontCare
      io.dcache.s0_req.bits.size := ldq(idx).uop.ctrl.mem_width
      io.dcache.s0_req.bits.signed := ldq(idx).uop.ctrl.mem_signed
      io.dcache.s0_req.bits.tpe := MemOp.Ld.EN
      io.dcache.s0_req.bits.tag := idx  // Use LDQ index as tag

      when (io.dcache.s0_req.fire) {
        ldq(idx).executed := true.B
      }
    } .elsewhen (execute_store) {
      val idx = stq_execute_idx.bits
      io.dcache.s0_req.bits.vaddr := stq(idx).addr.bits
      io.dcache.s0_req.bits.data := stq(idx).data.bits
      io.dcache.s0_req.bits.size := stq(idx).uop.ctrl.mem_width
      io.dcache.s0_req.bits.signed := DontCare
      io.dcache.s0_req.bits.tpe := MemOp.St.EN
      io.dcache.s0_req.bits.tag := idx + numLdqEntries.U  // Offset to distinguish from loads

      when (io.dcache.s0_req.fire) {
        stq(idx).executed := true.B
      }
    }

    // ------------------------------------------------------------------------
    // Memory Ordering Checks
    // ------------------------------------------------------------------------

    // Currently, check for address matches in cacheline granularity
    def addr_match(addr1: UInt, addr2: UInt): Bool = {
      val block_bits = log2Ceil(p.dc.cacheLineBytes)
      (addr1 >> block_bits) === (addr2 >> block_bits)
    }

    // When LOAD executes, search against STQ
    when (execute_load && io.dcache.s0_req.fire) {
      val load_idx = ldq_execute_idx.bits
      val load_addr = ldq(load_idx).addr.bits
      val load_next_stq = ldq(load_idx).next_stq_idx

      for (i <- 0 until numStqEntries) {
        val stq_entry = stq(i)
        val is_older_store = Wire(Bool())
        is_older_store := false.B

        when (load_next_stq >= stq_head) {
          is_older_store := (i.U >= stq_head) && (i.U < load_next_stq)
        } .otherwise {
          is_older_store := (i.U >= stq_head) || (i.U < load_next_stq)
        }

        when (is_older_store && stq_entry.addr.valid &&
              addr_match(load_addr, stq_entry.addr.bits)) {
          ldq(load_idx).order_fail := true.B
        }
      }
    }

    // When STORE executes, search against LDQ
    when (execute_store && io.dcache.s0_req.fire) {
      val store_idx = stq_execute_idx.bits
      val store_addr = stq(store_idx).addr.bits
      val store_next_ldq = stq(store_idx).next_ldq_idx

      for (i <- 0 until numLdqEntries) {
        val ldq_entry = ldq(i)
        val is_younger_load = Wire(Bool())
        is_younger_load := false.B

        when (ldq_tail >= store_next_ldq) {
          is_younger_load := (i.U >= store_next_ldq) && (i.U < ldq_tail)
        } .otherwise {
          is_younger_load := (i.U >= store_next_ldq) || (i.U < ldq_tail)
        }

        when (is_younger_load && ldq_entry.executed && ldq_entry.addr.valid &&
              addr_match(store_addr, ldq_entry.addr.bits)) {
          ldq(i).order_fail := true.B
        }
      }
    }

    io.dcache.s1_kill := false.B
    io.dcache.s1_paddr.valid := true.B
    io.dcache.s1_paddr.bits := io.dcache.s0_req.bits.vaddr
    io.dcache.s2_kill := false.B

    // -----------------------------------------------------------------------
    // D-Cache Response
    // ------------------------------------------------------------------------

    for (w <- 0 until p.intIssueWidth) {
      io.ld_resp(w).valid := false.B
      io.ld_resp(w).bits := DontCare
    }

    when (io.dcache.s2_resp.valid) {
      val tag = io.dcache.s2_resp.bits.tag
      val is_load = tag < numLdqEntries.U
      val is_store = tag >= numLdqEntries.U

      when (is_load) {
        val ldq_idx = tag
        ldq(ldq_idx).succeeded := true.B

        // Writeback to register file
        io.ld_resp(0).valid := true.B
        io.ld_resp(0).bits.prd := ldq(ldq_idx).uop.prd
        io.ld_resp(0).bits.data := io.dcache.s2_resp.bits.data
      }
      when (is_store) {
        val stq_idx = tag - numLdqEntries.U
        stq(stq_idx).succeeded := true.B
      }
    }

    // ------------------------------------------------------------------------
    // Commit Stores
    // ------------------------------------------------------------------------

    for (w <- 0 until p.retireWidth) {
      when (io.commit_store(w).valid) {
        val rob_idx = io.commit_store(w).bits

        // Find matching store in STQ
        for (i <- 0 until numStqEntries) {
          when (stq(i).uop.rob_idx === rob_idx) {
            stq(i).committed := true.B
          }
        }
      }
    }

    // Advance stq_commit_head when stores commit
    when (!stq_empty && stq(stq_commit_head).committed) {
      stq_commit_head := wrap_incr_stq(stq_commit_head, 1.U)
    }

    // ------------------------------------------------------------------------
    // Branch Mask Update & Flush
    // ------------------------------------------------------------------------

    def is_killed_by_branch(br_mask: UInt, br_tag: OneHot): Bool = {
      val killed = (io.br_resolve.valid &&
                   io.br_resolve.mispredict &&
                   ((br_mask & br_tag) =/= 0.U))
      killed.asWire
    }

    def get_new_br_mask(br_mask: UInt, br_tag: OneHot): UInt = {
      val new_mask = Mux(io.br_resolve.valid,
                         br_mask & ~br_tag,
                         br_mask)
      new_mask.asWire
    }

    // Update LDQ branch masks and flush killed entries
    for (i <- 0 until numLdqEntries) {
      val uop = ldq(i).uop
      ldq(i).uop.br_mask := get_new_br_mask(uop.br_mask, io.br_resolve.tag)

      when (is_killed_by_branch(uop.br_mask, io.br_resolve.tag)) {
        ldq(i).valid := false.B
      }
    }

    // Update STQ branch masks and flush killed entries
    for (i <- 0 until numStqEntries) {
      val uop = stq(i).uop
      stq(i).uop.br_mask := get_new_br_mask(uop.br_mask, io.br_resolve.tag)

      when (is_killed_by_branch(uop.br_mask, io.br_resolve.tag)) {
        stq(i).valid := false.B
      }
    }

    // ------------------------------------------------------------------------
    // Ordering Violation Handling
    // ------------------------------------------------------------------------

    val ordering_violation = Wire(Bool())
    val violating_load_rob_idx = Wire(UInt(p.robIdxBits.W))
    ordering_violation := false.B
    violating_load_rob_idx := DontCare

    // Scan for any load with order_fail set
    for (i <- 0 until numLdqEntries) {
      when (ldq(i).order_fail && ldq(i).succeeded && !ordering_violation) {
        ordering_violation := true.B
        violating_load_rob_idx := ldq(i).uop.rob_idx
      }
    }

    // ------------------------------------------------------------------------
    // Deallocation
    // ------------------------------------------------------------------------

    when (!ldq_empty &&
      (ldq(ldq_head).succeeded || !ldq(ldq_head).valid)) {
      ldq_head := wrap_incr_ldq(ldq_head, 1.U)
    }

    when (!stq_empty &&
      stq(stq_head).committed &&
      (stq(stq_head).succeeded || !stq(stq_head).valid)) {
      stq_head := wrap_incr_stq(stq_head, 1.U)
    }

    dontTouch(ldq)
    dontTouch(stq)
    dontTouch(ldq_head)
    dontTouch(ldq_tail)
    dontTouch(stq_head)
    dontTouch(stq_commit_head)
    dontTouch(stq_tail)
    dontTouch(ldq_slots_available)
    dontTouch(stq_slots_available)
    dontTouch(num_loads_dispatching)
    dontTouch(num_stores_dispatching)
  }
