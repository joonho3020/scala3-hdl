package riscv_ooo

import hdl._

case class FreeListSnapShot(
  freelist: UInt,
  brmask:   UInt,
  valid:    Bool,
) extends Bundle[FreeListSnapShot]

object FreeListSnapShot:
  def apply(p: CoreParams): FreeListSnapShot =
    FreeListSnapShot(
      freelist = UInt(p.nPhysicalRegs.W),
      brmask   = UInt(p.br.inFlightBranches.W),
      valid    = Bool()
    )


case class FreeListIO(
  alloc_reqs: Vec[Valid[UOp]],
  alloc_resp: Vec[UInt],
  count: UInt,
  resolve_tag: BranchResolve,
  comm_prds: Vec[Valid[UInt]]
) extends Bundle[FreeListIO]

// Assumes allocation requests happen only when there is enough entries
class FreeList(p: CoreParams) extends BitMaskModule with CoreCacheable(p):
  val maxInflight = p.br.inFlightBranches
  val coreWidth = p.coreWidth

  val io = IO(FreeListIO(
    alloc_reqs = Input(Vec.fill(coreWidth)(Valid(UOp(p)))),
    alloc_resp = Output(Vec.fill(p.coreWidth)(UInt(p.pRegIdxBits.W))),
    count      = Output(UInt(p.pRegIdxBits.W)),
    resolve_tag = Input(BranchResolve(p)), 
    comm_prds  = Input(Vec.fill(p.retireWidth)(Valid(UInt(p.pRegIdxBits.W))))
  ))

  body {
    val entries = p.nPhysicalRegs
    require(entries > 32)

    // Initial 32 registers should be taken
    val init = ((BigInt(1) << (entries - 32)) - 1) << 32
    val snapshots = Reg(Vec.fill(maxInflight)(FreeListSnapShot(p)))
    val freelist  = RegInit(Lit(UInt(entries.W))(init))

    io.count := PopCount(freelist)

    val next_freelist_base = Wire(Vec.fill(p.coreWidth)(UInt(entries.W)))
    val next_freelist_alloc = Wire(Vec.fill(p.coreWidth)(UInt(entries.W)))

    for (i <- 0 until coreWidth) {
      if i == 0 then
        next_freelist_base(i) := freelist
        next_freelist_alloc(i) := freelist
      else
        next_freelist_base(i) := next_freelist_alloc(i-1)
        next_freelist_alloc(i) := next_freelist_alloc(i-1)

      val req = io.alloc_reqs(i)
      when (req.valid && req.bits.ctrl.rd_wen) {
        val next_free_idx = PriorityEncoder(Reverse(next_freelist))
        io.alloc_resp(i) := next_free_idx
        next_freelist_alloc(i) := next_freelist_base(i) & ~UIntToOH(next_free_idx)
      }
      when (req.valid && req.bits.ctrl.is_cfi) {
        val tag_idx = PriorityEncoder(Reverse(req.bits.branch_tag))
        snapshots(tag_idx).brmask   := req.bits.branch_mask
        snapshots(tag_idx).freelist := next_freelist_alloc(i)
        snapshots(tag_idx).valid    := true.B
      }
    }

    val next_freelist_snapshot = Wire(UInt(entries.W))
    next_freelist_snapshot := DontCare

    when (io.resolve_tag.valid) {
      val tag_idx = PriorityEncoder(Reverse(io.resolve_tag.tag))
      snapshots(tag_idx).valid := false.B

      for (i <- 0 until branchTagBits) {
        val ss = snapshots(i)
        val mask_hit = ss.valid && ((ss.brmask & io.resolve_tag.tag.asUInt) =/= 0.U)
        when (mask_hit) {
          ss.brmask := ss.brmask & ~io.resolve_tag.tag
          when (io.resolve_tag.mispredict) {
            ss.valid := false.B
          }
        }
      }

      when (io.resolve_tag.mispredict) {
        next_freelist_snapshot := snapshots(tag_idx).freelist
        Assert(!io.alloc_reqs.map(_.valid).reduce(_ || _),
          "Freelist: Uops in the rename stage should be killed on a branch misprediction")
      }
    }

    var next_freelist = Mux(io.resolve_tag.mispredict && io.resolve_tag.valid,
                            next_freelist_snapshot,
                            next_freelist_alloc(p.coreWidth-1))

    for (i <- 0 until retireWidth) {
      next_freelist = next_free_list | Mux(io.comm_prds(i).valid, UIntToOH(io.comm_prds(i).bits), 0.U)
    }
    freelist := next_freelist

    when (reset.asBool) {
      snapshots.foreach(_.valid := false.B)
    }

    dontTouch(io)
  }
