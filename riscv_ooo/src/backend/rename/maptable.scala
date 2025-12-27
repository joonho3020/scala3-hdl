package riscv_ooo

import hdl._


case class MapTableSnapshot(
  valid: Bool,
  table: Vec[UInt],
) extends Bundle[MapTableSnapshot]


object MapTableSnapshot:
  def apply(p: CoreParams): MapTableSnapshot =
    MapTableSnapshot(
      valid = Bool(),
      table = Vec.fill(32)(UInt(p.pRegIdxBits.W)),
    )

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
  prd: UInt,
  is_cfi: Bool,
  brtag: OneHot,
  brmask: UInt,
) extends Bundle[RenameWrite]

object RenameWrite:
  def apply(p: CoreParams): RenameWrite =
    RenameWrite(
      lrd = UInt(5.W),
      prd = UInt(p.pRegIdxBits.W),
      is_cfi = Bool(),
      brtag = OneHot(p.br.inFlightBranches.W),
      brmask = UInt(p.br.inFlightBranches.W),
    )

case class MapTableIO(
  dec_req: Vec[RenameReq],
  dec_resp: Vec[RenameResp],
  rn1_update: Vec[Valid[RenameWrite]],
  resolve_tag: BranchResolve,
) extends Bundle[MapTableIO]

class MapTable(p: CoreParams) extends Module with CoreCacheable(p):
  given Module = this
  val io = IO(MapTableIO(
    dec_req    = Input (Vec.fill(p.coreWidth)(RenameReq (p))),
    dec_resp   = Output(Vec.fill(p.coreWidth)(RenameResp(p))),
    rn1_update = Input (Vec.fill(p.coreWidth)(Valid(RenameWrite(p)))),
    resolve_tag = Input(BranchResolve(p)), 
  ))

  val maxInflight = p.br.inFlightBranches

  body {
    val snapshots = Reg(Vec.fill(maxInflight)(MapTableSnapshot(p)))
    val table = Reg(Vec.fill(32)(UInt(p.pRegIdxBits.W)))

    for (i <- 0 until p.coreWidth) {
      val req = io.dec_req(i)

      val lrd_deps  = io.rn1_update.map(p => p.valid && p.bits.lrd === req.lrd)
      val lrs1_deps = io.rn1_update.map(p => p.valid && p.bits.lrd === req.lrs1)
      val lrs2_deps = io.rn1_update.map(p => p.valid && p.bits.lrd === req.lrs2)

      val lrd_dep_oh  = PriorityEncoderOH(Cat(lrd_deps ))
      val lrs1_dep_oh = PriorityEncoderOH(Cat(lrs1_deps))
      val lrs2_dep_oh = PriorityEncoderOH(Cat(lrs2_deps))

      io.dec_resp(i).prs1 := table(io.dec_req(i).lrs1)
      io.dec_resp(i).prs2 := table(io.dec_req(i).lrs2)
      io.dec_resp(i).stale_prd := table(io.dec_req(i).lrd)

      when (lrd_deps.reduce(_ || _)) {
        io.dec_resp(i).stale_prd := MuxOneHot(lrd_dep_oh, io.rn1_update.map(_.bits.prd).reverse.toSeq)
      }
      when (lrs1_deps.reduce(_ || _)) {
        io.dec_resp(i).prs1 := MuxOneHot(lrs1_dep_oh, io.rn1_update.map(_.bits.prd).reverse.toSeq)
      }
      when (lrs2_deps.reduce(_ || _)) {
        io.dec_resp(i).prs2 := MuxOneHot(lrs2_dep_oh, io.rn1_update.map(_.bits.prd).reverse.toSeq)
      }
    }

    val next_table = Wire(Vec.fill(p.coreWidth)(Vec.fill(32)(0.U(p.pRegIdxBits.W))))
    for (i <- 0 until p.coreWidth) {
      if i == 0 then
        next_table(i) := table(i)
      else
        next_table(i) := next_table(i-1)
    }

    for (i <- 0 until p.coreWidth) {
      when (io.rn1_update(i).valid) {
        next_table(i)(io.rn1_update(i).bits.lrd) := io.rn1_update(i).bits.prd

        when (io.rn1_update(i).bits.is_cfi) {
          val tag_idx = PriorityEncoder(Reverse(io.rn1_update(i).bits.brtag))
          snapshots(tag_idx).valid := true.B
          snapshots(tag_idx).brmask := io.rn1_update(i).bits.brmask
          snapshots(tag_idx).table := next_table(i)
        }
      }
    }

    table := next_table(p.coreWidth - 1)

    when (io.resolve_tag.valid) {
      val tag_idx = PriorityEncoder(Reverse(io.resolve_tag.tag))
      snapshots(tag_idx).valid := false.B
      when (io.resolve_tag.mispredict) {
        table := snapshots(tag_idx)
        Assert(!io.rn1_update.map(_.valid).reduce(_ || _),
          "MapTable: Uops in the rename stage should be killed on a branch misprediction")
      }
    }


    when (reset.asBool) {
      for (i <- 0 until 32) {
        table(i) := i.U
      }
    }
    dontTouch(io)
  }

