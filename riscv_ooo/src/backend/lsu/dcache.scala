package riscv_ooo

import hdl.core._
import hdl.util._
import hdl.elaboration._
import riscv_inorder.CoreConstants._

case class DCacheReq(
  addr: UInt,
  data: UInt,
  size: HWEnum[MemWidth],
  signed: Bool,
  is_load: Bool,
  is_store: Bool,
  ldq_idx: UInt,
  stq_idx: UInt,
  rob_idx: UInt,
  br_mask: UInt,
) extends Bundle[DCacheReq]

object DCacheReq:
  def apply(p: CoreParams): DCacheReq =
    DCacheReq(
      addr = UInt(p.paddrBits.W),
      data = UInt(p.xlenBits.W),
      size = HWEnum(MemWidth),
      signed = Bool(),
      is_load = Bool(),
      is_store = Bool(),
      ldq_idx = UInt(p.ldqIdxBits.W),
      stq_idx = UInt(p.stqIdxBits.W),
      rob_idx = UInt(p.robIdxBits.W),
      br_mask = UInt(p.branchTagBits.W),
    )

case class DCacheResp(
  data: UInt,
  ldq_idx: UInt,
) extends Bundle[DCacheResp]

object DCacheResp:
  def apply(p: CoreParams): DCacheResp =
    DCacheResp(
      data = UInt(p.xlenBits.W),
      ldq_idx = UInt(p.ldqIdxBits.W),
    )

case class LSUDCacheIO(
  req: Vec[Decoupled[DCacheReq]],
  s1_kill: Vec[Bool],
  resp: Vec[Valid[DCacheResp]],
  nack: Vec[Valid[DCacheReq]],
  store_ack: Vec[Valid[DCacheReq]],
  ll_resp: Decoupled[DCacheResp],
) extends Bundle[LSUDCacheIO]

object LSUDCacheIO:
  def apply(p: CoreParams): LSUDCacheIO =
    LSUDCacheIO(
      req = Vec.fill(p.lsuIssueWidth)(Decoupled(DCacheReq(p))),
      s1_kill = Output(Vec.fill(p.lsuIssueWidth)(Bool())),
      resp = Input(Vec.fill(p.lsuIssueWidth)(Valid(DCacheResp(p)))),
      nack = Input(Vec.fill(p.lsuIssueWidth)(Valid(DCacheReq(p)))),
      store_ack = Input(Vec.fill(p.lsuIssueWidth)(Valid(DCacheReq(p)))),
      ll_resp = Flipped(Decoupled(DCacheResp(p))),
    )

