package riscv_inorder

import hdl.core._
import hdl.util._
import hdl.elaboration._


enum MagicMemMsg:
  case
    Read,
    Write

case class MagicMemReq(
  addr: UInt,
  tpe: HWEnum[MagicMemMsg],
  data: Vec[UInt],
  mask: UInt,
  tag: UInt,
) extends Bundle[MagicMemReq]

case class MagicMemResp(
  tag: UInt,
  tpe: HWEnum[MagicMemMsg],
  lineWords: Vec[UInt]
) extends Bundle[MagicMemResp]

case class MagicMemIf(
  req: Decoupled[MagicMemReq],
  resp: Valid[MagicMemResp]
) extends Bundle[MagicMemIf]

object MagicMemReq:
  def apply(p: CoreParams): MagicMemReq =
    MagicMemReq(
      addr = UInt(p.pcBits.W),
      tpe =  HWEnum(MagicMemMsg),
      data = Vec.fill(p.memLineWords)(UInt(32.W)),
      mask = UInt(p.memLineBytes.W),
      tag  = UInt(log2Ceil(p.magic_mem_outstanding + 1).W)
    )

object MagicMemResp:
  def apply(p: CoreParams): MagicMemResp =
    MagicMemResp(
      tpe = HWEnum(MagicMemMsg),
      lineWords = Vec.fill(p.memLineWords)(UInt(32.W)),
      tag  = UInt(log2Ceil(p.magic_mem_outstanding + 1).W)
    )

object MagicMemIf:
  def apply(p: CoreParams): MagicMemIf =
    MagicMemIf(
      req  =     Decoupled(MagicMemReq (p)),
      resp = Flipped(Valid(MagicMemResp(p)))
    )
