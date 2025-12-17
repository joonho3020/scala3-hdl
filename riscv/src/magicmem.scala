package riscv

import hdl._


case class MagicMemReq(
  addr: UInt
) extends Bundle[MagicMemReq]

case class MagicMemResp(
  lineWords: Vec[UInt]
) extends Bundle[MagicMemResp]

case class MagicMemIf(
  req: Valid[MagicMemReq],
  resp: Valid[MagicMemResp]
) extends Bundle[MagicMemIf]

object MagicMemReq:
  def apply(p: CoreParams): MagicMemReq =
    MagicMemReq(addr = UInt(p.pcBits.W))

object MagicMemResp:
  def apply(p: CoreParams): MagicMemResp =
    MagicMemResp(lineWords = Vec.fill(p.ic.cacheLineBytes/4)(UInt(32.W)))

object MagicMemIf:
  def apply(p: CoreParams): MagicMemIf =
    MagicMemIf(
      req  =         Valid(MagicMemReq (p)),
      resp = Flipped(Valid(MagicMemResp(p)))
    )

