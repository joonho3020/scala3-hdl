package riscv_ooo

import hdl.core._
import hdl.util._
import hdl.elaboration._

import riscv_inorder.{ImmGen}
import riscv_inorder.CoreConstants._

case class AGUBundle(
  req: LSUExeReq,
  prs1_data: UInt,
  prs2_data: UInt,
  resp: LSUExeResp,
) extends Bundle[AGUBundle]

object AGUBundle:
  def apply(p: CoreParams): AGUBundle =
    AGUBundle(
      req = Input(LSUExeReq(
        ldq_idx = Valid(UInt(p.ldqIdxBits.W)),
        stq_idx = Valid(UInt(p.stqIdxBits.W)),
        is_data = Bool(),
        prs1    = UInt(p.pRegIdxBits.W),
        prs2    = UInt(p.pRegIdxBits.W),
        inst    = UInt(32.W),
      )),
      prs1_data = Input(UInt(p.xlenBits.W)),
      prs2_data = Input(UInt(p.xlenBits.W)),
      resp = Output(LSUExeResp(
        ldq_idx = Valid(UInt(p.ldqIdxBits.W)),
        stq_idx = Valid(UInt(p.stqIdxBits.W)),
        is_data = Bool(),
        output  = UInt(p.xlenBits.W),
      )),
    )

class AGU(p: CoreParams) extends Module:
  given Module = this

  val io = IO(AGUBundle(p))

  body {
    val imm_gen = Module(new ImmGen(p.xlenBits))
    imm_gen.io.inst := io.req.inst
    imm_gen.io.sel := Immediates.IMM_I.EN

    val output = Wire(UInt(p.xlenBits.W))
    when (io.req.is_data) {
      output := io.prs2_data
    } .otherwise {
      output := io.prs1_data + imm_gen.io.out
    }

    io.resp.ldq_idx := io.req.ldq_idx
    io.resp.stq_idx := io.req.stq_idx
    io.resp.is_data := io.req.is_data
    io.resp.output := output
  }
