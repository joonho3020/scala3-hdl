package riscv_ooo

import hdl.core._
import hdl.util._
import hdl.elaboration._

import riscv_inorder.{ImmGen}
import riscv_inorder.CoreConstants._

case class AGUBundle(
  uop: Valid[UOp],
  prs1_data: UInt,
  prs2_data: UInt,
  update: Valid[LSUExeUpdate],
) extends Bundle[AGUBundle]

object AGUBundle:
  def apply(p: CoreParams): AGUBundle =
    AGUBundle(
      uop = Input(Valid(UOp(p))),
      prs1_data = Input(UInt(p.xlenBits.W)),
      prs2_data = Input(UInt(p.xlenBits.W)),
      update = Output(Valid(LSUExeUpdate(p))),
    )

class AGU(p: CoreParams) extends Module:
  given Module = this

  val io = IO(AGUBundle(p))

  body {
    val imm_gen = Module(new ImmGen(p.xlenBits))
    imm_gen.io.inst := io.uop.bits.inst
    imm_gen.io.sel := io.uop.bits.ctrl.sel_imm

    val addr = io.prs1_data + imm_gen.io.out

    io.update.valid := io.uop.valid && io.uop.bits.ctrl.is_mem
    io.update.bits.is_load := io.uop.valid && io.uop.bits.ctrl.is_load
    io.update.bits.is_store := io.uop.valid && io.uop.bits.ctrl.is_store
    io.update.bits.ldq_idx := io.uop.bits.ldq_idx
    io.update.bits.stq_idx := io.uop.bits.stq_idx
    io.update.bits.addr := addr
    io.update.bits.data := io.prs2_data
  }
