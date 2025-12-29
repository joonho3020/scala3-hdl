package riscv_ooo

import hdl.core._
import hdl.util._
import hdl.elaboration._


abstract class BitMaskModule extends Module:
  given Module = this

  class BitMask(n: Int, init: BigInt):
    val initLit = Lit(UInt(n.W))(init)
    val data = RegInit(initLit)

    def clear: Unit =
      data := initLit

    def get(x: UInt): UInt =
      data(x)

    def getSetIds(cnt: Int): Vec[UInt] =
      val y = Wire(UInt(n.W))
      y := data

      val ret = Wire(Vec.fill(cnt)(UInt(log2Ceil(n+1).W)))
      ret.foreach(_ := DontCare)

      val found = Wire(Vec.fill(n)(UInt(log2Ceil(n+1).W)))
      found(0) := 0.U

      for (i <- 0 until n) {
        if i == 0 then
          when (y(i).asBool) {
            ret(0) := 0.U
            found(i) := 1.U
          }
        else
          when (y(i).asBool && found(i-1) < cnt.U) {
            ret(found(i-1)) := i.U
            found(i) := found(i-1) + 1.U
          } .otherwise {
            found(i) := found(i-1)
          }
      }
      ret

    def unset(input: UInt, indices: Vec[UInt], valid: Vec[Bool]): UInt =
      val mask = valid.zip(indices).map((v, idx) => {
        Mux(v, 1.U << idx, 0.U)
      }).reduce(_ | _)
      val ret = Wire(UInt(n.W))
      ret := input & ~mask
      ret

    def set(input: UInt, indices: Vec[UInt], valid: Vec[Bool]): UInt =
      val mask = valid.zip(indices).map((v, idx) => {
        Mux(v, 1.U << idx, 0.U)
      }).reduce(_ | _)
      val ret = Wire(UInt(n.W))
      ret := input | mask
      ret
