package riscv_inorder

import hdl._
import CoreConstants.Immediates

case class ImmGenIO(
  inst: UInt,
  sel: HWEnum[Immediates],
  out: UInt
) extends Bundle[ImmGenIO]

object ImmGenIO:
  def apply(xlen: Int): ImmGenIO =
    ImmGenIO(
      inst = Input(UInt(xlen.W)),
      sel = Input(HWEnum(Immediates)),
      out = Output(UInt(xlen.W))
    )

class ImmGen(xlen: Int) extends Module with CacheableModule:
  type ElabParams = Int
  given stableHashElabParams: StableHash[Int] = summon[StableHash[Int]]
  def elabParams: Int = xlen
  given Module = this

  val io = IO(ImmGenIO(xlen))
  body:
    val immI  = Wire(UInt(xlen.W))
    immI := Cat(Seq(Fill(xlen - 10, io.inst(31)), io.inst(31, 20)))

    val immS  = Wire(UInt(xlen.W))
    immS := Cat(Seq(Fill(xlen - 11, io.inst(31)), io.inst(31, 25), io.inst(11, 7)))

    val immSB = Wire(UInt(xlen.W))
    immSB := Cat(Seq(Fill(xlen - 12, io.inst(31)), io.inst(7), io.inst(30, 25), io.inst(11, 8), 0.U(1.W)))

    val immU  = Wire(UInt(xlen.W))
    immU := Cat(Seq(Fill(xlen - 32, io.inst(31)), io.inst(31, 12), 0.U(12.W)))

    val immUJ = Wire(UInt(xlen.W))
    immUJ := Cat(Seq(Fill(xlen - 20, io.inst(31)), io.inst(19, 12), io.inst(20), io.inst(30, 21), 0.U(1.W)))

    dontTouch(immI)
    dontTouch(immS)
    dontTouch(immSB)
    dontTouch(immU)
    dontTouch(immUJ)

    import Immediates._
    switch (io.sel) {
      is(IMM_I .EN) { io.out := immI     }
      is(IMM_S .EN) { io.out := immS     }
      is(IMM_SB.EN) { io.out := immSB    }
      is(IMM_U .EN) { io.out := immU     }
      is(IMM_UJ.EN) { io.out := immUJ    }
      default       { io.out := DontCare }
    }
