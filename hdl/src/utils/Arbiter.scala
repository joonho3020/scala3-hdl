package hdl.util

import hdl.core._
import hdl.elaboration._

final case class ArbiterIO[T <: HWData](
  in: Vec[Decoupled[T]],
  out: Decoupled[T],
  chosen: UInt
) extends Bundle[ArbiterIO[T]]

object ArbiterIO:
  def apply[T <: HWData](gen: T, n: Int): ArbiterIO[T] =
    require(n >= 1, "ArbiterIO requires n >= 1")
    ArbiterIO(
      in = Flipped(Vec.fill(n)(Decoupled(gen))),
      out = Decoupled(gen),
      chosen = Output(UInt(log2Ceil(n).W))
    )

class Arbiter[T <: HWData](gen: T, n: Int) extends Module with CacheableModule:
  require(n >= 1, "Arbiter requires n >= 1")

  type ElabParams = (HWData, Int)
  given stableHashElabParams: StableHash[ElabParams] = StableHash.derived
  def elabParams: ElabParams = (gen, n)

  val io = IO(ArbiterIO(gen, n))

  body:
    val requests = (0 until n).foldLeft(0.U(n.W)) { (acc, i) =>
      acc | (io.in(i).valid.asUInt << i)
    }

    val grantOH = PriorityEncoderOH(requests)

    io.out.valid := requests.orR
    io.out.bits := DontCare
    io.chosen := (0 until n).foldLeft(0.U(log2Ceil(n).W)) { (acc, i) =>
      Mux(grantOH.asUInt(i).asBool, i.U(log2Ceil(n).W), acc)
    }

    when(requests.orR) {
      io.out.bits := MuxOneHot(grantOH, io.in.elems.map(_.bits))
    }

    var i = 0
    while i < n do
      io.in(i).ready := io.out.ready && grantOH.asUInt(i).asBool
      i += 1

class RRArbiter[T <: HWData](gen: T, n: Int) extends Module with CacheableModule:
  require(n >= 1, "RRArbiter requires n >= 1")

  type ElabParams = (HWData, Int)
  given stableHashElabParams: StableHash[ElabParams] = StableHash.derived
  def elabParams: ElabParams = (gen, n)

  val io = IO(ArbiterIO(gen, n))

  body:
    val lastGrantOH = RegInit(0.U(n.W).asOH)

    val requests = (0 until n).foldLeft(0.U(n.W)) { (acc, i) =>
      acc | (io.in(i).valid.asUInt << i)
    }

    val afterLast = requests & ~MaskLower(lastGrantOH)
    val grantOH = Mux(afterLast.orR, PriorityEncoderOH(afterLast), PriorityEncoderOH(requests))

    io.out.valid := requests.orR
    io.out.bits := DontCare
    io.chosen := (0 until n).foldLeft(0.U(log2Ceil(n).W)) { (acc, i) =>
      Mux(grantOH.asUInt(i).asBool, i.U(log2Ceil(n).W), acc)
    }

    when(requests.orR) {
      io.out.bits := MuxOneHot(grantOH, io.in.elems.map(_.bits))
    }

    var i = 0
    while i < n do
      io.in(i).ready := io.out.ready && grantOH.asUInt(i).asBool
      i += 1

    when(io.out.fire) {
      lastGrantOH := grantOH
    }
