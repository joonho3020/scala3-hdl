package riscv_ooo

import hdl._

// From BOOM
// n-1                 0
//   t   i       h
// 1 1 1 1       1 1 1 1
//
//       h   t   i
//       0 1 1 1 1 1 1 1
// 1 1 1 1 1 1 1 0

object IsYoungerMask:
  def apply(i: UInt, head: UInt, n: Integer)(using m: Module): UInt =
    val hi_mask = ~MaskLower(UIntToOH(i)(n-1,0).asOH)
    val lo_mask = ~MaskUpper(UIntToOH(head)(n-1,0).asOH)
    Mux(i < head, hi_mask & lo_mask, hi_mask | lo_mask)(n-1,0)
