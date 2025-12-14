FIRRTL version 3.3.0
circuit Frontend :
  module Frontend :
    input clock : Clock
    input reset : Reset
    output io : { icache : { req : { valid : UInt<1>, bits : { addr : UInt<64> } }, flip resp : { insts : { valid : UInt<1>, bits : UInt<64> }[2] } }, core : { insts : { valid : UInt<1>, bits : { pc : UInt<64>, inst : UInt<64> } }[2] } }

        wire next_pc : UInt<64>
        connect next_pc, add(not(or(not(next_pc), UInt(7))), UInt(8))
        connect io.icache.req.valid, UInt<1>(1)
        connect io.icache.req.bits.addr, next_pc
        reg s1_pc : UInt<64>, clock
        connect s1_pc, next_pc
        reg s2_pc : UInt<64>, clock
        connect s2_pc, s1_pc
        connect io.core.insts[0].valid, io.icache.resp.insts[0].valid
        connect io.core.insts[0].bits.inst, io.icache.resp.insts[0].bits
        connect io.core.insts[0].bits.pc, add(s2_pc, UInt(0))
        connect io.core.insts[1].valid, io.icache.resp.insts[1].valid
        connect io.core.insts[1].bits.inst, io.icache.resp.insts[1].bits
        connect io.core.insts[1].bits.pc, add(s2_pc, UInt(4))

