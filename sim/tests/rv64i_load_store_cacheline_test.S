.option norvc
.text
.globl _start

_start:
  lui  x20, 0x80002                # x20 = 0x80002000 base (cacheline-aligned)
  addi x22, x0, 17                 # u32 byte3 0x11
  slli x22, x22, 8
  addi x22, x22, 34                # +byte2 0x22
  slli x22, x22, 8
  addi x22, x22, 51                # +byte1 0x33
  slli x22, x22, 8
  addi x22, x22, 68                # +byte0 0x44
  slli x22, x22, 32                # hi<<32
  addi x23, x0, 85                 # u32 byte3 0x55
  slli x23, x23, 8
  addi x23, x23, 102               # +byte2 0x66
  slli x23, x23, 8
  addi x23, x23, 119               # +byte1 0x77
  slli x23, x23, 8
  addi x23, x23, 136               # +byte0 0x88
  or   x21, x22, x23               # combine hi|lo
  sd   x21, 0(x20)                 # SD @+0
  ld   x24, 0(x20)                 # LD @+0 immediately
  bne  x24, x21, fail              # check
  addi x25, x0, 170                # x25=0xAA
  sb   x25, 0(x20)                 # SB @+0
  ld   x26, 0(x20)                 # LD @+0 after SB
  addi x28, x0, 17                 # u32 byte3 0x11
  slli x28, x28, 8
  addi x28, x28, 34                # +byte2 0x22
  slli x28, x28, 8
  addi x28, x28, 51                # +byte1 0x33
  slli x28, x28, 8
  addi x28, x28, 68                # +byte0 0x44
  slli x28, x28, 32                # hi<<32
  addi x29, x0, 85                 # u32 byte3 0x55
  slli x29, x29, 8
  addi x29, x29, 102               # +byte2 0x66
  slli x29, x29, 8
  addi x29, x29, 119               # +byte1 0x77
  slli x29, x29, 8
  addi x29, x29, 170               # +byte0 0xaa
  or   x27, x28, x29               # combine hi|lo
  bne  x26, x27, fail              # check merge
  addi x30, x0, 190                # u16 hi 0xbe
  slli x30, x30, 8
  addi x30, x30, 239               # +lo 0xef
  sh   x30, 2(x20)                 # SH 0xBEEF @+2
  lh   x31, 2(x20)                 # LH @+2
  addi x7, x30, 0                  # x7=copy
  slli x7, x7, 48                  # signext<<48
  srai x7, x7, 48                  # signext>>48
  bne  x31, x7, fail               # LH sign
  lhu  x6, 2(x20)                  # LHU @+2
  bne  x6, x30, fail               # LHU zero
  addi x9, x0, 128                 # u32 byte3 0x80
  slli x9, x9, 8
  addi x9, x9, 0                   # +byte2 0x00
  slli x9, x9, 8
  addi x9, x9, 0                   # +byte1 0x00
  slli x9, x9, 8
  addi x9, x9, 1                   # +byte0 0x01
  sw   x9, 4(x20)                  # SW 0x80000001 @+4
  lw   x10, 4(x20)                 # LW @+4
  addi x11, x9, 0                  # x11=copy
  slli x11, x11, 32                # signext<<32
  srai x11, x11, 32                # signext>>32
  bne  x10, x11, fail              # LW sign
  lwu  x12, 4(x20)                 # LWU @+4
  bne  x12, x9, fail               # LWU zero
  addi x13, x0, 128                # x13=0x80
  sb   x13, 1(x20)                 # SB @+1
  lb   x14, 1(x20)                 # LB @+1
  addi x15, x13, 0                 # x15=copy
  slli x15, x15, 56                # signext<<56
  srai x15, x15, 56                # signext>>56
  bne  x14, x15, fail              # LB sign
  lbu  x16, 1(x20)                 # LBU @+1
  bne  x16, x13, fail              # LBU zero
  addi x18, x0, 0                  # u32 byte3 0x00
  slli x18, x18, 8
  addi x18, x18, 17                # +byte2 0x11
  slli x18, x18, 8
  addi x18, x18, 34                # +byte1 0x22
  slli x18, x18, 8
  addi x18, x18, 51                # +byte0 0x33
  slli x18, x18, 32                # hi<<32
  addi x19, x0, 68                 # u32 byte3 0x44
  slli x19, x19, 8
  addi x19, x19, 85                # +byte2 0x55
  slli x19, x19, 8
  addi x19, x19, 102               # +byte1 0x66
  slli x19, x19, 8
  addi x19, x19, 119               # +byte0 0x77
  or   x17, x18, x19               # combine hi|lo
  addi x2, x0, 136                 # u32 byte3 0x88
  slli x2, x2, 8
  addi x2, x2, 153                 # +byte2 0x99
  slli x2, x2, 8
  addi x2, x2, 170                 # +byte1 0xaa
  slli x2, x2, 8
  addi x2, x2, 187                 # +byte0 0xbb
  slli x2, x2, 32                  # hi<<32
  addi x3, x0, 204                 # u32 byte3 0xcc
  slli x3, x3, 8
  addi x3, x3, 221                 # +byte2 0xdd
  slli x3, x3, 8
  addi x3, x3, 238                 # +byte1 0xee
  slli x3, x3, 8
  addi x3, x3, 255                 # +byte0 0xff
  or   x1, x2, x3                  # combine hi|lo
  sd   x17, 8(x20)                 # SD A @+8
  sd   x1, 8(x20)                  # SD B @+8
  ld   x4, 8(x20)                  # LD @+8
  bne  x4, x1, fail                # check last wins
  addi x5, x0, 17                  # x5=0x11
  sb   x5, 16(x20)                 # SB @+16
  addi x5, x0, 34                  # x5=0x22
  sb   x5, 24(x20)                 # SB @+24
  addi x5, x0, 51                  # x5=0x33
  sb   x5, 32(x20)                 # SB @+32
  addi x5, x0, 68                  # x5=0x44
  sb   x5, 40(x20)                 # SB @+40
  addi x5, x0, 85                  # x5=0x55
  sb   x5, 48(x20)                 # SB @+48
  addi x5, x0, 102                 # x5=0x66
  sb   x5, 56(x20)                 # SB @+56
  lbu  x5, 16(x20)                 # LBU @+16
  addi x6, x0, 17                  # exp=0x11
  bne  x5, x6, fail                # check @+16
  lbu  x5, 24(x20)                 # LBU @+24
  addi x6, x0, 34                  # exp=0x22
  bne  x5, x6, fail                # check @+24
  lbu  x5, 32(x20)                 # LBU @+32
  addi x6, x0, 51                  # exp=0x33
  bne  x5, x6, fail                # check @+32
  lbu  x5, 40(x20)                 # LBU @+40
  addi x6, x0, 68                  # exp=0x44
  bne  x5, x6, fail                # check @+40
  lbu  x5, 48(x20)                 # LBU @+48
  addi x6, x0, 85                  # exp=0x55
  bne  x5, x6, fail                # check @+48
  lbu  x5, 56(x20)                 # LBU @+56
  addi x6, x0, 102                 # exp=0x66
  bne  x5, x6, fail                # check @+56
  addi x7, x0, 8                   # iters=8
rw_loop:
  addi x5, x0, 90                  # val=0x5A
  sb   x5, 0(x20)                  # SB @+0
  lbu  x6, 0(x20)                  # LBU @+0 immediately
  bne  x6, x5, fail                # RAW byte
  addi x7, x7, -1                  # iters--
  bne  x7, x0, rw_loop             # repeat
  addi x5, x0, 1                   # pass=1
  sb   x5, 60(x20)                 # flag @+60
pass:
  jal  x0, pass                    # pass loop
fail:
  addi x5, x0, 255                 # fail=0xFF
  sb   x5, 61(x20)                 # flag @+61
  jal  x0, fail                    # fail loop
