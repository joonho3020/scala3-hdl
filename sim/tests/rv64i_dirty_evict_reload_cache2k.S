.option norvc
.text
.globl _start

_start:
  lui  x20, 0x80008                  # baseA = 0x80008000
  lui  x21, 0x80010                  # baseB = 0x80010000
  addi x22, x0, 64                   # stride = 64
  addi x23, x0, 256                  # nlines = 256
  addi x19, x0, 8                    # rounds = 8
  addi x18, x0, 0                    # round = 0
  addi x27, x0, 0                    # mix=0
round_loop:
  addi x24, x0, 0                    # i=0
  addi x25, x20, 0                   # addr=baseA
  addi x26, x23, 0                   # cnt=nlines
write_loop:
  addi x5, x24, 0                    # t0=i
  slli x6, x24, 32                   # t1=i<<32
  or   x5, x5, x6                    # t0|t1
  slli x7, x18, 48                   # t2=round<<48
  or   x5, x5, x7                    # val
  sd   x5, 0(x25)                    # store dirty
  add  x25, x25, x22                 # addr+=stride
  addi x24, x24, 1                   # i++
  addi x26, x26, -1                  # cnt--
  bne  x26, x0, write_loop           # loop
  addi x25, x21, 0                   # addr=baseB
  addi x26, x23, 0                   # cnt=nlines
thrash_loop:
  ld   x6, 0(x25)                    # ld thrash
  xor  x27, x27, x6                  # mix ^= ld
  add  x25, x25, x22                 # addr+=stride
  addi x26, x26, -1                  # cnt--
  bne  x26, x0, thrash_loop          # loop
  addi x24, x0, 0                    # i=0
  addi x25, x20, 0                   # addr=baseA
  addi x26, x23, 0                   # cnt=nlines
verify_loop:
  addi x5, x24, 0                    # t0=i
  slli x6, x24, 32                   # t1=i<<32
  or   x5, x5, x6                    # t0|t1
  slli x7, x18, 48                   # t2=round<<48
  or   x5, x5, x7                    # expected
  ld   x28, 0(x25)                   # ld back
  bne  x28, x5, fail                 # cmp
  add  x25, x25, x22                 # addr+=stride
  addi x24, x24, 1                   # i++
  addi x26, x26, -1                  # cnt--
  bne  x26, x0, verify_loop          # loop
  addi x18, x18, 1                   # round++
  addi x19, x19, -1                  # rounds--
  bne  x19, x0, round_loop           # repeat
  slli x30, x23, 6                   # off=nlines*64
  add  x30, x30, x20                 # flag_ptr
  addi x31, x0, 1                    # pass=1
  sd   x31, 0(x30)                   # store pass
pass:
  jal  x0, pass                      # halt
fail:
  slli x30, x23, 6                   # off=nlines*64
  add  x30, x30, x20                 # flag_ptr
  addi x31, x0, -1                   # fail=-1
  sd   x31, 0(x30)                   # store fail
  jal  x0, fail                      # halt
