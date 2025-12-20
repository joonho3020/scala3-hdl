.option norvc
.text
.globl _start

_start:
  addi x5, x0, 291               # state
  addi x6, x0, 0                 # scratch/toggle
  lui  x8, 0x12345               # const
  addi x8, x8, 1656              # const low
  jal  x1, corr_fn               # call correlated
done:
  jal  x0, done                  # halt
corr_fn:
  addi x7, x0, 4                 # iters=4
corr_loop:
  andi x9, x5, 1                 # tmp=state&1
  beq  x9, x0, b1_nt             # B1
b1_t:
  ori  x6, x6, 2                 # set bit1
  jal  x0, b1_join
b1_nt:
  andi x6, x6, -3                # clear bit1
b1_join:
  andi x9, x6, 2                 # tmp=bit1
  beq  x9, x0, b2_nt             # B2 correlated
b2_t:
  addi x5, x5, 7                 # t path
  jal  x0, b2_join
b2_nt:
  addi x5, x5, -7                # nt path
b2_join:
  addi x5, x5, 1                 # state++
  addi x7, x7, -1                # iters--
  bne  x7, x0, corr_loop
  jalr x0, x1, 0                 # ret
