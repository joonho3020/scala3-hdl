.option norvc
.text
.globl _start

_start:
  addi x5, x0, 291               # state
  addi x6, x0, 0                 # scratch/toggle
  lui  x8, 0x12345               # const
  addi x8, x8, 1656              # const low
  jal  x1, warmup_fn             # call warmup
done:
  jal  x0, done                  # halt
warmup_fn:
  addi x7, x0, 16                # iters=16
warmup_loop:
  addi x5, x5, 1                 # state++
  addi x7, x7, -1                # iters--
  bne  x7, x0, warmup_loop       # always-taken backedge
  jalr x0, x1, 0                 # ret
