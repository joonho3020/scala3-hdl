.option norvc
.text
.globl _start

_start:
  addi x5, x0, 291               # state
  addi x6, x0, 0                 # scratch/toggle
  lui  x8, 0x12345               # const
  addi x8, x8, 1656              # const low
  jal  x1, alt_fn                # call alt pattern
done:
  jal  x0, done                  # halt
alt_fn:
  addi x7, x0, 4                 # iters=4
alt_loop:
  xori x6, x6, 1                 # toggle bit0
  beq  x6, x0, alt_taken         # alternating T/NT
alt_nt:
  addi x5, x5, 3                 # nt path
  jal  x0, alt_join
alt_taken:
  addi x5, x5, -5                # t path
alt_join:
  addi x7, x7, -1                # iters--
  bne  x7, x0, alt_loop
  jalr x0, x1, 0                 # ret
