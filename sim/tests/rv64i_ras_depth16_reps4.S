.option norvc
.text
.globl _start

_start:
  addi x5, x0, 291               # state
  addi x6, x0, 0                 # scratch/toggle
  lui  x8, 0x12345               # const
  addi x8, x8, 1656              # const low
  jal  x1, ras_test              # call RAS test
done:
  jal  x0, done                  # halt
leaf0:
  xori x5, x5, 85                # mix
  slli x5, x5, 3                 # shift
  srli x5, x5, 1                 # shift
  jalr x0, x1, 0                 # ret
leaf1:
  xori x5, x5, 358               # mix
  slli x5, x5, 12                # shift
  srli x5, x5, 8                 # shift
  jalr x0, x1, 0                 # ret
ras_test:
  auipc x20, 0                   # addr leaf0
  addi x20, x20, -32
  auipc x21, 0                   # addr leaf1
  addi x21, x21, -24
  addi x10, x0, 4                # reps=4
ras_rep_loop:
  jal  x1, deep0                 # enter deep chain
  jalr x1, x20, 0                # indirect call leaf0
  jalr x1, x21, 0                # indirect call leaf1
  addi x10, x10, -1              # reps--
  bne  x10, x0, ras_rep_loop
  jalr x0, x1, 0                 # ret
deep0:
  andi x9, x5, 1                 # tmp=state&1
  beq  x9, x0, deep0_z           # branch
  addi x5, x5, 1                 # path1
  jal  x0, deep0_j
deep0_z:
  addi x5, x5, -1                # path0
deep0_j:
  jal  x1, deep1                 # call next
  jalr x0, x1, 0                 # ret
deep1:
  andi x9, x5, 1                 # tmp=state&1
  beq  x9, x0, deep1_z           # branch
  addi x5, x5, 1                 # path1
  jal  x0, deep1_j
deep1_z:
  addi x5, x5, -1                # path0
deep1_j:
  jal  x1, deep2                 # call next
  jalr x0, x1, 0                 # ret
deep2:
  andi x9, x5, 1                 # tmp=state&1
  beq  x9, x0, deep2_z           # branch
  addi x5, x5, 1                 # path1
  jal  x0, deep2_j
deep2_z:
  addi x5, x5, -1                # path0
deep2_j:
  jal  x1, deep3                 # call next
  jalr x0, x1, 0                 # ret
deep3:
  andi x9, x5, 1                 # tmp=state&1
  beq  x9, x0, deep3_z           # branch
  addi x5, x5, 1                 # path1
  jal  x0, deep3_j
deep3_z:
  addi x5, x5, -1                # path0
deep3_j:
  jal  x1, deep4                 # call next
  jalr x0, x1, 0                 # ret
deep4:
  andi x9, x5, 1                 # tmp=state&1
  beq  x9, x0, deep4_z           # branch
  addi x5, x5, 1                 # path1
  jal  x0, deep4_j
deep4_z:
  addi x5, x5, -1                # path0
deep4_j:
  jal  x1, deep5                 # call next
  jalr x0, x1, 0                 # ret
deep5:
  andi x9, x5, 1                 # tmp=state&1
  beq  x9, x0, deep5_z           # branch
  addi x5, x5, 1                 # path1
  jal  x0, deep5_j
deep5_z:
  addi x5, x5, -1                # path0
deep5_j:
  jal  x1, deep6                 # call next
  jalr x0, x1, 0                 # ret
deep6:
  andi x9, x5, 1                 # tmp=state&1
  beq  x9, x0, deep6_z           # branch
  addi x5, x5, 1                 # path1
  jal  x0, deep6_j
deep6_z:
  addi x5, x5, -1                # path0
deep6_j:
  jal  x1, deep7                 # call next
  jalr x0, x1, 0                 # ret
deep7:
  andi x9, x5, 1                 # tmp=state&1
  beq  x9, x0, deep7_z           # branch
  addi x5, x5, 1                 # path1
  jal  x0, deep7_j
deep7_z:
  addi x5, x5, -1                # path0
deep7_j:
  jal  x1, deep8                 # call next
  jalr x0, x1, 0                 # ret
deep8:
  andi x9, x5, 1                 # tmp=state&1
  beq  x9, x0, deep8_z           # branch
  addi x5, x5, 1                 # path1
  jal  x0, deep8_j
deep8_z:
  addi x5, x5, -1                # path0
deep8_j:
  jal  x1, deep9                 # call next
  jalr x0, x1, 0                 # ret
deep9:
  andi x9, x5, 1                 # tmp=state&1
  beq  x9, x0, deep9_z           # branch
  addi x5, x5, 1                 # path1
  jal  x0, deep9_j
deep9_z:
  addi x5, x5, -1                # path0
deep9_j:
  jal  x1, deep10                # call next
  jalr x0, x1, 0                 # ret
deep10:
  andi x9, x5, 1                 # tmp=state&1
  beq  x9, x0, deep10_z          # branch
  addi x5, x5, 1                 # path1
  jal  x0, deep10_j
deep10_z:
  addi x5, x5, -1                # path0
deep10_j:
  jal  x1, deep11                # call next
  jalr x0, x1, 0                 # ret
deep11:
  andi x9, x5, 1                 # tmp=state&1
  beq  x9, x0, deep11_z          # branch
  addi x5, x5, 1                 # path1
  jal  x0, deep11_j
deep11_z:
  addi x5, x5, -1                # path0
deep11_j:
  jal  x1, deep12                # call next
  jalr x0, x1, 0                 # ret
deep12:
  andi x9, x5, 1                 # tmp=state&1
  beq  x9, x0, deep12_z          # branch
  addi x5, x5, 1                 # path1
  jal  x0, deep12_j
deep12_z:
  addi x5, x5, -1                # path0
deep12_j:
  jal  x1, deep13                # call next
  jalr x0, x1, 0                 # ret
deep13:
  andi x9, x5, 1                 # tmp=state&1
  beq  x9, x0, deep13_z          # branch
  addi x5, x5, 1                 # path1
  jal  x0, deep13_j
deep13_z:
  addi x5, x5, -1                # path0
deep13_j:
  jal  x1, deep14                # call next
  jalr x0, x1, 0                 # ret
deep14:
  andi x9, x5, 1                 # tmp=state&1
  beq  x9, x0, deep14_z          # branch
  addi x5, x5, 1                 # path1
  jal  x0, deep14_j
deep14_z:
  addi x5, x5, -1                # path0
deep14_j:
  jal  x1, deep15                # call next
  jalr x0, x1, 0                 # ret
deep15:
  andi x9, x5, 1                 # tmp=state&1
  beq  x9, x0, deep15_z          # branch
  addi x5, x5, 1                 # path1
  jal  x0, deep15_j
deep15_z:
  addi x5, x5, -1                # path0
deep15_j:
  jalr x0, x1, 0                 # ret
