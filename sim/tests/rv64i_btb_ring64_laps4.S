.option norvc
.text
.globl _start

_start:
  addi x5, x0, 291               # state
  addi x6, x0, 0                 # scratch/toggle
  lui  x8, 0x12345               # const
  addi x8, x8, 1656              # const low
  jal  x1, btb_ring              # call BTB ring
done:
  jal  x0, done                  # halt
btb_ring:
  addi x11, x0, 4                # laps=4
  jal  x0, blk0                  # enter ring
blk0:
  andi x9, x5, 1                 # tmp=state&1
  beq  x9, x0, blk0_skip         # local cond br
  addi x5, x5, -7                # state update
blk0_skip:
  jal  x0, blk1                  # next (BTB)
blk1:
  andi x9, x5, 2                 # tmp=state&2
  beq  x9, x0, blk1_skip         # local cond br
  addi x5, x5, -6                # state update
blk1_skip:
  jal  x0, blk2                  # next (BTB)
blk2:
  andi x9, x5, 4                 # tmp=state&4
  beq  x9, x0, blk2_skip         # local cond br
  addi x5, x5, -5                # state update
blk2_skip:
  jal  x0, blk3                  # next (BTB)
blk3:
  andi x9, x5, 8                 # tmp=state&8
  beq  x9, x0, blk3_skip         # local cond br
  addi x5, x5, -4                # state update
blk3_skip:
  jal  x0, blk4                  # next (BTB)
blk4:
  andi x9, x5, 16                # tmp=state&16
  beq  x9, x0, blk4_skip         # local cond br
  addi x5, x5, -3                # state update
blk4_skip:
  jal  x0, blk5                  # next (BTB)
blk5:
  andi x9, x5, 32                # tmp=state&32
  beq  x9, x0, blk5_skip         # local cond br
  addi x5, x5, -2                # state update
blk5_skip:
  jal  x0, blk6                  # next (BTB)
blk6:
  andi x9, x5, 1                 # tmp=state&1
  beq  x9, x0, blk6_skip         # local cond br
  addi x5, x5, -1                # state update
blk6_skip:
  jal  x0, blk7                  # next (BTB)
blk7:
  andi x9, x5, 2                 # tmp=state&2
  beq  x9, x0, blk7_skip         # local cond br
  addi x5, x5, 0                 # state update
blk7_skip:
  jal  x0, blk8                  # next (BTB)
blk8:
  andi x9, x5, 4                 # tmp=state&4
  beq  x9, x0, blk8_skip         # local cond br
  addi x5, x5, 1                 # state update
blk8_skip:
  jal  x0, blk9                  # next (BTB)
blk9:
  andi x9, x5, 8                 # tmp=state&8
  beq  x9, x0, blk9_skip         # local cond br
  addi x5, x5, 2                 # state update
blk9_skip:
  jal  x0, blk10                 # next (BTB)
blk10:
  andi x9, x5, 16                # tmp=state&16
  beq  x9, x0, blk10_skip        # local cond br
  addi x5, x5, 3                 # state update
blk10_skip:
  jal  x0, blk11                 # next (BTB)
blk11:
  andi x9, x5, 32                # tmp=state&32
  beq  x9, x0, blk11_skip        # local cond br
  addi x5, x5, 4                 # state update
blk11_skip:
  jal  x0, blk12                 # next (BTB)
blk12:
  andi x9, x5, 1                 # tmp=state&1
  beq  x9, x0, blk12_skip        # local cond br
  addi x5, x5, 5                 # state update
blk12_skip:
  jal  x0, blk13                 # next (BTB)
blk13:
  andi x9, x5, 2                 # tmp=state&2
  beq  x9, x0, blk13_skip        # local cond br
  addi x5, x5, 6                 # state update
blk13_skip:
  jal  x0, blk14                 # next (BTB)
blk14:
  andi x9, x5, 4                 # tmp=state&4
  beq  x9, x0, blk14_skip        # local cond br
  addi x5, x5, 7                 # state update
blk14_skip:
  jal  x0, blk15                 # next (BTB)
blk15:
  andi x9, x5, 8                 # tmp=state&8
  beq  x9, x0, blk15_skip        # local cond br
  addi x5, x5, 8                 # state update
blk15_skip:
  jal  x0, blk16                 # next (BTB)
blk16:
  andi x9, x5, 16                # tmp=state&16
  beq  x9, x0, blk16_skip        # local cond br
  addi x5, x5, 9                 # state update
blk16_skip:
  jal  x0, blk17                 # next (BTB)
blk17:
  andi x9, x5, 32                # tmp=state&32
  beq  x9, x0, blk17_skip        # local cond br
  addi x5, x5, 10                # state update
blk17_skip:
  jal  x0, blk18                 # next (BTB)
blk18:
  andi x9, x5, 1                 # tmp=state&1
  beq  x9, x0, blk18_skip        # local cond br
  addi x5, x5, 11                # state update
blk18_skip:
  jal  x0, blk19                 # next (BTB)
blk19:
  andi x9, x5, 2                 # tmp=state&2
  beq  x9, x0, blk19_skip        # local cond br
  addi x5, x5, 12                # state update
blk19_skip:
  jal  x0, blk20                 # next (BTB)
blk20:
  andi x9, x5, 4                 # tmp=state&4
  beq  x9, x0, blk20_skip        # local cond br
  addi x5, x5, 13                # state update
blk20_skip:
  jal  x0, blk21                 # next (BTB)
blk21:
  andi x9, x5, 8                 # tmp=state&8
  beq  x9, x0, blk21_skip        # local cond br
  addi x5, x5, 14                # state update
blk21_skip:
  jal  x0, blk22                 # next (BTB)
blk22:
  andi x9, x5, 16                # tmp=state&16
  beq  x9, x0, blk22_skip        # local cond br
  addi x5, x5, 15                # state update
blk22_skip:
  jal  x0, blk23                 # next (BTB)
blk23:
  andi x9, x5, 32                # tmp=state&32
  beq  x9, x0, blk23_skip        # local cond br
  addi x5, x5, 16                # state update
blk23_skip:
  jal  x0, blk24                 # next (BTB)
blk24:
  andi x9, x5, 1                 # tmp=state&1
  beq  x9, x0, blk24_skip        # local cond br
  addi x5, x5, 17                # state update
blk24_skip:
  jal  x0, blk25                 # next (BTB)
blk25:
  andi x9, x5, 2                 # tmp=state&2
  beq  x9, x0, blk25_skip        # local cond br
  addi x5, x5, 18                # state update
blk25_skip:
  jal  x0, blk26                 # next (BTB)
blk26:
  andi x9, x5, 4                 # tmp=state&4
  beq  x9, x0, blk26_skip        # local cond br
  addi x5, x5, 19                # state update
blk26_skip:
  jal  x0, blk27                 # next (BTB)
blk27:
  andi x9, x5, 8                 # tmp=state&8
  beq  x9, x0, blk27_skip        # local cond br
  addi x5, x5, 20                # state update
blk27_skip:
  jal  x0, blk28                 # next (BTB)
blk28:
  andi x9, x5, 16                # tmp=state&16
  beq  x9, x0, blk28_skip        # local cond br
  addi x5, x5, 21                # state update
blk28_skip:
  jal  x0, blk29                 # next (BTB)
blk29:
  andi x9, x5, 32                # tmp=state&32
  beq  x9, x0, blk29_skip        # local cond br
  addi x5, x5, 22                # state update
blk29_skip:
  jal  x0, blk30                 # next (BTB)
blk30:
  andi x9, x5, 1                 # tmp=state&1
  beq  x9, x0, blk30_skip        # local cond br
  addi x5, x5, 23                # state update
blk30_skip:
  jal  x0, blk31                 # next (BTB)
blk31:
  andi x9, x5, 2                 # tmp=state&2
  beq  x9, x0, blk31_skip        # local cond br
  addi x5, x5, 24                # state update
blk31_skip:
  jal  x0, blk32                 # next (BTB)
blk32:
  andi x9, x5, 4                 # tmp=state&4
  beq  x9, x0, blk32_skip        # local cond br
  addi x5, x5, 25                # state update
blk32_skip:
  jal  x0, blk33                 # next (BTB)
blk33:
  andi x9, x5, 8                 # tmp=state&8
  beq  x9, x0, blk33_skip        # local cond br
  addi x5, x5, 26                # state update
blk33_skip:
  jal  x0, blk34                 # next (BTB)
blk34:
  andi x9, x5, 16                # tmp=state&16
  beq  x9, x0, blk34_skip        # local cond br
  addi x5, x5, 27                # state update
blk34_skip:
  jal  x0, blk35                 # next (BTB)
blk35:
  andi x9, x5, 32                # tmp=state&32
  beq  x9, x0, blk35_skip        # local cond br
  addi x5, x5, 28                # state update
blk35_skip:
  jal  x0, blk36                 # next (BTB)
blk36:
  andi x9, x5, 1                 # tmp=state&1
  beq  x9, x0, blk36_skip        # local cond br
  addi x5, x5, 29                # state update
blk36_skip:
  jal  x0, blk37                 # next (BTB)
blk37:
  andi x9, x5, 2                 # tmp=state&2
  beq  x9, x0, blk37_skip        # local cond br
  addi x5, x5, 30                # state update
blk37_skip:
  jal  x0, blk38                 # next (BTB)
blk38:
  andi x9, x5, 4                 # tmp=state&4
  beq  x9, x0, blk38_skip        # local cond br
  addi x5, x5, 31                # state update
blk38_skip:
  jal  x0, blk39                 # next (BTB)
blk39:
  andi x9, x5, 8                 # tmp=state&8
  beq  x9, x0, blk39_skip        # local cond br
  addi x5, x5, 32                # state update
blk39_skip:
  jal  x0, blk40                 # next (BTB)
blk40:
  andi x9, x5, 16                # tmp=state&16
  beq  x9, x0, blk40_skip        # local cond br
  addi x5, x5, 33                # state update
blk40_skip:
  jal  x0, blk41                 # next (BTB)
blk41:
  andi x9, x5, 32                # tmp=state&32
  beq  x9, x0, blk41_skip        # local cond br
  addi x5, x5, 34                # state update
blk41_skip:
  jal  x0, blk42                 # next (BTB)
blk42:
  andi x9, x5, 1                 # tmp=state&1
  beq  x9, x0, blk42_skip        # local cond br
  addi x5, x5, 35                # state update
blk42_skip:
  jal  x0, blk43                 # next (BTB)
blk43:
  andi x9, x5, 2                 # tmp=state&2
  beq  x9, x0, blk43_skip        # local cond br
  addi x5, x5, 36                # state update
blk43_skip:
  jal  x0, blk44                 # next (BTB)
blk44:
  andi x9, x5, 4                 # tmp=state&4
  beq  x9, x0, blk44_skip        # local cond br
  addi x5, x5, 37                # state update
blk44_skip:
  jal  x0, blk45                 # next (BTB)
blk45:
  andi x9, x5, 8                 # tmp=state&8
  beq  x9, x0, blk45_skip        # local cond br
  addi x5, x5, 38                # state update
blk45_skip:
  jal  x0, blk46                 # next (BTB)
blk46:
  andi x9, x5, 16                # tmp=state&16
  beq  x9, x0, blk46_skip        # local cond br
  addi x5, x5, 39                # state update
blk46_skip:
  jal  x0, blk47                 # next (BTB)
blk47:
  andi x9, x5, 32                # tmp=state&32
  beq  x9, x0, blk47_skip        # local cond br
  addi x5, x5, 40                # state update
blk47_skip:
  jal  x0, blk48                 # next (BTB)
blk48:
  andi x9, x5, 1                 # tmp=state&1
  beq  x9, x0, blk48_skip        # local cond br
  addi x5, x5, 41                # state update
blk48_skip:
  jal  x0, blk49                 # next (BTB)
blk49:
  andi x9, x5, 2                 # tmp=state&2
  beq  x9, x0, blk49_skip        # local cond br
  addi x5, x5, 42                # state update
blk49_skip:
  jal  x0, blk50                 # next (BTB)
blk50:
  andi x9, x5, 4                 # tmp=state&4
  beq  x9, x0, blk50_skip        # local cond br
  addi x5, x5, 43                # state update
blk50_skip:
  jal  x0, blk51                 # next (BTB)
blk51:
  andi x9, x5, 8                 # tmp=state&8
  beq  x9, x0, blk51_skip        # local cond br
  addi x5, x5, 44                # state update
blk51_skip:
  jal  x0, blk52                 # next (BTB)
blk52:
  andi x9, x5, 16                # tmp=state&16
  beq  x9, x0, blk52_skip        # local cond br
  addi x5, x5, 45                # state update
blk52_skip:
  jal  x0, blk53                 # next (BTB)
blk53:
  andi x9, x5, 32                # tmp=state&32
  beq  x9, x0, blk53_skip        # local cond br
  addi x5, x5, 46                # state update
blk53_skip:
  jal  x0, blk54                 # next (BTB)
blk54:
  andi x9, x5, 1                 # tmp=state&1
  beq  x9, x0, blk54_skip        # local cond br
  addi x5, x5, 47                # state update
blk54_skip:
  jal  x0, blk55                 # next (BTB)
blk55:
  andi x9, x5, 2                 # tmp=state&2
  beq  x9, x0, blk55_skip        # local cond br
  addi x5, x5, 48                # state update
blk55_skip:
  jal  x0, blk56                 # next (BTB)
blk56:
  andi x9, x5, 4                 # tmp=state&4
  beq  x9, x0, blk56_skip        # local cond br
  addi x5, x5, 49                # state update
blk56_skip:
  jal  x0, blk57                 # next (BTB)
blk57:
  andi x9, x5, 8                 # tmp=state&8
  beq  x9, x0, blk57_skip        # local cond br
  addi x5, x5, 50                # state update
blk57_skip:
  jal  x0, blk58                 # next (BTB)
blk58:
  andi x9, x5, 16                # tmp=state&16
  beq  x9, x0, blk58_skip        # local cond br
  addi x5, x5, 51                # state update
blk58_skip:
  jal  x0, blk59                 # next (BTB)
blk59:
  andi x9, x5, 32                # tmp=state&32
  beq  x9, x0, blk59_skip        # local cond br
  addi x5, x5, 52                # state update
blk59_skip:
  jal  x0, blk60                 # next (BTB)
blk60:
  andi x9, x5, 1                 # tmp=state&1
  beq  x9, x0, blk60_skip        # local cond br
  addi x5, x5, 53                # state update
blk60_skip:
  jal  x0, blk61                 # next (BTB)
blk61:
  andi x9, x5, 2                 # tmp=state&2
  beq  x9, x0, blk61_skip        # local cond br
  addi x5, x5, 54                # state update
blk61_skip:
  jal  x0, blk62                 # next (BTB)
blk62:
  andi x9, x5, 4                 # tmp=state&4
  beq  x9, x0, blk62_skip        # local cond br
  addi x5, x5, 55                # state update
blk62_skip:
  jal  x0, blk63                 # next (BTB)
blk63:
  andi x9, x5, 8                 # tmp=state&8
  beq  x9, x0, blk63_skip        # local cond br
  addi x5, x5, 56                # state update
blk63_skip:
  addi x11, x11, -1              # laps--
  bne  x11, x0, blk_last_more    # repeat ring
  jal  x0, btb_ret               # exit
blk_last_more:
  jal  x0, blk0                  # wrap
btb_ret:
  jalr x0, x1, 0                 # ret
