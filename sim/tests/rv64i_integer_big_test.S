.option norvc
.text
.globl _start

_start:
  lui  x20, 0x80003                  # base = 0x80003000
  addi x1, x0, -1                    # x1 = -1
  addi x2, x0, 1                     # x2 = 1
  addi x3, x0, 0                     # x3 = 0
  addi x5, x0, 1                     # x5 = 1
  slli x5, x5, 63                    # x5 = 1<<63 (highbit mask)
  lui  x6, 0x13579                   # stateA hi
  addi x6, x6, 582                   # stateA lo
  lui  x7, 0x2468a                   # stateB hi
  addi x7, x7, 309                   # stateB lo
  addi x8, x0, 256                   # iters=256
  addi x31, x0, 0                    # checksum=0
loop:
  slli x9, x6, 13                    # t=A<<13
  xor  x6, x6, x9                    # A^=t
  srli x9, x6, 7                     # t=A>>7
  xor  x6, x6, x9                    # A^=t
  slli x9, x6, 17                    # t=A<<17
  xor  x6, x6, x9                    # A^=t
  addi x7, x7, 158                   # B+=0x9e
  srli x9, x7, 11                    # t=B>>11
  xor  x7, x7, x9                    # B^=t
  addi x10, x6, 0                    # a=A
  addi x11, x7, 0                    # b=B
  xor  x12, x10, x11                 # c=a^b
  add  x13, x10, x11                 # t=a+b
  sub  x14, x13, x11                 # u=t-b
  bne  x14, x10, fail                # (a+b)-b==a
  sub  x15, x10, x11                 # t=a-b
  add  x16, x15, x11                 # u=t+b
  bne  x16, x10, fail                # (a-b)+b==a
  xor  x13, x10, x11                 # t=a^b
  xor  x14, x13, x11                 # u=t^b
  bne  x14, x10, fail                # (a^b)^b==a
  xor  x17, x11, x1                  # nb=~b
  and  x13, x10, x11                 # t1=a&b
  and  x14, x10, x17                 # t2=a&~b
  or   x15, x13, x14                 # t=t1|t2
  bne  x15, x10, fail                # (a&b)|(a&~b)==a
  addi x18, x10, 7                   # t=addi a,7
  addi x19, x0, 7                    # k=7
  add  x19, x10, x19                 # u=add a,k
  bne  x18, x19, fail                # ADDI vs ADD
  ori  x18, x12, 90                  # t=ori c,0x5a
  addi x19, x0, 90                   # k=0x5a
  or   x19, x12, x19                 # u=or c,k
  bne  x18, x19, fail                # ORI vs OR
  andi x18, x12, 63                  # t=andi c,0x3f
  addi x19, x0, 63                   # k=0x3f
  and  x19, x12, x19                 # u=and c,k
  bne  x18, x19, fail                # ANDI vs AND
  xori x18, x12, 125                 # t=xori c,0x7d
  addi x19, x0, 125                  # k=0x7d
  xor  x19, x12, x19                 # u=xor c,k
  bne  x18, x19, fail                # XORI vs XOR
  slt  x18, x1, x2                   # slt(-1,1)
  bne  x18, x2, fail                 # expect 1
  sltu x18, x1, x2                   # sltu(-1,1)
  bne  x18, x3, fail                 # expect 0
  blt  x1, x2, ok_blt1               # -1<1
  jal  x0, fail
ok_blt1:
  bge  x2, x1, ok_bge1               # 1>=-1
  jal  x0, fail
ok_bge1:
  bltu x2, x1, ok_bltu1              # 1<u -1
  jal  x0, fail
ok_bltu1:
  bgeu x1, x2, ok_bgeu1              # -1>=u1
  jal  x0, fail
ok_bgeu1:
  slti x18, x1, 0                    # slti(-1,0)
  bne  x18, x2, fail
  sltiu x18, x1, 0                   # sltiu(-1,0)
  bne  x18, x3, fail
  addi x18, x0, -2                   # neg=-2
  srli x19, x18, 1                   # srl1
  srai x21, x18, 1                   # sra1
  and  x19, x19, x5                  # srl1&hb
  and  x21, x21, x5                  # sra1&hb
  bne  x19, x3, fail                 # SRL clears hb
  bne  x21, x5, fail                 # SRA keeps hb
  andi x22, x11, 63                  # shamt=b&63
  addi x23, x0, 1                    # one=1
  sll  x24, x23, x22                 # one<<k
  srl  x25, x24, x22                 # >>k
  bne  x25, x23, fail                # recover 1
  xor  x31, x31, x10                 # chk^=a
  add  x31, x31, x11                 # chk+=b
  xor  x31, x31, x12                 # chk^=c
  addi x8, x8, -1                    # iters--
  bne  x8, x0, loop                  # repeat
  sd   x31, 8(x20)                   # store checksum @base+8
  addi x29, x0, 1                    # pass=1
  sd   x29, 0(x20)                   # store pass @base+0
pass:
  jal  x0, pass                      # halt
fail:
  addi x29, x0, -1                   # fail=-1
  sd   x29, 0(x20)                   # store fail @base+0
  sd   x31, 8(x20)                   # store checksum @base+8
  jal  x0, fail                      # halt
