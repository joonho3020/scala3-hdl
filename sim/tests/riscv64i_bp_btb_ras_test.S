.option norvc
.text
.globl _start

_start:
  addi x5, x0, 291               # state                           0x80000000
  addi x6, x0, 0                 # history/toggle                  0x80000004
  lui  x8, 0x12345               # const base                      0x80000008
  addi x8, x8, 1656              # const base low                  0x8000000C
  addi x10, x0, 200              # outer iterations                0x80000010
  jal  x1, warmup                # warm up predictor/BTB           0x80000014
main_loop:
  jal  x1, alt_pattern           # alternating branch              0x80000018
  jal  x1, corr_pattern          # correlated branches             0x8000001C
  jal  x1, btb_ras_ring          # BTB + RAS ring                  0x80000020
  addi x10, x10, -1              # outer--                         0x80000024
  bne  x10, x0, main_loop        # repeat
done:
  jal  x0, done                  # halt                            0x80000028
warmup:
  addi x7, x0, 32                # iters                           0x8000002C
warmup_loop:
  addi x5, x5, 1                 # state++                         0x80000030
  addi x7, x7, -1                # iters--                         0x80000034
  bne  x7, x0, warmup_loop       # always taken backedge           0x80000038
  jalr x0, x1, 0                 # ret                             0x8000003C
alt_pattern:
  addi x7, x0, 2048              # iters                           0x80000040
alt_loop:
  xori x6, x6, 1                 # toggle                          0x80000044
  beq  x6, x0, alt_taken         # alternating                     0x80000048
alt_nt:
  addi x5, x5, 3                 # nt path                         0x8000004C
  jal  x0, alt_join              #                                   0x80000050
alt_taken:
  addi x5, x5, -5                # t path                          0x80000054
alt_join:
  addi x7, x7, -1                # iters--                         0x80000058
  bne  x7, x0, alt_loop          #                                   0x8000005C
  jalr x0, x1, 0                 # ret                             0x80000060
corr_pattern:
  addi x7, x0, 2048              # iters                           0x80000064
corr_loop:
  andi x9, x5, 1                 # tmp=state&1                     0x80000068
  beq  x9, x0, b1_nt             # B1                              0x8000006C
b1_t:
  ori  x6, x6, 2                 # set bit1                         0x80000070
  jal  x0, b1_join              #                                   0x80000074
b1_nt:
  andi x6, x6, -3                # clear bit1                       0x80000078
b1_join:
  andi x9, x6, 2                 # tmp=bit1                         0x8000007C
  beq  x9, x0, b2_nt             # B2 correlated                    0x80000080
b2_t:
  addi x5, x5, 7                 # t path                          0x80000084
  jal  x0, b2_join              #                                   0x80000088
b2_nt:
  addi x5, x5, -7                # nt path                         0x8000008C
b2_join:
  addi x5, x5, 1                 # state++                         0x80000090
  addi x7, x7, -1                # iters--                         0x80000094
  bne  x7, x0, corr_loop          #                                   0x80000098
  jalr x0, x1, 0                 # ret                             0x8000009C
ind0:
  xori x5, x5, 90                # mix                             0x800000A0
  slli x5, x5, 3                 # shift bits                       0x800000A4
  srli x5, x5, 1                 # shift bits                       0x800000A8
  jalr x0, x1, 0                 # ret                             0x800000AC
ind1:
  xori x5, x5, 363               # mix                             0x800000B0
  slli x5, x5, 10                # shift bits                       0x800000B4
  srli x5, x5, 6                 # shift bits                       0x800000B8
  jalr x0, x1, 0                 # ret                             0x800000BC
ind2:
  xori x5, x5, 636               # mix                             0x800000C0
  slli x5, x5, 17                # shift bits                       0x800000C4
  srli x5, x5, 11                # shift bits                       0x800000C8
  jalr x0, x1, 0                 # ret                             0x800000CC
ind3:
  xori x5, x5, 909               # mix                             0x800000D0
  slli x5, x5, 24                # shift bits                       0x800000D4
  srli x5, x5, 16                # shift bits                       0x800000D8
  jalr x0, x1, 0                 # ret                             0x800000DC
deep_entry:
  jal  x1, deep0                 # enter deep chain                 0x800000E0
  jalr x0, x1, 0                 # ret                             0x800000E4
deep0:
  andi x9, x5, 63                # tmp                             0x800000E8
  sltiu x9, x9, 32               # tmp<32?                         0x800000EC
  beq  x9, x0, deep0_hi          # branch in frame                 0x800000F0
  addi x5, x5, 1                 # state++                         0x800000F4
  jal  x0, deep0_join            #                                   0x800000F8
deep0_hi:
  addi x5, x5, -1                # state--                         0x800000FC
deep0_join:
  jal  x1, deep1                 # call next                        0x80000100
  jalr x0, x1, 0                 # ret                             0x80000104
deep1:
  andi x9, x5, 63                # tmp                             0x80000108
  sltiu x9, x9, 32               # tmp<32?                         0x8000010C
  beq  x9, x0, deep1_hi          # branch in frame                 0x80000110
  addi x5, x5, 1                 # state++                         0x80000114
  jal  x0, deep1_join            #                                   0x80000118
deep1_hi:
  addi x5, x5, -1                # state--                         0x8000011C
deep1_join:
  jal  x1, deep2                 # call next                        0x80000120
  jalr x0, x1, 0                 # ret                             0x80000124
deep2:
  andi x9, x5, 63                # tmp                             0x80000128
  sltiu x9, x9, 32               # tmp<32?                         0x8000012C
  beq  x9, x0, deep2_hi          # branch in frame                 0x80000130
  addi x5, x5, 1                 # state++                         0x80000134
  jal  x0, deep2_join            #                                   0x80000138
deep2_hi:
  addi x5, x5, -1                #                                 0x8000013C
deep2_join:
  jal  x1, deep3                 # call next                        0x80000140
  jalr x0, x1, 0                 # ret                             0x80000144
deep3:
  andi x9, x5, 63                # tmp                             0x80000148
  sltiu x9, x9, 32               # tmp<32?                         0x8000014C
  beq  x9, x0, deep3_hi          # branch in frame                 0x80000150
  addi x5, x5, 1                 # state++                         0x80000154
  jal  x0, deep3_join            #                                   0x80000158
deep3_hi:
  addi x5, x5, -1                # state--                         0x8000015C
deep3_join:
  jal  x1, deep4                 # call next                        0x80000160
  jalr x0, x1, 0                 # ret                             0x80000164
deep4:
  andi x9, x5, 63                # tmp                             0x80000168
  sltiu x9, x9, 32               # tmp<32?                         0x8000016C
  beq  x9, x0, deep4_hi          # branch in frame                 0x80000170
  addi x5, x5, 1                 # state++                         0x80000174
  jal  x0, deep4_join            #                                   0x80000178
deep4_hi:
  addi x5, x5, -1                # state--                         0x8000017C
deep4_join:
  jal  x1, deep5                 # call next                        0x80000180
  jalr x0, x1, 0                 # ret                             0x80000184
deep5:
  andi x9, x5, 63                # tmp                             0x80000188
  sltiu x9, x9, 32               # tmp<32?                         0x8000018C
  beq  x9, x0, deep5_hi          # branch in frame                 0x80000190
  addi x5, x5, 1                 # state++                         0x80000194
  jal  x0, deep5_join            #                                   0x80000198
deep5_hi:
  addi x5, x5, -1                # state--                         0x8000019C
deep5_join:
  jal  x1, deep6                 # call next                        0x800001A0
  jalr x0, x1, 0                 # ret                             0x800001A4
deep6:
  andi x9, x5, 63                # tmp                             0x800001A8
  sltiu x9, x9, 32               # tmp<32?                         0x800001AC
  beq  x9, x0, deep6_hi          # branch in frame                 0x800001B0
  addi x5, x5, 1                 # state++                         0x800001B4
  jal  x0, deep6_join            #                                   0x800001B8
deep6_hi:
  addi x5, x5, -1                # state--                         0x800001BC
deep6_join:
  jal  x1, deep7                 # call next                        0x800001C0
  jalr x0, x1, 0                 # ret                             0x800001C4
deep7:
  andi x9, x5, 63                # tmp                             0x800001C8
  sltiu x9, x9, 32               # tmp<32?                         0x800001CC
  beq  x9, x0, deep7_hi          # branch in frame                 0x800001D0
  addi x5, x5, 1                 # state++                         0x800001D4
  jal  x0, deep7_join            #                                   0x800001D8
deep7_hi:
  addi x5, x5, -1                # state--                         0x800001DC
deep7_join:
  jalr x1, x20, 0                # indirect call via x20            0x800001E0
  jal  x1, deep8                 # call next                        0x800001E4
  jalr x0, x1, 0                 # ret                             0x800001E8
deep8:
  andi x9, x5, 63                # tmp                             0x800001EC
  sltiu x9, x9, 32               # tmp<32?                         0x800001F0
  beq  x9, x0, deep8_hi          # branch in frame                 0x800001F4
  addi x5, x5, 1                 # state++                         0x800001F8
  jal  x0, deep8_join            #                                   0x800001FC
deep8_hi:
  addi x5, x5, -1                # state--                         0x80000200
deep8_join:
  jal  x1, deep9                 # call next                        0x80000204
  jalr x0, x1, 0                 # ret                             0x80000208
deep9:
  andi x9, x5, 63                # tmp                             0x8000020C
  sltiu x9, x9, 32               # tmp<32?                         0x80000210
  beq  x9, x0, deep9_hi          # branch in frame                 0x80000214
  addi x5, x5, 1                 # state++                         0x80000218
  jal  x0, deep9_join            #                                   0x8000021C
deep9_hi:
  addi x5, x5, -1                # state--                         0x80000220
deep9_join:
  jal  x1, deep10                # call next                        0x80000224
  jalr x0, x1, 0                 # ret                              0x80000228
deep10:
  andi x9, x5, 63                # tmp                             0x8000022C
  sltiu x9, x9, 32               # tmp<32?                         0x80000230
  beq  x9, x0, deep10_hi         # branch in frame                 0x80000234
  addi x5, x5, 1                 # state++                         0x80000238
  jal  x0, deep10_join            #                                   0x8000023C
deep10_hi:
  addi x5, x5, -1                # state--                         0x80000240
deep10_join:
  jal  x1, deep11                # call next                        0x80000244
  jalr x0, x1, 0                 # ret                             0x80000248
deep11:
  andi x9, x5, 63                # tmp                             0x8000024C
  sltiu x9, x9, 32               # tmp<32?                         0x80000250
  beq  x9, x0, deep11_hi         # branch in frame                 0x80000254
  addi x5, x5, 1                 # state++                         0x80000258
  jal  x0, deep11_join            #                                   0x8000025C
deep11_hi:
  addi x5, x5, -1                # state--                         0x80000260
deep11_join:
  jal  x1, deep12                # call next                        0x80000264
  jalr x0, x1, 0                 # ret                             0x80000268
deep12:
  andi x9, x5, 63                # tmp                             0x8000026C
  sltiu x9, x9, 32               # tmp<32?                         0x80000270
  beq  x9, x0, deep12_hi         # branch in frame                 0x80000274
  addi x5, x5, 1                 # state++                         0x80000278
  jal  x0, deep12_join            #                                   0x8000027C
deep12_hi:
  addi x5, x5, -1                # state--                         0x80000280
deep12_join:
  jal  x1, deep13                # call next                        0x80000284
  jalr x0, x1, 0                 # ret                             0x80000288
deep13:
  andi x9, x5, 63                # tmp                             0x8000028C
  sltiu x9, x9, 32               # tmp<32?
  beq  x9, x0, deep13_hi         # branch in frame
  addi x5, x5, 1
  jal  x0, deep13_join
deep13_hi:
  addi x5, x5, -1
deep13_join:
  jal  x1, deep14                # call next
  jalr x0, x1, 0                 # ret
deep14:
  andi x9, x5, 63                # tmp
  sltiu x9, x9, 32               # tmp<32?
  beq  x9, x0, deep14_hi         # branch in frame
  addi x5, x5, 1
  jal  x0, deep14_join
deep14_hi:
  addi x5, x5, -1
deep14_join:
  jal  x1, deep15                # call next
  jalr x0, x1, 0                 # ret
deep15:
  andi x9, x5, 63                # tmp
  sltiu x9, x9, 32               # tmp<32?
  beq  x9, x0, deep15_hi         # branch in frame
  addi x5, x5, 1
  jal  x0, deep15_join
deep15_hi:
  addi x5, x5, -1
deep15_join:
  jal  x1, deep16                # call next
  jalr x0, x1, 0                 # ret
deep16:
  andi x9, x5, 63                # tmp
  sltiu x9, x9, 32               # tmp<32?
  beq  x9, x0, deep16_hi         # branch in frame
  addi x5, x5, 1
  jal  x0, deep16_join
deep16_hi:
  addi x5, x5, -1
deep16_join:
  jal  x1, deep17                # call next
  jalr x0, x1, 0                 # ret
deep17:
  andi x9, x5, 63                # tmp
  sltiu x9, x9, 32               # tmp<32?
  beq  x9, x0, deep17_hi         # branch in frame
  addi x5, x5, 1
  jal  x0, deep17_join
deep17_hi:
  addi x5, x5, -1
deep17_join:
  jal  x1, deep18                # call next
  jalr x0, x1, 0                 # ret
deep18:
  andi x9, x5, 63                # tmp
  sltiu x9, x9, 32               # tmp<32?
  beq  x9, x0, deep18_hi         # branch in frame
  addi x5, x5, 1
  jal  x0, deep18_join
deep18_hi:
  addi x5, x5, -1
deep18_join:
  jal  x1, deep19                # call next
  jalr x0, x1, 0                 # ret
deep19:
  andi x9, x5, 63                # tmp
  sltiu x9, x9, 32               # tmp<32?
  beq  x9, x0, deep19_hi         # branch in frame
  addi x5, x5, 1
  jal  x0, deep19_join
deep19_hi:
  addi x5, x5, -1
deep19_join:
  jalr x1, x21, 0                # indirect call via x21
  jal  x1, deep20                # call next
  jalr x0, x1, 0                 # ret
deep20:
  andi x9, x5, 63                # tmp
  sltiu x9, x9, 32               # tmp<32?
  beq  x9, x0, deep20_hi         # branch in frame
  addi x5, x5, 1
  jal  x0, deep20_join
deep20_hi:
  addi x5, x5, -1
deep20_join:
  jal  x1, deep21                # call next
  jalr x0, x1, 0                 # ret
deep21:
  andi x9, x5, 63                # tmp
  sltiu x9, x9, 32               # tmp<32?
  beq  x9, x0, deep21_hi         # branch in frame
  addi x5, x5, 1
  jal  x0, deep21_join
deep21_hi:
  addi x5, x5, -1
deep21_join:
  jal  x1, deep22                # call next
  jalr x0, x1, 0                 # ret
deep22:
  andi x9, x5, 63                # tmp
  sltiu x9, x9, 32               # tmp<32?
  beq  x9, x0, deep22_hi         # branch in frame
  addi x5, x5, 1
  jal  x0, deep22_join
deep22_hi:
  addi x5, x5, -1
deep22_join:
  jal  x1, deep23                # call next
  jalr x0, x1, 0                 # ret
deep23:
  andi x9, x5, 63                # tmp
  sltiu x9, x9, 32               # tmp<32?
  beq  x9, x0, deep23_hi         # branch in frame
  addi x5, x5, 1
  jal  x0, deep23_join
deep23_hi:
  addi x5, x5, -1
deep23_join:
  jal  x1, deep24                # call next
  jalr x0, x1, 0                 # ret
deep24:
  andi x9, x5, 63                # tmp
  sltiu x9, x9, 32               # tmp<32?
  beq  x9, x0, deep24_hi         # branch in frame
  addi x5, x5, 1
  jal  x0, deep24_join
deep24_hi:
  addi x5, x5, -1
deep24_join:
  jal  x1, deep25                # call next
  jalr x0, x1, 0                 # ret
deep25:
  andi x9, x5, 63                # tmp
  sltiu x9, x9, 32               # tmp<32?
  beq  x9, x0, deep25_hi         # branch in frame
  addi x5, x5, 1
  jal  x0, deep25_join
deep25_hi:
  addi x5, x5, -1
deep25_join:
  jal  x1, deep26                # call next
  jalr x0, x1, 0                 # ret
deep26:
  andi x9, x5, 63                # tmp
  sltiu x9, x9, 32               # tmp<32?
  beq  x9, x0, deep26_hi         # branch in frame
  addi x5, x5, 1
  jal  x0, deep26_join
deep26_hi:
  addi x5, x5, -1
deep26_join:
  jal  x1, deep27                # call next
  jalr x0, x1, 0                 # ret
deep27:
  andi x9, x5, 63                # tmp
  sltiu x9, x9, 32               # tmp<32?
  beq  x9, x0, deep27_hi         # branch in frame
  addi x5, x5, 1
  jal  x0, deep27_join
deep27_hi:
  addi x5, x5, -1
deep27_join:
  jal  x1, deep28                # call next
  jalr x0, x1, 0                 # ret
deep28:
  andi x9, x5, 63                # tmp
  sltiu x9, x9, 32               # tmp<32?
  beq  x9, x0, deep28_hi         # branch in frame
  addi x5, x5, 1
  jal  x0, deep28_join
deep28_hi:
  addi x5, x5, -1
deep28_join:
  jal  x1, deep29                # call next
  jalr x0, x1, 0                 # ret
deep29:
  andi x9, x5, 63                # tmp
  sltiu x9, x9, 32               # tmp<32?
  beq  x9, x0, deep29_hi         # branch in frame
  addi x5, x5, 1
  jal  x0, deep29_join
deep29_hi:
  addi x5, x5, -1
deep29_join:
  jal  x1, deep30                # call next
  jalr x0, x1, 0                 # ret
deep30:
  andi x9, x5, 63                # tmp
  sltiu x9, x9, 32               # tmp<32?
  beq  x9, x0, deep30_hi         # branch in frame
  addi x5, x5, 1
  jal  x0, deep30_join
deep30_hi:
  addi x5, x5, -1
deep30_join:
  jal  x1, deep31                # call next
  jalr x0, x1, 0                 # ret
deep31:
  andi x9, x5, 63                # tmp
  sltiu x9, x9, 32               # tmp<32?
  beq  x9, x0, deep31_hi         # branch in frame
  addi x5, x5, 1
  jal  x0, deep31_join
deep31_hi:
  addi x5, x5, -1
deep31_join:
  jalr x1, x22, 0                # indirect call via x22
  jal  x1, deep32                # call next
  jalr x0, x1, 0                 # ret
deep32:
  andi x9, x5, 63                # tmp
  sltiu x9, x9, 32               # tmp<32?
  beq  x9, x0, deep32_hi         # branch in frame
  addi x5, x5, 1
  jal  x0, deep32_join
deep32_hi:
  addi x5, x5, -1
deep32_join:
  jal  x1, deep33                # call next
  jalr x0, x1, 0                 # ret
deep33:
  andi x9, x5, 63                # tmp
  sltiu x9, x9, 32               # tmp<32?
  beq  x9, x0, deep33_hi         # branch in frame
  addi x5, x5, 1
  jal  x0, deep33_join
deep33_hi:
  addi x5, x5, -1
deep33_join:
  jal  x1, deep34                # call next
  jalr x0, x1, 0                 # ret
deep34:
  andi x9, x5, 63                # tmp
  sltiu x9, x9, 32               # tmp<32?
  beq  x9, x0, deep34_hi         # branch in frame
  addi x5, x5, 1
  jal  x0, deep34_join
deep34_hi:
  addi x5, x5, -1
deep34_join:
  jal  x1, deep35                # call next
  jalr x0, x1, 0                 # ret
deep35:
  andi x9, x5, 63                # tmp
  sltiu x9, x9, 32               # tmp<32?
  beq  x9, x0, deep35_hi         # branch in frame
  addi x5, x5, 1
  jal  x0, deep35_join
deep35_hi:
  addi x5, x5, -1
deep35_join:
  jal  x1, deep36                # call next
  jalr x0, x1, 0                 # ret
deep36:
  andi x9, x5, 63                # tmp
  sltiu x9, x9, 32               # tmp<32?
  beq  x9, x0, deep36_hi         # branch in frame
  addi x5, x5, 1
  jal  x0, deep36_join
deep36_hi:
  addi x5, x5, -1
deep36_join:
  jal  x1, deep37                # call next
  jalr x0, x1, 0                 # ret
deep37:
  andi x9, x5, 63                # tmp
  sltiu x9, x9, 32               # tmp<32?
  beq  x9, x0, deep37_hi         # branch in frame
  addi x5, x5, 1
  jal  x0, deep37_join
deep37_hi:
  addi x5, x5, -1
deep37_join:
  jal  x1, deep38                # call next
  jalr x0, x1, 0                 # ret
deep38:
  andi x9, x5, 63                # tmp
  sltiu x9, x9, 32               # tmp<32?
  beq  x9, x0, deep38_hi         # branch in frame
  addi x5, x5, 1
  jal  x0, deep38_join
deep38_hi:
  addi x5, x5, -1
deep38_join:
  jal  x1, deep39                # call next
  jalr x0, x1, 0                 # ret
deep39:
  andi x9, x5, 63                # tmp
  sltiu x9, x9, 32               # tmp<32?
  beq  x9, x0, deep39_hi         # branch in frame
  addi x5, x5, 1
  jal  x0, deep39_join
deep39_hi:
  addi x5, x5, -1
deep39_join:
  jal  x1, deep40                # call next
  jalr x0, x1, 0                 # ret
deep40:
  andi x9, x5, 63                # tmp
  sltiu x9, x9, 32               # tmp<32?
  beq  x9, x0, deep40_hi         # branch in frame
  addi x5, x5, 1
  jal  x0, deep40_join
deep40_hi:
  addi x5, x5, -1
deep40_join:
  jal  x1, deep41                # call next
  jalr x0, x1, 0                 # ret
deep41:
  andi x9, x5, 63                # tmp
  sltiu x9, x9, 32               # tmp<32?
  beq  x9, x0, deep41_hi         # branch in frame
  addi x5, x5, 1
  jal  x0, deep41_join
deep41_hi:
  addi x5, x5, -1
deep41_join:
  jal  x1, deep42                # call next
  jalr x0, x1, 0                 # ret
deep42:
  andi x9, x5, 63                # tmp
  sltiu x9, x9, 32               # tmp<32?
  beq  x9, x0, deep42_hi         # branch in frame
  addi x5, x5, 1
  jal  x0, deep42_join
deep42_hi:
  addi x5, x5, -1
deep42_join:
  jal  x1, deep43                # call next
  jalr x0, x1, 0                 # ret
deep43:
  andi x9, x5, 63                # tmp
  sltiu x9, x9, 32               # tmp<32?
  beq  x9, x0, deep43_hi         # branch in frame
  addi x5, x5, 1
  jal  x0, deep43_join
deep43_hi:
  addi x5, x5, -1
deep43_join:
  jalr x1, x23, 0                # indirect call via x23
  jal  x1, deep44                # call next
  jalr x0, x1, 0                 # ret
deep44:
  andi x9, x5, 63                # tmp
  sltiu x9, x9, 32               # tmp<32?
  beq  x9, x0, deep44_hi         # branch in frame
  addi x5, x5, 1
  jal  x0, deep44_join
deep44_hi:
  addi x5, x5, -1
deep44_join:
  jal  x1, deep45                # call next
  jalr x0, x1, 0                 # ret
deep45:
  andi x9, x5, 63                # tmp
  sltiu x9, x9, 32               # tmp<32?
  beq  x9, x0, deep45_hi         # branch in frame
  addi x5, x5, 1
  jal  x0, deep45_join
deep45_hi:
  addi x5, x5, -1
deep45_join:
  jal  x1, deep46                # call next
  jalr x0, x1, 0                 # ret
deep46:
  andi x9, x5, 63                # tmp
  sltiu x9, x9, 32               # tmp<32?
  beq  x9, x0, deep46_hi         # branch in frame
  addi x5, x5, 1
  jal  x0, deep46_join
deep46_hi:
  addi x5, x5, -1
deep46_join:
  jal  x1, deep47                # call next
  jalr x0, x1, 0                 # ret
deep47:
  andi x9, x5, 63                # tmp
  sltiu x9, x9, 32               # tmp<32?
  beq  x9, x0, deep47_hi         # branch in frame
  addi x5, x5, 1
  jal  x0, deep47_join
deep47_hi:
  addi x5, x5, -1
deep47_join:
  jalr x0, x1, 0                 # ret
btb_ras_ring:
  auipc x20, 0                   # addr ind0
  addi x20, x20, -1620
  auipc x21, 0                   # addr ind1
  addi x21, x21, -1612
  auipc x22, 0                   # addr ind2
  addi x22, x22, -1604
  auipc x23, 0                   # addr ind3
  addi x23, x23, -1596
  addi x11, x0, 96               # laps
  jal  x0, blk0                  # enter ring
blk0:
  andi x9, x5, 1                 # tmp=state&1
  beq  x9, x0, blk0_skip         # cond branch
  addi x5, x5, -97               # update state
blk0_skip:
  jal  x1, deep_entry            # RAS overflow
  jal  x0, blk1                  # next
blk1:
  andi x9, x5, 2                 # tmp=state&2
  beq  x9, x0, blk1_skip         # cond branch
  addi x5, x5, -84               # update state
blk1_skip:
  jal  x0, blk2                  # next
blk2:
  andi x9, x5, 4                 # tmp=state&4
  beq  x9, x0, blk2_skip         # cond branch
  addi x5, x5, -71               # update state
blk2_skip:
  jal  x0, blk3                  # next
blk3:
  andi x9, x5, 8                 # tmp=state&8
  beq  x9, x0, blk3_skip         # cond branch
  addi x5, x5, -58               # update state
blk3_skip:
  jal  x0, blk4                  # next
blk4:
  andi x9, x5, 16                # tmp=state&16
  beq  x9, x0, blk4_skip         # cond branch
  addi x5, x5, -45               # update state
blk4_skip:
  jal  x0, blk5                  # next
blk5:
  andi x9, x5, 32                # tmp=state&32
  beq  x9, x0, blk5_skip         # cond branch
  addi x5, x5, -32               # update state
blk5_skip:
  jal  x0, blk6                  # next
blk6:
  andi x9, x5, 64                # tmp=state&64
  beq  x9, x0, blk6_skip         # cond branch
  addi x5, x5, -19               # update state
blk6_skip:
  jal  x0, blk7                  # next
blk7:
  andi x9, x5, 128               # tmp=state&128
  beq  x9, x0, blk7_skip         # cond branch
  addi x5, x5, -6                # update state
blk7_skip:
  jal  x0, blk8                  # next
blk8:
  andi x9, x5, 256               # tmp=state&256
  beq  x9, x0, blk8_skip         # cond branch
  addi x5, x5, 7                 # update state
blk8_skip:
  jal  x0, blk9                  # next
blk9:
  andi x9, x5, 512               # tmp=state&512
  beq  x9, x0, blk9_skip         # cond branch
  addi x5, x5, 20                # update state
blk9_skip:
  jal  x0, blk10                 # next
blk10:
  andi x9, x5, 1024              # tmp=state&1024
  beq  x9, x0, blk10_skip        # cond branch
  addi x5, x5, 33                # update state
blk10_skip:
  jal  x0, blk11                 # next
blk11:
  andi x9, x5, 1                 # tmp=state&1
  beq  x9, x0, blk11_skip        # cond branch
  addi x5, x5, 46                # update state
blk11_skip:
  jalr x1, x20, 0                # ind call ind0
  jal  x0, blk12                 # next
blk12:
  andi x9, x5, 2                 # tmp=state&2
  beq  x9, x0, blk12_skip        # cond branch
  addi x5, x5, 59                # update state
blk12_skip:
  jal  x0, blk13                 # next
blk13:
  andi x9, x5, 4                 # tmp=state&4
  beq  x9, x0, blk13_skip        # cond branch
  addi x5, x5, 72                # update state
blk13_skip:
  jal  x0, blk14                 # next
blk14:
  andi x9, x5, 8                 # tmp=state&8
  beq  x9, x0, blk14_skip        # cond branch
  addi x5, x5, 85                # update state
blk14_skip:
  jal  x0, blk15                 # next
blk15:
  andi x9, x5, 16                # tmp=state&16
  beq  x9, x0, blk15_skip        # cond branch
  addi x5, x5, 98                # update state
blk15_skip:
  jal  x0, blk16                 # next
blk16:
  andi x9, x5, 32                # tmp=state&32
  beq  x9, x0, blk16_skip        # cond branch
  addi x5, x5, 111               # update state
blk16_skip:
  jal  x0, blk17                 # next
blk17:
  andi x9, x5, 64                # tmp=state&64
  beq  x9, x0, blk17_skip        # cond branch
  addi x5, x5, 124               # update state
blk17_skip:
  jal  x0, blk18                 # next
blk18:
  andi x9, x5, 128               # tmp=state&128
  beq  x9, x0, blk18_skip        # cond branch
  addi x5, x5, 137               # update state
blk18_skip:
  jal  x0, blk19                 # next
blk19:
  andi x9, x5, 256               # tmp=state&256
  beq  x9, x0, blk19_skip        # cond branch
  addi x5, x5, 150               # update state
blk19_skip:
  jalr x1, x21, 0                # ind call ind1
  jal  x0, blk20                 # next
blk20:
  andi x9, x5, 512               # tmp=state&512
  beq  x9, x0, blk20_skip        # cond branch
  addi x5, x5, 163               # update state
blk20_skip:
  jal  x0, blk21                 # next
blk21:
  andi x9, x5, 1024              # tmp=state&1024
  beq  x9, x0, blk21_skip        # cond branch
  addi x5, x5, 176               # update state
blk21_skip:
  jal  x0, blk22                 # next
blk22:
  andi x9, x5, 1                 # tmp=state&1
  beq  x9, x0, blk22_skip        # cond branch
  addi x5, x5, 189               # update state
blk22_skip:
  jal  x0, blk23                 # next
blk23:
  andi x9, x5, 2                 # tmp=state&2
  beq  x9, x0, blk23_skip        # cond branch
  addi x5, x5, 202               # update state
blk23_skip:
  jal  x0, blk24                 # next
blk24:
  andi x9, x5, 4                 # tmp=state&4
  beq  x9, x0, blk24_skip        # cond branch
  addi x5, x5, 215               # update state
blk24_skip:
  jal  x0, blk25                 # next
blk25:
  andi x9, x5, 8                 # tmp=state&8
  beq  x9, x0, blk25_skip        # cond branch
  addi x5, x5, 228               # update state
blk25_skip:
  jal  x0, blk26                 # next
blk26:
  andi x9, x5, 16                # tmp=state&16
  beq  x9, x0, blk26_skip        # cond branch
  addi x5, x5, 241               # update state
blk26_skip:
  jal  x0, blk27                 # next
blk27:
  andi x9, x5, 32                # tmp=state&32
  beq  x9, x0, blk27_skip        # cond branch
  addi x5, x5, 254               # update state
blk27_skip:
  jalr x1, x22, 0                # ind call ind2
  jal  x0, blk28                 # next
blk28:
  andi x9, x5, 64                # tmp=state&64
  beq  x9, x0, blk28_skip        # cond branch
  addi x5, x5, 267               # update state
blk28_skip:
  jal  x0, blk29                 # next
blk29:
  andi x9, x5, 128               # tmp=state&128
  beq  x9, x0, blk29_skip        # cond branch
  addi x5, x5, 280               # update state
blk29_skip:
  jal  x0, blk30                 # next
blk30:
  andi x9, x5, 256               # tmp=state&256
  beq  x9, x0, blk30_skip        # cond branch
  addi x5, x5, 293               # update state
blk30_skip:
  jal  x0, blk31                 # next
blk31:
  andi x9, x5, 512               # tmp=state&512
  beq  x9, x0, blk31_skip        # cond branch
  addi x5, x5, 306               # update state
blk31_skip:
  jal  x0, blk32                 # next
blk32:
  andi x9, x5, 1024              # tmp=state&1024
  beq  x9, x0, blk32_skip        # cond branch
  addi x5, x5, 319               # update state
blk32_skip:
  jal  x1, deep_entry            # RAS overflow
  jal  x0, blk33                 # next
blk33:
  andi x9, x5, 1                 # tmp=state&1
  beq  x9, x0, blk33_skip        # cond branch
  addi x5, x5, 332               # update state
blk33_skip:
  jal  x0, blk34                 # next
blk34:
  andi x9, x5, 2                 # tmp=state&2
  beq  x9, x0, blk34_skip        # cond branch
  addi x5, x5, 345               # update state
blk34_skip:
  jal  x0, blk35                 # next
blk35:
  andi x9, x5, 4                 # tmp=state&4
  beq  x9, x0, blk35_skip        # cond branch
  addi x5, x5, 358               # update state
blk35_skip:
  jal  x0, blk36                 # next
blk36:
  andi x9, x5, 8                 # tmp=state&8
  beq  x9, x0, blk36_skip        # cond branch
  addi x5, x5, 371               # update state
blk36_skip:
  jal  x0, blk37                 # next
blk37:
  andi x9, x5, 16                # tmp=state&16
  beq  x9, x0, blk37_skip        # cond branch
  addi x5, x5, 384               # update state
blk37_skip:
  jal  x0, blk38                 # next
blk38:
  andi x9, x5, 32                # tmp=state&32
  beq  x9, x0, blk38_skip        # cond branch
  addi x5, x5, 397               # update state
blk38_skip:
  jal  x0, blk39                 # next
blk39:
  andi x9, x5, 64                # tmp=state&64
  beq  x9, x0, blk39_skip        # cond branch
  addi x5, x5, 410               # update state
blk39_skip:
  jal  x0, blk40                 # next
blk40:
  andi x9, x5, 128               # tmp=state&128
  beq  x9, x0, blk40_skip        # cond branch
  addi x5, x5, 423               # update state
blk40_skip:
  jal  x0, blk41                 # next
blk41:
  andi x9, x5, 256               # tmp=state&256
  beq  x9, x0, blk41_skip        # cond branch
  addi x5, x5, 436               # update state
blk41_skip:
  jal  x0, blk42                 # next
blk42:
  andi x9, x5, 512               # tmp=state&512
  beq  x9, x0, blk42_skip        # cond branch
  addi x5, x5, 449               # update state
blk42_skip:
  jal  x0, blk43                 # next
blk43:
  andi x9, x5, 1024              # tmp=state&1024
  beq  x9, x0, blk43_skip        # cond branch
  addi x5, x5, 462               # update state
blk43_skip:
  jalr x1, x20, 0                # ind call ind0
  jal  x0, blk44                 # next
blk44:
  andi x9, x5, 1                 # tmp=state&1
  beq  x9, x0, blk44_skip        # cond branch
  addi x5, x5, 475               # update state
blk44_skip:
  jal  x0, blk45                 # next
blk45:
  andi x9, x5, 2                 # tmp=state&2
  beq  x9, x0, blk45_skip        # cond branch
  addi x5, x5, 488               # update state
blk45_skip:
  jal  x0, blk46                 # next
blk46:
  andi x9, x5, 4                 # tmp=state&4
  beq  x9, x0, blk46_skip        # cond branch
  addi x5, x5, 501               # update state
blk46_skip:
  jal  x0, blk47                 # next
blk47:
  andi x9, x5, 8                 # tmp=state&8
  beq  x9, x0, blk47_skip        # cond branch
  addi x5, x5, 514               # update state
blk47_skip:
  jal  x0, blk48                 # next
blk48:
  andi x9, x5, 16                # tmp=state&16
  beq  x9, x0, blk48_skip        # cond branch
  addi x5, x5, 527               # update state
blk48_skip:
  jal  x0, blk49                 # next
blk49:
  andi x9, x5, 32                # tmp=state&32
  beq  x9, x0, blk49_skip        # cond branch
  addi x5, x5, 540               # update state
blk49_skip:
  jal  x0, blk50                 # next
blk50:
  andi x9, x5, 64                # tmp=state&64
  beq  x9, x0, blk50_skip        # cond branch
  addi x5, x5, 553               # update state
blk50_skip:
  jal  x0, blk51                 # next
blk51:
  andi x9, x5, 128               # tmp=state&128
  beq  x9, x0, blk51_skip        # cond branch
  addi x5, x5, 566               # update state
blk51_skip:
  jalr x1, x21, 0                # ind call ind1
  jal  x0, blk52                 # next
blk52:
  andi x9, x5, 256               # tmp=state&256
  beq  x9, x0, blk52_skip        # cond branch
  addi x5, x5, 579               # update state
blk52_skip:
  jal  x0, blk53                 # next
blk53:
  andi x9, x5, 512               # tmp=state&512
  beq  x9, x0, blk53_skip        # cond branch
  addi x5, x5, 592               # update state
blk53_skip:
  jal  x0, blk54                 # next
blk54:
  andi x9, x5, 1024              # tmp=state&1024
  beq  x9, x0, blk54_skip        # cond branch
  addi x5, x5, 605               # update state
blk54_skip:
  jal  x0, blk55                 # next
blk55:
  andi x9, x5, 1                 # tmp=state&1
  beq  x9, x0, blk55_skip        # cond branch
  addi x5, x5, 618               # update state
blk55_skip:
  jal  x0, blk56                 # next
blk56:
  andi x9, x5, 2                 # tmp=state&2
  beq  x9, x0, blk56_skip        # cond branch
  addi x5, x5, 631               # update state
blk56_skip:
  jal  x0, blk57                 # next
blk57:
  andi x9, x5, 4                 # tmp=state&4
  beq  x9, x0, blk57_skip        # cond branch
  addi x5, x5, 644               # update state
blk57_skip:
  jal  x0, blk58                 # next
blk58:
  andi x9, x5, 8                 # tmp=state&8
  beq  x9, x0, blk58_skip        # cond branch
  addi x5, x5, 657               # update state
blk58_skip:
  jal  x0, blk59                 # next
blk59:
  andi x9, x5, 16                # tmp=state&16
  beq  x9, x0, blk59_skip        # cond branch
  addi x5, x5, 670               # update state
blk59_skip:
  jalr x1, x22, 0                # ind call ind2
  jal  x0, blk60                 # next
blk60:
  andi x9, x5, 32                # tmp=state&32
  beq  x9, x0, blk60_skip        # cond branch
  addi x5, x5, 683               # update state
blk60_skip:
  jal  x0, blk61                 # next
blk61:
  andi x9, x5, 64                # tmp=state&64
  beq  x9, x0, blk61_skip        # cond branch
  addi x5, x5, 696               # update state
blk61_skip:
  jal  x0, blk62                 # next
blk62:
  andi x9, x5, 128               # tmp=state&128
  beq  x9, x0, blk62_skip        # cond branch
  addi x5, x5, 709               # update state
blk62_skip:
  jal  x0, blk63                 # next
blk63:
  andi x9, x5, 256               # tmp=state&256
  beq  x9, x0, blk63_skip        # cond branch
  addi x5, x5, 722               # update state
blk63_skip:
  jal  x0, blk64                 # next
blk64:
  andi x9, x5, 512               # tmp=state&512
  beq  x9, x0, blk64_skip        # cond branch
  addi x5, x5, 735               # update state
blk64_skip:
  jal  x1, deep_entry            # RAS overflow
  jal  x0, blk65                 # next
blk65:
  andi x9, x5, 1024              # tmp=state&1024
  beq  x9, x0, blk65_skip        # cond branch
  addi x5, x5, 748               # update state
blk65_skip:
  jal  x0, blk66                 # next
blk66:
  andi x9, x5, 1                 # tmp=state&1
  beq  x9, x0, blk66_skip        # cond branch
  addi x5, x5, 761               # update state
blk66_skip:
  jal  x0, blk67                 # next
blk67:
  andi x9, x5, 2                 # tmp=state&2
  beq  x9, x0, blk67_skip        # cond branch
  addi x5, x5, 774               # update state
blk67_skip:
  jal  x0, blk68                 # next
blk68:
  andi x9, x5, 4                 # tmp=state&4
  beq  x9, x0, blk68_skip        # cond branch
  addi x5, x5, 787               # update state
blk68_skip:
  jal  x0, blk69                 # next
blk69:
  andi x9, x5, 8                 # tmp=state&8
  beq  x9, x0, blk69_skip        # cond branch
  addi x5, x5, 800               # update state
blk69_skip:
  jal  x0, blk70                 # next
blk70:
  andi x9, x5, 16                # tmp=state&16
  beq  x9, x0, blk70_skip        # cond branch
  addi x5, x5, 813               # update state
blk70_skip:
  jal  x0, blk71                 # next
blk71:
  andi x9, x5, 32                # tmp=state&32
  beq  x9, x0, blk71_skip        # cond branch
  addi x5, x5, 826               # update state
blk71_skip:
  jal  x0, blk72                 # next
blk72:
  andi x9, x5, 64                # tmp=state&64
  beq  x9, x0, blk72_skip        # cond branch
  addi x5, x5, 839               # update state
blk72_skip:
  jal  x0, blk73                 # next
blk73:
  andi x9, x5, 128               # tmp=state&128
  beq  x9, x0, blk73_skip        # cond branch
  addi x5, x5, 852               # update state
blk73_skip:
  jal  x0, blk74                 # next
blk74:
  andi x9, x5, 256               # tmp=state&256
  beq  x9, x0, blk74_skip        # cond branch
  addi x5, x5, 865               # update state
blk74_skip:
  jal  x0, blk75                 # next
blk75:
  andi x9, x5, 512               # tmp=state&512
  beq  x9, x0, blk75_skip        # cond branch
  addi x5, x5, 878               # update state
blk75_skip:
  jalr x1, x20, 0                # ind call ind0
  jal  x0, blk76                 # next
blk76:
  andi x9, x5, 1024              # tmp=state&1024
  beq  x9, x0, blk76_skip        # cond branch
  addi x5, x5, 891               # update state
blk76_skip:
  jal  x0, blk77                 # next
blk77:
  andi x9, x5, 1                 # tmp=state&1
  beq  x9, x0, blk77_skip        # cond branch
  addi x5, x5, 904               # update state
blk77_skip:
  jal  x0, blk78                 # next
blk78:
  andi x9, x5, 2                 # tmp=state&2
  beq  x9, x0, blk78_skip        # cond branch
  addi x5, x5, 917               # update state
blk78_skip:
  jal  x0, blk79                 # next
blk79:
  andi x9, x5, 4                 # tmp=state&4
  beq  x9, x0, blk79_skip        # cond branch
  addi x5, x5, 930               # update state
blk79_skip:
  jal  x0, blk80                 # next
blk80:
  andi x9, x5, 8                 # tmp=state&8
  beq  x9, x0, blk80_skip        # cond branch
  addi x5, x5, 943               # update state
blk80_skip:
  jal  x0, blk81                 # next
blk81:
  andi x9, x5, 16                # tmp=state&16
  beq  x9, x0, blk81_skip        # cond branch
  addi x5, x5, 956               # update state
blk81_skip:
  jal  x0, blk82                 # next
blk82:
  andi x9, x5, 32                # tmp=state&32
  beq  x9, x0, blk82_skip        # cond branch
  addi x5, x5, 969               # update state
blk82_skip:
  jal  x0, blk83                 # next
blk83:
  andi x9, x5, 64                # tmp=state&64
  beq  x9, x0, blk83_skip        # cond branch
  addi x5, x5, 982               # update state
blk83_skip:
  jalr x1, x21, 0                # ind call ind1
  jal  x0, blk84                 # next
blk84:
  andi x9, x5, 128               # tmp=state&128
  beq  x9, x0, blk84_skip        # cond branch
  addi x5, x5, 995               # update state
blk84_skip:
  jal  x0, blk85                 # next
blk85:
  andi x9, x5, 256               # tmp=state&256
  beq  x9, x0, blk85_skip        # cond branch
  addi x5, x5, 1008              # update state
blk85_skip:
  jal  x0, blk86                 # next
blk86:
  andi x9, x5, 512               # tmp=state&512
  beq  x9, x0, blk86_skip        # cond branch
  addi x5, x5, 1021              # update state
blk86_skip:
  jal  x0, blk87                 # next
blk87:
  andi x9, x5, 1024              # tmp=state&1024
  beq  x9, x0, blk87_skip        # cond branch
  addi x5, x5, 1034              # update state
blk87_skip:
  jal  x0, blk88                 # next
blk88:
  andi x9, x5, 1                 # tmp=state&1
  beq  x9, x0, blk88_skip        # cond branch
  addi x5, x5, 1047              # update state
blk88_skip:
  jal  x0, blk89                 # next
blk89:
  andi x9, x5, 2                 # tmp=state&2
  beq  x9, x0, blk89_skip        # cond branch
  addi x5, x5, 1060              # update state
blk89_skip:
  jal  x0, blk90                 # next
blk90:
  andi x9, x5, 4                 # tmp=state&4
  beq  x9, x0, blk90_skip        # cond branch
  addi x5, x5, 1073              # update state
blk90_skip:
  jal  x0, blk91                 # next
blk91:
  andi x9, x5, 8                 # tmp=state&8
  beq  x9, x0, blk91_skip        # cond branch
  addi x5, x5, 1086              # update state
blk91_skip:
  jalr x1, x22, 0                # ind call ind2
  jal  x0, blk92                 # next
blk92:
  andi x9, x5, 16                # tmp=state&16
  beq  x9, x0, blk92_skip        # cond branch
  addi x5, x5, 1099              # update state
blk92_skip:
  jal  x0, blk93                 # next
blk93:
  andi x9, x5, 32                # tmp=state&32
  beq  x9, x0, blk93_skip        # cond branch
  addi x5, x5, 1112              # update state
blk93_skip:
  jal  x0, blk94                 # next
blk94:
  andi x9, x5, 64                # tmp=state&64
  beq  x9, x0, blk94_skip        # cond branch
  addi x5, x5, 1125              # update state
blk94_skip:
  jal  x0, blk95                 # next
blk95:
  andi x9, x5, 128               # tmp=state&128
  beq  x9, x0, blk95_skip        # cond branch
  addi x5, x5, 1138              # update state
blk95_skip:
  jal  x0, blk96                 # next
blk96:
  andi x9, x5, 256               # tmp=state&256
  beq  x9, x0, blk96_skip        # cond branch
  addi x5, x5, 1151              # update state
blk96_skip:
  jal  x1, deep_entry            # RAS overflow
  jal  x0, blk97                 # next
blk97:
  andi x9, x5, 512               # tmp=state&512
  beq  x9, x0, blk97_skip        # cond branch
  addi x5, x5, 1164              # update state
blk97_skip:
  jal  x0, blk98                 # next
blk98:
  andi x9, x5, 1024              # tmp=state&1024
  beq  x9, x0, blk98_skip        # cond branch
  addi x5, x5, 1177              # update state
blk98_skip:
  jal  x0, blk99                 # next
blk99:
  andi x9, x5, 1                 # tmp=state&1
  beq  x9, x0, blk99_skip        # cond branch
  addi x5, x5, 1190              # update state
blk99_skip:
  jal  x0, blk100                # next
blk100:
  andi x9, x5, 2                 # tmp=state&2
  beq  x9, x0, blk100_skip       # cond branch
  addi x5, x5, 1203              # update state
blk100_skip:
  jal  x0, blk101                # next
blk101:
  andi x9, x5, 4                 # tmp=state&4
  beq  x9, x0, blk101_skip       # cond branch
  addi x5, x5, 1216              # update state
blk101_skip:
  jal  x0, blk102                # next
blk102:
  andi x9, x5, 8                 # tmp=state&8
  beq  x9, x0, blk102_skip       # cond branch
  addi x5, x5, 1229              # update state
blk102_skip:
  jal  x0, blk103                # next
blk103:
  andi x9, x5, 16                # tmp=state&16
  beq  x9, x0, blk103_skip       # cond branch
  addi x5, x5, 1242              # update state
blk103_skip:
  jal  x0, blk104                # next
blk104:
  andi x9, x5, 32                # tmp=state&32
  beq  x9, x0, blk104_skip       # cond branch
  addi x5, x5, 1255              # update state
blk104_skip:
  jal  x0, blk105                # next
blk105:
  andi x9, x5, 64                # tmp=state&64
  beq  x9, x0, blk105_skip       # cond branch
  addi x5, x5, 1268              # update state
blk105_skip:
  jal  x0, blk106                # next
blk106:
  andi x9, x5, 128               # tmp=state&128
  beq  x9, x0, blk106_skip       # cond branch
  addi x5, x5, 1281              # update state
blk106_skip:
  jal  x0, blk107                # next
blk107:
  andi x9, x5, 256               # tmp=state&256
  beq  x9, x0, blk107_skip       # cond branch
  addi x5, x5, 1294              # update state
blk107_skip:
  jalr x1, x20, 0                # ind call ind0
  jal  x0, blk108                # next
blk108:
  andi x9, x5, 512               # tmp=state&512
  beq  x9, x0, blk108_skip       # cond branch
  addi x5, x5, 1307              # update state
blk108_skip:
  jal  x0, blk109                # next
blk109:
  andi x9, x5, 1024              # tmp=state&1024
  beq  x9, x0, blk109_skip       # cond branch
  addi x5, x5, 1320              # update state
blk109_skip:
  jal  x0, blk110                # next
blk110:
  andi x9, x5, 1                 # tmp=state&1
  beq  x9, x0, blk110_skip       # cond branch
  addi x5, x5, 1333              # update state
blk110_skip:
  jal  x0, blk111                # next
blk111:
  andi x9, x5, 2                 # tmp=state&2
  beq  x9, x0, blk111_skip       # cond branch
  addi x5, x5, 1346              # update state
blk111_skip:
  jal  x0, blk112                # next
blk112:
  andi x9, x5, 4                 # tmp=state&4
  beq  x9, x0, blk112_skip       # cond branch
  addi x5, x5, 1359              # update state
blk112_skip:
  jal  x0, blk113                # next
blk113:
  andi x9, x5, 8                 # tmp=state&8
  beq  x9, x0, blk113_skip       # cond branch
  addi x5, x5, 1372              # update state
blk113_skip:
  jal  x0, blk114                # next
blk114:
  andi x9, x5, 16                # tmp=state&16
  beq  x9, x0, blk114_skip       # cond branch
  addi x5, x5, 1385              # update state
blk114_skip:
  jal  x0, blk115                # next
blk115:
  andi x9, x5, 32                # tmp=state&32
  beq  x9, x0, blk115_skip       # cond branch
  addi x5, x5, 1398              # update state
blk115_skip:
  jalr x1, x21, 0                # ind call ind1
  jal  x0, blk116                # next
blk116:
  andi x9, x5, 64                # tmp=state&64
  beq  x9, x0, blk116_skip       # cond branch
  addi x5, x5, 1411              # update state
blk116_skip:
  jal  x0, blk117                # next
blk117:
  andi x9, x5, 128               # tmp=state&128
  beq  x9, x0, blk117_skip       # cond branch
  addi x5, x5, 1424              # update state
blk117_skip:
  jal  x0, blk118                # next
blk118:
  andi x9, x5, 256               # tmp=state&256
  beq  x9, x0, blk118_skip       # cond branch
  addi x5, x5, 1437              # update state
blk118_skip:
  jal  x0, blk119                # next
blk119:
  andi x9, x5, 512               # tmp=state&512
  beq  x9, x0, blk119_skip       # cond branch
  addi x5, x5, 1450              # update state
blk119_skip:
  jal  x0, blk120                # next
blk120:
  andi x9, x5, 1024              # tmp=state&1024
  beq  x9, x0, blk120_skip       # cond branch
  addi x5, x5, 1463              # update state
blk120_skip:
  jal  x0, blk121                # next
blk121:
  andi x9, x5, 1                 # tmp=state&1
  beq  x9, x0, blk121_skip       # cond branch
  addi x5, x5, 1476              # update state
blk121_skip:
  jal  x0, blk122                # next
blk122:
  andi x9, x5, 2                 # tmp=state&2
  beq  x9, x0, blk122_skip       # cond branch
  addi x5, x5, 1489              # update state
blk122_skip:
  jal  x0, blk123                # next
blk123:
  andi x9, x5, 4                 # tmp=state&4
  beq  x9, x0, blk123_skip       # cond branch
  addi x5, x5, 1502              # update state
blk123_skip:
  jalr x1, x22, 0                # ind call ind2
  jal  x0, blk124                # next
blk124:
  andi x9, x5, 8                 # tmp=state&8
  beq  x9, x0, blk124_skip       # cond branch
  addi x5, x5, 1515              # update state
blk124_skip:
  jal  x0, blk125                # next
blk125:
  andi x9, x5, 16                # tmp=state&16
  beq  x9, x0, blk125_skip       # cond branch
  addi x5, x5, 1528              # update state
blk125_skip:
  jal  x0, blk126                # next
blk126:
  andi x9, x5, 32                # tmp=state&32
  beq  x9, x0, blk126_skip       # cond branch
  addi x5, x5, 1541              # update state
blk126_skip:
  jal  x0, blk127                # next
blk127:
  andi x9, x5, 64                # tmp=state&64
  beq  x9, x0, blk127_skip       # cond branch
  addi x5, x5, 1554              # update state
blk127_skip:
  jal  x0, blk128                # next
blk128:
  andi x9, x5, 128               # tmp=state&128
  beq  x9, x0, blk128_skip       # cond branch
  addi x5, x5, 1567              # update state
blk128_skip:
  jal  x1, deep_entry            # RAS overflow
  jal  x0, blk129                # next
blk129:
  andi x9, x5, 256               # tmp=state&256
  beq  x9, x0, blk129_skip       # cond branch
  addi x5, x5, 1580              # update state
blk129_skip:
  jal  x0, blk130                # next
blk130:
  andi x9, x5, 512               # tmp=state&512
  beq  x9, x0, blk130_skip       # cond branch
  addi x5, x5, 1593              # update state
blk130_skip:
  jal  x0, blk131                # next
blk131:
  andi x9, x5, 1024              # tmp=state&1024
  beq  x9, x0, blk131_skip       # cond branch
  addi x5, x5, 1606              # update state
blk131_skip:
  jal  x0, blk132                # next
blk132:
  andi x9, x5, 1                 # tmp=state&1
  beq  x9, x0, blk132_skip       # cond branch
  addi x5, x5, 1619              # update state
blk132_skip:
  jal  x0, blk133                # next
blk133:
  andi x9, x5, 2                 # tmp=state&2
  beq  x9, x0, blk133_skip       # cond branch
  addi x5, x5, 1632              # update state
blk133_skip:
  jal  x0, blk134                # next
blk134:
  andi x9, x5, 4                 # tmp=state&4
  beq  x9, x0, blk134_skip       # cond branch
  addi x5, x5, 1645              # update state
blk134_skip:
  jal  x0, blk135                # next
blk135:
  andi x9, x5, 8                 # tmp=state&8
  beq  x9, x0, blk135_skip       # cond branch
  addi x5, x5, 1658              # update state
blk135_skip:
  jal  x0, blk136                # next
blk136:
  andi x9, x5, 16                # tmp=state&16
  beq  x9, x0, blk136_skip       # cond branch
  addi x5, x5, 1671              # update state
blk136_skip:
  jal  x0, blk137                # next
blk137:
  andi x9, x5, 32                # tmp=state&32
  beq  x9, x0, blk137_skip       # cond branch
  addi x5, x5, 1684              # update state
blk137_skip:
  jal  x0, blk138                # next
blk138:
  andi x9, x5, 64                # tmp=state&64
  beq  x9, x0, blk138_skip       # cond branch
  addi x5, x5, 1697              # update state
blk138_skip:
  jal  x0, blk139                # next
blk139:
  andi x9, x5, 128               # tmp=state&128
  beq  x9, x0, blk139_skip       # cond branch
  addi x5, x5, 1710              # update state
blk139_skip:
  jalr x1, x20, 0                # ind call ind0
  jal  x0, blk140                # next
blk140:
  andi x9, x5, 256               # tmp=state&256
  beq  x9, x0, blk140_skip       # cond branch
  addi x5, x5, 1723              # update state
blk140_skip:
  jal  x0, blk141                # next
blk141:
  andi x9, x5, 512               # tmp=state&512
  beq  x9, x0, blk141_skip       # cond branch
  addi x5, x5, 1736              # update state
blk141_skip:
  jal  x0, blk142                # next
blk142:
  andi x9, x5, 1024              # tmp=state&1024
  beq  x9, x0, blk142_skip       # cond branch
  addi x5, x5, 1749              # update state
blk142_skip:
  jal  x0, blk143                # next
blk143:
  andi x9, x5, 1                 # tmp=state&1
  beq  x9, x0, blk143_skip       # cond branch
  addi x5, x5, 1762              # update state
blk143_skip:
  jal  x0, blk144                # next
blk144:
  andi x9, x5, 2                 # tmp=state&2
  beq  x9, x0, blk144_skip       # cond branch
  addi x5, x5, 1775              # update state
blk144_skip:
  jal  x0, blk145                # next
blk145:
  andi x9, x5, 4                 # tmp=state&4
  beq  x9, x0, blk145_skip       # cond branch
  addi x5, x5, 1788              # update state
blk145_skip:
  jal  x0, blk146                # next
blk146:
  andi x9, x5, 8                 # tmp=state&8
  beq  x9, x0, blk146_skip       # cond branch
  addi x5, x5, 1801              # update state
blk146_skip:
  jal  x0, blk147                # next
blk147:
  andi x9, x5, 16                # tmp=state&16
  beq  x9, x0, blk147_skip       # cond branch
  addi x5, x5, 1814              # update state
blk147_skip:
  jalr x1, x21, 0                # ind call ind1
  jal  x0, blk148                # next
blk148:
  andi x9, x5, 32                # tmp=state&32
  beq  x9, x0, blk148_skip       # cond branch
  addi x5, x5, 1827              # update state
blk148_skip:
  jal  x0, blk149                # next
blk149:
  andi x9, x5, 64                # tmp=state&64
  beq  x9, x0, blk149_skip       # cond branch
  addi x5, x5, 1840              # update state
blk149_skip:
  jal  x0, blk150                # next
blk150:
  andi x9, x5, 128               # tmp=state&128
  beq  x9, x0, blk150_skip       # cond branch
  addi x5, x5, 1853              # update state
blk150_skip:
  jal  x0, blk151                # next
blk151:
  andi x9, x5, 256               # tmp=state&256
  beq  x9, x0, blk151_skip       # cond branch
  addi x5, x5, 1866              # update state
blk151_skip:
  jal  x0, blk152                # next
blk152:
  andi x9, x5, 512               # tmp=state&512
  beq  x9, x0, blk152_skip       # cond branch
  addi x5, x5, 1879              # update state
blk152_skip:
  jal  x0, blk153                # next
blk153:
  andi x9, x5, 1024              # tmp=state&1024
  beq  x9, x0, blk153_skip       # cond branch
  addi x5, x5, 1892              # update state
blk153_skip:
  jal  x0, blk154                # next
blk154:
  andi x9, x5, 1                 # tmp=state&1
  beq  x9, x0, blk154_skip       # cond branch
  addi x5, x5, 1905              # update state
blk154_skip:
  jal  x0, blk155                # next
blk155:
  andi x9, x5, 2                 # tmp=state&2
  beq  x9, x0, blk155_skip       # cond branch
  addi x5, x5, 1918              # update state
blk155_skip:
  jalr x1, x22, 0                # ind call ind2
  jal  x0, blk156                # next
blk156:
  andi x9, x5, 4                 # tmp=state&4
  beq  x9, x0, blk156_skip       # cond branch
  addi x5, x5, 1931              # update state
blk156_skip:
  jal  x0, blk157                # next
blk157:
  andi x9, x5, 8                 # tmp=state&8
  beq  x9, x0, blk157_skip       # cond branch
  addi x5, x5, 1944              # update state
blk157_skip:
  jal  x0, blk158                # next
blk158:
  andi x9, x5, 16                # tmp=state&16
  beq  x9, x0, blk158_skip       # cond branch
  addi x5, x5, 1957              # update state
blk158_skip:
  jal  x0, blk159                # next
blk159:
  andi x9, x5, 32                # tmp=state&32
  beq  x9, x0, blk159_skip       # cond branch
  addi x5, x5, 1970              # update state
blk159_skip:
  jal  x0, blk160                # next
blk160:
  andi x9, x5, 64                # tmp=state&64
  beq  x9, x0, blk160_skip       # cond branch
  addi x5, x5, 1983              # update state
blk160_skip:
  jal  x1, deep_entry            # RAS overflow
  jal  x0, blk161                # next
blk161:
  andi x9, x5, 128               # tmp=state&128
  beq  x9, x0, blk161_skip       # cond branch
  addi x5, x5, 1996              # update state
blk161_skip:
  jal  x0, blk162                # next
blk162:
  andi x9, x5, 256               # tmp=state&256
  beq  x9, x0, blk162_skip       # cond branch
  addi x5, x5, 2009              # update state
blk162_skip:
  jal  x0, blk163                # next
blk163:
  andi x9, x5, 512               # tmp=state&512
  beq  x9, x0, blk163_skip       # cond branch
  addi x5, x5, 2022              # update state
blk163_skip:
  jal  x0, blk164                # next
blk164:
  andi x9, x5, 1024              # tmp=state&1024
  beq  x9, x0, blk164_skip       # cond branch
  addi x5, x5, 2035              # update state
blk164_skip:
  jal  x0, blk165                # next
blk165:
  andi x9, x5, 1                 # tmp=state&1
  beq  x9, x0, blk165_skip       # cond branch
  addi x5, x5, -2048             # update state
blk165_skip:
  jal  x0, blk166                # next
blk166:
  andi x9, x5, 2                 # tmp=state&2
  beq  x9, x0, blk166_skip       # cond branch
  addi x5, x5, -2035             # update state
blk166_skip:
  jal  x0, blk167                # next
blk167:
  andi x9, x5, 4                 # tmp=state&4
  beq  x9, x0, blk167_skip       # cond branch
  addi x5, x5, -2022             # update state
blk167_skip:
  jal  x0, blk168                # next
blk168:
  andi x9, x5, 8                 # tmp=state&8
  beq  x9, x0, blk168_skip       # cond branch
  addi x5, x5, -2009             # update state
blk168_skip:
  jal  x0, blk169                # next
blk169:
  andi x9, x5, 16                # tmp=state&16
  beq  x9, x0, blk169_skip       # cond branch
  addi x5, x5, -1996             # update state
blk169_skip:
  jal  x0, blk170                # next
blk170:
  andi x9, x5, 32                # tmp=state&32
  beq  x9, x0, blk170_skip       # cond branch
  addi x5, x5, -1983             # update state
blk170_skip:
  jal  x0, blk171                # next
blk171:
  andi x9, x5, 64                # tmp=state&64
  beq  x9, x0, blk171_skip       # cond branch
  addi x5, x5, -1970             # update state
blk171_skip:
  jalr x1, x20, 0                # ind call ind0
  jal  x0, blk172                # next
blk172:
  andi x9, x5, 128               # tmp=state&128
  beq  x9, x0, blk172_skip       # cond branch
  addi x5, x5, -1957             # update state
blk172_skip:
  jal  x0, blk173                # next
blk173:
  andi x9, x5, 256               # tmp=state&256
  beq  x9, x0, blk173_skip       # cond branch
  addi x5, x5, -1944             # update state
blk173_skip:
  jal  x0, blk174                # next
blk174:
  andi x9, x5, 512               # tmp=state&512
  beq  x9, x0, blk174_skip       # cond branch
  addi x5, x5, -1931             # update state
blk174_skip:
  jal  x0, blk175                # next
blk175:
  andi x9, x5, 1024              # tmp=state&1024
  beq  x9, x0, blk175_skip       # cond branch
  addi x5, x5, -1918             # update state
blk175_skip:
  jal  x0, blk176                # next
blk176:
  andi x9, x5, 1                 # tmp=state&1
  beq  x9, x0, blk176_skip       # cond branch
  addi x5, x5, -1905             # update state
blk176_skip:
  jal  x0, blk177                # next
blk177:
  andi x9, x5, 2                 # tmp=state&2
  beq  x9, x0, blk177_skip       # cond branch
  addi x5, x5, -1892             # update state
blk177_skip:
  jal  x0, blk178                # next
blk178:
  andi x9, x5, 4                 # tmp=state&4
  beq  x9, x0, blk178_skip       # cond branch
  addi x5, x5, -1879             # update state
blk178_skip:
  jal  x0, blk179                # next
blk179:
  andi x9, x5, 8                 # tmp=state&8
  beq  x9, x0, blk179_skip       # cond branch
  addi x5, x5, -1866             # update state
blk179_skip:
  jalr x1, x21, 0                # ind call ind1
  jal  x0, blk180                # next
blk180:
  andi x9, x5, 16                # tmp=state&16
  beq  x9, x0, blk180_skip       # cond branch
  addi x5, x5, -1853             # update state
blk180_skip:
  jal  x0, blk181                # next
blk181:
  andi x9, x5, 32                # tmp=state&32
  beq  x9, x0, blk181_skip       # cond branch
  addi x5, x5, -1840             # update state
blk181_skip:
  jal  x0, blk182                # next
blk182:
  andi x9, x5, 64                # tmp=state&64
  beq  x9, x0, blk182_skip       # cond branch
  addi x5, x5, -1827             # update state
blk182_skip:
  jal  x0, blk183                # next
blk183:
  andi x9, x5, 128               # tmp=state&128
  beq  x9, x0, blk183_skip       # cond branch
  addi x5, x5, -1814             # update state
blk183_skip:
  jal  x0, blk184                # next
blk184:
  andi x9, x5, 256               # tmp=state&256
  beq  x9, x0, blk184_skip       # cond branch
  addi x5, x5, -1801             # update state
blk184_skip:
  jal  x0, blk185                # next
blk185:
  andi x9, x5, 512               # tmp=state&512
  beq  x9, x0, blk185_skip       # cond branch
  addi x5, x5, -1788             # update state
blk185_skip:
  jal  x0, blk186                # next
blk186:
  andi x9, x5, 1024              # tmp=state&1024
  beq  x9, x0, blk186_skip       # cond branch
  addi x5, x5, -1775             # update state
blk186_skip:
  jal  x0, blk187                # next
blk187:
  andi x9, x5, 1                 # tmp=state&1
  beq  x9, x0, blk187_skip       # cond branch
  addi x5, x5, -1762             # update state
blk187_skip:
  jalr x1, x22, 0                # ind call ind2
  jal  x0, blk188                # next
blk188:
  andi x9, x5, 2                 # tmp=state&2
  beq  x9, x0, blk188_skip       # cond branch
  addi x5, x5, -1749             # update state
blk188_skip:
  jal  x0, blk189                # next
blk189:
  andi x9, x5, 4                 # tmp=state&4
  beq  x9, x0, blk189_skip       # cond branch
  addi x5, x5, -1736             # update state
blk189_skip:
  jal  x0, blk190                # next
blk190:
  andi x9, x5, 8                 # tmp=state&8
  beq  x9, x0, blk190_skip       # cond branch
  addi x5, x5, -1723             # update state
blk190_skip:
  jal  x0, blk191                # next
blk191:
  andi x9, x5, 16                # tmp=state&16
  beq  x9, x0, blk191_skip       # cond branch
  addi x5, x5, -1710             # update state
blk191_skip:
  jal  x0, blk192                # next
blk192:
  andi x9, x5, 32                # tmp=state&32
  beq  x9, x0, blk192_skip       # cond branch
  addi x5, x5, -1697             # update state
blk192_skip:
  jal  x1, deep_entry            # RAS overflow
  jal  x0, blk193                # next
blk193:
  andi x9, x5, 64                # tmp=state&64
  beq  x9, x0, blk193_skip       # cond branch
  addi x5, x5, -1684             # update state
blk193_skip:
  jal  x0, blk194                # next
blk194:
  andi x9, x5, 128               # tmp=state&128
  beq  x9, x0, blk194_skip       # cond branch
  addi x5, x5, -1671             # update state
blk194_skip:
  jal  x0, blk195                # next
blk195:
  andi x9, x5, 256               # tmp=state&256
  beq  x9, x0, blk195_skip       # cond branch
  addi x5, x5, -1658             # update state
blk195_skip:
  jal  x0, blk196                # next
blk196:
  andi x9, x5, 512               # tmp=state&512
  beq  x9, x0, blk196_skip       # cond branch
  addi x5, x5, -1645             # update state
blk196_skip:
  jal  x0, blk197                # next
blk197:
  andi x9, x5, 1024              # tmp=state&1024
  beq  x9, x0, blk197_skip       # cond branch
  addi x5, x5, -1632             # update state
blk197_skip:
  jal  x0, blk198                # next
blk198:
  andi x9, x5, 1                 # tmp=state&1
  beq  x9, x0, blk198_skip       # cond branch
  addi x5, x5, -1619             # update state
blk198_skip:
  jal  x0, blk199                # next
blk199:
  andi x9, x5, 2                 # tmp=state&2
  beq  x9, x0, blk199_skip       # cond branch
  addi x5, x5, -1606             # update state
blk199_skip:
  jal  x0, blk200                # next
blk200:
  andi x9, x5, 4                 # tmp=state&4
  beq  x9, x0, blk200_skip       # cond branch
  addi x5, x5, -1593             # update state
blk200_skip:
  jal  x0, blk201                # next
blk201:
  andi x9, x5, 8                 # tmp=state&8
  beq  x9, x0, blk201_skip       # cond branch
  addi x5, x5, -1580             # update state
blk201_skip:
  jal  x0, blk202                # next
blk202:
  andi x9, x5, 16                # tmp=state&16
  beq  x9, x0, blk202_skip       # cond branch
  addi x5, x5, -1567             # update state
blk202_skip:
  jal  x0, blk203                # next
blk203:
  andi x9, x5, 32                # tmp=state&32
  beq  x9, x0, blk203_skip       # cond branch
  addi x5, x5, -1554             # update state
blk203_skip:
  jalr x1, x20, 0                # ind call ind0
  jal  x0, blk204                # next
blk204:
  andi x9, x5, 64                # tmp=state&64
  beq  x9, x0, blk204_skip       # cond branch
  addi x5, x5, -1541             # update state
blk204_skip:
  jal  x0, blk205                # next
blk205:
  andi x9, x5, 128               # tmp=state&128
  beq  x9, x0, blk205_skip       # cond branch
  addi x5, x5, -1528             # update state
blk205_skip:
  jal  x0, blk206                # next
blk206:
  andi x9, x5, 256               # tmp=state&256
  beq  x9, x0, blk206_skip       # cond branch
  addi x5, x5, -1515             # update state
blk206_skip:
  jal  x0, blk207                # next
blk207:
  andi x9, x5, 512               # tmp=state&512
  beq  x9, x0, blk207_skip       # cond branch
  addi x5, x5, -1502             # update state
blk207_skip:
  jal  x0, blk208                # next
blk208:
  andi x9, x5, 1024              # tmp=state&1024
  beq  x9, x0, blk208_skip       # cond branch
  addi x5, x5, -1489             # update state
blk208_skip:
  jal  x0, blk209                # next
blk209:
  andi x9, x5, 1                 # tmp=state&1
  beq  x9, x0, blk209_skip       # cond branch
  addi x5, x5, -1476             # update state
blk209_skip:
  jal  x0, blk210                # next
blk210:
  andi x9, x5, 2                 # tmp=state&2
  beq  x9, x0, blk210_skip       # cond branch
  addi x5, x5, -1463             # update state
blk210_skip:
  jal  x0, blk211                # next
blk211:
  andi x9, x5, 4                 # tmp=state&4
  beq  x9, x0, blk211_skip       # cond branch
  addi x5, x5, -1450             # update state
blk211_skip:
  jalr x1, x21, 0                # ind call ind1
  jal  x0, blk212                # next
blk212:
  andi x9, x5, 8                 # tmp=state&8
  beq  x9, x0, blk212_skip       # cond branch
  addi x5, x5, -1437             # update state
blk212_skip:
  jal  x0, blk213                # next
blk213:
  andi x9, x5, 16                # tmp=state&16
  beq  x9, x0, blk213_skip       # cond branch
  addi x5, x5, -1424             # update state
blk213_skip:
  jal  x0, blk214                # next
blk214:
  andi x9, x5, 32                # tmp=state&32
  beq  x9, x0, blk214_skip       # cond branch
  addi x5, x5, -1411             # update state
blk214_skip:
  jal  x0, blk215                # next
blk215:
  andi x9, x5, 64                # tmp=state&64
  beq  x9, x0, blk215_skip       # cond branch
  addi x5, x5, -1398             # update state
blk215_skip:
  jal  x0, blk216                # next
blk216:
  andi x9, x5, 128               # tmp=state&128
  beq  x9, x0, blk216_skip       # cond branch
  addi x5, x5, -1385             # update state
blk216_skip:
  jal  x0, blk217                # next
blk217:
  andi x9, x5, 256               # tmp=state&256
  beq  x9, x0, blk217_skip       # cond branch
  addi x5, x5, -1372             # update state
blk217_skip:
  jal  x0, blk218                # next
blk218:
  andi x9, x5, 512               # tmp=state&512
  beq  x9, x0, blk218_skip       # cond branch
  addi x5, x5, -1359             # update state
blk218_skip:
  jal  x0, blk219                # next
blk219:
  andi x9, x5, 1024              # tmp=state&1024
  beq  x9, x0, blk219_skip       # cond branch
  addi x5, x5, -1346             # update state
blk219_skip:
  jalr x1, x22, 0                # ind call ind2
  jal  x0, blk220                # next
blk220:
  andi x9, x5, 1                 # tmp=state&1
  beq  x9, x0, blk220_skip       # cond branch
  addi x5, x5, -1333             # update state
blk220_skip:
  jal  x0, blk221                # next
blk221:
  andi x9, x5, 2                 # tmp=state&2
  beq  x9, x0, blk221_skip       # cond branch
  addi x5, x5, -1320             # update state
blk221_skip:
  jal  x0, blk222                # next
blk222:
  andi x9, x5, 4                 # tmp=state&4
  beq  x9, x0, blk222_skip       # cond branch
  addi x5, x5, -1307             # update state
blk222_skip:
  jal  x0, blk223                # next
blk223:
  andi x9, x5, 8                 # tmp=state&8
  beq  x9, x0, blk223_skip       # cond branch
  addi x5, x5, -1294             # update state
blk223_skip:
  jal  x0, blk224                # next
blk224:
  andi x9, x5, 16                # tmp=state&16
  beq  x9, x0, blk224_skip       # cond branch
  addi x5, x5, -1281             # update state
blk224_skip:
  jal  x1, deep_entry            # RAS overflow
  jal  x0, blk225                # next
blk225:
  andi x9, x5, 32                # tmp=state&32
  beq  x9, x0, blk225_skip       # cond branch
  addi x5, x5, -1268             # update state
blk225_skip:
  jal  x0, blk226                # next
blk226:
  andi x9, x5, 64                # tmp=state&64
  beq  x9, x0, blk226_skip       # cond branch
  addi x5, x5, -1255             # update state
blk226_skip:
  jal  x0, blk227                # next
blk227:
  andi x9, x5, 128               # tmp=state&128
  beq  x9, x0, blk227_skip       # cond branch
  addi x5, x5, -1242             # update state
blk227_skip:
  jal  x0, blk228                # next
blk228:
  andi x9, x5, 256               # tmp=state&256
  beq  x9, x0, blk228_skip       # cond branch
  addi x5, x5, -1229             # update state
blk228_skip:
  jal  x0, blk229                # next
blk229:
  andi x9, x5, 512               # tmp=state&512
  beq  x9, x0, blk229_skip       # cond branch
  addi x5, x5, -1216             # update state
blk229_skip:
  jal  x0, blk230                # next
blk230:
  andi x9, x5, 1024              # tmp=state&1024
  beq  x9, x0, blk230_skip       # cond branch
  addi x5, x5, -1203             # update state
blk230_skip:
  jal  x0, blk231                # next
blk231:
  andi x9, x5, 1                 # tmp=state&1
  beq  x9, x0, blk231_skip       # cond branch
  addi x5, x5, -1190             # update state
blk231_skip:
  jal  x0, blk232                # next
blk232:
  andi x9, x5, 2                 # tmp=state&2
  beq  x9, x0, blk232_skip       # cond branch
  addi x5, x5, -1177             # update state
blk232_skip:
  jal  x0, blk233                # next
blk233:
  andi x9, x5, 4                 # tmp=state&4
  beq  x9, x0, blk233_skip       # cond branch
  addi x5, x5, -1164             # update state
blk233_skip:
  jal  x0, blk234                # next
blk234:
  andi x9, x5, 8                 # tmp=state&8
  beq  x9, x0, blk234_skip       # cond branch
  addi x5, x5, -1151             # update state
blk234_skip:
  jal  x0, blk235                # next
blk235:
  andi x9, x5, 16                # tmp=state&16
  beq  x9, x0, blk235_skip       # cond branch
  addi x5, x5, -1138             # update state
blk235_skip:
  jalr x1, x20, 0                # ind call ind0
  jal  x0, blk236                # next
blk236:
  andi x9, x5, 32                # tmp=state&32
  beq  x9, x0, blk236_skip       # cond branch
  addi x5, x5, -1125             # update state
blk236_skip:
  jal  x0, blk237                # next
blk237:
  andi x9, x5, 64                # tmp=state&64
  beq  x9, x0, blk237_skip       # cond branch
  addi x5, x5, -1112             # update state
blk237_skip:
  jal  x0, blk238                # next
blk238:
  andi x9, x5, 128               # tmp=state&128
  beq  x9, x0, blk238_skip       # cond branch
  addi x5, x5, -1099             # update state
blk238_skip:
  jal  x0, blk239                # next
blk239:
  andi x9, x5, 256               # tmp=state&256
  beq  x9, x0, blk239_skip       # cond branch
  addi x5, x5, -1086             # update state
blk239_skip:
  jal  x0, blk240                # next
blk240:
  andi x9, x5, 512               # tmp=state&512
  beq  x9, x0, blk240_skip       # cond branch
  addi x5, x5, -1073             # update state
blk240_skip:
  jal  x0, blk241                # next
blk241:
  andi x9, x5, 1024              # tmp=state&1024
  beq  x9, x0, blk241_skip       # cond branch
  addi x5, x5, -1060             # update state
blk241_skip:
  jal  x0, blk242                # next
blk242:
  andi x9, x5, 1                 # tmp=state&1
  beq  x9, x0, blk242_skip       # cond branch
  addi x5, x5, -1047             # update state
blk242_skip:
  jal  x0, blk243                # next
blk243:
  andi x9, x5, 2                 # tmp=state&2
  beq  x9, x0, blk243_skip       # cond branch
  addi x5, x5, -1034             # update state
blk243_skip:
  jalr x1, x21, 0                # ind call ind1
  jal  x0, blk244                # next
blk244:
  andi x9, x5, 4                 # tmp=state&4
  beq  x9, x0, blk244_skip       # cond branch
  addi x5, x5, -1021             # update state
blk244_skip:
  jal  x0, blk245                # next
blk245:
  andi x9, x5, 8                 # tmp=state&8
  beq  x9, x0, blk245_skip       # cond branch
  addi x5, x5, -1008             # update state
blk245_skip:
  jal  x0, blk246                # next
blk246:
  andi x9, x5, 16                # tmp=state&16
  beq  x9, x0, blk246_skip       # cond branch
  addi x5, x5, -995              # update state
blk246_skip:
  jal  x0, blk247                # next
blk247:
  andi x9, x5, 32                # tmp=state&32
  beq  x9, x0, blk247_skip       # cond branch
  addi x5, x5, -982              # update state
blk247_skip:
  jal  x0, blk248                # next
blk248:
  andi x9, x5, 64                # tmp=state&64
  beq  x9, x0, blk248_skip       # cond branch
  addi x5, x5, -969              # update state
blk248_skip:
  jal  x0, blk249                # next
blk249:
  andi x9, x5, 128               # tmp=state&128
  beq  x9, x0, blk249_skip       # cond branch
  addi x5, x5, -956              # update state
blk249_skip:
  jal  x0, blk250                # next
blk250:
  andi x9, x5, 256               # tmp=state&256
  beq  x9, x0, blk250_skip       # cond branch
  addi x5, x5, -943              # update state
blk250_skip:
  jal  x0, blk251                # next
blk251:
  andi x9, x5, 512               # tmp=state&512
  beq  x9, x0, blk251_skip       # cond branch
  addi x5, x5, -930              # update state
blk251_skip:
  jalr x1, x22, 0                # ind call ind2
  jal  x0, blk252                # next
blk252:
  andi x9, x5, 1024              # tmp=state&1024
  beq  x9, x0, blk252_skip       # cond branch
  addi x5, x5, -917              # update state
blk252_skip:
  jal  x0, blk253                # next
blk253:
  andi x9, x5, 1                 # tmp=state&1
  beq  x9, x0, blk253_skip       # cond branch
  addi x5, x5, -904              # update state
blk253_skip:
  jal  x0, blk254                # next
blk254:
  andi x9, x5, 2                 # tmp=state&2
  beq  x9, x0, blk254_skip       # cond branch
  addi x5, x5, -891              # update state
blk254_skip:
  jal  x0, blk255                # next
blk255:
  andi x9, x5, 4                 # tmp=state&4
  beq  x9, x0, blk255_skip       # cond branch
  addi x5, x5, -878              # update state
blk255_skip:
  addi x11, x11, -1              # laps--
  bne  x11, x0, blk255_more      # if more
  jal  x0, btb_ras_ret           # exit
blk255_more:
  jal  x0, blk0                  # wrap
btb_ras_ret:
  jalr x0, x1, 0                 # ret
