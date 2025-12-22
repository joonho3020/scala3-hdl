.option norvc
.text
.globl _start

_start:
  lui  x20, 0x80004                # base = 0x80004000
  lui  x21, 0x2                    # N = 8192 (2^13)
  lui  x22, 0x2                    # tmp=0x2000
  addi x22, x22, -1                # mask = 0x1fff
  addi x23, x0, 0                  # i=0
  addi x24, x21, 0                 # cnt=N
init_loop:
  slli x5, x23, 5                  # t0 = i<<5
  add  x5, x5, x23                 # t0 = t0 + i (i*33)
  addi x5, x5, 17                  # t0 = t0 + 17
  and  x5, x5, x22                 # t0 &= mask
  slli x6, x23, 3                  # off = i*8
  add  x6, x6, x20                 # addr = base + off
  sd   x5, 0(x6)                   # table[i]=next
  addi x23, x23, 1                 # i++
  addi x24, x24, -1                # cnt--
  bne  x24, x0, init_loop          # init loop
  lui  x26, 0x10                   # iters = 65536
  addi x25, x0, 0                  # idx=0
  addi x27, x0, 0                  # checksum=0
chase_loop:
  slli x6, x25, 3                  # off = idx*8
  add  x6, x6, x20                 # addr = base+off
  ld   x25, 0(x6)                  # idx = table[idx]
  add  x27, x27, x25               # checksum += idx
  addi x26, x26, -1                # iters--
  bne  x26, x0, chase_loop         # chase loop
  slli x28, x21, 3                 # res_off = N*8
  add  x28, x28, x20               # res_ptr = base+N*8
  sd   x27, 0(x28)                 # store checksum
done:
  jal  x0, done                    # halt
