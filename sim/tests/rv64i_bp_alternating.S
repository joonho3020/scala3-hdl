.option norvc
.text
.globl _start

_start:
  addi x5, x0, 64                    # warmup
  addi x6, x0, 0                     # toggle=0
warm_loop:
  xori x6, x6, 1                     # toggle^=1
  beq   x6, x0, w_nt                 # alternate
w_t:
  addi x7, x7, 3                    
  jal  x0, w_join                   
w_nt:
  addi x7, x7, 1                    
w_join:
  addi x5, x5, -1                   
  bne   x5, x0, warm_loop           
  addi x5, x0, 8192                  # measure
  addi x6, x0, 0                     # toggle=0
  addi x7, x0, 0                     # acc=0
loop:
  xori x6, x6, 1                     # toggle^=1 (T,NT,T,NT,...)
  beq   x6, x0, nt                   # branch alternates
t:
  addi x7, x7, 7                     # taken path
  jal  x0, join                     
nt:
  addi x7, x7, 2                     # not-taken path
join:
  addi x5, x5, -1                   
  bne   x5, x0, loop                 # backward loop
done:
  jal  x0, done                      # halt
