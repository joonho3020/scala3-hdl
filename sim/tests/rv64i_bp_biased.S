.option norvc
.text
.globl _start

_start:
  addi x5, x0, 64                    # warmup iters
warm_loop:
  addi x5, x5, -1                   
  bne   x5, x0, warm_loop            # backward taken until 0
  addi x5, x0, 4096                  # measure iters
  addi x7, x0, 0                     # rare_taken_count
meas_loop:
  addi x5, x5, -1                    # ctr--
  andi x6, x5, 63                    # tmp=ctr&63
  beq   x6, x0, rare                 # rare taken (~1/64)
common:
  bne   x5, x0, meas_loop            # loop branch (mostly taken)
  jal  x0, done                     
rare:
  addi x7, x7, 1                     # rare++
  xori x6, x6, 42                   
  ori  x6, x6, 21                   
  bne   x5, x0, meas_loop            # continue
done:
  jal  x0, done                      # halt
