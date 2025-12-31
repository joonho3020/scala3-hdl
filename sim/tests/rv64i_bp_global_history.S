.option norvc
.text
.globl _start

_start:
  addi x5, x0, 64                    # warmup
  addi x6, x0, 1                     # state=1
warm:
  slli x7, x6, 1                    
  xor  x6, x6, x7                   
  srli x7, x6, 3                    
  xor  x6, x6, x7                   
  andi x8, x6, 1                     # bit=state&1
  beq   x8, x0, w_b1_nt             
w_b1_t:
  beq   x8, x0, w_b2_nt             
  jal  x0, w_b2_t                   
w_b2_nt:
  jal  x0, w_b2_join                
w_b2_t:
w_b2_join:
  jal  x0, w_join                   
w_b1_nt:
  bne   x8, x0, w_b2i_t             
  jal  x0, w_b2i_join               
w_b2i_t:
w_b2i_join:
w_join:
  addi x5, x5, -1                   
  bne   x5, x0, warm                
  addi x5, x0, 8192                  # measure
  addi x6, x0, 1                     # state=1
  addi x9, x0, 0                     # acc=0
loop:
  slli x7, x6, 1                    
  xor  x6, x6, x7                   
  srli x7, x6, 3                    
  xor  x6, x6, x7                   
  andi x8, x6, 1                     # bit
  beq   x8, x0, b1_nt                # B1: bit==0?
b1_t:
  addi x9, x9, 1                    
  xori x9, x9, 85                   
  beq   x8, x0, b2_nt                # B2: correlated
b2_t:
  addi x9, x9, 3                    
  jal  x0, b2_join                  
b2_nt:
  addi x9, x9, 2                    
b2_join:
  jal  x0, join                     
b1_nt:
  addi x9, x9, 4                    
  bne   x8, x0, b3_t                 # B3: inverted corr
  jal  x0, b3_nt                    
b3_t:
  addi x9, x9, 7                    
  jal  x0, b3_join                  
b3_nt:
  addi x9, x9, 1                    
b3_join:
join:
  addi x5, x5, -1                   
  bne   x5, x0, loop                
done:
  jal  x0, done                      # halt
