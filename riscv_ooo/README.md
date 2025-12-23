# RISCV Core

## Frontend: Typical 4-stage Pipeline

- S0: icache tag lookup
- S1: addr translation & icache tag matching & data array lookup on hit
- S2: hit data provided to frontend
- S3: omitted for now, should predecode and perform frontend redirects for jumps & branches
- FB: fetch buffer

S0 S1 S2 FB
0  x  x
4  0  x
8  4  0 <-miss
0  x  x

## Backend:

<!-- - Fix this to OoO later -->

<!-- D E M W -->

<!-- - D: decode & register file lookup -->
<!-- - E: execute -->
<!-- - M: memory -->
<!-- - W: writeback -->

Decode

Rename 1

Rename 2

Dispatch

Issue

WB

Commit




## LSU:

Dcache: 3-stage blocking, change this to nonblocking later

test back to back read & writes

## TODOs:

- OoO execution support
- Forwarding network
- LSU & make D$ non-blocking
- Better branch prediction
- I$ data array banking

## Target spec

- `rv64im_zicsr_zifencei`
- Add atomics later...

## RV64I instruction formats

|    | 31         25 | 24      20 | 19     15 | 14  12 | 11    7     | 6    0 |
| R  | funct7        |    rs2     |    rs1    | funct3 |    rd       | opcode |
| I  |     imm[11:0]              |    rs1    | funct3 |    rd       | opcode |
| S  | imm[11:5]     |    rs2     |    rs1    | funct3 | imm[4:0]    | opcode |
| SB | imm[12,10:5]  |    rs2     |    rs1    | funct3 | imm[4:1,11] | opcode |
| U  | imm[31:12]                                      |    rd       | opcode |
| UJ | imm[20,10:1,11,19:12]                           |    rd       | opcode |

## RV64I implementation order

### Group 1: Integer ALU and immediates

| Instruction | Format | Opcode | funct3 | funct7 | Notes |
<!-- | --- | --- | --- | --- | --- | --- | -->
<!-- | ADD | R | 0110011 (0x33) | 000 | 0000000 | - | -->
<!-- | SUB | R | 0110011 (0x33) | 000 | 0100000 | - | -->
<!-- | OR | R | 0110011 (0x33) | 110 | 0000000 | - | -->
<!-- | AND | R | 0110011 (0x33) | 111 | 0000000 | - | -->
<!-- | XOR | R | 0110011 (0x33) | 100 | 0000000 | - | -->

<!-- | ADDI | I | 0010011 (0x13) | 000 | - | imm[11:0] | -->
<!-- | XORI | I | 0010011 (0x13) | 100 | - | imm[11:0] | -->
<!-- | ORI | I | 0010011 (0x13) | 110 | - | imm[11:0] | -->
<!-- | ANDI | I | 0010011 (0x13) | 111 | - | imm[11:0] | -->

<!-- | LUI | U | 0110111 (0x37) | - | - | imm[31:12] << 12 | -->
<!-- | AUIPC | U | 0010111 (0x17) | - | - | pc + (imm[31:12] << 12) | -->

| SLT | R | 0110011 (0x33) | 010 | 0000000 | - |
| SLTI | I | 0010011 (0x13) | 010 | - | imm[11:0] |
| SLTIU | I | 0010011 (0x13) | 011 | - | imm[11:0] |
| SLTU | R | 0110011 (0x33) | 011 | 0000000 | - |

| SLL | R | 0110011 (0x33) | 001 | 0000000 | - |
| SLLI | I | 0010011 (0x13) | 001 | 0000000 | shamt[5:0] |

| SRL | R | 0110011 (0x33) | 101 | 0000000 | - |
| SRLI | I | 0010011 (0x13) | 101 | 0000000 | shamt[5:0] |

| SRA | R | 0110011 (0x33) | 101 | 0100000 | - |
| SRAI | I | 0010011 (0x13) | 101 | 0100000 | shamt[5:0] |

### Group 2: Control flow

| Instruction | Format | Opcode | funct3 | funct7 | Notes |
| --- | --- | --- | --- | --- | --- |
| JAL | J | 1101111 (0x6f) | - | - | imm[20,10:1,11,19:12] |
| JALR | I | 1100111 (0x67) | 000 | - | imm[11:0] |
| BEQ | B | 1100011 (0x63) | 000 | - | imm[12,10:5,4:1,11] |
| BNE | B | 1100011 (0x63) | 001 | - | imm[12,10:5,4:1,11] |
| BLT | B | 1100011 (0x63) | 100 | - | imm[12,10:5,4:1,11] |
| BGE | B | 1100011 (0x63) | 101 | - | imm[12,10:5,4:1,11] |
| BLTU | B | 1100011 (0x63) | 110 | - | imm[12,10:5,4:1,11] |
| BGEU | B | 1100011 (0x63) | 111 | - | imm[12,10:5,4:1,11] |

### Group 3: Loads and stores

| Instruction | Format | Opcode | funct3 | funct7 | Notes |
| --- | --- | --- | --- | --- | --- |
| LB | I | 0000011 (0x03) | 000 | - | signed |
| LH | I | 0000011 (0x03) | 001 | - | signed |
| LW | I | 0000011 (0x03) | 010 | - | signed |
| LD | I | 0000011 (0x03) | 011 | - | signed |
| LBU | I | 0000011 (0x03) | 100 | - | unsigned |
| LHU | I | 0000011 (0x03) | 101 | - | unsigned |
| LWU | I | 0000011 (0x03) | 110 | - | unsigned |
| SB | S | 0100011 (0x23) | 000 | - | - |
| SH | S | 0100011 (0x23) | 001 | - | - |
| SW | S | 0100011 (0x23) | 010 | - | - |
| SD | S | 0100011 (0x23) | 011 | - | - |

### Group 4: RV64 word ops

| Instruction | Format | Opcode | funct3 | funct7 | Notes |
| --- | --- | --- | --- | --- | --- |
| ADDIW | I | 0011011 (0x1b) | 000 | - | sign-extend 32-bit result |
| SLLIW | I | 0011011 (0x1b) | 001 | 0000000 | shamt[4:0] |
| SRLIW | I | 0011011 (0x1b) | 101 | 0000000 | shamt[4:0] |
| SRAIW | I | 0011011 (0x1b) | 101 | 0100000 | shamt[4:0] |
| ADDW | R | 0111011 (0x3b) | 000 | 0000000 | sign-extend 32-bit result |
| SUBW | R | 0111011 (0x3b) | 000 | 0100000 | sign-extend 32-bit result |
| SLLW | R | 0111011 (0x3b) | 001 | 0000000 | sign-extend 32-bit result |
| SRLW | R | 0111011 (0x3b) | 101 | 0000000 | sign-extend 32-bit result |
| SRAW | R | 0111011 (0x3b) | 101 | 0100000 | sign-extend 32-bit result |

### Group 5: Base system and misc-mem

| Instruction | Format | Opcode | funct3 | funct7 | Notes |
| --- | --- | --- | --- | --- | --- |
| FENCE | I | 0001111 (0x0f) | 000 | - | pred/succ in imm[11:0] |
| ECALL | I | 1110011 (0x73) | 000 | - | imm=0x000 |
| EBREAK | I | 1110011 (0x73) | 000 | - | imm=0x001 |

## Extensions in target spec

### Zifencei

| Instruction | Format | Opcode | funct3 | funct7 | Notes |
| --- | --- | --- | --- | --- | --- |
| FENCE.I | I | 0001111 (0x0f) | 001 | - | imm=0x000 |

### Zicsr

| Instruction | Format | Opcode | funct3 | funct7 | Notes |
| --- | --- | --- | --- | --- | --- |
| CSRRW | I | 1110011 (0x73) | 001 | - | csr[11:0] |
| CSRRS | I | 1110011 (0x73) | 010 | - | csr[11:0] |
| CSRRC | I | 1110011 (0x73) | 011 | - | csr[11:0] |
| CSRRWI | I | 1110011 (0x73) | 101 | - | zimm[4:0] |
| CSRRSI | I | 1110011 (0x73) | 110 | - | zimm[4:0] |
| CSRRCI | I | 1110011 (0x73) | 111 | - | zimm[4:0] |
