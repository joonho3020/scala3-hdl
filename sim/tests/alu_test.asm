# ALU Test Instructions
# Tests ADD, SUB, OR, AND, XOR, ADDI, XORI, ORI, ANDI, LUI, AUIPC
# SLT, SLTI, SLTIU, SLTU, SLL, SLLI, SRL, SRLI, SRA, SRAI

# Initialize Register File (x1-x31 with non-zero values)
ADDI x1, x0, 1
ADDI x2, x0, 2
ADDI x3, x0, 3
ADDI x4, x0, 4
ADDI x5, x0, 5
ADDI x6, x0, 6
ADDI x7, x0, 7
ADDI x8, x0, 8
ADDI x9, x0, 9
ADDI x10, x0, 10
ADDI x11, x0, 11
ADDI x12, x0, 12
ADDI x13, x0, 13
ADDI x14, x0, 14
ADDI x15, x0, 15
ADDI x16, x0, 16
ADDI x17, x0, 17
ADDI x18, x0, 18
ADDI x19, x0, 19
ADDI x20, x0, 20
ADDI x21, x0, 21
ADDI x22, x0, 22
ADDI x23, x0, 23
ADDI x24, x0, 24
ADDI x25, x0, 25
ADDI x26, x0, 26
ADDI x27, x0, 27
ADDI x28, x0, 28
ADDI x29, x0, 29
ADDI x30, x0, 30
ADDI x31, x0, 31

# Register-Register Arithmetic
ADD x5, x3, x2      # x5 = x3 + x2
SUB x6, x4, x1      # x6 = x4 - x1
ADD x7, x5, x6      # x7 = x5 + x6

# Logical Operations
OR x8, x1, x2       # x8 = x1 | x2
AND x9, x3, x4      # x9 = x3 & x4
XOR x10, x5, x6     # x10 = x5 ^ x6

# Immediate Arithmetic
ADDI x11, x0, 100   # x11 = 100
ADDI x12, x11, -50  # x12 = 50

# Immediate Logical
ORI x13, x0, 0xFF   # x13 = 0xFF
ANDI x14, x13, 0x0F # x14 = 0x0F
XORI x15, x13, 0xAA # x15 = 0x55

# Upper Immediate
LUI x16, 0x12345    # x16 = 0x12345000
AUIPC x17, 0x1      # x17 = PC + 0x1000

# Set Less Than
SLT x18, x1, x2     # x18 = (x1 < x2) ? 1 : 0
SLTU x19, x1, x2    # x19 = (x1 <u x2) ? 1 : 0
SLTI x20, x11, 200  # x20 = (x11 < 200) ? 1 : 0
SLTIU x21, x11, 200 # x21 = (x11 <u 200) ? 1 : 0

# Shift Operations (Register)
SLL x22, x1, x2     # x22 = x1 << x2
SRL x23, x16, x1    # x23 = x16 >> x1 (logical)
SRA x24, x16, x1    # x24 = x16 >> x1 (arithmetic)

# Shift Operations (Immediate)
SLLI x25, x1, 4     # x25 = x1 << 4
SRLI x26, x16, 8    # x26 = x16 >> 8 (logical)
SRAI x27, x16, 8    # x27 = x16 >> 8 (arithmetic)

# More tests with edge cases
ADDI x28, x0, -1    # x28 = 0xFFFFFFFFFFFFFFFF (all ones)
ANDI x29, x28, 0xFF # x29 = 0xFF
SRLI x30, x28, 32   # x30 = shift test

# ========================================
# STRESS TEST: Long instruction stream with dependencies
# ========================================

# Arithmetic chain with dependencies
ADD x1, x2, x3
ADD x4, x1, x5
SUB x6, x4, x2
ADD x7, x6, x1
SUB x8, x7, x4
ADD x9, x8, x6

# Logical operations mix
AND x10, x1, x2
OR x11, x10, x3
XOR x12, x11, x4
AND x13, x12, x5
OR x14, x13, x6
XOR x15, x14, x7

# Immediate arithmetic with hazards
ADDI x16, x1, 100
ADDI x17, x16, -50
ADDI x18, x17, 25
ADDI x19, x18, -10
ADDI x20, x19, 5

# Shift patterns
SLLI x21, x1, 1
SLLI x22, x21, 2
SRLI x23, x22, 1
SRAI x24, x23, 2
SLLI x25, x24, 3

# Complex dependency chain
ADD x1, x2, x3
SUB x2, x1, x4
AND x3, x2, x5
OR x4, x3, x6
XOR x5, x4, x7
ADD x6, x5, x1
SUB x7, x6, x2

# Set less than patterns
SLT x8, x1, x2
SLTU x9, x2, x3
SLTI x10, x3, 50
SLTIU x11, x4, 100
SLT x12, x5, x6
SLTU x13, x7, x8

# Immediate logical operations
ORI x14, x1, 0xFF
ANDI x15, x14, 0x0F
XORI x16, x15, 0xAA
ORI x17, x16, 0x55
ANDI x18, x17, 0xF0

# Register-register shifts
SLL x19, x1, x2
SRL x20, x19, x3
SRA x21, x20, x4
SLL x22, x21, x5
SRL x23, x22, x6

# Load upper immediate patterns
LUI x24, 0x12345
LUI x25, 0xABCDE
LUI x26, 0x55555
LUI x27, 0xAAAAA
AUIPC x28, 0x1

# Back-to-back arithmetic
ADD x1, x2, x3
ADD x4, x5, x6
ADD x7, x8, x9
ADD x10, x11, x12
ADD x13, x14, x15
SUB x16, x17, x18
SUB x19, x20, x21
SUB x22, x23, x24

# Interleaved operations
ADD x1, x2, x3
SLLI x4, x1, 2
AND x5, x4, x6
ADDI x7, x5, 10
OR x8, x7, x9
SRLI x10, x8, 1
XOR x11, x10, x12

# More dependency chains
ADD x1, x2, x3
ADD x2, x1, x4
ADD x3, x2, x5
ADD x4, x3, x6
ADD x5, x4, x7
ADD x6, x5, x8
ADD x7, x6, x9
ADD x8, x7, x10

# Mixed immediate and register operations
ADDI x9, x1, 10
ADD x10, x9, x2
ADDI x11, x10, 20
SUB x12, x11, x3
ANDI x13, x12, 0xFF
OR x14, x13, x4
XORI x15, x14, 0xAA

# Shift immediate patterns
SLLI x16, x1, 1
SLLI x17, x2, 2
SLLI x18, x3, 3
SRLI x19, x4, 4
SRLI x20, x5, 5
SRAI x21, x6, 6
SRAI x22, x7, 7

# Comparison chains
SLT x23, x1, x2
SLT x24, x23, x3
SLTU x25, x24, x4
SLTI x26, x25, 5
SLTIU x27, x26, 10

# High register usage
ADD x28, x29, x30
SUB x29, x28, x31
AND x30, x29, x28
OR x31, x30, x29
XOR x28, x31, x30

# More arithmetic variations
ADD x1, x3, x5
SUB x2, x4, x6
ADD x3, x7, x9
SUB x4, x8, x10
AND x5, x11, x13
OR x6, x12, x14
XOR x7, x15, x17

# Immediate operation burst
ADDI x8, x1, 1
ADDI x9, x2, 2
ADDI x10, x3, 3
ADDI x11, x4, 4
ADDI x12, x5, 5
ADDI x13, x6, 6
ADDI x14, x7, 7
ADDI x15, x8, 8

# Logical immediate burst
ORI x16, x9, 0x10
ORI x17, x10, 0x20
ANDI x18, x11, 0x30
ANDI x19, x12, 0x40
XORI x20, x13, 0x50
XORI x21, x14, 0x60

# Register shift burst
SLL x22, x15, x1
SRL x23, x16, x2
SRA x24, x17, x3
SLL x25, x18, x4
SRL x26, x19, x5

# Complex forwarding patterns
ADD x1, x2, x3
ADD x4, x1, x1
ADD x5, x4, x4
ADD x6, x5, x5
ADD x7, x6, x6

# More shift immediate
SLLI x8, x7, 8
SRLI x9, x8, 4
SRAI x10, x9, 2
SLLI x11, x10, 1
SRLI x12, x11, 3

# Final arithmetic burst
ADD x13, x14, x15
SUB x16, x17, x18
AND x19, x20, x21
OR x22, x23, x24
XOR x25, x26, x27
ADD x28, x29, x30
SUB x31, x1, x2

# Long dependency chain
ADD x1, x2, x3
ADDI x2, x1, 10
SUB x3, x2, x4
AND x4, x3, x5
OR x5, x4, x6
XOR x6, x5, x7
SLL x7, x6, x2
SRL x8, x7, x3
ADD x9, x8, x1

# More comparison operations
SLT x10, x9, x8
SLTU x11, x10, x9
SLT x12, x11, x10
SLTU x13, x12, x11
SLTI x14, x13, 100
SLTIU x15, x14, 200

# Mixed operation finale
ADD x16, x1, x2
SUB x17, x3, x4
AND x18, x5, x6
OR x19, x7, x8
XOR x20, x9, x10
SLLI x21, x11, 2
SRLI x22, x12, 2
ADDI x23, x13, 50
ANDI x24, x14, 0x7F
ORI x25, x15, 0x80

# Final NOPs for pipeline drain
NOP
NOP
NOP
NOP
NOP
NOP
NOP
NOP
NOP
NOP
