use hdl_sim::Dut;
use riscv_sim::RefCore;

const RESET_PC: u64 = 0x80000000;

struct RetireInfo {
    valid: bool,
    pc: u64,
    wb_valid: bool,
    wb_rd: u64,
    wb_data: u64,
}

fn get_retire_info_0(dut: &Dut) -> RetireInfo {
    RetireInfo {
        valid: dut.peek_io_retire_info_0_valid() != 0,
        pc: dut.peek_io_retire_info_0_pc(),
        wb_valid: dut.peek_io_retire_info_0_wb_valid() != 0,
        wb_rd: dut.peek_io_retire_info_0_wb_rd(),
        wb_data: dut.peek_io_retire_info_0_wb_data(),
    }
}

fn get_retire_info_1(dut: &Dut) -> RetireInfo {
    RetireInfo {
        valid: dut.peek_io_retire_info_1_valid() != 0,
        pc: dut.peek_io_retire_info_1_pc(),
        wb_valid: dut.peek_io_retire_info_1_wb_valid() != 0,
        wb_rd: dut.peek_io_retire_info_1_wb_rd(),
        wb_data: dut.peek_io_retire_info_1_wb_data(),
    }
}

fn compare_retire_with_ref(
    retire: &RetireInfo,
    ref_result: &riscv_sim::StepResult,
    pipe: usize,
    cycle: usize,
) -> bool {
    if retire.pc != ref_result.pc {
        println!(
            "MISMATCH at cycle {} pipe{}: PC mismatch: RTL=0x{:x}, Ref=0x{:x}",
            cycle, pipe, retire.pc, ref_result.pc
        );
        return false;
    }

    if retire.wb_valid != ref_result.wb_valid {
        println!(
            "MISMATCH at cycle {} pipe{} PC=0x{:x}: wb_valid mismatch: RTL={}, Ref={}",
            cycle, pipe, retire.pc, retire.wb_valid, ref_result.wb_valid
        );
        return false;
    }

    if retire.wb_valid {
        if retire.wb_rd != ref_result.wb_rd {
            println!(
                "MISMATCH at cycle {} pipe{} PC=0x{:x}: wb_rd mismatch: RTL={}, Ref={}",
                cycle, pipe, retire.pc, retire.wb_rd, ref_result.wb_rd
            );
            return false;
        }

        if retire.wb_data != ref_result.wb_data {
            println!(
                "MISMATCH at cycle {} pipe{} PC=0x{:x}: wb_data mismatch: RTL=0x{:x}, Ref=0x{:x}",
                cycle, pipe, retire.pc, retire.wb_data, ref_result.wb_data
            );
            return false;
        }

        println!(
            "PASS cycle {} pipe{}: PC=0x{:x} reg[{}] = 0x{:x}",
            cycle, pipe, retire.pc, retire.wb_rd, retire.wb_data
        );
    } else {
        println!(
            "PASS cycle {} pipe{}: PC=0x{:x} (no writeback)",
            cycle, pipe, retire.pc
        );
    }

    true
}

fn main() {
    let mut dut = Dut::new();
    dut.enable_trace();

    let instructions: Vec<u32> = vec![
        0x003282b3, // ADD x5, x5, x3  -> 5 + 3 = 8
        0x40450133, // SUB x2, x10, x4 -> 10 - 4 = 6
        0x007781b3, // ADD x3, x15, x7 -> 15 + 7 = 22
        0x408a0233, // SUB x4, x20, x8 -> 20 - 8 = 12
        0x00208133, // ADD x2, x1, x2  -> 1 + 2 = 3
        0x40310233, // SUB x4, x2, x3  -> 2 - 3 = -1 (0xFFFFFFFF)
        0x00000013, // NOP (ADDI x0, x0, 0)
        0x00000013, // NOP
        0x00000013, // NOP
        0x00000013, // NOP
        0x00000013, // NOP
        0x00000013, // NOP
        0x00000013, // NOP
        0x00000013, // NOP
        0x00000013, // NOP
        0x00000013, // NOP
    ];

    let mut ref_core = RefCore::new(RESET_PC, 1 << 20);
    ref_core.load_instructions(RESET_PC, &instructions);

    for i in 0..32 {
        ref_core.set_reg(i, i);
    }

    dut.reset();

    println!("Starting Group 1 instruction tests with reference core comparison...\n");

    let mut pending_mem_req = false;
    let mut pending_mem_addr = 0u64;
    let mut retired_count = 0;
    let mut mismatch_count = 0;

    for cycle in 0..300 {
        let retire_0 = get_retire_info_0(&dut);
        let retire_1 = get_retire_info_1(&dut);

        if retire_0.valid {
            let ref_result = ref_core.step();
            if !compare_retire_with_ref(&retire_0, &ref_result, 0, cycle) {
                mismatch_count += 1;
            }
            retired_count += 1;
        }

        if retire_1.valid {
            let ref_result = ref_core.step();
            if !compare_retire_with_ref(&retire_1, &ref_result, 1, cycle) {
                mismatch_count += 1;
            }
            retired_count += 1;
        }

        if pending_mem_req {
            println!(
                "Cycle {}: Providing memory response for addr 0x{:x}",
                cycle, pending_mem_addr
            );

            for i in 0..16 {
                let insn = if i < instructions.len() {
                    instructions[i] as u64
                } else {
                    0x00000013u64
                };
                match i {
                    0 => dut.poke_io_mem_resp_bits_lineWords_0(insn),
                    1 => dut.poke_io_mem_resp_bits_lineWords_1(insn),
                    2 => dut.poke_io_mem_resp_bits_lineWords_2(insn),
                    3 => dut.poke_io_mem_resp_bits_lineWords_3(insn),
                    4 => dut.poke_io_mem_resp_bits_lineWords_4(insn),
                    5 => dut.poke_io_mem_resp_bits_lineWords_5(insn),
                    6 => dut.poke_io_mem_resp_bits_lineWords_6(insn),
                    7 => dut.poke_io_mem_resp_bits_lineWords_7(insn),
                    8 => dut.poke_io_mem_resp_bits_lineWords_8(insn),
                    9 => dut.poke_io_mem_resp_bits_lineWords_9(insn),
                    10 => dut.poke_io_mem_resp_bits_lineWords_10(insn),
                    11 => dut.poke_io_mem_resp_bits_lineWords_11(insn),
                    12 => dut.poke_io_mem_resp_bits_lineWords_12(insn),
                    13 => dut.poke_io_mem_resp_bits_lineWords_13(insn),
                    14 => dut.poke_io_mem_resp_bits_lineWords_14(insn),
                    15 => dut.poke_io_mem_resp_bits_lineWords_15(insn),
                    _ => {}
                }
            }
            dut.poke_io_mem_resp_valid(1);
            pending_mem_req = false;
        } else {
            dut.poke_io_mem_resp_valid(0);
        }

        let mem_req_valid = dut.peek_io_mem_req_valid();
        if mem_req_valid != 0 {
            let addr = dut.peek_io_mem_req_bits_addr();
            println!("Cycle {}: Memory request for addr 0x{:x}", cycle, addr);
            pending_mem_req = true;
            pending_mem_addr = addr;
        }

        dut.step();

        if retired_count >= instructions.len() {
            break;
        }
    }

    println!("\n========================================");
    println!("Test Summary:");
    println!("  Total retired: {}", retired_count);
    println!("  Mismatches: {}", mismatch_count);
    if mismatch_count == 0 {
        println!("  Status: PASSED");
    } else {
        println!("  Status: FAILED");
    }
    println!("========================================");
}
