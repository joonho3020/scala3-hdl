use hdl_sim::asm_parser::parse_asm_file;
use hdl_sim::Dut;
use riscv_sim::RefCore;
use rvdasm::disassembler::Disassembler;
use std::env;

const RESET_PC: u64 = 0x80000000;
const CACHE_LINE_WORDS: usize = 16;
const WORD_SIZE: u64 = 4;

#[derive(Debug)]
enum MismatchType {
    PCMismatch,
    WBDstMismatch,
    WBDataMismatch,
}

#[derive(Debug)]
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

fn log_decoded_instruction(label: &str, instructions: &[u32], pc: u64, disasm: &Disassembler) {
    let word = get_instruction_at_addr(instructions, pc);
    match disasm.disassmeble_one(word) {
        Some(decoded) => println!("{} PC=0x{:x} inst=0x{:08x} {:?}", label, pc, word, decoded),
        None          => println!("{} PC=0x{:x} inst=0x{:08x} decode error", label, pc, word),
    }
}

fn compare_retire_with_ref(
    retire: &RetireInfo,
    ref_result: &riscv_sim::StepResult,
    pipe: usize,
    cycle: usize,
    instructions: &[u32],
    disasm: &Disassembler
) -> Option<MismatchType> {
    if retire.pc != ref_result.pc {
// println!(
// "MISMATCH at cycle {} pipe{}: PC mismatch: RTL=0x{:x}, Ref=0x{:x}",
// cycle, pipe, retire.pc, ref_result.pc
// );
// log_decoded_instruction("RTL", instructions, retire.pc, disasm);
// log_decoded_instruction("Ref", instructions, ref_result.pc, disasm);
        return Some(MismatchType::PCMismatch);
    }

// if retire.wb_valid != ref_result.wb_valid {
// println!(
// "MISMATCH at cycle {} pipe{} PC=0x{:x}: wb_valid mismatch: RTL={}, Ref={}",
// cycle, pipe, retire.pc, retire.wb_valid, ref_result.wb_valid
// );
// return false;
// }

    if retire.wb_valid {
        if retire.wb_rd != ref_result.wb_rd {
// println!(
// "MISMATCH at cycle {} pipe{} PC=0x{:x}: wb_rd mismatch: RTL={}, Ref={}",
// cycle, pipe, retire.pc, retire.wb_rd, ref_result.wb_rd
// );
// log_decoded_instruction("RTL", instructions, retire.pc, disasm);
// log_decoded_instruction("Ref", instructions, ref_result.pc, disasm);
            return Some(MismatchType::WBDstMismatch);
        }

        if retire.wb_data != ref_result.wb_data {
// println!(
// "MISMATCH at cycle {} pipe{} PC=0x{:x}: wb_data mismatch: RTL=0x{:x}, Ref=0x{:x}",
// cycle, pipe, retire.pc, retire.wb_data, ref_result.wb_data
// );
// log_decoded_instruction("RTL", instructions, retire.pc, disasm);
// log_decoded_instruction("Ref", instructions, ref_result.pc, disasm);
            return Some(MismatchType::WBDataMismatch);
        }
    }
    None
}

fn get_instruction_at_addr(instructions: &[u32], addr: u64) -> u32 {
    if addr < RESET_PC {
        return 0x00000013; // NOP for addresses below RESET_PC
    }
    let offset = (addr - RESET_PC) / WORD_SIZE;
    if offset as usize >= instructions.len() {
        0x00000013 // NOP for addresses beyond instruction memory
    } else {
        instructions[offset as usize]
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let asm_file = if args.len() > 1 {
        args[1].clone()
    } else {
        "tests/alu_test.hex".to_string()
    };

    println!("Loading instructions from: {}", asm_file);
    let instructions = match parse_asm_file(&asm_file) {
        Ok(insns) => insns,
        Err(e) => {
            eprintln!("Failed to parse assembly file: {}", e);
            std::process::exit(1);
        }
    };
    println!("Loaded {} instructions", instructions.len());

    let mut dut = Dut::new();
    dut.enable_trace();

    let mut ref_core = RefCore::new(RESET_PC, 1 << 20);
    ref_core.load_instructions(RESET_PC, &instructions);

    for i in 0..32 {
        ref_core.set_reg(i, i);
    }

    dut.reset();

    println!("Starting co-simulation with reference core comparison...\n");

    let mut pending_mem_req = false;
    let mut pending_mem_addr = 0u64;
    let mut retired_count = 0;
    let mut mismatch_count = 0;

    let disasm = Disassembler::new(rvdasm::disassembler::Xlen::XLEN64);

    for cycle in 0..300 {
        let retire_0 = get_retire_info_0(&dut);
        let retire_1 = get_retire_info_1(&dut);

        if retire_0.valid {
            let ref_result = ref_core.step();
            if let Some(mismatch) = compare_retire_with_ref(&retire_0, &ref_result, 0, cycle, &instructions, &disasm) {
                println!("Cycle: {} {:?}", cycle, mismatch);
                println!("- RefCore {:x?}", ref_result);
                println!("- RTL     {:x?}", retire_0);
                log_decoded_instruction("-", &instructions, retire_0.pc, &disasm);
                println!();
                mismatch_count += 1;
            }
            retired_count += 1;
        }

        if retire_1.valid {
            let ref_result = ref_core.step();
            if let Some(mismatch) = compare_retire_with_ref(&retire_1, &ref_result, 1, cycle, &instructions, &disasm) {
                println!("Cycle: {} {:?}", cycle, mismatch);
                println!("- RefCore {:x?}", ref_result);
                println!("- RTL     {:x?}", retire_1);
                log_decoded_instruction("-", &instructions, retire_1.pc, &disasm);
                println!();
                mismatch_count += 1;
            }
            retired_count += 1;
        }

        if pending_mem_req {
            println!(
                "Cycle {}: Providing memory response for addr 0x{:x}",
                cycle, pending_mem_addr
            );

            let line_base_addr = pending_mem_addr & !(((CACHE_LINE_WORDS * WORD_SIZE as usize) - 1) as u64);
            for i in 0..CACHE_LINE_WORDS {
                let word_addr = line_base_addr + (i as u64 * WORD_SIZE);
                let insn = get_instruction_at_addr(&instructions, word_addr) as u64;
// println!("Pushing inst 0x{:x}", insn);
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
