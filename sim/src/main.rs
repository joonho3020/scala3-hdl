use hdl_sim::asm_parser::parse_asm_file;
use hdl_sim::Dut;
use riscv_sim::RefCore;
use rvdasm::disassembler::Disassembler;
use std::{collections::HashMap, collections::HashSet, env};

const RESET_PC: u64 = 0x80000000;
const CACHE_LINE_WORDS: usize = 16;
const WORD_SIZE: u64 = 4;
const HALT_OPCODE: u32 = 0x0000006f;
const PAGE_SIZE: usize = 4096;
const PAGE_SHIFT: u32 = 12;

const MEM_TYPE_READ: u64 = 0;
const MEM_TYPE_WRITE: u64 = 1;

struct SparseMemory {
    pages: HashMap<u64, Box<[u8; PAGE_SIZE]>>,
}

impl SparseMemory {
    fn new() -> Self {
        SparseMemory {
            pages: HashMap::new(),
        }
    }

    fn get_page_base(addr: u64) -> u64 {
        addr & !((PAGE_SIZE as u64) - 1)
    }

    fn get_page_offset(addr: u64) -> usize {
        (addr & ((PAGE_SIZE as u64) - 1)) as usize
    }

    fn ensure_page(&mut self, page_base: u64) -> &mut Box<[u8; PAGE_SIZE]> {
        self.pages
            .entry(page_base)
            .or_insert_with(|| Box::new([0u8; PAGE_SIZE]))
    }

    fn read_u32(&mut self, addr: u64) -> u32 {
        let page_base = Self::get_page_base(addr);
        let offset = Self::get_page_offset(addr);
        let page = self.ensure_page(page_base);
        u32::from_le_bytes([
            page[offset],
            page[offset + 1],
            page[offset + 2],
            page[offset + 3],
        ])
    }

    fn write_u32(&mut self, addr: u64, value: u32) {
        let page_base = Self::get_page_base(addr);
        let offset = Self::get_page_offset(addr);
        let page = self.ensure_page(page_base);
        let bytes = value.to_le_bytes();
        page[offset] = bytes[0];
        page[offset + 1] = bytes[1];
        page[offset + 2] = bytes[2];
        page[offset + 3] = bytes[3];
    }

    fn load_instructions(&mut self, base_addr: u64, instructions: &[u32]) {
        for (i, &insn) in instructions.iter().enumerate() {
            let addr = base_addr + (i as u64) * 4;
            self.write_u32(addr, insn);
        }
    }
}

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

fn log_decoded_instruction(label: &str, memory: &mut SparseMemory, pc: u64, disasm: &Disassembler) {
    let word = get_word_at_addr(memory, pc);
    match disasm.disassmeble_one(word) {
        Some(decoded) => println!("{} PC=0x{:x} inst=0x{:08x} {:?}", label, pc, word, decoded),
        None          => println!("{} PC=0x{:x} inst=0x{:08x} decode error", label, pc, word),
    }
}

fn compare_retire_with_ref(
    retire: &RetireInfo,
    ref_result: &riscv_sim::StepResult
) -> Option<MismatchType> {
    if retire.pc != ref_result.pc {
        return Some(MismatchType::PCMismatch);
    } else if retire.wb_valid && retire.wb_rd != 0 {
        if retire.wb_rd != ref_result.wb_rd {
            return Some(MismatchType::WBDstMismatch);
        }
        if retire.wb_data != ref_result.wb_data {
            return Some(MismatchType::WBDataMismatch);
        }
    }
    None
}

fn get_word_at_addr(memory: &mut SparseMemory, addr: u64) -> u32 {
    memory.read_u32(addr)
}

fn record_coverage(pc: u64, instructions_len: usize, coverage: &mut HashSet<usize>) {
    if pc < RESET_PC {
        return;
    }
    let offset = ((pc - RESET_PC) / WORD_SIZE) as usize;
    if offset < instructions_len {
        coverage.insert(offset);
    }
}

fn process_retire(
    retire: &RetireInfo,
    pipe_label: &str,
    ref_core: &mut RefCore,
    memory: &mut SparseMemory,
    disasm: &Disassembler,
    cycle: usize,
    mismatch_count: &mut usize,
    retired_count: &mut usize,
    coverage: &mut HashSet<usize>,
    instructions_len: usize,
) -> bool {
    let ref_result = ref_core.step();
    if let Some(mismatch) = compare_retire_with_ref(retire, &ref_result) {
        println!("Cycle: {} {:?} {}", cycle, mismatch, pipe_label);
        println!("- RefCore {:x?}", ref_result);
        println!("- RTL     {:x?}", retire);
        log_decoded_instruction("-", memory, ref_result.pc, disasm);
        ref_core.dump_state();
        println!();
        *mismatch_count += 1;
    }
    *retired_count += 1;
    record_coverage(retire.pc, instructions_len, coverage);
    get_word_at_addr(memory, retire.pc) == HALT_OPCODE
}

fn read_mem_req_data(dut: &Dut) -> [u64; CACHE_LINE_WORDS] {
    [
        dut.peek_io_mem_req_bits_data_0(),
        dut.peek_io_mem_req_bits_data_1(),
        dut.peek_io_mem_req_bits_data_2(),
        dut.peek_io_mem_req_bits_data_3(),
        dut.peek_io_mem_req_bits_data_4(),
        dut.peek_io_mem_req_bits_data_5(),
        dut.peek_io_mem_req_bits_data_6(),
        dut.peek_io_mem_req_bits_data_7(),
        dut.peek_io_mem_req_bits_data_8(),
        dut.peek_io_mem_req_bits_data_9(),
        dut.peek_io_mem_req_bits_data_10(),
        dut.peek_io_mem_req_bits_data_11(),
        dut.peek_io_mem_req_bits_data_12(),
        dut.peek_io_mem_req_bits_data_13(),
        dut.peek_io_mem_req_bits_data_14(),
        dut.peek_io_mem_req_bits_data_15(),
    ]
}

fn poke_mem_resp_data(dut: &mut Dut, data: &[u64; CACHE_LINE_WORDS]) {
    dut.poke_io_mem_resp_bits_lineWords_0(data[0]);
    dut.poke_io_mem_resp_bits_lineWords_1(data[1]);
    dut.poke_io_mem_resp_bits_lineWords_2(data[2]);
    dut.poke_io_mem_resp_bits_lineWords_3(data[3]);
    dut.poke_io_mem_resp_bits_lineWords_4(data[4]);
    dut.poke_io_mem_resp_bits_lineWords_5(data[5]);
    dut.poke_io_mem_resp_bits_lineWords_6(data[6]);
    dut.poke_io_mem_resp_bits_lineWords_7(data[7]);
    dut.poke_io_mem_resp_bits_lineWords_8(data[8]);
    dut.poke_io_mem_resp_bits_lineWords_9(data[9]);
    dut.poke_io_mem_resp_bits_lineWords_10(data[10]);
    dut.poke_io_mem_resp_bits_lineWords_11(data[11]);
    dut.poke_io_mem_resp_bits_lineWords_12(data[12]);
    dut.poke_io_mem_resp_bits_lineWords_13(data[13]);
    dut.poke_io_mem_resp_bits_lineWords_14(data[14]);
    dut.poke_io_mem_resp_bits_lineWords_15(data[15]);
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
    let instructions_len = instructions.len();

    let mut memory = SparseMemory::new();
    memory.load_instructions(RESET_PC, &instructions);

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
    let mut pending_mem_is_write = false;
    let mut pending_mem_data: [u64; CACHE_LINE_WORDS] = [0; CACHE_LINE_WORDS];
    let mut pending_mem_mask: u64 = 0;
    let mut retired_count: usize = 0;
    let mut mismatch_count: usize = 0;
    let mut coverage = HashSet::new();
    let target_coverage = instructions_len;
    let mut stop_reason: Option<String> = None;

    let disasm = Disassembler::new(rvdasm::disassembler::Xlen::XLEN64);

    dut.poke_io_mem_req_ready(1);

    for cycle in 0..1000000 {
        let retire_0 = get_retire_info_0(&dut);
        let retire_1 = get_retire_info_1(&dut);

// if retire_0.valid {
// println!("retire_0 pc 0x{:x}", retire_0.pc);
// }
// if retire_1.valid {
// println!("retire_1 pc 0x{:x}", retire_1.pc);
// }

        let mut halt_detected = false;

        if retire_0.valid {
            if process_retire(
                &retire_0,
                "pipe 0",
                &mut ref_core,
                &mut memory,
                &disasm,
                cycle,
                &mut mismatch_count,
                &mut retired_count,
                &mut coverage,
                instructions_len,
            ) {
                halt_detected = true;
            }
        }

        if retire_1.valid {
            if process_retire(
                &retire_1,
                "pipe 1",
                &mut ref_core,
                &mut memory,
                &disasm,
                cycle,
                &mut mismatch_count,
                &mut retired_count,
                &mut coverage,
                instructions_len,
            ) {
                halt_detected = true;
            }
        }

        if pending_mem_req {
            let line_base_addr = pending_mem_addr & !(((CACHE_LINE_WORDS * WORD_SIZE as usize) - 1) as u64);
            println!("pending mem req 0x{:x} is_write {}", line_base_addr, pending_mem_is_write);

            if pending_mem_is_write {
                for i in 0..CACHE_LINE_WORDS {
                    let word_mask = (pending_mem_mask >> (i * 4)) & 0xF;
                    if word_mask != 0 {
                        let word_addr = line_base_addr + (i as u64 * WORD_SIZE);
                        let data_word = pending_mem_data[i] as u32;
                        let existing = memory.read_u32(word_addr);
                        let mut new_val = existing;
                        for byte_idx in 0..4 {
                            if (word_mask >> byte_idx) & 1 != 0 {
                                let byte_mask = 0xFF << (byte_idx * 8);
                                new_val = (new_val & !byte_mask) | (data_word & byte_mask);
                            }
                        }
                        memory.write_u32(word_addr, new_val);
                    }
                }
            }

            let mut resp_data: [u64; CACHE_LINE_WORDS] = [0; CACHE_LINE_WORDS];
            for i in 0..CACHE_LINE_WORDS {
                let word_addr = line_base_addr + (i as u64 * WORD_SIZE);
                resp_data[i] = get_word_at_addr(&mut memory, word_addr) as u64;
            }
            poke_mem_resp_data(&mut dut, &resp_data);
            dut.poke_io_mem_resp_valid(1);
            pending_mem_req = false;
        } else {
            dut.poke_io_mem_resp_valid(0);
        }

        let mem_req_valid = dut.peek_io_mem_req_valid();
        if mem_req_valid != 0 {
            let addr = dut.peek_io_mem_req_bits_addr();
            let req_type = dut.peek_io_mem_req_bits_tpe();
            pending_mem_req = true;
            pending_mem_addr = addr;
            pending_mem_is_write = req_type == MEM_TYPE_WRITE;
            if pending_mem_is_write {
                pending_mem_data = read_mem_req_data(&dut);
                pending_mem_mask = dut.peek_io_mem_req_bits_mask();
            }
        }

        let coverage_complete = coverage.len() >= target_coverage;
        if halt_detected {
            stop_reason = Some(format!("halt instruction retired at cycle {}", cycle));
        } else if coverage_complete {
            stop_reason = Some(format!(
                "retired all {} instructions by cycle {}",
                target_coverage, cycle
            ));
        }
        if stop_reason.is_some() {
            break;
        }

        dut.step();
    }

    if stop_reason.is_none() {
        stop_reason = Some("cycle limit reached".to_string());
    }

    println!("\n========================================");
    println!("Test Summary:");
    println!("  Total retired: {}", retired_count);
    println!("  Mismatches: {}", mismatch_count);
    println!("  Stop reason: {}", stop_reason.unwrap());
    if mismatch_count == 0 {
        println!("  Status: PASSED");
    } else {
        println!("  Status: FAILED");
    }
    println!("========================================");
}
