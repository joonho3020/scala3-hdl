use hdl_sim::asm_parser::parse_asm_file;
use hdl_sim::Dut;
use riscv_sim::RefCore;
use rvdasm::disassembler::Disassembler;
use std::{collections::HashMap, collections::HashSet, collections::VecDeque, env};

const RESET_PC: u64 = 0x80000000;
const CACHE_LINE_WORDS: usize = 16;
const WORD_SIZE: u64 = 4;
const HALT_OPCODE: u32 = 0x0000006f;
const PAGE_SIZE: usize = 4096;
const MEM_TYPE_WRITE: u64 = 1;
const RETIRE_WIDTH: usize = 4;
const MAX_MISMATCHES: usize = 10;
const PC_HISTORY_SIZE: usize = 20;
const HANG_THRESHOLD: usize = 200;

#[derive(Clone)]
struct MemReq {
    addr: u64,
    tpe: u64,
    tag: u64,
    is_write: bool,
    data: [u64; CACHE_LINE_WORDS],
    mask: u64,
}

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
    MemAddrMismatch,
    StoreDataMismatch,
}

#[derive(Debug)]
struct RetireInfo {
    valid: bool,
    pc: u64,
    wb_valid: bool,
    wb_rd: u64,
    wb_data: u64,
    bpu_preds: u64,
    bpu_hits: u64,
    mem_addr: u64,
    is_load: bool,
    is_store: bool,
    store_data: u64,
}

fn get_retire_info(dut: &mut Dut, idx: usize) -> RetireInfo {
    RetireInfo {
        valid: dut.io().retire_info().get(idx).valid().peek() != 0,
        pc: dut.io().retire_info().get(idx).pc().peek(),
        wb_valid: dut.io().retire_info().get(idx).wb_valid().peek() != 0,
        wb_rd: dut.io().retire_info().get(idx).wb_rd().peek(),
        wb_data: dut.io().retire_info().get(idx).wb_data().peek(),
        bpu_preds: dut.io().retire_info().get(idx).bpu_preds().peek(),
        bpu_hits: dut.io().retire_info().get(idx).bpu_hits().peek(),
        mem_addr: dut.io().retire_info().get(idx).mem_addr().peek(),
        is_load: dut.io().retire_info().get(idx).is_load().peek() != 0,
        is_store: dut.io().retire_info().get(idx).is_store().peek() != 0,
        store_data: dut.io().retire_info().get(idx).store_data().peek(),
    }
}

fn log_decoded_instruction(label: &str, memory: &mut SparseMemory, pc: u64, disasm: &Disassembler) {
    let word = get_word_at_addr(memory, pc);
    match disasm.disassmeble_one(word) {
        Some(decoded) => println!("{} PC=0x{:x} inst=0x{:08x} {:?}", label, pc, word, decoded),
        None => println!("{} PC=0x{:x} inst=0x{:08x} decode error", label, pc, word),
    }
}

fn compare_retire_with_ref(retire: &RetireInfo, ref_result: &riscv_sim::StepResult) -> Option<MismatchType> {
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

    if retire.is_load && ref_result.mem_read_addr.is_some() {
        if retire.mem_addr != ref_result.mem_read_addr.unwrap() {
            return Some(MismatchType::MemAddrMismatch);
        }
    }

    if retire.is_store && ref_result.mem_write_addr.is_some() {
        if retire.mem_addr != ref_result.mem_write_addr.unwrap() {
            return Some(MismatchType::MemAddrMismatch);
        }
        if ref_result.mem_write_data.is_some() && retire.store_data != ref_result.mem_write_data.unwrap() {
            return Some(MismatchType::StoreDataMismatch);
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
    pipe_idx: usize,
    ref_core: &mut RefCore,
    memory: &mut SparseMemory,
    disasm: &Disassembler,
    cycle: usize,
    mismatch_count: &mut usize,
    retired_count: &mut usize,
    coverage: &mut HashSet<usize>,
    instructions_len: usize,
    pc_history: &mut VecDeque<u64>,
    dut: &mut Dut,
) -> bool {
    let pipe_label = format!("pipe {}", pipe_idx);
    let ref_result = ref_core.step();
    let mismatch_opt = compare_retire_with_ref(retire, &ref_result);
    let has_mismatch = mismatch_opt.is_some();
    if let Some(mismatch) = mismatch_opt {
        println!("Cycle: {} {:?} {}", cycle, mismatch, pipe_label);
        println!("- RefCore {:x?}", ref_result);
        println!("- RTL     {:x?}", retire);
        match (ref_result.mem_read_addr, ref_result.mem_write_addr) {
            (Some(read), Some(write)) => {
                println!("- RefCore mem read 0x{:x} write 0x{:x}", read, write);
                if let Some(write_data) = ref_result.mem_write_data {
                    println!("- RefCore mem write data 0x{:x}", write_data);
                }
            }
            (Some(read), None) => println!("- RefCore mem read 0x{:x}", read),
            (None, Some(write)) => {
                println!("- RefCore mem write 0x{:x}", write);
                if let Some(write_data) = ref_result.mem_write_data {
                    println!("- RefCore mem write data 0x{:x}", write_data);
                }
            }
            (None, None) => {}
        }
        if retire.is_load {
            println!("- RTL     mem load 0x{:x}", retire.mem_addr);
        } else if retire.is_store {
            println!("- RTL     mem store 0x{:x} data 0x{:x}", retire.mem_addr, retire.store_data);
        }
        log_decoded_instruction(&pipe_label, memory, ref_result.pc, disasm);
        ref_core.dump_state();
        println!("- Previous {} PCs:", pc_history.len());
        for (i, pc) in pc_history.iter().enumerate() {
            print!("  0x{:x}", pc);
            if (i + 1) % 5 == 0 {
                println!();
            }
        }
        if pc_history.len() % 5 != 0 {
            println!();
        }
        println!();
        *mismatch_count += 1;
    }
    let mem_addr = ref_result.mem_read_addr.or(ref_result.mem_write_addr).unwrap_or(0);
    let is_load = ref_result.mem_read_addr.is_some();
    let is_store = ref_result.mem_write_addr.is_some();
    let store_data = ref_result.mem_write_data.unwrap_or(0);

    poke_cosim_info(
        dut,
        pipe_idx,
        true,
        ref_result.pc,
        ref_result.next_pc,
        ref_result.wb_valid,
        ref_result.wb_data,
        ref_result.wb_rd,
        has_mismatch,
        mem_addr,
        is_load,
        is_store,
        store_data,
    );
    if pc_history.len() >= PC_HISTORY_SIZE {
        pc_history.pop_front();
    }
    pc_history.push_back(retire.pc);
    *retired_count += 1;
    record_coverage(retire.pc, instructions_len, coverage);
    get_word_at_addr(memory, retire.pc) == HALT_OPCODE
}

fn read_mem_req_data(dut: &mut Dut) -> [u64; CACHE_LINE_WORDS] {
    let mut result = [0u64; CACHE_LINE_WORDS];
    for i in 0..CACHE_LINE_WORDS {
        result[i] = dut.io().mem().req().bits().data().get(i).peek();
    }
    result
}

fn poke_mem_resp_data(dut: &mut Dut, data: &[u64; CACHE_LINE_WORDS]) {
    for i in 0..CACHE_LINE_WORDS {
        dut.io().mem().resp().bits().lineWords().get(i).poke(data[i]);
    }
}

fn poke_mem_resp(dut: &mut Dut, req: &MemReq, data: &[u64; CACHE_LINE_WORDS]) {
    dut.io().mem().resp().bits().tag().poke(req.tag);
    dut.io().mem().resp().bits().tpe().poke(req.tpe);
    poke_mem_resp_data(dut, data);
}

fn poke_cosim_info(
    dut: &mut Dut,
    idx: usize,
    valid: bool,
    pc: u64,
    next_pc: u64,
    wb_valid: bool,
    wb_data: u64,
    wb_rd: u64,
    mismatch: bool,
    mem_addr: u64,
    is_load: bool,
    is_store: bool,
    store_data: u64,
) {
    dut.io().cosim_info().get(idx).valid().poke(valid as u64);
    dut.io().cosim_info().get(idx).pc().poke(pc);
    dut.io().cosim_info().get(idx).next_pc().poke(next_pc);
    dut.io().cosim_info().get(idx).wb_valid().poke(wb_valid as u64);
    dut.io().cosim_info().get(idx).wb_data().poke(wb_data);
    dut.io().cosim_info().get(idx).wb_rd().poke(wb_rd);
    dut.io().cosim_info().get(idx).mismatch().poke(mismatch as u64);
    dut.io().cosim_info().get(idx).mem_addr().poke(mem_addr);
    dut.io().cosim_info().get(idx).is_load().poke(is_load as u64);
    dut.io().cosim_info().get(idx).is_store().poke(is_store as u64);
    dut.io().cosim_info().get(idx).store_data().poke(store_data);
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
        ref_core.set_reg(i, 0);
    }

    dut.reset();

    println!("Starting OOO co-simulation with reference core comparison...\n");

    let mut pending_mem_reqs: VecDeque<MemReq> = VecDeque::new();
    let mut retired_count: usize = 0;
    let mut mismatch_count: usize = 0;
    let mut coverage = HashSet::new();
    let target_coverage = instructions_len;
    let mut stop_reason: Option<String> = None;
    let mut pc_history: VecDeque<u64> = VecDeque::with_capacity(PC_HISTORY_SIZE);
    let mut cycles_since_retire: usize = 0;

    let disasm = Disassembler::new(rvdasm::disassembler::Xlen::XLEN64);

    dut.io().mem().req().ready().poke(1);

    for cycle in 0..100000 {
        let mut halt_detected = false;
        let mut any_retired = false;
        for lane in 0..RETIRE_WIDTH {
            let retire = get_retire_info(&mut dut, lane);
            if retire.valid {
                any_retired = true;
                if process_retire(
                    &retire,
                    lane,
                    &mut ref_core,
                    &mut memory,
                    &disasm,
                    cycle,
                    &mut mismatch_count,
                    &mut retired_count,
                    &mut coverage,
                    instructions_len,
                    &mut pc_history,
                    &mut dut,
                ) {
                    halt_detected = true;
                }
            } else {
                poke_cosim_info(&mut dut, lane, false, 0, 0, false, 0, 0, false, 0, false, false, 0);
            }
        }

        if any_retired {
            cycles_since_retire = 0;
        } else {
            cycles_since_retire += 1;
        }

        if let Some(mem_req) = pending_mem_reqs.pop_front() {
            let line_base_addr =
                mem_req.addr & !(((CACHE_LINE_WORDS * WORD_SIZE as usize) - 1) as u64);
            if mem_req.is_write {
                for i in 0..CACHE_LINE_WORDS {
                    let word_mask = (mem_req.mask >> (i * 4)) & 0xF;
                    if word_mask != 0 {
                        let word_addr = line_base_addr + (i as u64 * WORD_SIZE);
                        let data_word = mem_req.data[i] as u32;
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
            poke_mem_resp(&mut dut, &mem_req, &resp_data);
            dut.io().mem().resp().valid().poke(1);
        } else {
            dut.io().mem().resp().valid().poke(0);
        }

        let mem_req_valid = dut.io().mem().req().valid().peek();
        if mem_req_valid != 0 {
            let addr = dut.io().mem().req().bits().addr().peek();
            let req_type = dut.io().mem().req().bits().tpe().peek();
            let tag = dut.io().mem().req().bits().tag().peek();
            let is_write = req_type == MEM_TYPE_WRITE;
            let (data, mask) = if is_write {
                (
                    read_mem_req_data(&mut dut),
                    dut.io().mem().req().bits().mask().peek(),
                )
            } else {
                ([0u64; CACHE_LINE_WORDS], 0)
            };
            pending_mem_reqs.push_back(MemReq {
                addr,
                tpe: req_type,
                tag,
                is_write,
                data,
                mask,
            });
        }

        let coverage_complete = coverage.len() >= target_coverage;
        if mismatch_count >= MAX_MISMATCHES {
            stop_reason = Some(format!("reached {} mismatches at cycle {}", MAX_MISMATCHES, cycle));
        } else if halt_detected {
            stop_reason = Some(format!("halt instruction retired at cycle {}", cycle));
        } else if coverage_complete {
            stop_reason = Some(format!("retired all {} instructions by cycle {}", target_coverage, cycle));
        } else if cycles_since_retire >= HANG_THRESHOLD {
            stop_reason = Some(format!("core hung: no instructions retired for {} cycles (cycle {})", HANG_THRESHOLD, cycle));
        }
        if stop_reason.is_some() {
            break;
        }

        dut.step();
    }

    if stop_reason.is_none() {
        stop_reason = Some("cycle limit reached".to_string());
    }

    let final_stats = get_retire_info(&mut dut, 0);
    let bpu_preds = final_stats.bpu_preds;
    let bpu_hits = final_stats.bpu_hits;
    let bpu_rate = if bpu_preds == 0 {
        0.0
    } else {
        bpu_hits as f64 / bpu_preds as f64
    };

    println!("\n========================================");
    println!("OOO Test Summary:");
    println!("  Total retired: {}", retired_count);
    println!("  Mismatches: {}", mismatch_count);
    println!("  Stop reason: {}", stop_reason.unwrap());
    println!("  BPU preds: {} hits: {} rate: {:.3}", bpu_preds, bpu_hits, bpu_rate);
    if mismatch_count == 0 {
        println!("  Status: PASSED");
    } else {
        println!("  Status: FAILED");
    }
    println!("========================================");
}
