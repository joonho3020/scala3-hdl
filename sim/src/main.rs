use hdl_sim::Dut;

fn main() {
    let mut dut = Dut::new();
    dut.enable_trace();

    // Create test instructions (cache line aligned at 0x80000000)
    // Note: Due to missing register file, rs1/rs2 values are used directly by ALU
    let instructions = [
        0x003282b3u64, // ADD x1, x5, x3  -> 5 + 3 = 8
        0x40450133u64, // SUB x2, x10, x4 -> 10 - 4 = 6
        0x007781b3u64, // ADD x3, x15, x7 -> 15 + 7 = 22
        0x408a0233u64, // SUB x4, x20, x8 -> 20 - 8 = 12
        0x00208133u64, // ADD x2, x1, x2  -> 1 + 2 = 3
        0x40310233u64, // SUB x4, x2, x3  -> 2 - 3 = -1 (0xFFFFFFFF)
        0x00000013u64, // NOP (ADDI x0, x0, 0)
        0x00000013u64, // NOP
        0x00000013u64, // NOP
        0x00000013u64, // NOP
        0x00000013u64, // NOP
        0x00000013u64, // NOP
        0x00000013u64, // NOP
        0x00000013u64, // NOP
        0x00000013u64, // NOP
        0x00000013u64, // NOP
    ];

    let expected_results = [
        8u64,  // 5 + 3
        6u64,  // 10 - 4
        22u64, // 15 + 7
        12u64, // 20 - 8
        3u64,  // 1 + 2
        0xFFFFFFFFu64, // 2 - 3 = -1 (in 32-bit)
    ];

    // Reset sequence
    dut.poke_reset(1);
    for _ in 0..20 {
        dut.step();
    }
    dut.poke_reset(0);

    println!("Starting ADD/SUB instruction tests...\n");

    let mut test_count = 0;
    let mut pass_count = 0;
    let mut pending_mem_req = false;
    let mut pending_mem_addr = 0u64;

    // Run simulation
    for cycle in 0..300 {
        dut.step();

        // Check for memory request after negedge
        let mem_req_valid = dut.peek_io_mem_req_valid();
        if mem_req_valid != 0 {
            let addr = dut.peek_io_mem_req_bits_addr();
            println!("Cycle {}: Memory request for addr 0x{:x}", cycle, addr);
            pending_mem_req = true;
            pending_mem_addr = addr;
        }

        // Provide response if we had a pending request
        if pending_mem_req {
            println!("Cycle {}: Providing memory response for addr 0x{:x}", cycle, pending_mem_addr);

            // Provide cache line with instructions
            for i in 0..16 {
                match i {
                    0 => dut.poke_io_mem_resp_bits_lineWords_0(instructions[i]),
                    1 => dut.poke_io_mem_resp_bits_lineWords_1(instructions[i]),
                    2 => dut.poke_io_mem_resp_bits_lineWords_2(instructions[i]),
                    3 => dut.poke_io_mem_resp_bits_lineWords_3(instructions[i]),
                    4 => dut.poke_io_mem_resp_bits_lineWords_4(instructions[i]),
                    5 => dut.poke_io_mem_resp_bits_lineWords_5(instructions[i]),
                    6 => dut.poke_io_mem_resp_bits_lineWords_6(instructions[i]),
                    7 => dut.poke_io_mem_resp_bits_lineWords_7(instructions[i]),
                    8 => dut.poke_io_mem_resp_bits_lineWords_8(instructions[i]),
                    9 => dut.poke_io_mem_resp_bits_lineWords_9(instructions[i]),
                    10 => dut.poke_io_mem_resp_bits_lineWords_10(instructions[i]),
                    11 => dut.poke_io_mem_resp_bits_lineWords_11(instructions[i]),
                    12 => dut.poke_io_mem_resp_bits_lineWords_12(instructions[i]),
                    13 => dut.poke_io_mem_resp_bits_lineWords_13(instructions[i]),
                    14 => dut.poke_io_mem_resp_bits_lineWords_14(instructions[i]),
                    15 => dut.poke_io_mem_resp_bits_lineWords_15(instructions[i]),
                    _ => {}
                }
            }
            dut.poke_io_mem_resp_valid(1);
            pending_mem_req = false;
        } else {
            dut.poke_io_mem_resp_valid(0);
        }

        // Check ALU output
        let alu_valid = dut.peek_io_alu_valid();
        if alu_valid != 0 {
            let alu_out = dut.peek_io_alu_out();

            if test_count < expected_results.len() {
                let expected = expected_results[test_count];
                let passed = alu_out == expected;

                println!(
                    "Cycle {}, Test {}: ALU output = 0x{:08x} (expected 0x{:08x}) - {}",
                    cycle,
                    test_count,
                    alu_out,
                    expected,
                    if passed { "PASS" } else { "FAIL" }
                );

                if passed {
                    pass_count += 1;
                }
                test_count += 1;
            } else {
                println!("Cycle {}: ALU output = 0x{:08x}", cycle, alu_out);
            }
        }

        // Stop after we've seen all expected results
        if test_count >= expected_results.len() {
            break;
        }
    }

    println!("\n=== Test Summary ===");
    println!("Total tests: {}", test_count);
    println!("Passed: {}", pass_count);
    println!("Failed: {}", test_count - pass_count);
    println!("Simulation completed at timestep {}", dut.timestep());

    if pass_count == expected_results.len() {
        println!("\nAll tests PASSED!");
    } else {
        println!("\nSome tests FAILED!");
    }
}
