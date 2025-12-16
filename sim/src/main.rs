use hdl_sim::Dut;

fn main() {
    let mut dut = Dut::new();
    dut.enable_trace();

    dut.poke_reset(1);
    dut.poke_clock(0);
    dut.poke_io_in(0);
    dut.step();
    dut.poke_clock(1);
    dut.step();

    dut.poke_reset(0);

    for i in 0..10 {
        dut.poke_io_in(1);

        dut.poke_clock(0);
        dut.step();
        dut.poke_clock(1);
        dut.step();

        let out = dut.peek_io_out();
        println!("Cycle {}: io_out = {}", i, out);
    }

    println!("Simulation completed at timestep {}", dut.timestep());
}
