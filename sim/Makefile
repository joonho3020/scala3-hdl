.PHONY: all verilog sim clean

all: verilog sim

verilog:
	cd .. && ./mill riscv.run

sim: verilog
	cargo run --release

clean:
	cargo clean
	rm -rf test-outputs
	rm -f libVdut.so
	rm -f *.vcd
