.PHONY: all sim clean

RISCV_SOURCES := $(shell find ../riscv/src -name "*.scala" 2>/dev/null)
VERILOG_OUTPUT := test-outputs/chirrtl/Tile.fir

all: verilog sim

verilog: $(VERILOG_OUTPUT)

$(VERILOG_OUTPUT): $(RISCV_SOURCES)
	cd .. && ./mill riscv.run

sim: verilog
	cargo run --release -- $(TEST)

clean:
	cargo clean
	rm -rf test-outputs
	rm -f libVdut.so
	rm -f *.vcd
