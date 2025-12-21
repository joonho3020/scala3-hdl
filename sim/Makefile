.PHONY: all sim clean test-all

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

test-all: verilog
	@for hex in tests/*.hex; do \
		echo "Running $$hex..."; \
		cargo run --release -- $$hex || exit 1; \
	done
