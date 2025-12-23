.PHONY: all sim inorder-cosim clean test-all verilog-ooo verilog-inorder

INORDER_SOURCES := $(shell find ../riscv_inorder/src -name "*.scala" 2>/dev/null)
OOO_SOURCES := $(shell find ../riscv_ooo/src -name "*.scala" 2>/dev/null)

all: sim

verilog-inorder: $(INORDER_SOURCES)
	cd .. && ./mill -i riscv_inorder.run

verilog-ooo: $(OOO_SOURCES)
	cd .. && ./mill -i riscv_ooo.run

sim: verilog-ooo
	cargo run --release --bin hdl-sim -- $(TEST)

inorder-cosim: verilog-inorder
	cargo run --release --bin inorder-cosim -- $(TEST)

clean:
	cargo clean
	rm -rf test-outputs
	rm -f libVdut.so
	rm -f *.vcd

test-all: verilog-ooo
	@for hex in tests/*.hex; do \
		echo "Running $$hex..."; \
		cargo run --release --bin hdl-sim -- $$hex || exit 1; \
	done
