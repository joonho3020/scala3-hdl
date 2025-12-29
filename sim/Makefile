.PHONY: all sim inorder-cosim ooo-cosim clean test-all \
        test-alu test-branch test-load-store test-bp test-cache

INORDER_SOURCES := $(shell find ../riscv_inorder/src -name "*.scala" 2>/dev/null)
OOO_SOURCES := $(shell find ../riscv_ooo/src -name "*.scala" 2>/dev/null)
HDL_SOURCES := $(shell find ../hdl/src -name "*.scala" 2>/dev/null)

VERILOG_DIR := test-outputs/verilog
INORDER_VERILOG := $(VERILOG_DIR)/Tile.sv
OOO_VERILOG := $(VERILOG_DIR)/Tile.sv

ALU_TESTS := \
	tests/alu_test.hex

BRANCH_TESTS := \
	tests/riscv64i_shift_cf_test.hex \
	tests/riscv64i_shift_cf_test_long.hex \
	tests/riscv64i_bp_btb_ras_test.hex \
	tests/rv64i_bp_alternating_4.hex \
	tests/rv64i_bp_correlated_4.hex \
	tests/rv64i_bp_warmup_16.hex \
	tests/rv64i_btb_ring64_laps4.hex \
	tests/rv64i_ras_depth16_reps4.hex \
	tests/rv64i_integer_big_test.hex

LOAD_STORE_TESTS := \
	tests/rv64i_load_store_cacheline_test.hex \
	tests/rv64i_pointer_chase_cache_stress.hex \
	tests/rv64i_dirty_evict_reload_cache2k.hex

all: sim

$(VERILOG_DIR):
	mkdir -p $(VERILOG_DIR)

.inorder-verilog-stamp: $(INORDER_SOURCES) $(HDL_SOURCES) | $(VERILOG_DIR)
	-rm -f $(INORDER_VERILOG)
	cd .. && ./mill -i riscv_inorder.run
	test -s $(INORDER_VERILOG)
	touch $@

.ooo-verilog-stamp: $(OOO_SOURCES) $(HDL_SOURCES) | $(VERILOG_DIR)
	-rm -f $(OOO_VERILOG)
	cd .. && ./mill -i riscv_ooo.run
	test -s $(OOO_VERILOG)
	touch $@

ooo-cosim: .ooo-verilog-stamp
	cargo run --release --bin ooo-cosim -- $(TEST)

inorder-cosim: .inorder-verilog-stamp
	cargo run --release --bin inorder-cosim -- $(TEST)

clean:
	cargo clean
	rm -rf test-outputs
	rm -f libVdut.so
	rm -f .inorder-verilog-stamp .ooo-verilog-stamp

test-all: .inorder-verilog-stamp
	@for hex in tests/*.hex; do \
		echo "Running $$hex..."; \
		cargo run --release --bin hdl-sim -- $$hex || exit 1; \
	done

test-alu: .ooo-verilog-stamp
	@for hex in $(ALU_TESTS); do \
		echo "Running $$hex..."; \
		cargo run --release --bin ooo-cosim -- $$hex || exit 1; \
	done

test-branch: .ooo-verilog-stamp
	@for hex in $(BRANCH_TESTS); do \
		echo "Running $$hex..."; \
		cargo run --release --bin ooo-cosim -- $$hex || exit 1; \
	done

test-load-store: .ooo-verilog-stamp
	@for hex in $(LOAD_STORE_TESTS); do \
		echo "Running $$hex..."; \
		cargo run --release --bin ooo-cosim -- $$hex || exit 1; \
	done

test-bp: .ooo-verilog-stamp
	@for hex in $(BP_TESTS); do \
		echo "Running $$hex..."; \
		cargo run --release --bin ooo-cosim -- $$hex || exit 1; \
	done

test-cache: .ooo-verilog-stamp
	@for hex in $(CACHE_TESTS); do \
		echo "Running $$hex..."; \
		cargo run --release --bin ooo-cosim -- $$hex || exit 1; \
	done
