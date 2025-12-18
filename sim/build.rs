use std::env;
use std::fs;
use std::fs::File;
use std::io::{self, BufRead, BufWriter, Write};
use std::path::Path;
use std::process::Command;

#[derive(Debug)]
struct Signal {
    input: bool,
    name: String,
    bits: u32,
}

fn parse_verilator_header(file_path: &str) -> io::Result<Vec<Signal>> {
    let mut signals = Vec::new();
    let file = File::open(file_path)?;
    let reader = io::BufReader::new(file);

    for line in reader.lines() {
        let line = line?.trim().to_string();
        if line.contains("VL_IN") || line.contains("VL_OUT") {
            let parts: Vec<String> = line
                .replace("(", ",")
                .replace(")", ",")
                .replace("&", ",")
                .split(',')
                .map(|s| s.trim().to_string())
                .collect();

            if parts.len() >= 5 {
                let input = parts[0].contains("VL_IN");
                let name = parts[2].to_string();
                let bits: u32 =
                    parts[3].parse::<u32>().unwrap() - parts[4].parse::<u32>().unwrap() + 1;

                signals.push(Signal { input, name, bits });
            }
        }
    }
    Ok(signals)
}

fn generate_c_bindings(top: &str, signals: &Vec<Signal>, output_path: &str) -> io::Result<()> {
    let vtop = format!("V{}", top);
    let file = File::create(output_path)?;
    let mut writer = BufWriter::new(file);

    writeln!(writer, "#include <inttypes.h>")?;
    writeln!(writer, "#include \"verilated.h\"")?;
    writeln!(writer, "#include \"verilated_vcd_c.h\"")?;
    writeln!(writer, "#include \"V{}.h\"", top)?;
    writeln!(writer)?;
    writeln!(writer, "vluint64_t main_time = 0;")?;
    writeln!(writer, "double sc_time_stamp() {{ return main_time; }}")?;
    writeln!(writer)?;
    writeln!(writer, "extern \"C\" {{\n")?;

    writeln!(writer, "    {}* {}_new() {{", vtop, top)?;
    writeln!(writer, "        return new {};", vtop)?;
    writeln!(writer, "    }}\n")?;

    writeln!(writer, "    void {}_eval({}* dut) {{", top, vtop)?;
    writeln!(writer, "        dut->eval();")?;
    writeln!(writer, "    }}\n")?;

    writeln!(writer, "    void {}_delete({}* dut) {{", top, vtop)?;
    writeln!(writer, "        delete dut;")?;
    writeln!(writer, "    }}\n")?;

    writeln!(writer, "    VerilatedVcdC* enable_trace({}* dut) {{", vtop)?;
    writeln!(writer, "        Verilated::traceEverOn(true);")?;
    writeln!(writer, "        VerilatedVcdC* tfp = new VerilatedVcdC;")?;
    writeln!(writer, "        dut->trace(tfp, 99);")?;
    writeln!(writer, "        tfp->open(\"sim.vcd\");")?;
    writeln!(writer, "        return tfp;")?;
    writeln!(writer, "    }}\n")?;

    writeln!(writer, "    void close_trace(VerilatedVcdC* tfp) {{")?;
    writeln!(writer, "        tfp->close();")?;
    writeln!(writer, "    }}\n")?;

    writeln!(
        writer,
        "    void dump_vcd(VerilatedVcdC* tfp, unsigned int i) {{"
    )?;
    writeln!(writer, "        tfp->dump(i);")?;
    writeln!(writer, "    }}\n")?;

    for signal in signals {
        if signal.bits > 64 {
            let chunks = (signal.bits + 32 - 1) / 32;
            if signal.input {
                writeln!(
                    writer,
                    "    void poke_{} ({}* dut, uint32_t* {}) {{",
                    signal.name, vtop, signal.name
                )?;
                writeln!(writer, "        for (int i = 0; i < {}; i++) {{", chunks)?;
                writeln!(
                    writer,
                    "            dut->{}[i] = {}[i];",
                    signal.name, signal.name
                )?;
                writeln!(writer, "        }}")?;
                writeln!(writer, "    }}\n")?;
            } else {
                writeln!(
                    writer,
                    "    void peek_{} ({}* dut, uint32_t* value) {{",
                    signal.name, vtop
                )?;
                writeln!(writer, "        for (int i = 0; i < {}; i++) {{", chunks)?;
                writeln!(writer, "            value[i] = dut->{}[i];", signal.name)?;
                writeln!(writer, "        }}")?;
                writeln!(writer, "    }}\n")?;
            }
        } else {
            if signal.input {
                writeln!(
                    writer,
                    "    void poke_{} ({}* dut, uint64_t {}) {{",
                    signal.name, vtop, signal.name
                )?;
                writeln!(writer, "        dut->{} = {};", signal.name, signal.name)?;
                writeln!(writer, "    }}\n")?;
            } else {
                writeln!(
                    writer,
                    "    uint64_t peek_{} ({}* dut) {{",
                    signal.name, vtop
                )?;
                writeln!(writer, "        return dut->{};", signal.name)?;
                writeln!(writer, "    }}\n")?;
            }
        }
    }

    writeln!(writer, "}} // extern \"C\"\n")?;

    Ok(())
}

fn generate_rust_bindings(top: &str, signals: &Vec<Signal>, output_path: &str) -> io::Result<()> {
    let vtop = format!("V{}", top);
    let file = File::create(output_path)?;
    let mut writer = BufWriter::new(file);

    writeln!(writer, "#[repr(C)]")?;
    writeln!(writer, "pub struct {} {{", vtop)?;
    writeln!(writer, "    _private: [u8; 0],")?;
    writeln!(writer, "}}\n")?;

    writeln!(writer, "#[repr(C)]")?;
    writeln!(writer, "pub struct VerilatedVcdC {{")?;
    writeln!(writer, "    _private: [u8; 0],")?;
    writeln!(writer, "}}\n")?;

    writeln!(writer, "extern \"C\" {{")?;
    writeln!(writer, "    pub fn {}_new() -> *mut {};", top, vtop)?;
    writeln!(writer, "    pub fn {}_eval(dut: *mut {});", top, vtop)?;
    writeln!(writer, "    pub fn {}_delete(dut: *mut {});", top, vtop)?;
    writeln!(
        writer,
        "    pub fn enable_trace(dut: *mut {}) -> *mut VerilatedVcdC;",
        vtop
    )?;
    writeln!(writer, "    pub fn close_trace(tfp: *mut VerilatedVcdC);")?;
    writeln!(
        writer,
        "    pub fn dump_vcd(tfp: *mut VerilatedVcdC, timestep: u32);"
    )?;

    for signal in signals {
        if signal.bits > 64 {
            if signal.input {
                writeln!(
                    writer,
                    "    pub fn poke_{}(dut: *mut {}, {}: *const u32);",
                    signal.name, vtop, signal.name
                )?;
            } else {
                writeln!(
                    writer,
                    "    pub fn peek_{}(dut: *mut {}, {}: *mut u32);",
                    signal.name, vtop, signal.name
                )?;
            }
        } else {
            if signal.input {
                writeln!(
                    writer,
                    "    pub fn poke_{}(dut: *mut {}, {}: u64);",
                    signal.name, vtop, signal.name
                )?;
            } else {
                writeln!(
                    writer,
                    "    pub fn peek_{}(dut: *mut {}) -> u64;",
                    signal.name, vtop
                )?;
            }
        }
    }

    writeln!(writer, "}}\n")?;

    writeln!(writer, "pub struct Dut {{")?;
    writeln!(writer, "    ptr: *mut {},", vtop)?;
    writeln!(writer, "    trace: Option<*mut VerilatedVcdC>,")?;
    writeln!(writer, "    timestep: u32,")?;
    writeln!(writer, "}}\n")?;

    writeln!(writer, "unsafe impl Send for Dut {{}}\n")?;

    writeln!(writer, "impl Dut {{")?;
    writeln!(writer, "    pub fn new() -> Self {{")?;
    writeln!(writer, "        unsafe {{")?;
    writeln!(writer, "            Dut {{")?;
    writeln!(writer, "                ptr: {}_new(),", top)?;
    writeln!(writer, "                trace: None,")?;
    writeln!(writer, "                timestep: 0,")?;
    writeln!(writer, "            }}")?;
    writeln!(writer, "        }}")?;
    writeln!(writer, "    }}\n")?;

    writeln!(writer, "    pub fn enable_trace(&mut self) {{")?;
    writeln!(writer, "        unsafe {{")?;
    writeln!(writer, "            self.trace = Some(enable_trace(self.ptr));")?;
    writeln!(writer, "        }}")?;
    writeln!(writer, "    }}\n")?;

    writeln!(writer, "    pub fn eval(&mut self) {{")?;
    writeln!(writer, "        unsafe {{")?;
    writeln!(writer, "            {}_eval(self.ptr);", top)?;
    writeln!(writer, "        }}")?;
    writeln!(writer, "    }}\n")?;

    writeln!(writer, "    pub fn step(&mut self) {{")?;
    writeln!(writer, "        if let Some(tfp) = self.trace {{")?;
    writeln!(writer, "            unsafe {{")?;
    writeln!(writer, "                dump_vcd(tfp, self.timestep);")?;
    writeln!(writer, "            }}")?;
    writeln!(writer, "        }}")?;
    writeln!(writer, "        self.timestep += 1;")?;
    writeln!(writer, "        self.poke_clock(0);")?;
    writeln!(writer, "        self.eval();")?;
    writeln!(writer, "        if let Some(tfp) = self.trace {{")?;
    writeln!(writer, "            unsafe {{")?;
    writeln!(writer, "                dump_vcd(tfp, self.timestep);")?;
    writeln!(writer, "            }}")?;
    writeln!(writer, "        }}")?;
    writeln!(writer, "        self.timestep += 1;")?;
    writeln!(writer, "        self.poke_clock(1);")?;
    writeln!(writer, "        self.eval();")?;
    writeln!(writer, "    }}\n")?;

    writeln!(writer, "    pub fn timestep(&self) -> u32 {{")?;
    writeln!(writer, "        self.timestep")?;
    writeln!(writer, "    }}\n")?;

    writeln!(writer, "    pub fn reset(&mut self) {{")?;
    writeln!(writer, "        self.poke_clock(1);")?;
    writeln!(writer, "        self.poke_reset(1);")?;
    writeln!(writer, "        self.eval();")?;
    writeln!(writer, "        self.step();")?;
    writeln!(writer, "        self.step();")?;
    writeln!(writer, "        self.step();")?;
    writeln!(writer, "        self.step();")?;
    writeln!(writer, "        self.poke_reset(0);")?;
    writeln!(writer, "        self.step();")?;
    writeln!(writer, "    }}\n")?;

    for signal in signals {
        if signal.input {
            if signal.bits > 64 {
                writeln!(writer, "    pub fn poke_{}(&mut self, value: &[u32]) {{", signal.name)?;
                writeln!(writer, "        unsafe {{ poke_{}(self.ptr, value.as_ptr()); }}", signal.name)?;
                writeln!(writer, "    }}\n")?;
            } else {
                writeln!(writer, "    pub fn poke_{}(&mut self, value: u64) {{", signal.name)?;
                writeln!(writer, "        unsafe {{ poke_{}(self.ptr, value); }}", signal.name)?;
                writeln!(writer, "    }}\n")?;
            }
        } else {
            if signal.bits > 64 {
                let chunks = (signal.bits + 31) / 32;
                writeln!(writer, "    pub fn peek_{}(&self) -> [u32; {}] {{", signal.name, chunks)?;
                writeln!(writer, "        let mut value = [0u32; {}];", chunks)?;
                writeln!(writer, "        unsafe {{ peek_{}(self.ptr, value.as_mut_ptr()); }}", signal.name)?;
                writeln!(writer, "        value")?;
                writeln!(writer, "    }}\n")?;
            } else {
                writeln!(writer, "    pub fn peek_{}(&self) -> u64 {{", signal.name)?;
                writeln!(writer, "        unsafe {{ peek_{}(self.ptr) }}", signal.name)?;
                writeln!(writer, "    }}\n")?;
            }
        }
    }

    writeln!(writer, "}}\n")?;

    writeln!(writer, "impl Drop for Dut {{")?;
    writeln!(writer, "    fn drop(&mut self) {{")?;
    writeln!(writer, "        if let Some(tfp) = self.trace {{")?;
    writeln!(writer, "            unsafe {{ close_trace(tfp); }}")?;
    writeln!(writer, "        }}")?;
    writeln!(writer, "        unsafe {{ {}_delete(self.ptr); }}", top)?;
    writeln!(writer, "    }}")?;
    writeln!(writer, "}}")?;

    Ok(())
}

fn main() -> std::io::Result<()> {
    println!("cargo:rerun-if-changed=build.rs");

    let sv_file_path =
        env::var("SV_FILE").unwrap_or_else(|_| "./test-outputs/verilog/Tile.sv".to_string());
    let filelist_path =
        env::var("FILELIST").unwrap_or_else(|_| "./test-outputs/verilog/filelist.f".to_string());
    let build_dir = env::var("BUILD_DIR").unwrap_or_else(|_| "test-outputs/verilog".to_string());

    let mut cwd = env::current_dir()?;
    cwd.push(&build_dir);
    fs::create_dir_all(&cwd)?;

    let verilator_check = Command::new("which").arg("verilator").output();
    if verilator_check.is_err() || !verilator_check.unwrap().status.success() {
        panic!("verilator is not in PATH");
    }

    let sv_file_path = Path::new(&sv_file_path);
    if !sv_file_path.exists() {
        panic!(
            "SystemVerilog file not found: {}. Set SV_FILE env var.",
            sv_file_path.display()
        );
    }
    let top = sv_file_path.file_stem().unwrap().to_str().unwrap();

    let filelist_path = Path::new(&filelist_path);
    if !filelist_path.exists() {
        panic!(
            "Filelist not found: {}. Set FILELIST env var.",
            filelist_path.display()
        );
    }
    let filelist = filelist_path.file_name().unwrap().to_str().unwrap();

    println!("cargo:warning=Running verilator on {}", top);

    let status = Command::new("verilator")
        .current_dir(&cwd)
        .arg("-j")
        .arg("8")
        .arg("--cc")
        .arg("-f")
        .arg(filelist)
        .arg("--build")
        .arg("-CFLAGS")
        .arg("-fPIC")
        .arg("--trace")
        .arg("--top-module")
        .arg(top)
        .status()?;

    if !status.success() {
        panic!("verilator failed");
    } else {
        println!("cargo:warning=Verilator compile passed");
    }


    let obj_dir = format!("{}/obj_dir", cwd.to_str().unwrap());
    let vtop_h_path = format!("{}/V{}.h", obj_dir, top);

    let signals = parse_verilator_header(&vtop_h_path)?;
    println!("cargo:warning=Found {} signals", signals.len());

    let ctop_c_path = format!("{}/C{}.cpp", obj_dir, top);
    generate_c_bindings(top, &signals, &ctop_c_path)?;

    let verilator_root = env::var("VERILATOR_ROOT").unwrap_or_else(|_| {
        let output = Command::new("verilator")
            .arg("--getenv")
            .arg("VERILATOR_ROOT")
            .output()
            .expect("Failed to get VERILATOR_ROOT");
        String::from_utf8(output.stdout).unwrap().trim().to_string()
    });
    let verilator_include = format!("{}/include", verilator_root);

    println!("cargo:warning=Using verilator include: {}", verilator_include);

    let compile_status = Command::new("g++")
        .current_dir(&cwd)
        .arg("-I.")
        .arg("-MMD")
        .arg(&format!("-I{}", verilator_include))
        .arg(&format!("-I{}/vltstd", verilator_include))
        .arg("-DVM_COVERAGE=0")
        .arg("-DVM_SC=0")
        .arg("-DVM_TIMING=0")
        .arg("-DVM_TRACE=1")
        .arg("-DVM_TRACE_FST=0")
        .arg("-DVM_TRACE_VCD=1")
        .arg("-faligned-new")
        .arg("-fcf-protection=none")
        .arg("-Wno-bool-operation")
        .arg("-Wno-shadow")
        .arg("-Wno-sign-compare")
        .arg("-Wno-tautological-compare")
        .arg("-Wno-uninitialized")
        .arg("-Wno-unused-but-set-parameter")
        .arg("-Wno-unused-but-set-variable")
        .arg("-Wno-unused-parameter")
        .arg("-Wno-unused-variable")
        .arg("-fPIC")
        .arg("-Os")
        .arg("-c")
        .arg("-o")
        .arg(&format!("{}/C{}.o", obj_dir, top))
        .arg(&format!("{}/C{}.cpp", obj_dir, top))
        .status()?;

    if !compile_status.success() {
        panic!("Failed to compile C bindings");
    } else {
        println!("cargo:warning=Verilator compile passed");
    }

    let link_status = Command::new("g++")
        .current_dir(&cwd)
        .arg("-shared")
        .arg("-o")
        .arg("../../libVdut.so")
        .arg(&format!("{}/verilated.o", obj_dir))
        .arg(&format!("{}/verilated_threads.o", obj_dir))
        .arg(&format!("{}/verilated_vcd_c.o", obj_dir))
        .arg(&format!("{}/C{}.o", obj_dir, top))
        .arg(&format!("{}/V{}__ALL.a", obj_dir, top))
        .status()?;

    if !link_status.success() {
        panic!("Failed to link shared library");
    } else {
        println!("cargo:warning=libVdut.so linking passed");
    }

    let out_dir = env::var("OUT_DIR").unwrap();
    let rust_binding_path = format!("{}/dut.rs", out_dir);
    generate_rust_bindings(top, &signals, &rust_binding_path)?;

    let cwd_str = env::current_dir()?.to_str().unwrap().to_string();
    println!("cargo:rustc-link-search=native={}", cwd_str);
    println!("cargo:rustc-link-arg=-Wl,-rpath,{}", cwd_str);
    println!("cargo:rustc-link-lib=dylib=Vdut");

    Ok(())
}
