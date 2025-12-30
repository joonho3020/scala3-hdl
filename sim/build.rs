use std::env;
use std::fs;
use std::fs::File;
use std::io::{self, BufRead, BufWriter, Write};
use std::path::Path;
use std::process::Command;
use std::collections::HashMap;

use serde::{Deserialize, Serialize};

#[derive(Debug)]
struct Signal {
    input: bool,
    name: String,
    bits: u32,
}

// Schema types for parsing JSON
#[derive(Debug, Deserialize)]
struct IOSchema {
    module_name: String,
    ports: Vec<Port>,
}

#[derive(Debug, Deserialize)]
struct Port {
    name: String,
    direction: String,
    #[serde(rename = "type")]
    port_type: IRType,
}

#[derive(Debug, Deserialize, Clone)]
#[serde(tag = "type")]
enum IRType {
    UInt { width: Option<u32> },
    SInt { width: Option<u32> },
    Bool,
    Clock,
    Reset,
    OneHot { width: Option<u32> },
    Vec { length: u32, elem_type: Box<IRType> },
    Bundle { fields: Vec<BundleField> },
}

#[derive(Debug, Deserialize, Clone)]
struct BundleField {
    name: String,
    flipped: bool,
    #[serde(rename = "type")]
    field_type: IRType,
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

// Load and parse the IO schema JSON file
fn load_io_schema(schema_path: &str) -> io::Result<IOSchema> {
    let file = File::open(schema_path)?;
    let reader = io::BufReader::new(file);
    serde_json::from_reader(reader)
        .map_err(|e| io::Error::new(io::ErrorKind::InvalidData, e))
}

// Convert HDL path to Verilator signal name (e.g., "io.mem.req.valid" -> "io_mem_req_valid")
fn hdl_path_to_verilator_name(path: &[String]) -> String {
    path.join("_")
}

// Generate a unique type name for a bundle based on its path
fn bundle_type_name(path: &[String]) -> String {
    let name = path.iter()
        .map(|s| {
            let mut chars = s.chars();
            match chars.next() {
                None => String::new(),
                Some(first) => first.to_uppercase().collect::<String>() + chars.as_str(),
            }
        })
        .collect::<Vec<_>>()
        .join("");
    format!("{}Bundle", name)
}

// Get the Rust type for a signal based on bit width
fn rust_type_for_bits(bits: u32, is_signed: bool) -> String {
    if bits <= 64 {
        if is_signed {
            "i64".to_string()
        } else {
            "u64".to_string()
        }
    } else {
        "WideValue".to_string()
    }
}

// Get bit width for an IR type
fn get_bit_width(ir_type: &IRType) -> Option<u32> {
    match ir_type {
        IRType::UInt { width } | IRType::SInt { width } | IRType::OneHot { width } => *width,
        IRType::Bool => Some(1),
        IRType::Clock => Some(1),
        IRType::Reset => Some(1),
        _ => None,
    }
}

// Check if type is signed
fn is_signed_type(ir_type: &IRType) -> bool {
    matches!(ir_type, IRType::SInt { .. })
}

// Flatten a bundle into all leaf signal paths with their types
#[derive(Debug, Clone)]
struct SignalPath {
    path: Vec<String>,
    ir_type: IRType,
    direction: SignalDirection,
}

#[derive(Debug, Clone, Copy, PartialEq)]
enum SignalDirection {
    Input,
    Output,
}

fn flatten_bundle(
    ir_type: &IRType,
    current_path: Vec<String>,
    current_dir: SignalDirection,
    result: &mut Vec<SignalPath>,
) {
    match ir_type {
        IRType::Bundle { fields } => {
            for field in fields {
                let mut new_path = current_path.clone();
                new_path.push(field.name.clone());

                // Flipped reverses the direction
                let field_dir = if field.flipped {
                    match current_dir {
                        SignalDirection::Input => SignalDirection::Output,
                        SignalDirection::Output => SignalDirection::Input,
                    }
                } else {
                    current_dir
                };

                flatten_bundle(&field.field_type, new_path, field_dir, result);
            }
        }
        IRType::Vec { length, elem_type } => {
            // For vectors, we expand into indexed elements
            for i in 0..*length {
                let mut new_path = current_path.clone();
                new_path.push(i.to_string());
                flatten_bundle(elem_type, new_path, current_dir, result);
            }
        }
        _ => {
            // Leaf type (UInt, SInt, Bool, etc.)
            result.push(SignalPath {
                path: current_path,
                ir_type: ir_type.clone(),
                direction: current_dir,
            });
        }
    }
}

// Collect all unique bundle types from the schema
fn collect_bundle_types(ir_type: &IRType, path: &[String], bundles: &mut HashMap<String, (Vec<String>, IRType, bool)>, parent_flipped: bool) {
    match ir_type {
        IRType::Bundle { fields } => {
            let bundle_name = bundle_type_name(path);
            if !bundles.contains_key(&bundle_name) {
                bundles.insert(bundle_name, (path.to_vec(), ir_type.clone(), parent_flipped));
            }

            for field in fields {
                let mut new_path = path.to_vec();
                new_path.push(field.name.clone());
                let field_flipped = parent_flipped ^ field.flipped;
                collect_bundle_types(&field.field_type, &new_path, bundles, field_flipped);
            }
        }
        IRType::Vec { elem_type, .. } => {
            collect_bundle_types(elem_type, path, bundles, parent_flipped);
        }
        _ => {}
    }
}

// Generate a bundle struct definition
fn generate_bundle_struct(
    writer: &mut BufWriter<File>,
    path: &[String],
    ir_type: &IRType,
    all_bundles: &HashMap<String, (Vec<String>, IRType, bool)>,
) -> io::Result<()> {
    generate_bundle_struct_with_flip(writer, path, ir_type, all_bundles, false)
}

fn generate_bundle_struct_with_flip(
    writer: &mut BufWriter<File>,
    path: &[String],
    ir_type: &IRType,
    all_bundles: &HashMap<String, (Vec<String>, IRType, bool)>,
    parent_flipped: bool,
) -> io::Result<()> {
    if let IRType::Bundle { fields } = ir_type {
        let struct_name = bundle_type_name(path);

        // Generate struct definition
        writeln!(writer, "/// Bundle struct for {}", path.join("."))?;
        writeln!(writer, "pub struct {}<'a> {{", struct_name)?;
        writeln!(writer, "    dut: &'a mut Dut,")?;
        writeln!(writer, "    path: Vec<String>,")?;
        writeln!(writer, "}}\n")?;

        // Generate impl block with field accessors
        writeln!(writer, "impl<'a> {}<'a> {{", struct_name)?;

        for field in fields {
            let mut field_path = path.to_vec();
            field_path.push(field.name.clone());
            let field_verilator_name = hdl_path_to_verilator_name(&field_path);

            // XOR parent_flipped with field.flipped to get actual direction
            let is_flipped = parent_flipped ^ field.flipped;

            // Determine return type based on field type
            match &field.field_type {
                IRType::Bundle { .. } => {
                    // Nested bundle - return another bundle struct
                    let field_struct_name = bundle_type_name(&field_path);
                    writeln!(writer, "    pub fn {}(&mut self) -> {}<'_> {{", field.name, field_struct_name)?;
                    writeln!(writer, "        let mut path = self.path.clone();")?;
                    writeln!(writer, "        path.push(\"{}\".to_string());", field.name)?;
                    writeln!(writer, "        {} {{ dut: self.dut, path }}", field_struct_name)?;
                    writeln!(writer, "    }}\n")?;
                }
                IRType::Vec { length, elem_type } => {
                    // Vector - return a Vec wrapper
                    let elem_rust_type = match elem_type.as_ref() {
                        IRType::Bundle { .. } => {
                            let elem_bundle_name = bundle_type_name(&field_path);
                            format!("{}<'_>", elem_bundle_name)
                        }
                        _ => {
                            if let Some(width) = get_bit_width(elem_type) {
                                if width > 64 {
                                    "WideValue".to_string()
                                } else {
                                    "u64".to_string()
                                }
                            } else {
                                "u64".to_string()
                            }
                        }
                    };

                    writeln!(writer, "    pub fn {}(&mut self) -> VecBundle<'_, {}, {}> {{", field.name, elem_rust_type, is_flipped)?;
                    writeln!(writer, "        let mut path = self.path.clone();")?;
                    writeln!(writer, "        path.push(\"{}\".to_string());", field.name)?;
                    writeln!(writer, "        VecBundle {{ dut: self.dut, path, length: {}, is_flipped: {}, _phantom: PhantomData }}", length, is_flipped)?;
                    writeln!(writer, "    }}\n")?;
                }
                _ => {
                    // Leaf signal - return Signal or SignalMut based on direction
                    if let Some(width) = get_bit_width(&field.field_type) {
                        let rust_type = if width > 64 { "WideValue" } else { "u64" };

                        if is_flipped {
                            // Flipped = input = mutable
                            writeln!(writer, "    pub fn {}(&mut self) -> SignalMut<'_, {}> {{", field.name, rust_type)?;
                            writeln!(writer, "        let mut path = self.path.clone();")?;
                            writeln!(writer, "        path.push(\"{}\".to_string());", field.name)?;
                            writeln!(writer, "        let signal_name = path.join(\"_\");")?;
                            writeln!(writer, "        SignalMut {{ dut: self.dut, signal_name: Box::leak(signal_name.into_boxed_str()), _phantom: PhantomData }}")?;
                        } else {
                            // Not flipped = output = read-only
                            writeln!(writer, "    pub fn {}(&self) -> Signal<'_, {}> {{", field.name, rust_type)?;
                            writeln!(writer, "        let mut path = self.path.clone();")?;
                            writeln!(writer, "        path.push(\"{}\".to_string());", field.name)?;
                            writeln!(writer, "        let signal_name = path.join(\"_\");")?;
                            writeln!(writer, "        Signal {{ dut: self.dut, signal_name: Box::leak(signal_name.into_boxed_str()), _phantom: PhantomData }}")?;
                        }
                        writeln!(writer, "    }}\n")?;
                    }
                }
            }
        }

        writeln!(writer, "}}\n")?;
    }

    Ok(())
}

// Generate schema-based Rust bindings with struct field access (Option C)
fn generate_schema_rust_bindings(
    top: &str,
    schema: &IOSchema,
    signals: &HashMap<String, Signal>,
    output_path: &str,
) -> io::Result<()> {
    let vtop = format!("V{}", top);
    let file = File::create(output_path)?;
    let mut writer = BufWriter::new(file);

    writeln!(writer, "// Auto-generated by build.rs from IO schema - DO NOT EDIT\n")?;
    writeln!(writer, "use std::ops::Index;\n")?;
    writeln!(writer, "use std::marker::PhantomData;\n")?;

    // Generate FFI declarations (same as before)
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
    writeln!(writer, "    pub fn enable_trace(dut: *mut {}) -> *mut VerilatedVcdC;", vtop)?;
    writeln!(writer, "    pub fn close_trace(tfp: *mut VerilatedVcdC);")?;
    writeln!(writer, "    pub fn dump_vcd(tfp: *mut VerilatedVcdC, timestep: u32);")?;

    for (name, signal) in signals {
        if signal.bits > 64 {
            if signal.input {
                writeln!(writer, "    pub fn poke_{}(dut: *mut {}, {}: *const u32);", name, vtop, name)?;
            } else {
                writeln!(writer, "    pub fn peek_{}(dut: *mut {}, {}: *mut u32);", name, vtop, name)?;
            }
        } else {
            if signal.input {
                writeln!(writer, "    pub fn poke_{}(dut: *mut {}, {}: u64);", name, vtop, name)?;
            } else {
                writeln!(writer, "    pub fn peek_{}(dut: *mut {}) -> u64;", name, vtop)?;
            }
        }
    }
    writeln!(writer, "}}\n")?;

    // Generate WideValue type for >64 bit signals
    writeln!(writer, "/// Represents a signal wider than 64 bits")?;
    writeln!(writer, "#[derive(Debug, Clone, PartialEq, Eq)]")?;
    writeln!(writer, "pub struct WideValue {{")?;
    writeln!(writer, "    pub words: Vec<u32>,")?;
    writeln!(writer, "}}\n")?;

    writeln!(writer, "impl WideValue {{")?;
    writeln!(writer, "    pub fn new(words: Vec<u32>) -> Self {{")?;
    writeln!(writer, "        WideValue {{ words }}")?;
    writeln!(writer, "    }}")?;
    writeln!(writer, "}}\n")?;

    // Generate Signal wrapper types
    writeln!(writer, "/// Read-only signal wrapper")?;
    writeln!(writer, "pub struct Signal<'a, T> {{")?;
    writeln!(writer, "    dut: &'a Dut,")?;
    writeln!(writer, "    signal_name: &'static str,")?;
    writeln!(writer, "    _phantom: PhantomData<T>,")?;
    writeln!(writer, "}}\n")?;

    writeln!(writer, "impl<'a> Signal<'a, u64> {{")?;
    writeln!(writer, "    pub fn peek(&self) -> u64 {{")?;
    writeln!(writer, "        match self.signal_name {{")?;
    for (name, signal) in signals {
        if !signal.input && signal.bits <= 64 {
            writeln!(writer, "            \"{}\" => self.dut.peek_{}(),", name, name)?;
        }
    }
    writeln!(writer, "            _ => panic!(\"Unknown signal: {{}}\", self.signal_name),")?;
    writeln!(writer, "        }}")?;
    writeln!(writer, "    }}")?;
    writeln!(writer, "}}\n")?;

    writeln!(writer, "impl<'a> Signal<'a, WideValue> {{")?;
    writeln!(writer, "    pub fn peek(&self) -> WideValue {{")?;
    writeln!(writer, "        match self.signal_name {{")?;
    for (name, signal) in signals {
        if !signal.input && signal.bits > 64 {
            writeln!(writer, "            \"{}\" => WideValue::new(self.dut.peek_{}().to_vec()),", name, name)?;
        }
    }
    writeln!(writer, "            _ => panic!(\"Unknown signal: {{}}\", self.signal_name),")?;
    writeln!(writer, "        }}")?;
    writeln!(writer, "    }}")?;
    writeln!(writer, "}}\n")?;

    // Mutable signal wrapper
    writeln!(writer, "/// Read-write signal wrapper")?;
    writeln!(writer, "pub struct SignalMut<'a, T> {{")?;
    writeln!(writer, "    dut: &'a mut Dut,")?;
    writeln!(writer, "    signal_name: &'static str,")?;
    writeln!(writer, "    _phantom: PhantomData<T>,")?;
    writeln!(writer, "}}\n")?;

    writeln!(writer, "impl<'a> SignalMut<'a, u64> {{")?;
    writeln!(writer, "    pub fn poke(&mut self, value: u64) {{")?;
    writeln!(writer, "        match self.signal_name {{")?;
    for (name, signal) in signals {
        if signal.input && signal.bits <= 64 {
            writeln!(writer, "            \"{}\" => self.dut.poke_{}(value),", name, name)?;
        }
    }
    writeln!(writer, "            _ => panic!(\"Unknown signal: {{}}\", self.signal_name),")?;
    writeln!(writer, "        }}")?;
    writeln!(writer, "    }}")?;
    writeln!(writer, "}}\n")?;

    writeln!(writer, "impl<'a> SignalMut<'a, WideValue> {{")?;
    writeln!(writer, "    pub fn poke(&mut self, value: &WideValue) {{")?;
    writeln!(writer, "        match self.signal_name {{")?;
    for (name, signal) in signals {
        if signal.input && signal.bits > 64 {
            writeln!(writer, "            \"{}\" => self.dut.poke_{}(&value.words),", name, name)?;
        }
    }
    writeln!(writer, "            _ => panic!(\"Unknown signal: {{}}\", self.signal_name),")?;
    writeln!(writer, "        }}")?;
    writeln!(writer, "    }}")?;
    writeln!(writer, "}}\n")?;

    // Generate VecBundle type for vector fields
    writeln!(writer, "/// Vector bundle wrapper with Index trait")?;
    writeln!(writer, "pub struct VecBundle<'a, T, const FLIPPED: bool> {{")?;
    writeln!(writer, "    dut: &'a mut Dut,")?;
    writeln!(writer, "    path: Vec<String>,")?;
    writeln!(writer, "    length: u32,")?;
    writeln!(writer, "    is_flipped: bool,")?;
    writeln!(writer, "    _phantom: PhantomData<T>,")?;
    writeln!(writer, "}}\n")?;

    // VecBundle for output signals (FLIPPED = false)
    writeln!(writer, "impl<'a> VecBundle<'a, u64, false> {{")?;
    writeln!(writer, "    pub fn get(&mut self, index: usize) -> Signal<'_, u64> {{")?;
    writeln!(writer, "        if index >= self.length as usize {{")?;
    writeln!(writer, "            panic!(\"Index {{}} out of bounds for vector of length {{}}\", index, self.length);")?;
    writeln!(writer, "        }}")?;
    writeln!(writer, "        let mut path = self.path.clone();")?;
    writeln!(writer, "        path.push(index.to_string());")?;
    writeln!(writer, "        let signal_name = path.join(\"_\");")?;
    writeln!(writer, "        Signal {{ dut: self.dut, signal_name: Box::leak(signal_name.into_boxed_str()), _phantom: PhantomData }}")?;
    writeln!(writer, "    }}")?;
    writeln!(writer, "}}\n")?;

    // VecBundle for input signals (FLIPPED = true)
    writeln!(writer, "impl<'a> VecBundle<'a, u64, true> {{")?;
    writeln!(writer, "    pub fn get(&mut self, index: usize) -> SignalMut<'_, u64> {{")?;
    writeln!(writer, "        if index >= self.length as usize {{")?;
    writeln!(writer, "            panic!(\"Index {{}} out of bounds for vector of length {{}}\", index, self.length);")?;
    writeln!(writer, "        }}")?;
    writeln!(writer, "        let mut path = self.path.clone();")?;
    writeln!(writer, "        path.push(index.to_string());")?;
    writeln!(writer, "        let signal_name = path.join(\"_\");")?;
    writeln!(writer, "        SignalMut {{ dut: self.dut, signal_name: Box::leak(signal_name.into_boxed_str()), _phantom: PhantomData }}")?;
    writeln!(writer, "    }}")?;
    writeln!(writer, "}}\n")?;

    writeln!(writer, "impl<'a> VecBundle<'a, WideValue, false> {{")?;
    writeln!(writer, "    pub fn get(&mut self, index: usize) -> Signal<'_, WideValue> {{")?;
    writeln!(writer, "        if index >= self.length as usize {{")?;
    writeln!(writer, "            panic!(\"Index {{}} out of bounds for vector of length {{}}\", index, self.length);")?;
    writeln!(writer, "        }}")?;
    writeln!(writer, "        let mut path = self.path.clone();")?;
    writeln!(writer, "        path.push(index.to_string());")?;
    writeln!(writer, "        let signal_name = path.join(\"_\");")?;
    writeln!(writer, "        Signal {{ dut: self.dut, signal_name: Box::leak(signal_name.into_boxed_str()), _phantom: PhantomData }}")?;
    writeln!(writer, "    }}")?;
    writeln!(writer, "}}\n")?;

    writeln!(writer, "impl<'a> VecBundle<'a, WideValue, true> {{")?;
    writeln!(writer, "    pub fn get(&mut self, index: usize) -> SignalMut<'_, WideValue> {{")?;
    writeln!(writer, "        if index >= self.length as usize {{")?;
    writeln!(writer, "            panic!(\"Index {{}} out of bounds for vector of length {{}}\", index, self.length);")?;
    writeln!(writer, "        }}")?;
    writeln!(writer, "        let mut path = self.path.clone();")?;
    writeln!(writer, "        path.push(index.to_string());")?;
    writeln!(writer, "        let signal_name = path.join(\"_\");")?;
    writeln!(writer, "        SignalMut {{ dut: self.dut, signal_name: Box::leak(signal_name.into_boxed_str()), _phantom: PhantomData }}")?;
    writeln!(writer, "    }}")?;
    writeln!(writer, "}}\n")?;

    // Generic VecBundle implementations for bundle element types will be generated after bundle structs are defined

    // Collect all bundle types from the schema
    let mut all_bundles = HashMap::new();
    for port in &schema.ports {
        if port.name == "io" {
            collect_bundle_types(&port.port_type, &vec!["io".to_string()], &mut all_bundles, false);
        }
    }

    // Generate bundle struct definitions
    let mut bundle_list: Vec<_> = all_bundles.iter().collect();
    bundle_list.sort_by_key(|(name, _)| *name); // Sort for deterministic output

    for (_, (path, ir_type, parent_flipped)) in &bundle_list {
        generate_bundle_struct_with_flip(&mut writer, path, ir_type, &all_bundles, *parent_flipped)?;
    }

    // Generate VecBundle.get() implementations for bundle element types
    for (_, (path, _, _)) in &bundle_list {
        let bundle_type_name = bundle_type_name(path);

        // Generate for non-flipped (output) bundles
        writeln!(writer, "impl<'a> VecBundle<'a, {}<'a>, false> {{", bundle_type_name)?;
        writeln!(writer, "    pub fn get(&mut self, index: usize) -> {}<'_> {{", bundle_type_name)?;
        writeln!(writer, "        if index >= self.length as usize {{")?;
        writeln!(writer, "            panic!(\"Index {{}} out of bounds for vector of length {{}}\", index, self.length);")?;
        writeln!(writer, "        }}")?;
        writeln!(writer, "        let mut path = self.path.clone();")?;
        writeln!(writer, "        path.push(index.to_string());")?;
        writeln!(writer, "        {} {{ dut: self.dut, path }}", bundle_type_name)?;
        writeln!(writer, "    }}")?;
        writeln!(writer, "}}\n")?;

        // Generate for flipped (input) bundles
        writeln!(writer, "impl<'a> VecBundle<'a, {}<'a>, true> {{", bundle_type_name)?;
        writeln!(writer, "    pub fn get(&mut self, index: usize) -> {}<'_> {{", bundle_type_name)?;
        writeln!(writer, "        if index >= self.length as usize {{")?;
        writeln!(writer, "            panic!(\"Index {{}} out of bounds for vector of length {{}}\", index, self.length);")?;
        writeln!(writer, "        }}")?;
        writeln!(writer, "        let mut path = self.path.clone();")?;
        writeln!(writer, "        path.push(index.to_string());")?;
        writeln!(writer, "        {} {{ dut: self.dut, path }}", bundle_type_name)?;
        writeln!(writer, "    }}")?;
        writeln!(writer, "}}\n")?;
    }

    // Generate the Dut struct with basic methods
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
    writeln!(writer, "        unsafe {{ self.trace = Some(enable_trace(self.ptr)); }}")?;
    writeln!(writer, "    }}\n")?;

    writeln!(writer, "    pub fn eval(&mut self) {{")?;
    writeln!(writer, "        unsafe {{ {}_eval(self.ptr); }}", top)?;
    writeln!(writer, "    }}\n")?;

    writeln!(writer, "    pub fn step(&mut self) {{")?;
    writeln!(writer, "        if let Some(tfp) = self.trace {{ unsafe {{ dump_vcd(tfp, self.timestep); }} }}")?;
    writeln!(writer, "        self.timestep += 1;")?;
    writeln!(writer, "        self.poke_clock(0);")?;
    writeln!(writer, "        self.eval();")?;
    writeln!(writer, "        if let Some(tfp) = self.trace {{ unsafe {{ dump_vcd(tfp, self.timestep); }} }}")?;
    writeln!(writer, "        self.timestep += 1;")?;
    writeln!(writer, "        self.poke_clock(1);")?;
    writeln!(writer, "        self.eval();")?;
    writeln!(writer, "    }}\n")?;

    writeln!(writer, "    pub fn timestep(&self) -> u32 {{ self.timestep }}\n")?;

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

    // Add io() accessor for the top-level IO bundle
    writeln!(writer, "    /// Access the IO bundle with structured field access")?;
    writeln!(writer, "    pub fn io(&mut self) -> IoBundle {{")?;
    writeln!(writer, "        IoBundle {{ dut: self, path: vec![\"io\".to_string()] }}")?;
    writeln!(writer, "    }}\n")?;

    // Generate direct peek/poke methods for all signals (compatibility)
    for (name, signal) in signals {
        if signal.input {
            if signal.bits > 64 {
                writeln!(writer, "    pub fn poke_{}(&mut self, value: &[u32]) {{", name)?;
                writeln!(writer, "        unsafe {{ poke_{}(self.ptr, value.as_ptr()); }}", name)?;
                writeln!(writer, "    }}\n")?;
            } else {
                writeln!(writer, "    pub fn poke_{}(&mut self, value: u64) {{", name)?;
                writeln!(writer, "        unsafe {{ poke_{}(self.ptr, value); }}", name)?;
                writeln!(writer, "    }}\n")?;
            }
        } else {
            if signal.bits > 64 {
                let chunks = (signal.bits + 31) / 32;
                writeln!(writer, "    pub fn peek_{}(&self) -> [u32; {}] {{", name, chunks)?;
                writeln!(writer, "        let mut value = [0u32; {}];", chunks)?;
                writeln!(writer, "        unsafe {{ peek_{}(self.ptr, value.as_mut_ptr()); }}", name)?;
                writeln!(writer, "        value")?;
                writeln!(writer, "    }}\n")?;
            } else {
                writeln!(writer, "    pub fn peek_{}(&self) -> u64 {{", name)?;
                writeln!(writer, "        unsafe {{ peek_{}(self.ptr) }}", name)?;
                writeln!(writer, "    }}\n")?;
            }
        }
    }

    writeln!(writer, "}}\n")?;

    // Drop impl
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
    let sv_file_path =
        env::var("SV_FILE").unwrap_or_else(|_| "./test-outputs/verilog/Tile.sv".to_string());
    let filelist_path =
        env::var("FILELIST").unwrap_or_else(|_| "./test-outputs/verilog/filelist.f".to_string());

    println!("cargo:rerun-if-changed=build.rs");
    println!("cargo:rerun-if-changed={}", sv_file_path);
    println!("cargo:rerun-if-changed={}", filelist_path);
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

    let signals_vec = parse_verilator_header(&vtop_h_path)?;
    println!("cargo:warning=Found {} signals", signals_vec.len());

    // Convert signals vec to HashMap for easier lookup
    let mut signals_map = HashMap::new();
    for sig in signals_vec.iter() {
        signals_map.insert(sig.name.clone(), Signal {
            input: sig.input,
            name: sig.name.clone(),
            bits: sig.bits,
        });
    }

    let ctop_c_path = format!("{}/C{}.cpp", obj_dir, top);
    generate_c_bindings(top, &signals_vec, &ctop_c_path)?;

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

    // Try to use schema-based generation if io_schema.json exists
    let schema_path = "test-outputs/io_schema.json";
    if Path::new(schema_path).exists() {
        println!("cargo:warning=Using schema-based code generation");
        println!("cargo:rerun-if-changed={}", schema_path);
        match load_io_schema(schema_path) {
            Ok(schema) => {
                generate_schema_rust_bindings(top, &schema, &signals_map, &rust_binding_path)?;
            }
            Err(e) => {
                println!("cargo:warning=Failed to load schema: {}, falling back to basic generation", e);
                generate_rust_bindings(top, &signals_vec, &rust_binding_path)?;
            }
        }
    } else {
        println!("cargo:warning=Schema not found, using basic code generation");
        generate_rust_bindings(top, &signals_vec, &rust_binding_path)?;
    }

    let cwd_str = env::current_dir()?.to_str().unwrap().to_string();
    println!("cargo:rustc-link-search=native={}", cwd_str);
    println!("cargo:rustc-link-arg=-Wl,-rpath,{}", cwd_str);
    println!("cargo:rustc-link-lib=dylib=Vdut");

    Ok(())
}
