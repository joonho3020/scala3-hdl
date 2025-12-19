use riscv_codec::immediates::{IImmediate, Shamt, UImmediate};
use riscv_codec::instruction::Instruction;
use riscv_codec::register::IRegister;
use std::fs;
use std::path::Path;

fn parse_register(s: &str) -> Result<IRegister, String> {
    let s = s.trim().to_lowercase();
    let num: u32 = if s.starts_with('x') {
        let reg_num: u32 = s[1..].parse().map_err(|_| format!("Invalid register: {}", s))?;
        if reg_num > 31 {
            return Err(format!("Register number out of range (0-31): {}", s));
        }
        reg_num
    } else if s == "zero" {
        0
    } else if s == "ra" {
        1
    } else if s == "sp" {
        2
    } else if s == "gp" {
        3
    } else if s == "tp" {
        4
    } else if s.starts_with('t') {
        let n: u32 = s[1..].parse().map_err(|_| format!("Invalid register: {}", s))?;
        match n {
            0 => 5,
            1 => 6,
            2 => 7,
            3 => 28,
            4 => 29,
            5 => 30,
            6 => 31,
            _ => return Err(format!("Invalid t register: {}", s)),
        }
    } else if s.starts_with('s') && s.len() > 1 {
        let rest = &s[1..];
        if rest == "p" {
            2
        } else {
            let n: u32 = rest.parse().map_err(|_| format!("Invalid register: {}", s))?;
            match n {
                0 => 8,
                1 => 9,
                2..=11 => 18 + (n - 2),
                _ => return Err(format!("Invalid s register: {}", s)),
            }
        }
    } else if s.starts_with('a') {
        let n: u32 = s[1..].parse().map_err(|_| format!("Invalid register: {}", s))?;
        if n <= 7 {
            10 + n
        } else {
            return Err(format!("Invalid a register: {}", s));
        }
    } else if s == "fp" {
        8
    } else {
        return Err(format!("Unknown register: {}", s));
    };

    Ok(IRegister::from_int(num))
}

fn parse_immediate(s: &str) -> Result<i32, String> {
    let s = s.trim();
    if s.starts_with("0x") || s.starts_with("0X") {
        i32::from_str_radix(&s[2..], 16).map_err(|e| format!("Invalid hex: {}", e))
    } else if s.starts_with("-0x") || s.starts_with("-0X") {
        let val = i32::from_str_radix(&s[3..], 16).map_err(|e| format!("Invalid hex: {}", e))?;
        Ok(-val)
    } else {
        s.parse().map_err(|e| format!("Invalid immediate: {}", e))
    }
}

fn parse_instruction(line: &str) -> Result<Option<Instruction>, String> {
    let line = line.trim();
    if line.is_empty() || line.starts_with('#') || line.starts_with("//") {
        return Ok(None);
    }

    let line = if let Some(pos) = line.find('#') {
        &line[..pos]
    } else {
        line
    };
    let line = line.trim();

    let parts: Vec<&str> = line.split_whitespace().collect();
    if parts.is_empty() {
        return Ok(None);
    }

    let mnemonic = parts[0].to_uppercase();
    let args: Vec<String> = if parts.len() > 1 {
        parts[1..].join(" ").split(',').map(|s| s.trim().to_string()).collect()
    } else {
        vec![]
    };

    match mnemonic.as_str() {
        "ADD" => {
            if args.len() != 3 {
                return Err(format!("ADD requires 3 operands, got {}", args.len()));
            }
            let dest = parse_register(&args[0])?;
            let src1 = parse_register(&args[1])?;
            let src2 = parse_register(&args[2])?;
            Ok(Some(Instruction::Add { dest, src1, src2 }))
        }
        "SUB" => {
            if args.len() != 3 {
                return Err(format!("SUB requires 3 operands, got {}", args.len()));
            }
            let dest = parse_register(&args[0])?;
            let src1 = parse_register(&args[1])?;
            let src2 = parse_register(&args[2])?;
            Ok(Some(Instruction::Sub { dest, src1, src2 }))
        }
        "AND" => {
            if args.len() != 3 {
                return Err(format!("AND requires 3 operands, got {}", args.len()));
            }
            let dest = parse_register(&args[0])?;
            let src1 = parse_register(&args[1])?;
            let src2 = parse_register(&args[2])?;
            Ok(Some(Instruction::And { dest, src1, src2 }))
        }
        "OR" => {
            if args.len() != 3 {
                return Err(format!("OR requires 3 operands, got {}", args.len()));
            }
            let dest = parse_register(&args[0])?;
            let src1 = parse_register(&args[1])?;
            let src2 = parse_register(&args[2])?;
            Ok(Some(Instruction::Or { dest, src1, src2 }))
        }
        "XOR" => {
            if args.len() != 3 {
                return Err(format!("XOR requires 3 operands, got {}", args.len()));
            }
            let dest = parse_register(&args[0])?;
            let src1 = parse_register(&args[1])?;
            let src2 = parse_register(&args[2])?;
            Ok(Some(Instruction::Xor { dest, src1, src2 }))
        }
        "SLT" => {
            if args.len() != 3 {
                return Err(format!("SLT requires 3 operands, got {}", args.len()));
            }
            let dest = parse_register(&args[0])?;
            let src1 = parse_register(&args[1])?;
            let src2 = parse_register(&args[2])?;
            Ok(Some(Instruction::Slt { dest, src1, src2 }))
        }
        "SLTU" => {
            if args.len() != 3 {
                return Err(format!("SLTU requires 3 operands, got {}", args.len()));
            }
            let dest = parse_register(&args[0])?;
            let src1 = parse_register(&args[1])?;
            let src2 = parse_register(&args[2])?;
            Ok(Some(Instruction::Sltu { dest, src1, src2 }))
        }
        "SLL" => {
            if args.len() != 3 {
                return Err(format!("SLL requires 3 operands, got {}", args.len()));
            }
            let dest = parse_register(&args[0])?;
            let src1 = parse_register(&args[1])?;
            let src2 = parse_register(&args[2])?;
            Ok(Some(Instruction::Sll { dest, src1, src2 }))
        }
        "SRL" => {
            if args.len() != 3 {
                return Err(format!("SRL requires 3 operands, got {}", args.len()));
            }
            let dest = parse_register(&args[0])?;
            let src1 = parse_register(&args[1])?;
            let src2 = parse_register(&args[2])?;
            Ok(Some(Instruction::Srl { dest, src1, src2 }))
        }
        "SRA" => {
            if args.len() != 3 {
                return Err(format!("SRA requires 3 operands, got {}", args.len()));
            }
            let dest = parse_register(&args[0])?;
            let src1 = parse_register(&args[1])?;
            let src2 = parse_register(&args[2])?;
            Ok(Some(Instruction::Sra { dest, src1, src2 }))
        }
        "ADDI" => {
            if args.len() != 3 {
                return Err(format!("ADDI requires 3 operands, got {}", args.len()));
            }
            let dest = parse_register(&args[0])?;
            let src = parse_register(&args[1])?;
            let imm_val = parse_immediate(&args[2])?;
            let imm = IImmediate::from_u32(imm_val as u32);
            Ok(Some(Instruction::Addi { dest, src, imm }))
        }
        "ANDI" => {
            if args.len() != 3 {
                return Err(format!("ANDI requires 3 operands, got {}", args.len()));
            }
            let dest = parse_register(&args[0])?;
            let src = parse_register(&args[1])?;
            let imm_val = parse_immediate(&args[2])?;
            let imm = IImmediate::from_u32(imm_val as u32);
            Ok(Some(Instruction::Andi { dest, src, imm }))
        }
        "ORI" => {
            if args.len() != 3 {
                return Err(format!("ORI requires 3 operands, got {}", args.len()));
            }
            let dest = parse_register(&args[0])?;
            let src = parse_register(&args[1])?;
            let imm_val = parse_immediate(&args[2])?;
            let imm = IImmediate::from_u32(imm_val as u32);
            Ok(Some(Instruction::Ori { dest, src, imm }))
        }
        "XORI" => {
            if args.len() != 3 {
                return Err(format!("XORI requires 3 operands, got {}", args.len()));
            }
            let dest = parse_register(&args[0])?;
            let src = parse_register(&args[1])?;
            let imm_val = parse_immediate(&args[2])?;
            let imm = IImmediate::from_u32(imm_val as u32);
            Ok(Some(Instruction::Xori { dest, src, imm }))
        }
        "SLTI" => {
            if args.len() != 3 {
                return Err(format!("SLTI requires 3 operands, got {}", args.len()));
            }
            let dest = parse_register(&args[0])?;
            let src = parse_register(&args[1])?;
            let imm_val = parse_immediate(&args[2])?;
            let imm = IImmediate::from_u32(imm_val as u32);
            Ok(Some(Instruction::Slti { dest, src, imm }))
        }
        "SLTIU" => {
            if args.len() != 3 {
                return Err(format!("SLTIU requires 3 operands, got {}", args.len()));
            }
            let dest = parse_register(&args[0])?;
            let src = parse_register(&args[1])?;
            let imm_val = parse_immediate(&args[2])?;
            let imm = IImmediate::from_u32(imm_val as u32);
            Ok(Some(Instruction::Sltiu { dest, src, imm }))
        }
        "SLLI" => {
            if args.len() != 3 {
                return Err(format!("SLLI requires 3 operands, got {}", args.len()));
            }
            let dest = parse_register(&args[0])?;
            let src = parse_register(&args[1])?;
            let shamt_val = parse_immediate(&args[2])? as u32;
            let shamt = Shamt::from_u32(shamt_val);
            Ok(Some(Instruction::Slli { dest, src, shamt }))
        }
        "SRLI" => {
            if args.len() != 3 {
                return Err(format!("SRLI requires 3 operands, got {}", args.len()));
            }
            let dest = parse_register(&args[0])?;
            let src = parse_register(&args[1])?;
            let shamt_val = parse_immediate(&args[2])? as u32;
            let shamt = Shamt::from_u32(shamt_val);
            Ok(Some(Instruction::Srli { dest, src, shamt }))
        }
        "SRAI" => {
            if args.len() != 3 {
                return Err(format!("SRAI requires 3 operands, got {}", args.len()));
            }
            let dest = parse_register(&args[0])?;
            let src = parse_register(&args[1])?;
            let shamt_val = parse_immediate(&args[2])? as u32;
            let shamt = Shamt::from_u32(shamt_val);
            Ok(Some(Instruction::Srai { dest, src, shamt }))
        }
        "LUI" => {
            if args.len() != 2 {
                return Err(format!("LUI requires 2 operands, got {}", args.len()));
            }
            let dest = parse_register(&args[0])?;
            let imm_val = parse_immediate(&args[1])? as u32;
            let imm = UImmediate::from_u32(imm_val);
            Ok(Some(Instruction::Lui { dest, imm }))
        }
        "AUIPC" => {
            if args.len() != 2 {
                return Err(format!("AUIPC requires 2 operands, got {}", args.len()));
            }
            let dest = parse_register(&args[0])?;
            let imm_val = parse_immediate(&args[1])? as u32;
            let imm = UImmediate::from_u32(imm_val);
            Ok(Some(Instruction::Auipc { dest, imm }))
        }
        "NOP" => {
            Ok(Some(Instruction::Addi {
                dest: IRegister::Zero,
                src: IRegister::Zero,
                imm: IImmediate::from_u32(0),
            }))
        }
        _ => Err(format!("Unknown instruction: {}", mnemonic)),
    }
}

pub fn parse_asm_file<P: AsRef<Path>>(path: P) -> Result<Vec<u32>, String> {
    let content = fs::read_to_string(path).map_err(|e| format!("Failed to read file: {}", e))?;
    parse_asm_string(&content)
}

pub fn parse_asm_string(content: &str) -> Result<Vec<u32>, String> {
    let mut instructions = Vec::new();
    for (line_num, line) in content.lines().enumerate() {
        match parse_instruction(line) {
            Ok(Some(insn)) => {
                let encoded = Instruction::encode(&insn);
                instructions.push(encoded);
            }
            Ok(None) => {}
            Err(e) => return Err(format!("Line {}: {}", line_num + 1, e)),
        }
    }
    Ok(instructions)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_add() {
        let insns = parse_asm_string("ADD x5, x3, x2").unwrap();
        assert_eq!(insns.len(), 1);
        let decoded = Instruction::decode(insns[0]).unwrap();
        assert!(matches!(decoded, Instruction::Add { .. }));
    }

    #[test]
    fn test_parse_addi() {
        let insns = parse_asm_string("ADDI x1, x0, 100").unwrap();
        assert_eq!(insns.len(), 1);
        let decoded = Instruction::decode(insns[0]).unwrap();
        assert!(matches!(decoded, Instruction::Addi { .. }));
    }

    #[test]
    fn test_parse_lui() {
        let insns = parse_asm_string("LUI x5, 0x12345").unwrap();
        assert_eq!(insns.len(), 1);
        let decoded = Instruction::decode(insns[0]).unwrap();
        assert!(matches!(decoded, Instruction::Lui { .. }));
    }

    #[test]
    fn test_parse_alu_test_file() {
        let result = parse_asm_file("tests/alu_test.asm");
        assert!(result.is_ok(), "Failed to parse alu_test.asm: {:?}", result.err());
        let insns = result.unwrap();
        assert!(insns.len() > 0, "No instructions parsed");
    }

    #[test]
    fn test_register_range_validation() {
        let result = parse_asm_string("ADD x32, x0, x1");
        assert!(result.is_err(), "Should reject x32 (out of range)");
        assert!(result.unwrap_err().contains("out of range"));
    }
}
