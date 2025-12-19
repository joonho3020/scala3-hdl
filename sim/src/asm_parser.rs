use std::fs;
use std::path::Path;

fn parse_hex_line(line: &str, line_num: usize) -> Result<Option<u32>, String> {
    let mut trimmed = line.trim();
    if trimmed.is_empty() {
        return Ok(None);
    }
    if let Some(idx) = trimmed.find("//") {
        trimmed = trimmed[..idx].trim();
    }
    if let Some(idx) = trimmed.find('#') {
        trimmed = trimmed[..idx].trim();
    }
    if trimmed.is_empty() {
        return Ok(None);
    }
    let hex = trimmed
        .strip_prefix("0x")
        .or_else(|| trimmed.strip_prefix("0X"))
        .unwrap_or(trimmed);
    if hex.is_empty() {
        return Ok(None);
    }
    let value = u32::from_str_radix(hex, 16).map_err(|e| format!("Line {}: invalid hex: {}", line_num, e))?;
    Ok(Some(value))
}

pub fn parse_asm_file<P: AsRef<Path>>(path: P) -> Result<Vec<u32>, String> {
    let content = fs::read_to_string(path).map_err(|e| format!("Failed to read file: {}", e))?;
    parse_asm_string(&content)
}

pub fn parse_asm_string(content: &str) -> Result<Vec<u32>, String> {
    let mut instructions = Vec::new();
    for (idx, line) in content.lines().enumerate() {
        if let Some(value) = parse_hex_line(line, idx + 1)? {
            instructions.push(value);
        }
    }
    Ok(instructions)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parses_hex_lines() {
        let insns = parse_asm_string("00000093\n0xe0058513\n").unwrap();
        assert_eq!(insns, vec![0x00000093, 0xe0058513]);
    }

    #[test]
    fn rejects_invalid_hex() {
        let result = parse_asm_string("zzzz");
        assert!(result.is_err());
    }

    #[test]
    fn rejects_invalid_instruction() {
        let result = parse_asm_string("ffffffff");
        assert!(result.is_err());
    }

    #[test]
    fn test_parse_alu_test_file() {
        let result = parse_asm_file("tests/alu_test.asm");
        assert!(result.is_ok(), "Failed to parse alu_test.asm: {:?}", result.err());
        let insns = result.unwrap();
        assert!(insns.len() > 0, "No instructions parsed");
    }
}
