extern crate regex;
use regex::*;
use std::path::Path;
use std::fs::File;
use std::io::{BufReader, BufRead};

/// Used to indicate what type an Immediate or Address is.
/// Immediates can be of any type, while Addresses cannot be ASCII.
pub enum ValueType {
    Binary,
    Decimal,
    Hexadecimal,
    ASCII
}

/// An immediate can be a byte or word (non-ASCII) or a character or string (ASCII).
/// The ImmediateInfo new() function checks ValueType and ImmediateSize combinations to make sure
/// it isn't invalid.
pub enum ImmediateSize {
    Byte,
    Word,
    Character,
    String
}

/// A structure that contains information about an Immediate value.
pub struct ImmediateInfo {
    vtype: ValueType,
    size: ImmediateSize
}

impl ImmediateInfo {

    /// Used to create an ImmediateInfo structure.
    /// Checks the combination of ValueType & ImmediateSize to make sure it isn't invalid. If it is invalid, Err().
    pub fn new(v: ValueType, s: ImmediateSize) -> Result<Self, String> {

        // Checking the value type first, as it determines which immediate sizes are valid and invalid.
        match v {
            ValueType::ASCII => {

                // ASCII Immediates can only be a character or a string.
                match s {
                    ImmediateSize::Character => Ok(ImmediateInfo {
                        vtype: v,
                        size: s
                    }),
                    ImmediateSize::String => Ok(ImmediateInfo {
                        vtype: v,
                        size: s
                    }),
                    _ => Err(format!("An immediate with value type {} and size {} (invalid combo) was found", v, s))
                }
            }
            _ => {

                // Numeric Immediates can only be a byte or a word.
                match s {
                    ImmediateSize::Byte => Ok(ImmediateInfo {
                        vtype: v,
                        size: s
                    }),
                    ImmediateSize::Word => Ok(ImmediateInfo {
                        vtype: v,
                        size: s
                    }),
                    _ => Err(format!("An immediate with value type {} and size {} (invalid combo) was found", v, s))
                }
            }
        }
    }
}

/// Indicates the pointer (Stack Pointer or Instruction Pointer) that a Relative address is relative to.
pub enum Pointer {
    Stack,
    Instruction
}

/// Addresses can be one of the following address modes.
/// Zero Page - one byte, assumes the first two bytes of the address are 00 hex.
/// Zero Bank - two bytes, assumes the first byte of the address is 00 hex.
/// Relative - two bytes, a signed displacement from one of two pointers indicated by enum Pointer.
/// Absolute Port - two bytes, a word indicating one of 65,536 I/O registers.
/// Absolute Memory - three bytes, can access the full 16 mebibytes of memory.
pub enum AddressMode {
    ZeroPage,
    ZeroBank,
    Relative(Pointer),
    AbsolutePort,
    AbsoluteMemory
}

/// A structure that contains information about an Address.
pub struct AddressInfo {
    vtype: ValueType,
    mode: AddressMode
}

impl AddressInfo {

    /// Used to create an AddressInfo structure. Checks to make sure the address isn't ASCII. If it is ASCII, Err().
    pub fn new(v: ValueType, m: AddressMode) -> Result<Self, String> {
        match v {
            ValueType::ASCII => Err(format!("An address with value type {} (addresses cannot be ASCII) was found", v)),
            _ => Ok(AddressInfo {
                vtype: v,
                mode: m
            })
        }
    }
}

/// An identifier can be a label or a symbol.
/// Label - identifies an address within a source file.
/// Symbol - identifies an address outside a source file, or a frequently used immediate.
pub enum IdentifierType {
    Label,
    Symbol
}

/// Right now, registers are just integer because we only have an integer ALU. But, futureproofing!
pub enum RegisterType {
    Integer
}

/// An enumeration containing information about a token.
pub enum TokenInfo {
    Immediate(ImmediateInfo),
    Address(AddressInfo),
    Identifier(Option<IdentifierType>),
    Register(RegisterType),
    Operation,
    Directive,
    Tab
}

/// A token! Has content and information about that content (so data and metadata)
pub struct Token {
    content: String,
    info: TokenInfo
}

impl Token {

    /// Used to make a new token. No checking here - that should already be done by the time this is called.
    pub fn new(c: &str, i: TokenInfo) -> Self {
        Self {
            content: String::from(c),
            info: i
        }
    }
}

/// Borrows a string and checks if it begins with an immediate value. Just a helper function.
fn immediate(s: &str) -> Result<Token, String> {
    let bin_regex = Regex::new("[01]+").unwrap();
    let dec_regex = Regex::new("[0-9]+").unwrap();
    let hex_regex = Regex::new("[0-9a-fA-F]+").unwrap();
    let mut imm_info = ImmediateInfo::new(ValueType::ASCII, ImmediateSize::Word);
    let mut content = "";

    if s.starts_with("#") {
        // Numeric Immediate
        if s.starts_with("#%") {
            // Binary
            if bin_regex.is_match(&s[2..18]) {
                // 16-bit VALID
                imm_info = ImmediateInfo::new(ValueType::Binary, ImmediateSize::Word);
                content = &s[0..18];
            } else if bin_regex.is_match(&s[2..10]) {
                // 8-bit VALID
                imm_info = ImmediateInfo::new(ValueType::Binary, ImmediateSize::Byte);
                content = &s[0..10];
            } else {
                // INVALID
                Err(format!("Invalid binary immediate"))
            }
        } else if s.starts_with("#$") {
            // Hexadecimal
            if hex_regex.is_match(&s[2..6]) {
                // 16-bit VALID
                imm_info = ImmediateInfo::new(ValueType::Hexadecimal, ImmediateSize::Word);
                content = &s[0..6];
            } else if hex_regex.is_match(&s[2..4]) {
                // 8-bit VALID
                imm_info = ImmediateInfo::new(ValueType::Hexadecimal, ImmediateSize::Byte);
                content = &s[0..4];
            } else {
                // INVALID
                Err(format!("Invalid hexadecimal immediate"))
            }
        } else {
            // Decimal

            // Behold, the LENGTH-FINDER-INATOR!
            // it's just a loop though...
            let mut l = 1;
            loop {
                if s[l..l+1] == " " || s[l..l+1] == "," || s[l..l+2] == "\r\n" || s[l..l+1] == "\n" {
                    break;
                }
                l = l + 1;
            }

            let poss_dec = &s[1..l];
            if dec_regex.is_match(poss_dec) {
                // VALID
                if usize::from_str_radix(poss_dec, 10) <= 255 {
                    // 8-bit
                    imm_info = ImmediateInfo::new(ValueType::Decimal, ImmediateSize::Byte);
                    content = &s[0..l];
                } else if usize::from_str_radix(poss_dec, 10) <= 65535 {
                    // 16-bit
                    imm_info = ImmediateInfo::new(ValueType::Decimal, ImmediateSize::Word);
                    content = &s[0..l];
                } else {
                    // Out of bounds
                    Err(format!("Decimal immediate out of range"))
                }
            } else {
                // INVALID
                Err(format!("Invalid decimal immediate"))
            }
        }
    } else if s.starts_with("'") {
        // ASCII Character Immediate
        if &s[1..2] == '\\' {
            // Escape
            if &s[2..3] == "x" {
                // ASCII character ID value
                if &s[5..6] == '\'' {
                    // character immediate is properly closed
                    imm_info = ImmediateInfo::new(ValueType::ASCII, ImmediateSize::Character);
                    content = &s[0..6]
                } else {
                    // error!
                    Err(format!("Character immediate not closed"))
                }
            }

            if &s[3..4] == '\'' {
                // character immediate is properly closed
                imm_info = ImmediateInfo::new(ValueType::ASCII, ImmediateSize::Character);
                content = &s[0..4]
            } else {
                // error!
                Err(format!("Character immediate not closed"))
            }
        } else {
            // Regular character
            if &s[2..3] == '\'' {
                // character immediate is properly closed
                imm_info = ImmediateInfo::new(ValueType::ASCII, ImmediateSize::Character);
                content = &s[0..3]
            } else {
                // error!
                Err(format!("Character immediate not closed"))
            }
        }
    } else if s.starts_with("\"") {
        // ASCII String Immediate

        // Behold, the LENGTH-FINDER-INATOR!
        // it's just a loop though...
        let mut l = 1;
        loop {
            if s[l..l+1] == "\"" {
                if s[l-1..l] == "\\" {
                    if s[l-2..l-1] == "\\" {
                        break;
                    }
                } else {
                    break;
                }
            }
            l = l + 1;
        }

        content = &s[0..l];
        imm_info = ImmediateInfo::new(ValueType::ASCII, ImmediateSize::String);
    } else {
        // Not an immediate.
        Err(format!("e"))
    }

    match imm_info {
        Ok(i) => Ok(Token::new(content, TokenInfo::Immediate(i))),
        Err(e) => Err(format!("If you see this message, run."))
    }
}

/// Borrows a string and checks if it begins with an address. Just a helper function.
fn address(s: &str) -> Result<Token, String> {
    let bin_regex = Regex::new("[01]+").unwrap();
    let dec_regex = Regex::new("[0-9]+").unwrap();
    let hex_regex = Regex::new("[0-9a-fA-F]+").unwrap();
    let mut add_info = AddressInfo::new(ValueType::ASCII, AddressMode::AbsolutePort);
    let mut content = "";

    if &s[1..2] == "P" {
        // Relative Address
        let mut ptr = Pointer::Stack;
        if &s[0..1] == "I" {
            ptr = Pointer::Instruction;
        } else if &s[0..1] == "S" {
            // Do Nothing.
        } else {
            // Error!
            Err(format!("Invalid pointer for relative address"))
        }

        if &s[3..4] == "%" {
            // Binary
            if bin_regex.is_match(&s[4..20]) {
                // Valid
                if usize::from_str_usize(&s[4..20], 2) <= 32767 {
                    // Good!
                    add_info = AddressInfo::new(ValueType::Binary, AddressMode::Relative(ptr));
                    content = &s[0..20];
                } else {
                    // Out of range
                    Err(format!("Binary relative address out of range"))
                }
            } else {
                // Invalid
                Err(format!("Invalid binary relative address"))
            }
        } else if &s[3..4] == "$" {
            // Hexadecimal
            if hex_regex.is_match(&s[4..8]) {
                // Valid
                if usize::from_str_usize(&s[4..8], 16) <= 32767 {
                    // Good!
                    add_info = AddressInfo::new(ValueType::Hexadecimal, AddressMode::Relative(ptr));
                    content = &s[0..8];
                } else {
                    // Out of range
                    Err(format!("Hexadecimal relative address out of range"))
                }
            } else {
                // Invalid
                Err(format!("Invalid Hexadecimal relative address"))
            }
        } else {
            // Decimal

            // Behold, the LENGTH-FINDER-INATOR!
            // it's just a loop though...
            let mut l = 3;
            loop {
                if s[l..l+1] == " " || s[l..l+1] == "," || s[l..l+2] == "\r\n" || s[l..l+1] == "\n" {
                    break;
                }
                l = l + 1;
            }

            let poss_dec = &s[3..l];
            if dec_regex.is_match(poss_dec) {
                // Valid
                if usize::from_str_usize(poss_dec, 10) <= 32767 {
                    // Good!
                    add_info = AddressInfo::new(ValueType::Binary, AddressMode::Relative(ptr));
                    content = &s[0..l];
                } else {
                    // Out of range
                    Err(format!("Decimal relative address out of range"))
                }
            } else {
                // Invalid
                Err(format!("Invalid Decimal relative address"))
            }
        }
    } else {
        // Normal
        if s.starts_with("%") {
            // Binary
            if bin_regex.is_match(&s[1..25]) {
                // Absolute Memory
                add_info = AddressInfo::new(ValueType::Binary, AddressMode::AbsoluteMemory);
                content = &s[0..25];
            } else if bin_regex.is_match(&s[1..17]) {
                if &s[17..18] == "p" {
                    // Absolute Port
                    add_info = AddressInfo::new(ValueType::Binary, AddressMode::AbsolutePort);
                    content = &s[0..18];
                } else {
                    // Zero Bank
                    add_info = AddressInfo::new(ValueType::Binary, AddressMode::ZeroBank);
                    content = &s[0..17];
                }
            } else if bin_regex.is_match(&s[1..9]) {
                // Zero Page
                add_info = AddressInfo::new(ValueType::Binary, AddressMode::ZeroPage);
                content = &s[0..9];
            } else {
                // Error
                Err(format!("Invalid Binary address or Binary address out of range"))
            }
        } else if s.starts_with("$") {
            // Hexadecimal
            if hex_regex.is_match(&s[1..7]) {
                // Absolute Memory
                add_info = AddressInfo::new(ValueType::Hexadecimal, AddressMode::AbsoluteMemory);
                content = &s[0..7];
            } else if hex_regex.is_match(&s[1..5]) {
                if &s[17..18] == "p" {
                    // Absolute Port
                    add_info = AddressInfo::new(ValueType::Hexadecimal, AddressMode::AbsolutePort);
                    content = &s[0..6];
                } else {
                    // Zero Bank
                    add_info = AddressInfo::new(ValueType::Hexadecimal, AddressMode::ZeroBank);
                    content = &s[0..7];
                }
            } else if hex_regex.is_match(&s[1..3]) {
                // Zero Page
                add_info = AddressInfo::new(ValueType::Hexadecimal, AddressMode::ZeroPage);
                content = &s[0..3];
            } else {
                // Error
                Err(format!("Invalid Hexadecimal address or Hexadecimal address out of range"))
            }
        } else {
            // Decimal

            // Behold, the LENGTH-FINDER-INATOR!
            // it's just a loop though...
            let mut l = 0;
            loop {
                if s[l..l+1] == " " || s[l..l+1] == "," || s[l..l+1] == "p" || s[l..l+2] == "\r\n" || s[l..l+1] == "\n" {
                    break;
                }
                l = l + 1;
            }

            let poss_dec = &s[0..l];

            if dec_regex.is_match(poss_dec) {
                if &s[l..l+1] == "p" {
                    // Absolute Port Address
                    if usize::from_str_radix(poss_dec, 10) <= 65535 {
                        // Good!
                        add_info = AddressInfo::new(ValueType::Decimal, AddressMode::AbsolutePort);
                        content = &s[0..l+1];
                    } else {
                        // Error
                        Err(format!("Decimal address out of range"))
                    }
                } else {
                    if usize::from_str_radix(poss_dec, 10) <= 255 {
                        // Zero Page
                        add_info = AddressInfo::new(ValueType::Decimal, AddressMode::ZeroPage);
                        content = poss_dec;
                    } else if usize::from_str_radix(poss_dec, 10) <= 65535 {
                        // Zero Bank
                        add_info = AddressInfo::new(ValueType::Decimal, AddressMode::ZeroBank);
                        content = poss_dec;
                    } else if usize::from_str_radix(poss_dec, 10) <= 16777215 {
                        // Absolute Memory
                        add_info = AddressInfo::new(ValueType::Decimal, AddressMode::AbsoluteMemory);
                        content = poss_dec;
                    } else {
                        // Error
                        Err(format!("Decimal address out of range"))
                    }
                }
            } else {
                // Error
                Err(format!("Invalid Decimal address"))
            }
        }
    }

    match add_info {
        Ok(i) => Ok(Token::new(content, TokenInfo::Address(i))),
        Err(e) => Err(format!("e"))
    }
}

/// Borrows a string and checks if it begins with a valid identifier. Just a helper function.
/// A valid identifier consists of only numbers, letters, underscores, and periods.
fn identifier(s: &str) -> Result<Token, String> {
    let regid = Regex::new("[0-9a-zA-Z_.]+").unwrap();

    // Behold, the LENGTH-FINDER-INATOR!
    // it's just a loop though...
    let mut l = 0;
    loop {
        if s[l..l+1] == " " || s[l..l+1] == "," || s[l..l+1] == ":" || s[l..l+2] == "\r\n" || s[l..l+1] == "\n" {
            break;
        }
        l = l + 1;
    }

    let ident = &s[0..l];

    if regid.is_match(ident) {
        Ok(Token::new(ident, TokenInfo::Identifier(None)))
    } else {
        Err(format!("Invalid identifier"))
    }
}

/// Borrows a string and checks if it begins with a register. Just a helper function.
fn register(s: &str) -> Result<Token, String> {
    let mut ret: Token = Token::new("", TokenInfo::Register(RegisterType::Integer));

    if s.starts_with("iA") {
        if s.starts_with("iA ") || s.starts_with("iA,") || s.starts_with("iA\n") || s.starts_with("iA\r\n") {
            // 16-bit integer A register
            ret = Token::new("iA", TokenInfo::Register(RegisterType::Integer));
        } else if s.starts_with("iAL ") || s.starts_with("iAL,") || s.starts_with("iAL\n") || s.starts_with("iAL\r\n") {
            // 8-bit integer A Low register
            ret = Token::new("iAL", TokenInfo::Register(RegisterType::Integer));
        } else if s.starts_with("iAH ") || s.starts_with("iAH,") || s.starts_with("iAH\n") || s.starts_with("iAH\r\n") {
            // 8-bit integer A High register
            ret = Token::new("iAH", TokenInfo::Register(RegisterType::Integer));
        } else {
            // Invalid! Register is not sufficiently separate from surrounding text!
            Err(format!("Register is not sufficiently separate from surrounding text"))
        }
    } else if s.starts_with("iB") {
        if s.starts_with("iB ") || s.starts_with("iB,") || s.starts_with("iB\n") || s.starts_with("iB\r\n") {
            // 16-bit integer B register
            ret = Token::new("iB", TokenInfo::Register(RegisterType::Integer));
        } else if s.starts_with("iBL ") || s.starts_with("iBL,") || s.starts_with("iBL\n") || s.starts_with("iBL\r\n") {
            // 8-bit integer B Low register
            ret = Token::new("iBL", TokenInfo::Register(RegisterType::Integer));
        } else if s.starts_with("iBH ") || s.starts_with("iBH,") || s.starts_with("iBH\n") || s.starts_with("iBH\r\n") {
            // 8-bit integer B High register
            ret = Token::new("iBH", TokenInfo::Register(RegisterType::Integer));
        } else {
            // Invalid! Register is not sufficiently separate from surrounding text!
            Err(format!("Register is not sufficiently separate from surrounding text"))
        }
    } else if s.starts_with("iX ") || s.starts_with("iX,") || s.starts_with("iX\n") || s.starts_with("iX\r\n") {
        // 16-bit integer X index register
        ret = Token::new("iX", TokenInfo::Register(RegisterType::Integer));
    } else if s.starts_with("iY ") || s.starts_with("iY,") || s.starts_with("iY\n") || s.starts_with("iY\r\n") {
        // 16-bit integer Y index register
        ret = Token::new("iY", TokenInfo::Register(RegisterType::Integer));
    } else {
        // Invalid! Not a register, or register is not sufficiently separate from surrounding text
        Err(format!("Not a register, or register is not sufficiently separate from surrounding text"))
    }

    Ok(ret)
}

/// Takes a string, verbose mode flag, spaces-per-tab count, and line number, and tokenizes the string.
/// There's a lot of stuff going on in here.
fn tokenize_line(lc: &str, v: bool, spt: usize, l: usize) -> Result<Vec<Token>, String> {
    let mut lcontent = String::from(lc);
    let mut tokens: Vec<Token> = vec!();

    if lcontent.contains(":") {
        // We've got a label (definition)!
        let mut idtok = identifier(&lcontent);
        match idtok {
            Ok(mut t) => {
                t.info = TokenType::Identifier(Some(IdentifierType::Label));
                tokens.push(t);

                if v {
                    println!("INFO: Label \"{}\" defined on line {}.", &t.content, l);
                }

                lcontent = lcontent.replacen(format!("{}:", &t.content), "", 1);
            }
            Err(e) => Err(format!("{} on line {}.", e, l))
        }
    } else if lcontent.starts_with("\t") {
        // We've got a tab!
        tokens.push(Token::new("\t", TokenInfo::Tab));
        lcontent = lcontent.replacen("\t", "", 1);
    } else if lcontent.starts_with(" ".repeat(spt)) {
        // We've got a bunch of spaces masquerading as a tab!
        tokens.push(Token::new("\t", TokenInfo::Tab));
        lcontent = lcontent.replacen(" ".repeat(spt), "", 1);
    } else {
        // If it isn't any of the above, we don't care and should return early.
        Ok(tokens)
    }

    // The whitespace eradication loop. Gets rid of extra whitespace that we don't need.
    loop {
        if lcontent.starts_with("\t") {
            // We've got a tab!
            lcontent = lcontent.replacen("\t", "", 1);
        } else if lcontent.starts_with(" ") {
            // We've got a space!
            lcontent = lcontent.replacen(" ", "", 1);
        } else {
            // Escape the whitespace eradication loop!
            break;
        }
    }

    if lcontent.is_empty() || lcontent.starts_with(";") {
        // We don't care.
    } else if lcontent.starts_with(".") {
        // Assembler directives!
        if lcontent.to_lowercase().starts_with(".org") {
            // Origin Directive
            tokens.push(Token::new(&lcontent[0..4], TokenInfo::Directive));
            lcontent = lcontent.replacen(&lcontent[0..5], "", 1);

            if v {
                println!("INFO: Origin directive found on line {}.", l);
            }

            let arg = address(&lcontent);
            match arg {
                Ok(t) => {
                    match &t.info {
                        TokenInfo::Address(i) => {
                            match i.mode {
                                AddressMode::AbsoluteMemory => {
                                    tokens.push(t);

                                    if v {
                                        println!("INFO: Address {} found on line {}.", &t.content, l);
                                    }
                                }
                                _ => Err(format!("Origin Directive with invalid argument (.org must have an absolute memory address) on line {}.", l))
                            }
                        }
                        _ => Err(format!("This message should never be shown."))
                    }
                }
                Err(e) => {
                    if e == String::from("e") {
                        Err(format!("Origin Directive with invalid argument (.org must have an absolute memory address) on line {}.", l))
                    } else {
                        Err(format!("{} on line {}.", e, l))
                    }
                }
            }
        } else if lcontent.to_lowercase().starts_with(".def") {
            // Symbol Definition Directive
            tokens.push(Token::new(&lcontent[0..4], TokenInfo::Directive));
            lcontent = lcontent.replacen(&lcontent[0..5], "", 1);

            if v {
                println!("INFO: Symbol definition directive found on line {}.", l);
            }

            let ident = identifier(&lcontent);
            match ident {
                Ok(mut t) => {
                    t.info = TokenInfo::Identifier(Some(IdentifierType::Symbol));
                    tokens.push(t);

                    if v {
                        println!("INFO: Symbol \"{}\" defined on line {}.", &t.content, l);
                    }

                    lcontent = lcontent.replacen(&t.content, "", 1);
                }
                Err(e) => Err(format!("{} on line {}.", e, l))
            }

            if lcontent.starts_with(",") {
                lcontent = lcontent.replacen(",", "", 1);
            }

            if lcontent.starts_with(" ") {
                lcontent = lcontent.replacen(" ", "", 1);
            }

            // might as well get both of them done in one go!
            let arg = (immediate(&lcontent), address(&lcontent));
            match arg.0 {
                Ok(t) => {
                    match &t.info {
                        TokenInfo::Immediate(i) => {
                            if let ValueType::ASCII = i.vtype {
                                if let ImmediateSize::String = i.size {
                                    Err(format!("Symbol definition directive with invalid argument (.def cannot take a string) on line {}.", l))
                                } else {
                                    tokens.push(t);

                                    if v {
                                        println!("INFO: Character immediate '{}' found on line {}.", &t.content, l);
                                    }
                                }
                            } else {
                                tokens.push(t);

                                if v {
                                    println!("INFO: Numeric immediate {} found on line {}", &t.content, l);
                                }
                            }
                        }
                        _ => Err(format!("You should never see this message."))
                    }
                }
                Err(e) => {
                    if e == String::from("e") {
                        match arg.1 {
                            Ok(t) => {
                                tokens.push(t);

                                if v {
                                    println!("INFO: Address {} found on line {}.", &t.content, l);
                                }
                            }
                            Err(e) => {
                                if e == String::from("e") {
                                    Err(format!("Symbol definition directive with invalid argument (.def needs an address or immediate) on line {}.", l))
                                } else {
                                    Err(format!("{} on line {}.", e, l))
                                }
                            }
                        }
                    } else {
                        Err(format!("{} on line {}.", e, l))
                    }
                }
            }
        } else if lcontent.to_lowercase().starts_with(".byt") {
            // Byte Placement Directive
            tokens.push(Token::new(&lcontent[0..4], TokenInfo::Directive));
            lcontent = lcontent.replacen(&lcontent[0..5], "", 1);

            if v {
                println!("INFO: Byte placement directive found on line {}.", l);
            }

            let arg = immediate(&lcontent);
            match arg {
                Ok(t) => {
                    match &t.info {
                        TokenInfo::Immediate(i) => {
                            if let ValueType::ASCII = i.vtype {
                                if let ImmediateSize::Character = i.size {
                                    tokens.push(t);

                                    if v {
                                        println!("INFO: Character immediate '{}' found on line {}.", &t.content, l);
                                    }
                                } else {
                                    Err(format!("Byte placement directive with invalid argument (.byt cannot handle strings) on line {}.", l))
                                }
                            } else {
                                if let ImmediateSize::Byte = i.size {
                                    tokens.push(t);

                                    if v {
                                        println!("INFO: Numeric immediate {} found on line {}", &t.content, l);
                                    }
                                } else {
                                    Err(format!("Byte placement directive with invalid argument (.byt needs a byte, not a word) on line {}.", l))
                                }
                            }
                        }
                        _ => Err(format!("You should never see this message."))
                    }
                }
                Err(e) => {
                    if e == String::from("e") {
                        let ident = identifier(&lcontent);
                        match ident {
                            Ok(t) => {
                                tokens.push(t);

                                if v {
                                    println!("INFO: Identifier \"{}\" used on line {}.", &t.content, l);
                                }
                            }
                            Err(e) => Err(format!("Byte placement directive with invalid argument (.byt needs a byte immediate or a valid identifier (label/symbol)) on line {}.", l))
                        }
                    } else {
                        Err(format!("{} on line {}.", e, l))
                    }
                }
            }
        } else if lcontent.to_lowercase().starts_with(".wrd") {
            // Word Placement Directive
            tokens.push(Token::new(&lcontent[0..4], TokenInfo::Directive));
            lcontent = lcontent.replacen(&lcontent[0..5], "", 1);

            if v {
                println!("INFO: Word placement directive found on line {}.", l);
            }

            let arg = immediate(&lcontent);
            match arg {
                Ok(t) => {
                    match &t.info {
                        TokenInfo::Immediate(i) => {
                            if let ValueType::ASCII = i.vtype {
                                Err(format!("Word placement directive with invalid argument (.wrd can't handle characters or strings) on line {}.", l))
                            } else {
                                if let ImmediateSize::Word = i.size {
                                    tokens.push(t);

                                    if v {
                                        println!("INFO: Numeric immediate {} found on line {}", &t.content, l);
                                    }
                                } else {
                                    Err(format!("Word placement directive with invalid argument (.wrd needs a word, not a byte) on line {}.", l))
                                }
                            }
                        }
                        _ => Err(format!("You should never see this message."))
                    }
                }
                Err(e) => {
                    if e == String::from("e") {
                        let ident = identifier(&lcontent);
                        match ident {
                            Ok(t) => {
                                tokens.push(t);

                                if v {
                                    println!("INFO: Identifier \"{}\" used on line {}.", &t.content, l);
                                }
                            }
                            Err(e) => Err(format!("Word placement directive with invalid argument (.wrd needs a word immediate or a valid identifier (label/symbol)) on line {}.", l))
                        }
                    } else {
                        Err(format!("{} on line {}.", e, l))
                    }
                }
            }
        } else if lcontent.to_lowercase().starts_with(".vec") {
            // Vector Placement Directive
            tokens.push(Token::new(&lcontent[0..4], TokenInfo::Directive));
            lcontent = lcontent.replacen(&lcontent[0..5], "", 1);

            if v {
                println!("INFO: Vector placement directive found on line {}.", l);
            }

            let arg = address(&lcontent);
            match arg {
                Ok(t) => {
                    match &t.info {
                        TokenInfo::Address(i) => {
                            if let AddressMode::AbsoluteMemory = i.mode {
                                tokens.push(t);

                                if v {
                                    println!("INFO: Address {} found on line {}.", &t.content, l);
                                }
                            } else {
                                Err(format!("Vector placement directive with invalid argument (.vec needs an absolute memory address) on line {}.", l))
                            }
                        }
                        _ => Err(format!("You should never see this message."))
                    }
                }
                Err(e) => {
                    if e == String::from("e") {
                        let ident = identifier(&lcontent);
                        match ident {
                            Ok(t) => {
                                tokens.push(t);

                                if v {
                                    println!("INFO: Identifier \"{}\" used on line {}.", &t.content, l);
                                }
                            }
                            Err(e) => Err(format!("Vector placement directive with invalid argument (.vec needs an absolute memory address or a valid identifier (label/symbol)) on line {}.", l))
                        }
                    } else {
                        Err(format!("{} on line {}.", e, l))
                    }
                }
            }
        } else if lcontent.to_lowercase().starts_with(".str") {
            // Zero-Terminated ASCII String Placement Directive
            tokens.push(Token::new(&lcontent[0..4], TokenInfo::Directive));
            lcontent = lcontent.replacen(&lcontent[0..5], "", 1);

            if v {
                println!("INFO: Zero-terminated ASCII string placement directive found on line {}.", l);
            }

            let arg = immediate(&lcontent);
            match arg {
                Ok(t) => {
                    match &t.info {
                        TokenInfo::Immediate(i) => {
                            if let ValueType::ASCII = i.vtype {
                                if let ImmediateSize::String = i.size {
                                    tokens.push(t);

                                    if v {
                                        println!("String {} found on line {}.", &t.content, l);
                                    }
                                } else {
                                    Err(format!("String placement directive with invalid argument (.str can only take a string) on line {}.", l))
                                }
                            } else {
                                Err(format!("String placement directive with invalid argument (.str can only take a string) on line {}.", l))
                            }
                        }
                        _ => Err(format!("This message should never be shown."))
                    }
                }
                Err(e) => {
                    if e == String::from("e") {
                        Err(format!("String placement directive with invalid argument (.str can only take a string) on line {}.", l))
                    } else {
                        Err(format!("{} on line {}.", e, l))
                    }
                }
            }
        } else {
            // Error!
            Err(format!("Unknown assembler directive on line {}.", line))
        }
    } else {
        // Processor operations!
        if lcontent.to_lowercase().starts_with("a") {
            // ADCI, ADDI, AND
            if lcontent.to_lowercase().starts_with("ad") {
                // ADCI, ADDI
                if lcontent.to_lowercase().starts_with("adc") {
                    // ADCI
                    if lcontent.to_lowercase().starts_with("adci") {
                        tokens.push(Token::new(&lcontent[0..4], TokenInfo::Operation));
                        lcontent = lcontent.replacen(&lcontent[0..5], "", 1);

                        if v {
                            println!("INFO: Operation '{}' found on line {}.", tokens[tokens.len()-1], l);
                        }
                    } else {
                        Err(format!("Unknown operation mnemonic on line {}. Did you mean 'adci'?", l))
                    }
                } else if lcontent.to_lowercase().starts_with("add") {
                    // ADDI
                    if lcontent.to_lowercase().starts_with("addi") {
                        tokens.push(Token::new(&lcontent[0..4], TokenInfo::Operation));
                        lcontent = lcontent.replacen(&lcontent[0..5], "", 1);

                        if v {
                            println!("INFO: Operation '{}' found on line {}.", tokens[tokens.len()-1], l);
                        }
                    } else {
                        Err(format!("Unknown operation mnemonic on line {}. Did you mean 'addi'?", l))
                    }
                } else {
                    Err(format!("Unknown operation mnemonic on line {}. Did you mean 'adci' or 'addi'?", l))
                }
            } else if lcontent.to_lowercase().starts_with("an") {
                // AND
                if lcontent.to_lowercase().starts_with("and") {
                    tokens.push(Token::new(&lcontent[0..3], TokenInfo::Operation));
                    lcontent = lcontent.replacen(&lcontent[0..4], "", 1);

                    if v {
                        println!("INFO: Operation '{}' found on line {}.", tokens[tokens.len()-1], l);
                    }
                } else {
                    Err(format!("Unknown operation mnemonic on line {}. Did you mean 'and'?", l))
                }
            } else {
                Err(format!("Unknown operation mnemonic on line {}. Did you mean 'adci', 'addi', or 'and'?", l))
            }
        } else if lcontent.to_lowercase().starts_with("b") {
            // BEQU, BGES, BGEU, BGTS, BGTU, BLES, BLEU, BLTS, BLTU, BNEQ, BRK
            if lcontent.to_lowercase().starts_with("be") {
                // BEQU
                if lcontent.to_lowercase().starts_with("beq") {
                    if lcontent.to_lowercase().starts_with("bequ") {
                        tokens.push(Token::new(&lcontent[0..4], TokenInfo::Operation));
                        lcontent = lcontent.replacen(&lcontent[0..5], "", 1);

                        if v {
                            println!("INFO: Operation '{}' found on line {}.", tokens[tokens.len()-1], l);
                        }
                    } else {
                        Err(format!("Unknown operation mnemonic on line {}. Did you mean 'bequ'?", l))
                    }
                } else {
                    Err(format!("Unknown operation mnemonic on line {}. Did you mean 'bequ'?", l))
                }
            } else if lcontent.to_lowercase().starts_with("bg") {
                // BGES, BGEU, BGTS, BGTU
                if lcontent.to_lowercase().starts_with("bge") {
                    // BGES, BGEU
                    if lcontent.to_lowercase().starts_with("bges") {
                        tokens.push(Token::new(&lcontent[0..4], TokenInfo::Operation));
                        lcontent = lcontent.replacen(&lcontent[0..5], "", 1);

                        if v {
                            println!("INFO: Operation '{}' found on line {}.", tokens[tokens.len()-1], l);
                        }
                    } else if lcontent.to_lowercase().starts_with("bgeu") {
                        tokens.push(Token::new(&lcontent[0..4], TokenInfo::Operation));
                        lcontent = lcontent.replacen(&lcontent[0..5], "", 1);

                        if v {
                            println!("INFO: Operation '{}' found on line {}.", tokens[tokens.len()-1], l);
                        }
                    } else {
                        Err(format!("Unknown operation mnemonic on line {}. Did you mean 'bges' or 'bgeu'?", l))
                    }
                } else if lcontent. to_lowercase().starts_with("bgt") {
                    // BGTS, BGTU
                    if lcontent.to_lowercase().starts_with("bgts") {
                        tokens.push(Token::new(&lcontent[0..4], TokenInfo::Operation));
                        lcontent = lcontent.replacen(&lcontent[0..5], "", 1);

                        if v {
                            println!("INFO: Operation '{}' found on line {}.", tokens[tokens.len()-1], l);
                        }
                    } else if lcontent.to_lowercase().starts_with("bgtu") {
                        tokens.push(Token::new(&lcontent[0..4], TokenInfo::Operation));
                        lcontent = lcontent.replacen(&lcontent[0..5], "", 1);

                        if v {
                            println!("INFO: Operation '{}' found on line {}.", tokens[tokens.len()-1], l);
                        }
                    } else {
                        Err(format!("Unknown operation mnemonic on line {}. Did you mean 'bgts' or 'bgtu'?", l))
                    }
                } else {
                    Err(format!("Unknown operation mnemonic on line {}. Did you mean 'bges', 'bgeu', 'bgts', or 'bgtu'?", l))
                }
            } else if lcontent.to_lowercase().starts_with("bl") {
                // BLES, BLEU, BLTS, BLTU
                if lcontent.to_lowercase().starts_with("ble") {
                    // BLES, BLEU
                    if lcontent.to_lowercase().starts_with("bles") {
                        tokens.push(Token::new(&lcontent[0..4], TokenInfo::Operation));
                        lcontent = lcontent.replacen(&lcontent[0..5], "", 1);

                        if v {
                            println!("INFO: Operation '{}' found on line {}.", tokens[tokens.len()-1], l);
                        }
                    } else if lcontent.to_lowercase().starts_with("bleu") {
                        tokens.push(Token::new(&lcontent[0..4], TokenInfo::Operation));
                        lcontent = lcontent.replacen(&lcontent[0..5], "", 1);

                        if v {
                            println!("INFO: Operation '{}' found on line {}.", tokens[tokens.len()-1], l);
                        }
                    } else {
                        Err(format!("Unknown operation mnemonic on line {}. Did you mean 'bles' or 'bleu'?", l))
                    }
                } else if lcontent. to_lowercase().starts_with("blt") {
                    // BLTS, BLTU
                    if lcontent.to_lowercase().starts_with("blts") {
                        tokens.push(Token::new(&lcontent[0..4], TokenInfo::Operation));
                        lcontent = lcontent.replacen(&lcontent[0..5], "", 1);

                        if v {
                            println!("INFO: Operation '{}' found on line {}.", tokens[tokens.len()-1], l);
                        }
                    } else if lcontent.to_lowercase().starts_with("bltu") {
                        tokens.push(Token::new(&lcontent[0..4], TokenInfo::Operation));
                        lcontent = lcontent.replacen(&lcontent[0..5], "", 1);

                        if v {
                            println!("INFO: Operation '{}' found on line {}.", tokens[tokens.len()-1], l);
                        }
                    } else {
                        Err(format!("Unknown operation mnemonic on line {}. Did you mean 'blts' or 'bltu'?", l))
                    }
                } else {
                    Err(format!("Unknown operation mnemonic on line {}. Did you mean 'bles', 'bleu', 'blts', or 'bltu'?", l))
                }
            } else if lcontent.to_lowercase().starts_with("bn") {
                // BNEQ
                if lcontent.to_lowercase().starts_with("bne") {
                    if lcontent.to_lowercase().starts_with("bneq") {
                        tokens.push(Token::new(&lcontent[0..4], TokenInfo::Operation));
                        lcontent = lcontent.replacen(&lcontent[0..5], "", 1);

                        if v {
                            println!("INFO: Operation '{}' found on line {}.", tokens[tokens.len()-1], l);
                        }
                    } else {
                        Err(format!("Unknown operation mnemonic on line {}. Did you mean 'bneq'?", l))
                    }
                } else {
                    Err(format!("Unknown operation mnemonic on line {}. Did you mean 'bneq'?", l))
                }
            } else if lcontent.to_lowercase().starts_with("br") {
                // BRK
                if lcontent.to_lowercase().starts_with("brk") {
                    tokens.push(Token::new(&lcontent[0..3], TokenInfo::Operation));

                    if v {
                        println!("INFO: Operation '{}' found on line {}.", tokens[tokens.len()-1], l);
                    }
                } else {
                    Err(format!("Unknown operation mnemonic on line {}. Did you mean 'brk'?", l))
                }
            } else {
                Err(format!("Unknown operation mnemonic on line {}. Did you mean 'bequ', 'bges', 'bgeu', 'bgts', 'bgtu', 'bles', 'bleu', 'blts', 'bltu', 'bneq', or 'brk'?", l))
            }
        } else if lcontent.to_lowercase().starts_with("c") {
            // CALL, CLR, CLRF, CMPI
            if lcontent.to_lowercase().starts_with("ca") {
                // CALL
                if lcontent.to_lowercase().starts_with("cal") {
                    if lcontent.to_lowercase().starts_with("call") {
                        tokens.push(Token::new(&lcontent[0..4], TokenInfo::Operation));
                        lcontent = lcontent.replacen(&lcontent[0..5], "", 1);

                        if v {
                            println!("INFO: Operation '{}' found on line {}.", tokens[tokens.len()-1], l);
                        }
                    } else {
                        Err(format!("Unknown operation mnemonic on line {}. Did you mean 'call'?", l))
                    }
                } else {
                    Err(format!("Unknown operation mnemonic on line {}. Did you mean 'call'?", l))
                }
            } else if lcontent.to_lowercase().starts_with("cl") {
                // CLR, CLRF
                if lcontent.to_lowercase().starts_with("clr") {
                    if lcontent.to_lowercase().starts_with("clr ") {
                        tokens.push(Token::new(&lcontent[0..3], TokenInfo::Operation));
                        lcontent = lcontent.replacen(&lcontent[0..4], "", 1);

                        if v {
                            println!("INFO: Operation '{}' found on line {}.", tokens[tokens.len()-1], l);
                        }
                    } else if lcontent.to_lowercase().starts_with("clrf") {
                        tokens.push(Token::new(&lcontent[0..4], TokenInfo::Operation));
                        lcontent = lcontent.replacen(&lcontent[0..5], "", 1);

                        if v {
                            println!("INFO: Operation '{}' found on line {}.", tokens[tokens.len()-1], l);
                        }
                    } else {
                        Err(format!("Unknown operation mnemonic on line {}. Did you mean 'clr' or 'clrf'?", l))
                    }
                } else {
                    Err(format!("Unknown operation mnemonic on line {}. Did you mean 'clr' or 'clrf'?", l))
                }
            } else if lcontent.to_lowercase().starts_with("cm") {
                // CMPI
                if lcontent.to_lowercase().starts_with("cmp") {
                    if lcontent.to_lowercase().starts_with("cmpi") {
                        tokens.push(Token::new(&lcontent[0..4], TokenInfo::Operation));
                        lcontent = lcontent.replacen(&lcontent[0..5], "", 1);

                        if v {
                            println!("INFO: Operation '{}' found on line {}.", tokens[tokens.len()-1], l);
                        }
                    } else {
                        Err(format!("Unknown operation mnemonic on line {}. Did you mean 'cmpi'?", l))
                    }
                } else {
                    Err(format!("Unknown operation mnemonic on line {}. Did you mean 'cmpi'?", l))
                }
            } else {
                Err(format!("Unknown operation mnemonic on line {}. Did you mean 'call', 'clr', 'clrf', or 'cmpi'?", l))
            }
        } else if lcontent.to_lowercase().starts_with("h") {
            // HACF
            if lcontent.to_lowercase().starts_with("ha") {
                if lcontent.to_lowercase().starts_with("hac") {
                    if lcontent.to_lowercase().starts_with("hacf") {
                        tokens.push(Token::new(&lcontent[0..4], TokenInfo::Operation));

                        if v {
                            println!("INFO: Operation '{}' found on line {}.", tokens[tokens.len()-1], l);
                        }
                    } else {
                        Err(format!("Unknown operation mnemonic on line {}. Did you mean 'hacf'?", l))
                    }
                } else {
                    Err(format!("Unknown operation mnemonic on line {}. Did you mean 'hacf'?", l))
                }
            } else {
                Err(format!("Unknown operation mnemonic on line {}. Did you mean 'hacf'?", l))
            }
        } else if lcontent.to_lowercase().starts_with("j") {
            // JUMP
            if lcontent.to_lowercase().starts_with("ju") {
                if lcontent.to_lowercase().starts_with("jum") {
                    if lcontent.to_lowercase().starts_with("jump") {
                        tokens.push(Token::new(&lcontent[0..4], TokenInfo::Operation));
                        lcontent = lcontent.replacen(&lcontent[0..5], "", 1);

                        if v {
                            println!("INFO: Operation '{}' found on line {}.", tokens[tokens.len()-1], l);
                        }
                    } else {
                        Err(format!("Unknown operation mnemonic on line {}. Did you mean 'jump'?", l))
                    }
                } else {
                    Err(format!("Unknown operation mnemonic on line {}. Did you mean 'jump'?", l))
                }
            } else {
                Err(format!("Unknown operation mnemonic on line {}. Did you mean 'jump'?", l))
            }
        } else if lcontent.to_lowercase().starts_with("m") {
            // MOV
            if lcontent.to_lowercase().starts_with("mo") {
                if lcontent.to_Lowercase().starts_with("mov") {
                    tokens.push(Token::new(&lcontent[0..3], TokenInfo::Operation));
                    lcontent = lcontent.replacen(&lcontent[0..4], "", 1);

                    if v {
                        println!("INFO: Operation '{}' found on line {}.", tokens[tokens.len()-1], l);
                    }
                } else {
                    Err(format!("Unknown operation mnemonic on line {}. Did you mean 'mov'?", l))
                }
            } else {
                Err(format!("Unknown operation mnemonic on line {}. Did you mean 'mov'?", l))
            }
        } else if lcontent.to_lowercase().starts_with("n") {
            // NOP, NOT
            if lcontent.to_lowercase().starts_with("no") {
                if lcontent.to_lowercase().starts_with("nop") {
                    tokens.push(Token::new(&lcontent[0..3], TokenInfo::Operation));

                    if v {
                        println!("INFO: Operation '{}' found on line {}.", tokens[tokens.len()-1], l);
                    }
                } else if lcontent.to_lowercase().starts_with("not") {
                    tokens.push(Token::new(&lcontent[0..3], TokenInfo::Operation));
                    lcontent = lcontent.replacen(&lcontent[0..4], "", 1);

                    if v {
                        println!("INFO: Operation '{}' found on line {}.", tokens[tokens.len()-1], l);
                    }
                } else {
                    Err(format!("Unknown operation mnemonic on line {}. Did you mean 'nop' or 'not'?", l))
                }
            } else {
                Err(format!("Unknown operation mnemonic on line {}. Did you mean 'nop' or 'not'?", l))
            }
        } else if lcontent.to_lowercase().starts_with("o") {
            // OR
            if lcontent.to_lowercase().starts_with("or") {
                tokens.push(Token::new(&lcontent[0..2], TokenInfo::Operation));
                lcontent = lcontent.replacen(&lcontent[0..3], "", 1);

                if v {
                    println!("INFO: Operation '{}' found on line {}.", tokens[tokens.len()-1], l);
                }
            } else {
                Err(format!("Unknown operation mnemonic on line {}. Did you mean 'or'?", l))
            }
        } else if lcontent.to_lowercase().starts_with("p") {
            // PCNT, POP, POPF, PSHF, PUSH
            if lcontent.to_lowercase().starts_with("pc") {
                // PCNT
                if lcontent.to_lowercase().starts_with("pcn") {
                    if lcontent.to_lowercase().starts_with("pcnt") {
                        tokens.push(Token::new(&lcontent[0..4], TokenInfo::Operation));
                        lcontent = lcontent.replacen(&lcontent[0..5], "", 1);

                        if v {
                            println!("INFO: Operation '{}' found on line {}.", tokens[tokens.len()-1], l);
                        }
                    } else {
                        Err(format!("Unknown operation mnemonic on line {}. Did you mean 'pcnt'?", l))
                    }
                } else {
                    Err(format!("Unknown operation mnemonic on line {}. Did you mean 'pcnt'?", l))
                }
            } else if lcontent.to_lowercase().starts_with("po") {
                // POP, POPF
                if lcontent.to_lowercase().starts_with("pop") {
                    if lcontent.to_lowercase().starts_with("pop ") {
                        tokens.push(Token::new(&lcontent[0..3], TokenInfo::Operation));
                        lcontent = lcontent.replacen(&lcontent[0..4], "", 1);

                        if v {
                            println!("INFO: Operation '{}' found on line {}.", tokens[tokens.len()-1], l);
                        }
                    } else if lcontent.to_lowercase().starts_with("popf") {
                        tokens.push(Token::new(&lcontent[0..4], TokenInfo::Operation));

                        if v {
                            println!("INFO: Operation '{}' found on line {}.", tokens[tokens.len()-1], l);
                        }
                    } else {
                        Err(format!("Unknown operation mnemonic on line {}. Did you mean 'pop' or 'popf'?", l))
                    }
                } else {
                    Err(format!("Unknown operation mnemonic on line {}. Did you mean 'pop' or 'popf'?", l))
                }
            } else if lcontent.to_lowercase().starts_with("ps") {
                // PSHF
                if lcontent.to_lowercase().starts_with("psh") {
                    if lcontent.to_lowercase().starts_with("pshf") {
                        tokens.push(Token::new(&lcontent[0..4], TokenInfo::Operation));

                        if v {
                            println!("INFO: Operation '{}' found on line {}.", tokens[tokens.len()-1], l);
                        }
                    } else {
                        Err(format!("Unknown operation mnemonic on line {}. Did you mean 'pshf'?", l))
                    }
                } else {
                    Err(format!("Unknown operation mnemonic on line {}. Did you mean 'pshf'?", l))
                }
            } else if lcontent.to_lowercase().starts_with("pu") {
                // PUSH
                if lcontent.to_lowercase().starts_with("pus") {
                    if lcontent.to_lowercase().starts_with("push") {
                        tokens.push(Token::new(&lcontent[0..4], TokenInfo::Operation));
                        lcontent = lcontent.replacen(&lcontent[0..5], "", 1);

                        if v {
                            println!("INFO: Operation '{}' found on line {}.", tokens[tokens.len()-1], l);
                        }
                    } else {
                        Err(format!("Unknown operation mnemonic on line {}. Did you mean 'push'?", l))
                    }
                } else {
                    Err(format!("Unknown operation mnemonic on line {}. Did you mean 'push'?", l))
                }
            } else {
                Err(format!("Unknown operation mnemonic on line {}. Did you mean 'pcnt', 'pop', 'popf', 'pshf', or 'push'?", l))
            }
        } else if lcontent.to_lowercase().starts_with("r") {
            // RETI, RETS, ROL, ROR
            if lcontent.to_lowercase().starts_with("re") {
                // RETI, RETS
                if lcontent.to_lowercase().starts_with("ret") {
                    if lcontent.to_lowercase().starts_with("reti") {
                        tokens.push(Token::new(&lcontent[0..4], TokenInfo::Operation));

                        if v {
                            println!("INFO: Operation '{}' found on line {}.", tokens[tokens.len()-1], l);
                        }
                    } else if lcontent.to_lowercase().starts_with("rets") {
                        tokens.push(Token::new(&lcontent[0..4], TokenInfo::Operation));

                        if v {
                            println!("INFO: Operation '{}' found on line {}.", tokens[tokens.len()-1], l);
                        }
                    } else {
                        Err(format!("Unknown operation mnemonic on line {}. Did you mean 'reti' or 'rets'?", l))
                    }
                } else {
                    Err(format!("Unknown operation mnemonic on line {}. Did you mean 'reti' or 'rets'?", l))
                }
            } else if lcontent.to_lowercase().starts_with("ro") {
                // ROL, ROR
                if lcontent.to_lowercase().starts_with("rol") {
                    tokens.push(Token::new(&lcontent[0..3], TokenInfo::Operation));
                    lcontent = lcontent.replacen(&lcontent[0..4], "", 1);

                    if v {
                        println!("INFO: Operation '{}' found on line {}.", tokens[tokens.len()-1], l);
                    }
                } else if lcontent.to_lowercase().starts_with("ror") {
                    tokens.push(Token::new(&lcontent[0..3], TokenInfo::Operation));
                    lcontent = lcontent.replacen(&lcontent[0..4], "", 1);

                    if v {
                        println!("INFO: Operation '{}' found on line {}.", tokens[tokens.len()-1], l);
                    }
                } else {
                    Err(format!("Unknown operation mnemonic on line {}. Did you mean 'rol' or 'ror'?", l))
                }
            } else {
                Err(format!("Unknown operation mnemonic on line {}. Did you mean 'reti', 'rets', 'rol', or 'ror'?", l))
            }
        } else if lcontent.to_lowercase().starts_with("s") {
            // SBCI, SET, SETF, SHL, SHR, SUBI
            if lcontent.to_lowercase().starts_with("sb") {
                // SBCI
                if lcontent.to_lowercase().starts_with("sbc") {
                    if lcontent.to_lowercase().starts_with("sbci") {
                        tokens.push(Token::new(&lcontent[0..4], TokenInfo::Operation));
                        lcontent = lcontent.replacen(&lcontent[0..5], "", 1);

                        if v {
                            println!("INFO: Operation '{}' found on line {}.", tokens[tokens.len()-1], l);
                        }
                    } else {
                        Err(format!("Unknown operation mnemonic on line {}. Did you mean 'sbci'?", l))
                    }
                } else {
                    Err(format!("Unknown operation mnemonic on line {}. Did you mean 'sbci'?", l))
                }
            } else if lcontent.to_lowercase().starts_with("se") {
                // SET, SETF
                if lcontent.to_lowercase().starts_with("set") {
                    if lcontent.to_lowercase().starts_with("set ") {
                        tokens.push(Token::new(&lcontent[0..3], TokenInfo::Operation));
                        lcontent = lcontent.replacen(&lcontent[0..4], "", 1);

                        if v {
                            println!("INFO: Operation '{}' found on line {}.", tokens[tokens.len()-1], l);
                        }
                    } else if lcontent.to_lowercase().starts_with("setf") {
                        tokens.push(Token::new(&lcontent[0..4], TokenInfo::Operation));
                        lcontent = lcontent.replacen(&lcontent[0..5], "", 1);

                        if v {
                            println!("INFO: Operation '{}' found on line {}.", tokens[tokens.len()-1], l);
                        }
                    } else {
                        Err(format!("Unknown operation mnemonic on line {}. Did you mean 'set' or 'setf'?", l))
                    }
                } else {
                    Err(format!("Unknown operation mnemonic on line {}. Did you mean 'set' or 'setf'?", l))
                }
            } else if lcontent.to_lowercase().starts_with("sh") {
                // SHL, SHR
                if lcontent.to_lowercase().starts_with("shl") {
                    tokens.push(Token::new(&lcontent[0..3], TokenInfo::Operation));
                    lcontent = lcontent.replacen(&lcontent[0..4], "", 1);

                    if v {
                        println!("INFO: Operation '{}' found on line {}.", tokens[tokens.len()-1], l);
                    }
                } else if lcontent.to_lowercase().starts_with("shr") {
                    tokens.push(Token::new(&lcontent[0..3], TokenInfo::Operation));
                    lcontent = lcontent.replacen(&lcontent[0..4], "", 1);

                    if v {
                        println!("INFO: Operation '{}' found on line {}.", tokens[tokens.len()-1], l);
                    }
                } else {
                    Err(format!("Unknown operation mnemonic on line {}. Did you mean 'shl' or 'shr'?", l))
                }
            } else if lcontent.to_lowercase().starts_with("su") {
                // SUBI
                if lcontent.to_lowercase().starts_with("sub") {
                    if lcontent.to_lowercase().starts_with("subi") {
                        tokens.push(Token::new(&lcontent[0..4], TokenInfo::Operation));
                        lcontent = lcontent.replacen(&lcontent[0..5], "", 1);

                        if v {
                            println!("INFO: Operation '{}' found on line {}.", tokens[tokens.len()-1], l);
                        }
                    } else {
                        Err(format!("Unknown operation mnemonic on line {}. Did you mean 'subi'?", l))
                    }
                } else {
                    Err(format!("Unknown operation mnemonic on line {}. Did you mean 'subi'?", l))
                }
            } else {
                Err(format!("Unknown operation mnemonic on line {}. Did you mean 'sbci', 'set', 'setf', 'shl', 'shr', or 'subi'?", l))
            }
        } else if lcontent.to_lowercase().starts_with("t") {
            // TEST, TSTF
            if lcontent.to_lowercase().starts_with("te") {
                // TEST
                if lcontent.to_lowercase().starts_with("tes") {
                    if lcontent.to_lowercase().starts_with("test") {
                        tokens.push(Token::new(&lcontent[0..4], TokenInfo::Operation));
                        lcontent = lcontent.replacen(&lcontent[0..5], "", 1);

                        if v {
                            println!("INFO: Operation '{}' found on line {}.", tokens[tokens.len()-1], l);
                        }
                    } else {
                        Err(format!("Unknown operation mnemonic on line {}. Did you mean 'test'?", l))
                    }
                } else {
                    Err(format!("Unknown operation mnemonic on line {}. Did you mean 'test'?", l))
                }
            } else if lcontent.to_lowercase().starts_with("ts") {
                // TSTF
                if lcontent.to_lowercase().starts_with("tst") {
                    if lcontent.to_lowercase().starts_with("tstf") {
                        tokens.push(Token::new(&lcontent[0..4], TokenInfo::Operation));
                        lcontent = lcontent.replacen(&lcontent[0..5], "", 1);

                        if v {
                            println!("INFO: Operation '{}' found on line {}.", tokens[tokens.len()-1], l);
                        }
                    } else {
                        Err(format!("Unknown operation mnemonic on line {}. Did you mean 'tstf'?", l))
                    }
                } else {
                    Err(format!("Unknown operation mnemonic on line {}. Did you mean 'tstf'?", l))
                }
            } else {
                Err(format!("Unknown operation mnemonic on line {}. Did you mean 'test' or 'tstf'?", l))
            }
        } else if lcontent.to_lowercase().starts_with("v") {
            // VCNT
            if lcontent.to_lowercase().starts_with("vc") {
                if lcontent.to_lowercase().starts_with("vcn") {
                    if lcontent.to_lowercase().starts_with("vcnt") {
                        tokens.push(Token::new(&lcontent[0..4], TokenInfo::Operation));
                        lcontent = lcontent.replacen(&lcontent[0..5], "", 1);

                        if v {
                            println!("INFO: Operation '{}' found on line {}.", tokens[tokens.len()-1], l);
                        }
                    } else {
                        Err(format!("Unknown operation mnemonic on line {}. Did you mean 'vcnt'?", l))
                    }
                } else {
                    Err(format!("Unknown operation mnemonic on line {}. Did you mean 'vcnt'?", l))
                }
            } else {
                Err(format!("Unknown operation mnemonic on line {}. Did you mean 'vcnt'?", l))
            }
        } else if lcontent.to_lowercase().starts_with("w") {
            // WAIT
            if lcontent.to_lowercase().starts_with("wa") {
                if lcontent.to_lowercase().starts_with("wai") {
                    if lcontent.to_lowercase().starts_with("wait") {
                        tokens.push(Token::new(&lcontent[0..4], TokenInfo::Operation));

                        if v {
                            println!("INFO: Operation '{}' found on line {}.", tokens[tokens.len()-1], l);
                        }
                    } else {
                        Err(format!("Unknown operation mnemonic on line {}. Did you mean 'wait'?", l))
                    }
                } else {
                    Err(format!("Unknown operation mnemonic on line {}. Did you mean 'wait'?", l))
                }
            } else {
                Err(format!("Unknown operation mnemonic on line {}. Did you mean 'wait'?", l))
            }
        } else if lcontent.to_lowercase().starts_with("x") {
            // XOR
            if lcontent.to_lowercase().starts_with("xo") {
                if lcontent.to_lowercase().starts_with("xor") {
                    tokens.push(Token::new(&lcontent[0..3], TokenInfo::Operation));
                    lcontent = lcontent.replacen(&lcontent[0..4], "", 1);

                    if v {
                        println!("INFO: Operation '{}' found on line {}.", tokens[tokens.len()-1], l);
                    }
                } else {
                    Err(format!("Unknown operation mnemonic on line {}. Did you mean 'xor'?", l))
                }
            } else {
                Err(format!("Unknown operation mnemonic on line {}. Did you mean 'xor'?", l))
            }
        } else {
            Err(format!("Unknown operation mnemonic on line {}.", l))
        }
    }

    Ok(tokens)
}

/// Grabs a path, verbose mode flag, and spaces-per-tab count, and lexes the file.
/// Really it just calls tokenize_line() in a loop until the end of the file is reached or an error occurs.
/// Then it does a final run through, doing stuff that couldn't be done in tokenize_line(), before sending off a vector of vectors of tokens to main().
pub fn lex(p: Path, &v: bool, spt: usize) -> Result<Vec<Vec<Token>>, Vec<String>> {
    let f = File::open(p)?;
    let mut lines = BufReader::new(f).lines().map(|l| l.unwrap());

    let mut linecount: usize = 1;
    let mut tokens: Vec<Vec<Token>> = vec!();
    let mut errors: Vec<String> = vec!();
    for line in lines {
        let tokline = tokenize_line(&line, v, spt, linecount);
        match tokline {
            Ok(t) => {
                tokens.push(t);

                if v {
                    println!("INFO: Tokenized line {}.", linecount);
                }
            },
            Err(e) => {
                if v {
                    eprintln!("ERR: {}", e);

                    /* I did it like this for two reasons:
                     * [1] We need at least one item in this vector to return with Result::Err.
                     * [2] main() knows whether it is verbose mode, so it'll know to just quit if it is.
                     */
                    if errors.is_empty() {
                        errors.push(e);
                    }

                } else {
                    errors.push(e);
                }
            }
        }

        linecount = linecount + 1;
    }

    if errors.len() >=1 {
        Err(errors)
    } else {
        Ok(tokens)
    }
}
