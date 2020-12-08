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
    pub fn new(c: String, i: TokenInfo) -> Self {
        Self {
            content: c,
            info: i
        }
    }
}

/// Borrows a string and checks if it begins with an immediate value. Just a helper function.
fn immediate(s: &str) -> Result<Token, String> {}

/// Borrows a string and checks if it begins with an address. Just a helper function.
fn address(s: &str) -> Result<Token, String> {}

/// Borrows a string and checks if it begins with a valid identifier. Just a helper function.
/// A valid identifier consists of only numbers, lowercase letters, underscores, and periods/dots.
fn identifier(s: &str) -> Result<Token, String> {}

/// Borrows a string and checks if it begins with a register. Just a helper function.
fn register(s: &str) -> Result<Token, String> {}

/// Takes a string, verbose mode flag, spaces-per-tab count, and line number, and tokenizes the string.
/// There's a lot of stuff going on in here.
fn tokenize_line(lc: &str, v: bool, spt: usize, l: usize) -> Result<Vec<Token>, String> {
    let mut lcontent = String::from(lc);
    let mut tokens: Vec<Token> = vec!();

    if lcontent.contains(":") {
        // We've got a label (definition)!
    } else if lcontent.starts_with("\t") {
        // We've got a tab!
    } else if lcontent.starts_with(" ".repeat(spt)) {
        // We've got a bunch of spaces masquerading as a tab!
    } else {
        // If it isn't any of the above, we don't care and should return early.
        Ok(tokens)
    }

    // The whitespace eradication loop. Gets rid of extra whitespace that we don't need.
    loop {
        if lcontent.starts_with("\t") {
            // We've got a tab!
        } else if lcontent.starts_with(" ") {
            // We've got a space!
        } else {
            // Escape the whitespace eradication loop!
            break;
        }
    }

    if lcontent.is_empty() || lcontent.starts_with(";") {
        // We don't care.
    } else if lcontent.starts_with(".") {
        // Assembler directives!
        if lcontent.starts_with(".org") {
            // Origin Directive
        } else if lcontent.starts_with(".def") {
            // Symbol Definition Directive
        } else if lcontent.starts_with(".byt") {
            // Byte Placement Directive
        } else if lcontent.starts_with(".wrd") {
            // Word Placement Directive
        } else if lcontent.starts_with(".vec") {
            // Vector Placement Directive
        } else if lcontent.starts_with(".str") {
            // Zero-Terminated ASCII String Placement Directive
        } else {
            // Error!
            Err(format!("Unknown assembler directive on line {}.", line))
        }
    } else {
        // Processor operations!
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
                    println!("Tokenized line {}.", linecount);
                }
            },
            Err(e) => {
                if v {
                    eprintln!("{}", e);

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
