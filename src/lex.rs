//--> Imports <--

use crate::StringList;
use logos::{Logos, Lexer};
use std::path::{Path, PathBuf};
use std::str::FromStr;
use std::fs::{File};
use std::io::{BufRead, BufReader, ErrorKind};
use std::fmt;

//--> Type Aliases <--

pub type TokenStream = Vec<Token>;

type TokenResult = Result<TokenStream, StringList>;

//--> Enums <--

#[derive(Logos, Clone)]
#[logos(subpattern bin = r"[01][_01]*")]
#[logos(subpattern dec = r"[0-9][_0-9]*")]
#[logos(subpattern hex = r"[0-9a-fA-F][_0-9a-fA-F]*")]
pub enum Token {
	#[regex(r"#(?&dec)", Const::dec)]
	#[regex(r"#%(?&bin)", Const::bin)]
	#[regex(r"#\$(?&hex)", Const::hex)]
	#[regex(r"'[\x00-\x7F]*'", Const::character)]
	#[regex(r#""(?:[^"]|\\")*""#, Const::string)]
	Constant(Const),

	#[regex(r"(?&dec)", Addr::dec)]
	#[regex(r"%(?&bin)", Addr::bin)]
	#[regex(r"\$(?&hex)", Addr::hex)]
	#[regex(r"(?&dec)d", Addr::dec)]
	#[regex(r"(?&dec)p", Addr::dec)]
	#[regex(r"%(?&bin)p", Addr::bin)]
	#[regex(r"\$(?&hex)p", Addr::hex)]
	#[regex(r"\((?&dec)\)", Addr::dec)]
	#[regex(r"\(%(?&bin)\)", Addr::bin)]
	#[regex(r"\(\$(?&hex)\)", Addr::hex)]
	#[regex(r"IP[+-](?&dec)", Addr::dec)]
	#[regex(r"IP[+-]%(?&bin)", Addr::bin)]
	#[regex(r"IP[+-]\$(?&hex)", Addr::hex)]
	#[regex(r"SP[+-](?&dec)", Addr::dec)]
	#[regex(r"SP[+-]%(?&bin)", Addr::bin)]
	#[regex(r"SP[+-]\$(?&hex)", Addr::hex)]
	Address(Addr),

	#[regex(r#"p"(?:[^"]|\\")*""#, Token::path)]
	Path(PathBuf),

	#[regex(r".?[a-zA-Z][_0-9a-zA-Z]*\$?", Wrd::new)]
	Word(Wrd),

	#[token(":")]
	Colon,

	Newline,
	
	#[regex(r"[ \t\n\r\f]+", logos::skip)]
	#[regex(r";[^\n]*", logos::skip)]
	#[error]
	Error
}

#[derive(Clone)]
pub enum Const {
	Byte(u8),
	String(Vec<u8>),
	Word(u16)
}

#[derive(Clone)]
pub enum Addr {
	// bool field says whether the address is indirect (true) or direct (false).
	Absolute(u32, bool),
	// bool field says whether the address is indirect (true) or direct (false).
	ZeroBank(u16, bool),
	Port(u16),
	DirectPage(u8),
	PointerRelative(i16, Ptr),
}

#[derive(Clone)]
pub enum Ptr {
	Instruction,
	Stack
}

#[derive(Clone)]
pub enum Wrd {
	NoOperation,
	MoveToRegister,
	MoveToMemory,
	MoveToRegisterX,
	MoveToRegisterY,
	MoveToMemoryX,
	MoveToMemoryY,
	SetRegisterToZero,
	SetMemoryToZero,
	SetMemoryToZeroX,
	SetMemoryToZeroY,
	HaltAndCatchFire,
	TransferRegisterContents,
	SwapRegisterContents,
	StoreStackPointerToX,
	GetStackPointerFromX,
	SetStackBank,
	SetDirectPage,
	PushToStack,
	PopFromStack,
	PushIntegerFlagsToStack,
	PopIntegerFlagsFromStack,
	And,
	Or,
	ExclusiveOr,
	AddIntegers,
	AddIntegersWithCarry,
	SubtractIntegers,
	SubtractIntegersWithCarry,
	Not,
	ShiftLeft,
	ShiftRight,
	RotateLeft,
	RotateRight,
	PopulationCount,
	VacancyCount,
	Swizzle,
	SetBits,
	ClearBits,
	EnableMaskableInterrupts,
	DisableMaskableInterrupts,
	SetDecimalFlag,
	ClearDecimalFlag,
	SetSignFlag,
	ClearSignFlag,
	SetNegativeFlag,
	ClearNegativeFlag,
	SetHalfCarryFlag,
	ClearHalfCarryFlag,
	SetCarryFlag,
	ClearCarryFlag,
	ClearOverflowFlag,
	SetZeroFlag,
	ClearZeroFlag,
	Jump,
	JumpX,
	JumpY,
	CallSubroutine,
	CompareIntegers,
	BranchIfEqual,
	BranchIfNotEqual,
	BranchIfLessThanUnsigned,
	BranchIfLessThanSigned,
	BranchIfGreaterThanUnsigned,
	BranchIfGreaterThanSigned,
	BranchIfLessThanOrEqualUnsigned,
	BranchIfLessThanOrEqualSigned,
	BranchIfGreaterThanOrEqualUnsigned,
	BranchIfGreaterThanOrEqualSigned,
	BranchIfCarrySet,
	BranchIfCarryClear,
	BranchIfHalfCarrySet,
	BranchIfHalfCarryClear,
	BranchIfOverflowSet,
	BranchIfOverflowClear,
	Increment,
	IncrementXBranchIfNotEqual,
	IncrementYBranchIfNotEqual,
	Decrement,
	DecrementXBranchIfNotZero,
	DecrementYBranchIfNotZero,
	TestBits,
	BranchIfStackFull,
	BranchIfStackEmpty,
	BranchDataParity,
	CallInterrupt,
	ReturnFromSubroutine,
	ReturnFromInterrupt,
	Break,
	WaitForInterrupt,
	IntegerRegisterA,
	IntegerRegisterALow,
	IntegerRegisterAHigh,
	IntegerRegisterB,
	IntegerRegisterBLow,
	IntegerRegisterBHigh,
	IntegerRegisterX,
	IntegerRegisterY,
	Origin,
	DefineSymbol,
	PlaceByte,
	PlaceWord,
	PlaceVector,
	PlaceString,
	PlaceNullTerminatedString,
	IncludeSource,
	IncludeBinary,
	Identifier(String)
}

//--> Functions <--

impl Token {
	pub fn lex(p: &Path) -> TokenResult {
		let mut toks = Vec::new();
		let mut errs = Vec::new();

		match File::open(p) {
			Ok(f) => {
				for (lno, line) in BufReader::new(f).lines().enumerate() {
					match line {
						Ok(l) => {
							for (tok, span) in Token::lexer(&l).spanned() {
								match tok {
									Token::Error => {
										let span2 = span.clone();
										// TOOD: Better errors.
										errs.push(format!("<ERR! {}:{}:{}..{}> '{}' couldn't be tokenized.", p.display(), lno, span.start, span.end, &l[span2]));
									}
									_ => toks.push(tok)
								}
							}
							toks.push(Token::Newline);
						},
						Err(e) => match e.kind() {
							ErrorKind::InvalidData => errs.push(format!("")),
							_ => errs.push(format!(""))
						}
					}
				}
			},
			Err(e) => match e.kind() {
				ErrorKind::NotFound => errs.push(format!("")),
				ErrorKind::PermissionDenied => errs.push(format!("")),
				_ => errs.push(format!(""))
			}
		}

		if errs.is_empty() {
			Ok(toks)
		} else {
			Err(errs)
		}
	}

	fn path(l: &mut Lexer<Token>) -> Option<PathBuf> {
		let s = l.slice().strip_prefix("p\"")?.strip_suffix("\"")?;
		Some(PathBuf::from_str(s).ok()?)
	}
}

impl fmt::Display for Token {
	fn fmt(&self, fmtr: &mut fmt::Formatter) -> fmt::Result {
		match self {
			Token::Constant(c) => c.fmt(fmtr),
			Token::Address(a) => a.fmt(fmtr),
			Token::Path(p) => write!(fmtr, "path '{}'", p.display()),
			Token::Word(w) => w.fmt(fmtr),
			Token::Colon => write!(fmtr, "colon"),
			Token::Newline => write!(fmtr, "newline"),
			Token::Error => write!(fmtr, "error")
		}
	}
}

impl Const {
	pub fn character(l: &mut Lexer<Token>) -> Option<Const> {
		let chars = l.slice().strip_prefix("'")?.strip_suffix("'")?.chars().collect::<Vec<char>>();

		if chars[0] == '\\' {
			// escape sequence
			if chars.len() >= 2 {
				match chars[1] {
					'"' => Some(Const::Byte(Const::char_to_byte('"')?)),
					'\\' => Some(Const::Byte(Const::char_to_byte('\\')?)),
					'0' => Some(Const::Byte(0)),
					'a' => Some(Const::Byte(7)),
					'b' => Some(Const::Byte(8)),
					'f' => Some(Const::Byte(12)),
					'n' => Some(Const::Byte(Const::char_to_byte('\n')?)),
					'r' => Some(Const::Byte(Const::char_to_byte('\r')?)),
					't' => Some(Const::Byte(Const::char_to_byte('\t')?)),
					'v' => Some(Const::Byte(11)),
					'e' => Some(Const::Byte(0x1B)),
					'x' => {
						// byte escape sequence
						if chars.len() == 4 {
							let mut valstr = String::new();
							valstr.push(chars[2]);
							valstr.push(chars[3]);

							if let Ok(b) = u8::from_str_radix(&valstr, 16) {
								Some(Const::Byte(b))
							} else { None }
						} else { None }
					},
					_ => None
				}
			} else { None }
		} else {
			if chars.len() == 1 {
				let b = Const::char_to_byte(chars[0])?;

				Some(Const::Byte(b))
			} else { None }
		}
	}

	pub fn string(l: &mut Lexer<Token>) -> Option<Const> {
		let mut chars = l.slice().strip_prefix('"')?.strip_suffix('"')?.chars().rev().collect::<Vec<char>>();
		let mut ret = Vec::new();

		loop {
			let c0 = chars.pop()?;

			if c0 == '\\' {
				// escape sequence
				match chars.pop()? {
					'"' => ret.push(Const::char_to_byte('"')?),
					'\\' => ret.push(Const::char_to_byte('\\')?),
					'0' => ret.push(0),
					'a' => ret.push(7),
					'b' => ret.push(8),
					'f' => ret.push(12),
					'n' => ret.push(Const::char_to_byte('\n')?),
					'r' => ret.push(Const::char_to_byte('\r')?),
					't' => ret.push(Const::char_to_byte('\t')?),
					'v' => ret.push(11),
					'x' => {
						// byte escape sequence
						if chars.len() == 4 {
							let mut valstr = String::new();
							valstr.push(chars[2]);
							valstr.push(chars[3]);

							if let Ok(b) = u8::from_str_radix(&valstr, 16) {
								ret.push(b);
							} else { return None }
						} else { return None }
					},
					_ => return None
				}
			} else {
				ret.push(Const::char_to_byte(c0)?);
			}

			if chars.is_empty() { break; }
		}

		Some(Const::String(ret))
	}

	fn char_to_byte(c: char) -> Option<u8> {
		if c.is_ascii() {
			let mut buf: [u8; 1] = [0x00];
			c.encode_utf8(&mut buf);
			Some(buf[0])
		} else { None }
	}

	// used in fmt::Display implementation.
	fn byte_to_ascii(b: &u8, string: bool) -> &'static str {
		match b {
			0x00 => "\\0",
			0x01 => "\\x01",
			0x02 => "\\x02",
			0x03 => "\\x03",
			0x04 => "\\x04",
			0x05 => "\\x05",
			0x06 => "\\x06",
			0x07 => "\\a",
			0x08 => "\\b",
			0x09 => "\\t",
			0x0A => "\\n",
			0x0B => "\\v",
			0x0C => "\\f",
			0x0D => "\\r",
			0x0E => "\\x0E",
			0x0F => "\\x0F",
			0x10 => "\\x10",
			0x11 => "\\x11",
			0x12 => "\\x12",
			0x13 => "\\x13",
			0x14 => "\\x14",
			0x15 => "\\x15",
			0x16 => "\\x16",
			0x17 => "\\x17",
			0x18 => "\\x18",
			0x19 => "\\x19",
			0x1A => "\\x1A",
			0x1B => "\\e",
			0x1C => "\\x1C",
			0x1D => "\\x1D",
			0x1E => "\\x1E",
			0x1F => "\\x1F",
			0x20 => " ",
			0x21 => "!",
			0x22 => if string { "\\\"" } else { "\"" },
			0x23 => "#",
			0x24 => "$",
			0x25 => "%",
			0x26 => "&",
			0x27 => if string { "'" } else { "\\'" },
			0x28 => "(",
			0x29 => ")",
			0x2A => "*",
			0x2B => "+",
			0x2C => ",",
			0x2D => "-",
			0x2E => ".",
			0x2F => "/",
			0x30 => "0",
			0x31 => "1",
			0x32 => "2",
			0x33 => "3",
			0x34 => "4",
			0x35 => "5",
			0x36 => "6",
			0x37 => "7",
			0x38 => "8",
			0x39 => "9",
			0x3A => ":",
			0x3B => ";",
			0x3C => "<",
			0x3D => "=",
			0x3E => ">",
			0x3F => "?",
			0x40 => "@",
			0x41 => "A",
			0x42 => "B",
			0x43 => "C",
			0x44 => "D",
			0x45 => "E",
			0x46 => "F",
			0x47 => "G",
			0x48 => "H",
			0x49 => "I",
			0x4A => "J",
			0x4B => "K",
			0x4C => "L",
			0x4D => "M",
			0x4E => "N",
			0x4F => "O",
			0x50 => "P",
			0x51 => "Q",
			0x52 => "R",
			0x53 => "S",
			0x54 => "T",
			0x55 => "U",
			0x56 => "V",
			0x57 => "W",
			0x58 => "X",
			0x59 => "Y",
			0x5A => "Z",
			0x5B => "[",
			0x5C => "\\\\",
			0x5D => "]",
			0x5E => "^",
			0x5F => "_",
			0x60 => "`",
			0x61 => "a",
			0x62 => "b",
			0x63 => "c",
			0x64 => "d",
			0x65 => "e",
			0x66 => "f",
			0x67 => "g",
			0x68 => "h",
			0x69 => "i",
			0x6A => "j",
			0x6B => "k",
			0x6C => "l",
			0x6D => "m",
			0x6E => "n",
			0x6F => "o",
			0x70 => "p",
			0x71 => "q",
			0x72 => "r",
			0x73 => "s",
			0x74 => "t",
			0x75 => "u",
			0x76 => "v",
			0x77 => "w",
			0x78 => "x",
			0x79 => "y",
			0x7A => "z",
			0x7B => "{",
			0x7C => "|",
			0x7D => "}",
			0x7E => "~",
			0x7F => "\\x7F",
			0x80 => "\\x80",
			0x81 => "\\x81",
			0x82 => "\\x82",
			0x83 => "\\x83",
			0x84 => "\\x84",
			0x85 => "\\x85",
			0x86 => "\\x86",
			0x87 => "\\x87",
			0x88 => "\\x88",
			0x89 => "\\x89",
			0x8A => "\\x8A",
			0x8B => "\\x8B",
			0x8C => "\\x8C",
			0x8D => "\\x8D",
			0x8E => "\\x8E",
			0x8F => "\\x8F",
			0x90 => "\\x90",
			0x91 => "\\x91",
			0x92 => "\\x92",
			0x93 => "\\x93",
			0x94 => "\\x94",
			0x95 => "\\x95",
			0x96 => "\\x96",
			0x97 => "\\x97",
			0x98 => "\\x98",
			0x99 => "\\x99",
			0x9A => "\\x9A",
			0x9B => "\\x9B",
			0x9C => "\\x9C",
			0x9D => "\\x9D",
			0x9E => "\\x9E",
			0x9F => "\\x9F",
			0xA0 => "\\xA0",
			0xA1 => "\\xA1",
			0xA2 => "\\xA2",
			0xA3 => "\\xA3",
			0xA4 => "\\xA4",
			0xA5 => "\\xA5",
			0xA6 => "\\xA6",
			0xA7 => "\\xA7",
			0xA8 => "\\xA8",
			0xA9 => "\\xA9",
			0xAA => "\\xAA",
			0xAB => "\\xAB",
			0xAC => "\\xAC",
			0xAD => "\\xAD",
			0xAE => "\\xAE",
			0xAF => "\\xAF",
			0xB0 => "\\xB0",
			0xB1 => "\\xB1",
			0xB2 => "\\xB2",
			0xB3 => "\\xB3",
			0xB4 => "\\xB4",
			0xB5 => "\\xB5",
			0xB6 => "\\xB6",
			0xB7 => "\\xB7",
			0xB8 => "\\xB8",
			0xB9 => "\\xB9",
			0xBA => "\\xBA",
			0xBB => "\\xBB",
			0xBC => "\\xBC",
			0xBD => "\\xBD",
			0xBE => "\\xBE",
			0xBF => "\\xBF",
			0xC0 => "\\xC0",
			0xC1 => "\\xC1",
			0xC2 => "\\xC2",
			0xC3 => "\\xC3",
			0xC4 => "\\xC4",
			0xC5 => "\\xC5",
			0xC6 => "\\xC6",
			0xC7 => "\\xC7",
			0xC8 => "\\xC8",
			0xC9 => "\\xC9",
			0xCA => "\\xCA",
			0xCB => "\\xCB",
			0xCC => "\\xCC",
			0xCD => "\\xCD",
			0xCE => "\\xCE",
			0xCF => "\\xCF",
			0xD0 => "\\xD0",
			0xD1 => "\\xD1",
			0xD2 => "\\xD2",
			0xD3 => "\\xD3",
			0xD4 => "\\xD4",
			0xD5 => "\\xD5",
			0xD6 => "\\xD6",
			0xD7 => "\\xD7",
			0xD8 => "\\xD8",
			0xD9 => "\\xD9",
			0xDA => "\\xDA",
			0xDB => "\\xDB",
			0xDC => "\\xDC",
			0xDD => "\\xDD",
			0xDE => "\\xDE",
			0xDF => "\\xDF",
			0xE0 => "\\xE0",
			0xE1 => "\\xE1",
			0xE2 => "\\xE2",
			0xE3 => "\\xE3",
			0xE4 => "\\xE4",
			0xE5 => "\\xE5",
			0xE6 => "\\xE6",
			0xE7 => "\\xE7",
			0xE8 => "\\xE8",
			0xE9 => "\\xE9",
			0xEA => "\\xEA",
			0xEB => "\\xEB",
			0xEC => "\\xEC",
			0xED => "\\xED",
			0xEE => "\\xEE",
			0xEF => "\\xEF",
			0xF0 => "\\xF0",
			0xF1 => "\\xF1",
			0xF2 => "\\xF2",
			0xF3 => "\\xF3",
			0xF4 => "\\xF4",
			0xF5 => "\\xF5",
			0xF6 => "\\xF6",
			0xF7 => "\\xF7",
			0xF8 => "\\xF8",
			0xF9 => "\\xF9",
			0xFA => "\\xFA",
			0xFB => "\\xFB",
			0xFC => "\\xFC",
			0xFD => "\\xFD",
			0xFE => "\\xFE",
			0xFF => "\\xFF"
		}
	}

	pub fn dec(l: &mut Lexer<Token>) -> Option<Const> {
		let s = l.slice().strip_prefix("#")?;

		if let Ok(b) = u8::from_str_radix(s, 10) {
			Some(Const::Byte(b))
		} else if let Ok(w) = u16::from_str_radix(s, 10) {
			Some(Const::Word(w))
		} else { None }
	}

	pub fn bin(l: &mut Lexer<Token>) -> Option<Const> {
		let s = l.slice().strip_prefix("#%")?;

		if let Ok(b) = u8::from_str_radix(s, 2) {
			Some(Const::Byte(b))
		} else if let Ok(w) = u16::from_str_radix(s, 2) {
			Some(Const::Word(w))
		} else { None }
	}

	pub fn hex(l: &mut Lexer<Token>) -> Option<Const> {
		let s = l.slice().strip_prefix("#$")?;

		if let Ok(b) = u8::from_str_radix(s, 16) {
			Some(Const::Byte(b))
		} else if let Ok(w) = u16::from_str_radix(s, 16) {
			Some(Const::Word(w))
		} else { None }
	}
}

impl fmt::Display for Const {
	fn fmt(&self, fmtr: &mut fmt::Formatter) -> fmt::Result {
		match self {
			Const::Byte(b) => write!(fmtr, "byte immediate {} (as char: '{}')", b, Const::byte_to_ascii(b, false)),
			Const::String(s) => {
				let mut string = String::new();

				for b in s {
					string.push_str(Const::byte_to_ascii(b, true))
				}

				write!(fmtr, "string literal \"{}\"", string)
			},
			Const::Word(w) => write!(fmtr, "word immediate {}", w)
		}
	}
}

impl Addr {
	pub fn dec(l: &mut Lexer<Token>) -> Option<Addr> {
		let mut s = l.slice();

		if s.starts_with("IP") || s.starts_with("SP") {
			// pointer relative, store pointer in variable
			let ptr = match &s[0..2] {
				"IP" => {
					s = s.strip_prefix("IP")?;
					Ptr::Instruction
				},
				"SP" => {
					s = s.strip_prefix("SP")?;
					Ptr::Stack
				},
				_ => return None
			};

			if let Ok(xpr) = i16::from_str_radix(s, 10) {
				Some(Addr::PointerRelative(xpr, ptr))
			} else { None }
		} else if s.starts_with("(") && s.ends_with(")") {
			// indirect
			s = s.strip_prefix("(")?.strip_suffix(")")?;

			if let Ok(zb) = u16::from_str_radix(s, 10) {
				Some(Addr::ZeroBank(zb, true))
			} else if let Ok(a) = u32::from_str_radix(s, 10) {
				if a <= 0xFFFFFF {
					Some(Addr::Absolute(a, true))
				} else { None }
			} else { None }
		} else if s.ends_with("p") {
			// port
			s = s.strip_suffix("p")?;

			if let Ok(p) = u16::from_str_radix(s, 10) {
				Some(Addr::Port(p))
			} else { None }
		} else if s.ends_with("d") {
			// direct page
			s = s.strip_suffix("d")?;

			if let Ok(dp) = u8::from_str_radix(s, 10) {
				Some(Addr::DirectPage(dp))
			} else { None }
		} else {
			// direct
			if let Ok(zb) = u16::from_str_radix(s, 10) {
				Some(Addr::ZeroBank(zb, false))
			} else if let Ok(a) = u32::from_str_radix(s, 10) {
				if a <= 0xFFFFFF {
					Some(Addr::Absolute(a, false))
				} else { None }
			} else { None }
		}
	}

	pub fn bin(l: &mut Lexer<Token>) -> Option<Addr> {
		let mut s = l.slice();

		if s.starts_with("IP") || s.starts_with("SP") {
			// pointer relative
			let ptr = match &s[0..2] {
				"IP" => {
					s = s.strip_prefix("IP")?;
					Ptr::Instruction
				},
				"SP" => {
					s = s.strip_prefix("SP")?;
					Ptr::Stack
				},
				_ => return None
			};

			let positive = if s.starts_with("+") {
				s = s.strip_prefix("+%")?;
				true
			} else if s.starts_with("-") {
				s = s.strip_prefix("-%")?;
				true
			} else { return None };

			if let Ok(xpr) = i16::from_str_radix(s, 2) {
				if positive {
					Some(Addr::PointerRelative(xpr, ptr))
				} else {
					Some(Addr::PointerRelative(-xpr, ptr))
				}
			} else { None }
		} else if s.starts_with("(") && s.ends_with(")") {
			// indirect
			s = s.strip_prefix("(")?.strip_suffix(")")?;

			if let Ok(zb) = u16::from_str_radix(s, 2) {
				Some(Addr::ZeroBank(zb, true))
			} else if let Ok(a) = u32::from_str_radix(s, 2) {
				if a <= 0xFFFFFF {
					Some(Addr::Absolute(a, true))
				} else { None }
			} else { None }
		} else if s.ends_with("p") {
			// port
			s = s.strip_suffix("p")?;

			if let Ok(p) = u16::from_str_radix(s, 2) {
				Some(Addr::Port(p))
			} else { None }
		} else {
			// direct
			s = s.strip_prefix("%")?;

			if let Ok(dp) = u8::from_str_radix(s, 2) {
				Some(Addr::DirectPage(dp))
			} else if let Ok(zb) = u16::from_str_radix(s, 2) {
				Some(Addr::ZeroBank(zb, false))
			} else if let Ok(a) = u32::from_str_radix(s, 2) {
				if a <= 0xFFFFFF {
					Some(Addr::Absolute(a, false))
				} else { None }
			} else { None }
		}
	}

	pub fn hex(l: &mut Lexer<Token>) -> Option<Addr> {
		let mut s = l.slice();

		if s.starts_with("IP") || s.starts_with("SP") {
			// pointer relative
			let ptr = match &s[0..2] {
				"IP" => {
					s = s.strip_prefix("IP")?;
					Ptr::Instruction
				},
				"SP" => {
					s = s.strip_prefix("SP")?;
					Ptr::Stack
				},
				_ => return None
			};

			let positive = if s.starts_with("+") {
				s = s.strip_prefix("+$")?;
				true
			} else if s.starts_with("-") {
				s = s.strip_prefix("-$")?;
				true
			} else { return None };

			if let Ok(xpr) = i16::from_str_radix(s, 16) {
				if positive {
					Some(Addr::PointerRelative(xpr, ptr))
				} else {
					Some(Addr::PointerRelative(-xpr, ptr))
				}
			} else { None }
		} else if s.starts_with("(") && s.ends_with(")") {
			// indirect
			s = s.strip_prefix("(")?.strip_suffix(")")?;

			if let Ok(zb) = u16::from_str_radix(s, 16) {
				Some(Addr::ZeroBank(zb, true))
			} else if let Ok(a) = u32::from_str_radix(s, 16) {
				if a <= 0xFFFFFF {
					Some(Addr::Absolute(a, true))
				} else { None }
			} else { None }
		} else if s.ends_with("p") {
			// port
			s = s.strip_suffix("p")?;

			if let Ok(p) = u16::from_str_radix(s, 16) {
				Some(Addr::Port(p))
			} else { None }
		} else {
			// direct
			s = s.strip_prefix("$")?;

			if let Ok(dp) = u8::from_str_radix(s, 16) {
				Some(Addr::DirectPage(dp))
			} else if let Ok(zb) = u16::from_str_radix(s, 16) {
				Some(Addr::ZeroBank(zb, false))
			} else if let Ok(a) = u32::from_str_radix(s, 16) {
				if a <= 0xFFFFFF {
					Some(Addr::Absolute(a, false))
				} else { None }
			} else { None }
		}
	}
}

impl fmt::Display for Addr {
	fn fmt(&self, fmtr: &mut fmt::Formatter) -> fmt::Result {
		match self {
			Addr::Absolute(a, i) => if *i { write!(fmtr, "absolute address {}", a) } else { write!(fmtr, "indirect absolute address ({})", a) },
			Addr::ZeroBank(zb, i) => if *i { write!(fmtr, "zero bank address {}", zb) } else { write!(fmtr, "indirect zero bank address ({})", zb) },
			Addr::Port(p) => write!(fmtr, "port address {}p", p),
			Addr::DirectPage(dp) => write!(fmtr, "direct page address {}", dp),
			Addr::PointerRelative(xpr, ptr) => match ptr {
				Ptr::Instruction => write!(fmtr, "instruction pointer relative address IP{}", xpr),
				Ptr::Stack => write!(fmtr, "stack pointer relative address SP{}", xpr)
			}
		}
	}
}

impl Wrd {
	pub fn new(l: &mut Lexer<Token>) -> Wrd {
		let s = l.slice();
		let s_lower = s.to_lowercase();

		match s_lower.as_str() {
			"nop" => Wrd::NoOperation,
			"mvr" => Wrd::MoveToRegister,
			"mvm" => Wrd::MoveToMemory,
			"mvrx" => Wrd::MoveToRegisterX,
			"mvry" => Wrd::MoveToRegisterY,
			"mvmx" => Wrd::MoveToMemoryX,
			"mvmy" => Wrd::MoveToMemoryY,
			"srz" => Wrd::SetRegisterToZero,
			"smz" => Wrd::SetMemoryToZero,
			"smzx" => Wrd::SetMemoryToZeroX,
			"smzy" => Wrd::SetMemoryToZeroY,
			"hacf" => Wrd::HaltAndCatchFire,
			"trc" => Wrd::TransferRegisterContents,
			"src" => Wrd::SwapRegisterContents,
			"sspx" => Wrd::StoreStackPointerToX,
			"gspx" => Wrd::GetStackPointerFromX,
			"ssb" => Wrd::SetStackBank,
			"sdp" => Wrd::SetDirectPage,
			"push" => Wrd::PushToStack,
			"pop" => Wrd::PopFromStack,
			"pshif" => Wrd::PushIntegerFlagsToStack,
			"popif" => Wrd::PopIntegerFlagsFromStack,
			"and" => Wrd::And,
			"or" => Wrd::Or,
			"eor" => Wrd::ExclusiveOr,
			"addi" => Wrd::AddIntegers,
			"adci" => Wrd::AddIntegersWithCarry,
			"subi" => Wrd::SubtractIntegers,
			"sbci" => Wrd::SubtractIntegersWithCarry,
			"not" => Wrd::Not,
			"shl" => Wrd::ShiftLeft,
			"shr" => Wrd::ShiftRight,
			"rol" => Wrd::RotateLeft,
			"ror" => Wrd::RotateRight,
			"pcnt" => Wrd::PopulationCount,
			"vcnt" => Wrd::VacancyCount,
			"swz" => Wrd::Swizzle,
			"set" => Wrd::SetBits,
			"clr" => Wrd::ClearBits,
			"eirq" => Wrd::EnableMaskableInterrupts,
			"dirq" => Wrd::DisableMaskableInterrupts,
			"sed" => Wrd::SetDecimalFlag,
			"cld" => Wrd::ClearDecimalFlag,
			"ses" => Wrd::SetSignFlag,
			"cls" => Wrd::ClearSignFlag,
			"sen" => Wrd::SetNegativeFlag,
			"cln" => Wrd::ClearNegativeFlag,
			"seh" => Wrd::SetHalfCarryFlag,
			"clh" => Wrd::ClearHalfCarryFlag,
			"sec" => Wrd::SetCarryFlag,
			"clc" => Wrd::ClearCarryFlag,
			"clo" => Wrd::ClearOverflowFlag,
			"sez" => Wrd::SetZeroFlag,
			"clz" => Wrd::ClearZeroFlag,
			"jmp" => Wrd::Jump,
			"jmpx" => Wrd::JumpX,
			"jmpy" => Wrd::JumpY,
			"call" => Wrd::CallSubroutine,
			"cmpi" => Wrd::CompareIntegers,
			"beq" => Wrd::BranchIfEqual,
			"bne" => Wrd::BranchIfNotEqual,
			"blu" => Wrd::BranchIfLessThanUnsigned,
			"bls" => Wrd::BranchIfLessThanSigned,
			"bgu" => Wrd::BranchIfGreaterThanUnsigned,
			"bgs" => Wrd::BranchIfGreaterThanSigned,
			"bleu" => Wrd::BranchIfLessThanOrEqualUnsigned,
			"bles" => Wrd::BranchIfLessThanOrEqualSigned,
			"bgeu" => Wrd::BranchIfGreaterThanOrEqualUnsigned,
			"bges" => Wrd::BranchIfGreaterThanOrEqualSigned,
			"bcs" => Wrd::BranchIfCarrySet,
			"bcc" => Wrd::BranchIfCarryClear,
			"bhs" => Wrd::BranchIfHalfCarrySet,
			"bhc" => Wrd::BranchIfHalfCarryClear,
			"bos" => Wrd::BranchIfOverflowSet,
			"boc" => Wrd::BranchIfOverflowClear,
			"inc" => Wrd::Increment,
			"ixbne" => Wrd::IncrementXBranchIfNotEqual,
			"iybne" => Wrd::IncrementYBranchIfNotEqual,
			"dec" => Wrd::Decrement,
			"dxbnz" => Wrd::DecrementXBranchIfNotZero,
			"dybnz" => Wrd::DecrementYBranchIfNotZero,
			"bit" => Wrd::TestBits,
			"bsf" => Wrd::BranchIfStackFull,
			"bse" => Wrd::BranchIfStackEmpty,
			"bdp" => Wrd::BranchDataParity,
			"cirq" => Wrd::CallInterrupt,
			"ret" => Wrd::ReturnFromSubroutine,
			"rti" => Wrd::ReturnFromInterrupt,
			"brk" => Wrd::Break,
			"wait" => Wrd::WaitForInterrupt,
			"ia" => Wrd::IntegerRegisterA,
			"ial" => Wrd::IntegerRegisterALow,
			"iah" => Wrd::IntegerRegisterAHigh,
			"ib" => Wrd::IntegerRegisterB,
			"ibl" => Wrd::IntegerRegisterBLow,
			"ibh" => Wrd::IntegerRegisterBHigh,
			"ix" => Wrd::IntegerRegisterX,
			"iy" => Wrd::IntegerRegisterY,
			".org" => Wrd::Origin,
			".def" => Wrd::DefineSymbol,
			".byte" => Wrd::PlaceByte,
			".word" => Wrd::PlaceWord,
			".vec" => Wrd::PlaceVector,
			".str" => Wrd::PlaceString,
			".strz" => Wrd::PlaceNullTerminatedString,
			".incsrc" => Wrd::IncludeSource,
			".incbin" => Wrd::IncludeBinary,
			_ => Wrd::Identifier(String::from(s))
		}
	}
}

impl fmt::Display for Wrd {
	fn fmt(&self, fmtr: &mut fmt::Formatter) -> fmt::Result {
		match self {
			Wrd::NoOperation => write!(fmtr, "NOP instruction"),
			Wrd::MoveToRegister => write!(fmtr, "MVR instruction"),
			Wrd::MoveToMemory => write!(fmtr, "MVM instruction"),
			Wrd::MoveToRegisterX => write!(fmtr, "MVRX instruction"),
			Wrd::MoveToRegisterY => write!(fmtr, "MVRY instruction"),
			Wrd::MoveToMemoryX => write!(fmtr, "MVMX instruction"),
			Wrd::MoveToMemoryY => write!(fmtr, "MVMY instruction"),
			Wrd::SetRegisterToZero => write!(fmtr, "SRZ instruction"),
			Wrd::SetMemoryToZero => write!(fmtr, "SMZ instruction"),
			Wrd::SetMemoryToZeroX => write!(fmtr, "SMZX instruction"),
			Wrd::SetMemoryToZeroY => write!(fmtr, "SMZY instruction"),
			Wrd::HaltAndCatchFire => write!(fmtr, "HACF instruction"),
			Wrd::TransferRegisterContents => write!(fmtr, "TRC instruction"),
			Wrd::SwapRegisterContents => write!(fmtr, "SRC instruction"),
			Wrd::StoreStackPointerToX => write!(fmtr, "SSPX instruction"),
			Wrd::GetStackPointerFromX => write!(fmtr, "GSPX instruction"),
			Wrd::SetStackBank => write!(fmtr, "SSB instruction"),
			Wrd::SetDirectPage => write!(fmtr, "SDP instruction"),
			Wrd::PushToStack => write!(fmtr, "PUSH instruction"),
			Wrd::PopFromStack => write!(fmtr, "POP instruction"),
			Wrd::PushIntegerFlagsToStack => write!(fmtr, "PSHIF instruction"),
			Wrd::PopIntegerFlagsFromStack => write!(fmtr, "POPIF instruction"),
			Wrd::And => write!(fmtr, "AND instruction"),
			Wrd::Or => write!(fmtr, "OR instruction"),
			Wrd::ExclusiveOr => write!(fmtr, "EOR instruction"),
			Wrd::AddIntegers => write!(fmtr, "ADDI instruction"),
			Wrd::AddIntegersWithCarry => write!(fmtr, "ADCI instruction"),
			Wrd::SubtractIntegers => write!(fmtr, "SUBI instruction"),
			Wrd::SubtractIntegersWithCarry => write!(fmtr, "SBCI instruction"),
			Wrd::Not => write!(fmtr, "NOT instruction"),
			Wrd::ShiftLeft => write!(fmtr, "SHL instruction"),
			Wrd::ShiftRight => write!(fmtr, "SHR instruction"),
			Wrd::RotateLeft => write!(fmtr, "ROL instruction"),
			Wrd::RotateRight => write!(fmtr, "ROR instruction"),
			Wrd::PopulationCount => write!(fmtr, "PCNT instruction"),
			Wrd::VacancyCount => write!(fmtr, "VCNT instruction"),
			Wrd::Swizzle => write!(fmtr, "SWZ instruction"),
			Wrd::SetBits => write!(fmtr, "SET instruction"),
			Wrd::ClearBits => write!(fmtr, "CLR instruction"),
			Wrd::EnableMaskableInterrupts => write!(fmtr, "EIRQ instruction"),
			Wrd::DisableMaskableInterrupts => write!(fmtr, "DIRQ instruction"),
			Wrd::SetDecimalFlag => write!(fmtr, "SED instruction"),
			Wrd::ClearDecimalFlag => write!(fmtr, "CLD instruction"),
			Wrd::SetSignFlag => write!(fmtr, "SES instruction"),
			Wrd::ClearSignFlag => write!(fmtr, "CLS instruction"),
			Wrd::SetNegativeFlag => write!(fmtr, "SEN instruction"),
			Wrd::ClearNegativeFlag => write!(fmtr, "CLN instruction"),
			Wrd::SetHalfCarryFlag => write!(fmtr, "SEH instruction"),
			Wrd::ClearHalfCarryFlag => write!(fmtr, "CLH instruction"),
			Wrd::SetCarryFlag => write!(fmtr, "SEC instruction"),
			Wrd::ClearCarryFlag => write!(fmtr, "CLC instruction"),
			Wrd::ClearOverflowFlag => write!(fmtr, "CLO instruction"),
			Wrd::SetZeroFlag => write!(fmtr, "SEZ instruction"),
			Wrd::ClearZeroFlag => write!(fmtr, "CLZ instruction"),
			Wrd::Jump => write!(fmtr, "JMP instruction"),
			Wrd::JumpX => write!(fmtr, "JMPX instruction"),
			Wrd::JumpY => write!(fmtr, "JMPY instruction"),
			Wrd::CallSubroutine => write!(fmtr, "CALL instruction"),
			Wrd::CompareIntegers => write!(fmtr, "CMPI instruction"),
			Wrd::BranchIfEqual => write!(fmtr, "BEQ instruction"),
			Wrd::BranchIfNotEqual => write!(fmtr, "BNE instruction"),
			Wrd::BranchIfLessThanUnsigned => write!(fmtr, "BLTU instruction"),
			Wrd::BranchIfLessThanSigned => write!(fmtr, "BLTS instruction"),
			Wrd::BranchIfGreaterThanUnsigned => write!(fmtr, "BGTU instruction"),
			Wrd::BranchIfGreaterThanSigned => write!(fmtr, "BGTS instruction"),
			Wrd::BranchIfLessThanOrEqualUnsigned => write!(fmtr, "BLEU instruction"),
			Wrd::BranchIfLessThanOrEqualSigned => write!(fmtr, "BLES instruction"),
			Wrd::BranchIfGreaterThanOrEqualUnsigned => write!(fmtr, "BGEU instruction"),
			Wrd::BranchIfGreaterThanOrEqualSigned => write!(fmtr, "BGES instruction"),
			Wrd::BranchIfCarrySet => write!(fmtr, "BCS instruction"),
			Wrd::BranchIfCarryClear => write!(fmtr, "BCC instruction"),
			Wrd::BranchIfHalfCarrySet => write!(fmtr, "BHS instruction"),
			Wrd::BranchIfHalfCarryClear => write!(fmtr, "BHC instruction"),
			Wrd::BranchIfOverflowSet => write!(fmtr, "BOS instruction"),
			Wrd::BranchIfOverflowClear => write!(fmtr, "BOC instruction"),
			Wrd::Increment => write!(fmtr, "INC instruction"),
			Wrd::IncrementXBranchIfNotEqual => write!(fmtr, "IXBNE instruction"),
			Wrd::IncrementYBranchIfNotEqual => write!(fmtr, "IYBNE instruction"),
			Wrd::Decrement => write!(fmtr, "DEC instruction"),
			Wrd::DecrementXBranchIfNotZero => write!(fmtr, "DXBNZ instruction"),
			Wrd::DecrementYBranchIfNotZero => write!(fmtr, "DYBNZ instruction"),
			Wrd::TestBits => write!(fmtr, "BIT instruction"),
			Wrd::BranchIfStackFull => write!(fmtr, "BSF instruction"),
			Wrd::BranchIfStackEmpty => write!(fmtr, "BSE instruction"),
			Wrd::BranchDataParity => write!(fmtr, "BDP instruction"),
			Wrd::CallInterrupt => write!(fmtr, "CIRQ instruction"),
			Wrd::ReturnFromSubroutine => write!(fmtr, "RET instruction"),
			Wrd::ReturnFromInterrupt => write!(fmtr, "RTI instruction"),
			Wrd::Break => write!(fmtr, "BRK instruction"),
			Wrd::WaitForInterrupt => write!(fmtr, "WAIT instruction"),
			Wrd::IntegerRegisterA => write!(fmtr, "register iA"),
			Wrd::IntegerRegisterALow => write!(fmtr, "register iAL"),
			Wrd::IntegerRegisterAHigh => write!(fmtr, "register iAH"),
			Wrd::IntegerRegisterB => write!(fmtr, "register iB"),
			Wrd::IntegerRegisterBLow => write!(fmtr, "register iBL"),
			Wrd::IntegerRegisterBHigh => write!(fmtr, "register iBH"),
			Wrd::IntegerRegisterX => write!(fmtr, "register iX"),
			Wrd::IntegerRegisterY => write!(fmtr, "register iY"),
			Wrd::Origin => write!(fmtr, "Origin directive"),
			Wrd::DefineSymbol => write!(fmtr, "Define Symbol directive"),
			Wrd::PlaceByte => write!(fmtr, "Place Byte directive"),
			Wrd::PlaceWord => write!(fmtr, "Place Word directive"),
			Wrd::PlaceVector => write!(fmtr, "Place Vector directive"),
			Wrd::PlaceString => write!(fmtr, "Place String directive"),
			Wrd::PlaceNullTerminatedString => write!(fmtr, "Place Null-Terminated Stringdirective"),
			Wrd::IncludeSource => write!(fmtr, "Include Source directive"),
			Wrd::IncludeBinary => write!(fmtr, "Include Binary directive"),
			Wrd::Identifier(id) => write!(fmtr, "identifier '{}'", id)
		}
	}
}