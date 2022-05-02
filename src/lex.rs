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

pub type TokenResult = Result<TokenStream, StringList>;

//--> Structs <--

pub struct Token {
	pub contents: Tok,
	pub slice: String
}

//--> Enums <--

#[derive(Logos)]
#[logos(subpattern bin = r"[01][_01]*")]
#[logos(subpattern dec = r"[0-9][_0-9]*")]
#[logos(subpattern hex = r"[0-9a-fA-F][_0-9a-fA-F]*")]
pub enum Tok {
	#[regex(r#""(\\"|[\x00-x7F])*""#, Tok::string)]
	String(Vec<u8>),

	#[regex(r#"p"(?:[^"]|\\")*""#, Tok::path)]
	Path(PathBuf),

	#[regex(r"'(\\'|[\x00-\x7F]+)'", Imm::character)]
	#[regex(r"#(?&dec)", Imm::dec)]
	#[regex(r"#%(?&bin)", Imm::bin)]
	#[regex(r"#\$(?&hex)", Imm::hex)]
	Immediate(Imm),

	#[regex(r"(?&dec)", Addr::dec)]
	#[regex(r"%(?&bin)", Addr::bin)]
	#[regex(r"\$(?&hex)", Addr::hex)]
	#[regex(r"(?&dec)d", Addr::dec)]
	#[regex(r"%(?&bin)d", Addr::bin)]
	#[regex(r"\$(?&hex)d", Addr::hex)]
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

	#[regex(r"[!#%^&*\-+|:<>/]", Op::new)]
	Operator(Op),

	#[regex(r".?[a-zA-Z][_0-9a-zA-Z]*\$?", Wrd::new)]
	Word(Wrd),

	#[error]
	#[regex(r"[ \t\n\r\f]+", logos::skip)]
	#[regex(r";[^\n]*", logos::skip)]
	Error,
}

pub enum Imm {
	Byte(u8),
	Word(u16),
}

pub enum Addr {
	Absolute(u32, bool),
	ZeroBank(u16, bool),
	DirectPage(u8),
	Port(u16),
	PointerRelative(i16, Ptr)
}

pub enum Ptr {
	Instruction,
	Stack
}

pub enum Op {
	Not,
	Middle,
	Modulo,
	ExclOr,
	And,
	Multiply,
	Subtract,
	Add,
	Or,
	Colon,
	Low,
	High,
	Divide,
	Newline
}

pub enum Wrd {
	Instruction(Inst),
	Directive(Dir),
	Register(Reg),
	Identifier(String)
}

pub enum Inst {
	NoOp,
	MoveToReg,
	MoveToMem,
	MoveToRegX,
	MoveToRegY,
	MoveToMemX,
	MoveToMemY,
	SetRegZero,
	SetMemZero,
	SetMemZeroX,
	SetMemZeroY,
	HaltCatchFire,
	TransferRegContents,
	SwapRegContents,
	StoreStackPtrInX,
	GetStackPtrFromX,
	SetStackBank,
	SetDirectPage,
	Push,
	Pop,
	PushIntFlags,
	PopIntFlags,
	And,
	Or,
	ExclOr,
	AddInts,
	AddIntsCarry,
	SubInts,
	SubIntsCarry,
	Not,
	ShiftLeft,
	ShiftRight,
	RotateLeft,
	RotateRight,
	PopCount,
	VacCount,
	Swizzle,
	SetBits,
	ClearBits,
	EnableIRQs,
	DisableIRQs,
	SetDecimal,
	ClearDecimal,
	SetSign,
	ClearSign,
	SetNegative,
	ClearNegative,
	SetHalfCarry,
	ClearHalfCarry,
	SetCarry,
	ClearCarry,
	SetZero,
	ClearZero,
	ClearOverflow,
	Jump,
	JumpX,
	JumpY,
	Call,
	CompareInts,
	BranchEqual,
	BranchNotEqual,
	BranchLessUnsigned,
	BranchLessSigned,
	BranchGreaterUnsigned,
	BranchGreaterSigned,
	BranchLessEqualUnsigned,
	BranchLessEqualSigned,
	BranchGreaterEqualUnsigned,
	BranchGreaterEqualSigned,
	BranchCarrySet,
	BranchCarryClear,
	BranchHalfCarrySet,
	BranchHalfCarryClear,
	BranchOverflowSet,
	BranchOverflowClear,
	BranchStackFull,
	BranchStackEmpty,
	Increment,
	IncrementXBranchNotEqual,
	IncrementYBranchNotEqual,
	Decrement,
	DecrementXBranchNotZero,
	DecrementYBranchNotZero,
	TestBits,
	BranchDataParity,
	CallIRQs,
	Return,
	ReturnInterrupt,
	Break,
	Wait
}

pub enum Dir {
	Origin,
	Define,
	Byte,
	Word,
	Vector,
	String,
	StringZero,
	IncludeSource,
	IncludeBinary
}

pub enum Reg {
	IntA,
	IntALow,
	IntAHigh,
	IntB,
	IntBLow,
	IntBHigh,
	IntX,
	IntY
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
							for (t, s) in Tok::lexer(&l).spanned() {
								match t {
									Tok::Error => errs.push(format!("<ERR! {}:{}> '{}' couldn't be lexed for some reason.", p.display(), lno, &l[s])),
									_ => toks.push(Token { contents: t, slice: String::from(&l[s]) })
								}
							}
							toks.push(Token { contents: Tok::Operator(Op::Newline), slice: String::new() });
						},
						Err(e) => match e.kind() {
							ErrorKind::InvalidData => errs.push(format!("<ERR! {}:{}> Invalid data. Make sure the file only contains UTF-8 encoded text.", p.display(), lno)),
							_ => errs.push(format!("<ERR! {}:{}> Unexpected I/O error '{}' encountered.", p.display(), lno, e))
						}
					}
				}
			},
			Err(e) => match e.kind() {
				ErrorKind::NotFound => errs.push(format!("<ERR! {}> Not found. Make sure the file exists.", p.display())),
				ErrorKind::PermissionDenied => errs.push(format!("<ERR! {}> Permission denied. Make sure you have read permission.", p.display())),
				_ => errs.push(format!("<ERR! {}> Unexpected I/O error '{}' encountered.", p.display(), e))
			}
		}

		if errs.is_empty() {
			Ok(toks)
		} else {
			Err(errs)
		}
	}
}

impl fmt::Display for Token {
	fn fmt(&self, fmtr: &mut fmt::Formatter) -> fmt::Result {
		match &self.contents {
			Tok::String(_) => write!(fmtr, "string {}", self.slice),
			Tok::Path(p) => write!(fmtr, "path {}", p.display()),
			Tok::Immediate(i) => match i {
				Imm::Byte(_) => write!(fmtr, "byte immediate {}", self.slice),
				Imm::Word(_) => write!(fmtr, "word immediate {}", self.slice)
			},
			Tok::Address(a) => match a {
				Addr::Absolute(_, i) => if *i { write!(fmtr, "indirect absolute address {}", self.slice) } else { write!(fmtr, "absolute address {}", self.slice) },
				Addr::ZeroBank(_, i) => if *i { write!(fmtr, "indirect zero bank address {}", self.slice) } else { write!(fmtr, "zero bank address {}", self.slice) },
				Addr::DirectPage(_) => write!(fmtr, "direct page address {}", self.slice),
				Addr::Port(_) => write!(fmtr, "port address {}", self.slice),
				Addr::PointerRelative(_, ptr) => match ptr {
					Ptr::Instruction => write!(fmtr, "instruction pointer relative address {}", self.slice),
					Ptr::Stack => write!(fmtr, "stack pointer relative address {}", self.slice)
				}
			},
			Tok::Operator(o) => match o {
				Op::Not => write!(fmtr, "bitwise not operator"),
				Op::Middle => write!(fmtr, "middle byte operator"),
				Op::Modulo => write!(fmtr, "modulo operator"),
				Op::ExclOr => write!(fmtr, "bitwise exclusive or operator"),
				Op::And => write!(fmtr, "bitwise and operator"),
				Op::Multiply => write!(fmtr, "multiplication operator"),
				Op::Subtract => write!(fmtr, "subtraction operator"),
				Op::Add => write!(fmtr, "addition operator"),
				Op::Or => write!(fmtr, "bitwise or operator"),
				Op::Colon => write!(fmtr, "colon"),
				Op::Low => write!(fmtr, "low byte operator (or half of the shift left operator)"),
				Op::High => write!(fmtr, "high byte operator (or half of the shift right operator)"),
				Op::Divide => write!(fmtr, "division operator"),
				Op::Newline => write!(fmtr, "newline")
			},
			Tok::Word(w) => match w {
				Wrd::Instruction(i) => match i {
					Inst::NoOp => write!(fmtr, "NOP instruction"),
					Inst::MoveToReg => write!(fmtr, "MVR instruction"),
					Inst::MoveToMem => write!(fmtr, "MVM instruction"),
					Inst::MoveToRegX => write!(fmtr, "MVRX instruction"),
					Inst::MoveToRegY => write!(fmtr, "MVRY instruction"),
					Inst::MoveToMemX => write!(fmtr, "MVMX instruction"),
					Inst::MoveToMemY => write!(fmtr, "MVMY instruction"),
					Inst::SetRegZero => write!(fmtr, "SRZ instruction"),
					Inst::SetMemZero => write!(fmtr, "SMZ instruction"),
					Inst::SetMemZeroX => write!(fmtr, "SMZX instruction"),
					Inst::SetMemZeroY => write!(fmtr, "SMZY instruction"),
					Inst::HaltCatchFire => write!(fmtr, "HACF instruction"),
					Inst::TransferRegContents => write!(fmtr, "TRC instruction"),
					Inst::SwapRegContents => write!(fmtr, "SRC instruction"),
					Inst::StoreStackPtrInX => write!(fmtr, "SSPX instruction"),
					Inst::GetStackPtrFromX => write!(fmtr, "GSPX instruction"),
					Inst::SetStackBank => write!(fmtr, "SSB instruction"),
					Inst::SetDirectPage => write!(fmtr, "SDP instruction"),
					Inst::Push => write!(fmtr, "PUSH instruction"),
					Inst::Pop => write!(fmtr, "POP instruction"),
					Inst::PushIntFlags => write!(fmtr, "PSHIF instruction"),
					Inst::PopIntFlags => write!(fmtr, "POPIF instruction"),
					Inst::And => write!(fmtr, "AND instruction"),
					Inst::Or => write!(fmtr, "OR instruction"),
					Inst::ExclOr => write!(fmtr, "EOR instruction"),
					Inst::AddInts => write!(fmtr, "ADDI instruction"),
					Inst::AddIntsCarry => write!(fmtr, "ADCI instruction"),
					Inst::SubInts => write!(fmtr, "SUBI instruction"),
					Inst::SubIntsCarry => write!(fmtr, "SBCI instruction"),
					Inst::Not => write!(fmtr, "NOT instruction"),
					Inst::ShiftLeft => write!(fmtr, "SHL instruction"),
					Inst::ShiftRight => write!(fmtr, "SHR instruction"),
					Inst::RotateLeft => write!(fmtr, "ROL instruction"),
					Inst::RotateRight => write!(fmtr, "ROR instruction"),
					Inst::PopCount => write!(fmtr, "PCNT instruction"),
					Inst::VacCount => write!(fmtr, "VCNT instruction"),
					Inst::Swizzle => write!(fmtr, "SWIZ instruction"),
					Inst::SetBits => write!(fmtr, "SET instruction"),
					Inst::ClearBits => write!(fmtr, "CLR instruction"),
					Inst::EnableIRQs => write!(fmtr, "EIRQ instruction"),
					Inst::DisableIRQs => write!(fmtr, "DIRQ instruction"),
					Inst::SetDecimal => write!(fmtr, "SED instruction"),
					Inst::ClearDecimal => write!(fmtr, "CLD instruction"),
					Inst::SetSign => write!(fmtr, "SES instruction"),
					Inst::ClearSign => write!(fmtr, "CLS instruction"),
					Inst::SetNegative => write!(fmtr, "SEN instruction"),
					Inst::ClearNegative => write!(fmtr, "CLN instruction"),
					Inst::SetHalfCarry => write!(fmtr, "SEH instruction"),
					Inst::ClearHalfCarry => write!(fmtr, "CLH instruction"),
					Inst::SetCarry => write!(fmtr, "SEC instruction"),
					Inst::ClearCarry => write!(fmtr, "CLC instruction"),
					Inst::SetZero => write!(fmtr, "SEZ instruction"),
					Inst::ClearZero => write!(fmtr, "CLZ instruction"),
					Inst::ClearOverflow => write!(fmtr, "CLO instruction"),
					Inst::Jump => write!(fmtr, "JMP instruction"),
					Inst::JumpX => write!(fmtr, "JMPX instruction"),
					Inst::JumpY => write!(fmtr, "JMPY instruction"),
					Inst::Call => write!(fmtr, "CALL instruction"),
					Inst::CompareInts => write!(fmtr, "CMPI instruction"),
					Inst::BranchEqual => write!(fmtr, "BEQ instruction"),
					Inst::BranchNotEqual => write!(fmtr, "BNE instruction"),
					Inst::BranchLessUnsigned => write!(fmtr, "BLU instruction"),
					Inst::BranchLessSigned => write!(fmtr, "BLS instruction"),
					Inst::BranchGreaterUnsigned => write!(fmtr, "BGU instruction"),
					Inst::BranchGreaterSigned => write!(fmtr, "BGS instruction"),
					Inst::BranchLessEqualUnsigned => write!(fmtr, "BLEU instruction"),
					Inst::BranchLessEqualSigned => write!(fmtr, "BLES instruction"),
					Inst::BranchGreaterEqualUnsigned => write!(fmtr, "BGEU instruction"),
					Inst::BranchGreaterEqualSigned => write!(fmtr, "BGES instruction"),
					Inst::BranchCarrySet => write!(fmtr, "BCS instruction"),
					Inst::BranchCarryClear => write!(fmtr, "BCC instruction"),
					Inst::BranchHalfCarrySet => write!(fmtr, "BHS instruction"),
					Inst::BranchHalfCarryClear => write!(fmtr, "BHC instruction"),
					Inst::BranchOverflowSet => write!(fmtr, "BOS instruction"),
					Inst::BranchOverflowClear => write!(fmtr, "BOC instruction"),
					Inst::BranchStackFull => write!(fmtr, "BSF instruction"),
					Inst::BranchStackEmpty => write!(fmtr, "BSE instruction"),
					Inst::Increment => write!(fmtr, "INC instruction"),
					Inst::IncrementXBranchNotEqual => write!(fmtr, "IXBNE instruction"),
					Inst::IncrementYBranchNotEqual => write!(fmtr, "IYBNE instruction"),
					Inst::Decrement => write!(fmtr, "DEC instruction"),
					Inst::DecrementXBranchNotZero => write!(fmtr, "DXBNZ instruction"),
					Inst::DecrementYBranchNotZero => write!(fmtr, "DYBNZ instruction"),
					Inst::TestBits => write!(fmtr, "BIT instruction"),
					Inst::BranchDataParity => write!(fmtr, "BDP instruction"),
					Inst::CallIRQs => write!(fmtr, "CIRQ instruction"),
					Inst::Return => write!(fmtr, "RET instruction"),
					Inst::ReturnInterrupt => write!(fmtr, "RTI instruction"),
					Inst::Break => write!(fmtr, "BRK instruction"),
					Inst::Wait => write!(fmtr, "WAIT instruction")
				},
				Wrd::Directive(d) => match d {
					Dir::Origin => write!(fmtr, "origin directive"),
					Dir::Define => write!(fmtr, "define symbol directive"),
					Dir::Byte => write!(fmtr, "place byte directive"),
					Dir::Word => write!(fmtr, "place word directive"),
					Dir::Vector => write!(fmtr, "place vector directive"),
					Dir::String => write!(fmtr, "place ASCII string directive"),
					Dir::StringZero => write!(fmtr, "place null-terminated ASCII string directive"),
					Dir::IncludeSource => write!(fmtr, "include source directive"),
					Dir::IncludeBinary => write!(fmtr, "include binary directive")
				},
				Wrd::Register(r) => match r {
					Reg::IntA => write!(fmtr, "integer register A"),
					Reg::IntALow => write!(fmtr, "integer register A Low"),
					Reg::IntAHigh => write!(fmtr, "integer register A High"),
					Reg::IntB => write!(fmtr, "integer register B"),
					Reg::IntBLow => write!(fmtr, "integer register B Low"),
					Reg::IntBHigh => write!(fmtr, "integer register B High"),
					Reg::IntX => write!(fmtr, "integer register X"),
					Reg::IntY => write!(fmtr, "integer register Y")
				},
				Wrd::Identifier(id) => write!(fmtr, "identifier {}", id)
			},
			Tok::Error => write!(fmtr, "error")
		}
	}
}

impl Tok {
	fn string(l: &mut Lexer<Tok>) -> Option<Vec<u8>> {
		// character stack
		let mut chars = l.slice().strip_prefix("\"")?.strip_suffix("\"")?.chars().rev().collect::<Vec<char>>();
		// return value
		let mut ret = Vec::new();

		// there is no reason to have empty strings
		if chars.is_empty() { return None }

		loop {
			let c0 = chars.pop()?;

			if c0 == '\\' {
				// escape sequence
				match chars.pop()? {
					'"' => ret.push(0x22), // \"	Double Quote Escape
					'\\' => ret.push(0x5C), // \\	Backslash Escape
					'0' => ret.push(0x00), // \0	Null Escape
					'a' => ret.push(0x07), // \a	Bell/'Alert' Escape
					'b' => ret.push(0x08), // \b	Backspace Escape
					'e' => ret.push(0x1B), // \e	Escape Escape (lol)
					'f' => ret.push(0x0C), // \f	Form Feed Escape
					'n' => ret.push(0x0A), // \n	Line Feed Escape (newline)
					'r' => ret.push(0x0D), // \r	Carriage Return Escape
					't' => ret.push(0x09), // \t	Horizontal Tab Escape
					'v' => ret.push(0x0B), // \v	Vertical Tab Escape
					'x' => {
						// byte escape sequence
						let mut vs = String::new();
						vs.push(chars.pop()?);
						vs.push(chars.pop()?);

						ret.push(u8::from_str_radix(&vs, 16).ok()?);
					},
					_ => return None // unrecognized escape sequence
				}
			} else {
				// not an escape sequence
				ret.push(char_to_byte(c0)?);
			}

			if chars.is_empty() { break; }
		}

		Some(ret)
	}

	fn path(l: &mut Lexer<Tok>) -> Option<PathBuf> {
		Some(PathBuf::from_str(l.slice()).ok()?)
	}
}

impl Imm {
	pub fn character(l: &mut Lexer<Tok>) -> Option<Imm> {
		let chars = l.slice().strip_prefix("'")?.strip_suffix("'")?.chars().collect::<Vec<char>>();

		if chars[0] == '\\' {
			// escape sequence
			if chars.len() >= 2 {
				match chars[1] {
					'"' => Some(Imm::Byte(0x22)), // \"	Double Quote Escape
					'\\' => Some(Imm::Byte(0x5C)), // \\	Backslash Escape
					'0' => Some(Imm::Byte(0x00)), // \0	Null Escape
					'a' => Some(Imm::Byte(0x07)), // \a	Bell/'Alert' Escape
					'b' => Some(Imm::Byte(0x08)), // \b	Backspace Escape
					'e' => Some(Imm::Byte(0x1B)), // \e	Escape Escape (lol)
					'f' => Some(Imm::Byte(0x0C)), // \f	Form Feed Escape
					'n' => Some(Imm::Byte(0x0A)), // \n	Line Feed Escape (newline)
					'r' => Some(Imm::Byte(0x0D)), // \r	Carriage Return Escape
					't' => Some(Imm::Byte(0x09)), // \t	Horizontal Tab Escape
					'v' => Some(Imm::Byte(0x0B)), // \v	Vertical Tab Escape
					'x' => {
						// byte escape sequence
						if chars.len() == 4 {
							let mut vs = String::new();
							vs.push(chars[2]);
							vs.push(chars[3]);

							Some(Imm::Byte(u8::from_str_radix(&vs, 16).ok()?))
						} else { None }
					},
					_ => None // unrecognized escape sequence
				}
			} else { None }
		} else {
			// not an escape sequence
			if chars.len() == 1 {
				Some(Imm::Byte(char_to_byte(chars[0])?))
			} else { None } 
		}
	}

	pub fn bin(l: &mut Lexer<Tok>) -> Option<Imm> {
		let s = l.slice().strip_prefix("#%")?;

		if let Ok(b) = u8::from_str_radix(s, 2) {
			Some(Imm::Byte(b))
		} else if let Ok(w) = u16::from_str_radix(s, 2) {
			Some(Imm::Word(w))
		} else { None }
	}

	pub fn dec(l: &mut Lexer<Tok>) -> Option<Imm> {
		let s = l.slice().strip_prefix("#")?;

		if let Ok(b) = u8::from_str(s) {
			Some(Imm::Byte(b))
		} else if let Ok(w) = u16::from_str(s) {
			Some(Imm::Word(w))
		} else { None }
	}

	pub fn hex(l: &mut Lexer<Tok>) -> Option<Imm> {
		let s = l.slice().strip_prefix("#$")?;

		if let Ok(b) = u8::from_str_radix(s, 16) {
			Some(Imm::Byte(b))
		} else if let Ok(w) = u16::from_str_radix(s, 16) {
			Some(Imm::Word(w))
		} else { None }
	}
}

impl Addr {
	pub fn bin(l: &mut Lexer<Tok>) -> Option<Addr> {
		let mut s = l.slice();

		if s.starts_with("IP") {
			// instruction pointer relative
			s = s.strip_prefix("IP")?;
			
			let negative = if s.starts_with("-") {
				s = s.strip_prefix("-")?;
				true
			} else {
				s = s.strip_prefix("+")?;
				false
			};

			s = s.strip_prefix("%")?;

			let mut val = i16::from_str_radix(s, 2).ok()?;

			if negative { val = -val; }

			Some(Addr::PointerRelative(val, Ptr::Instruction))
		} else if s.starts_with("SP") {
			// stack pointer relative
			s = s.strip_prefix("SP")?;

			let negative = if s.starts_with("-") {
				s = s.strip_prefix("-")?;
				true
			} else {
				s = s.strip_prefix("+")?;
				false
			};

			s = s.strip_prefix("%")?;

			let mut val = i16::from_str_radix(s, 2).ok()?;

			if negative { val = -val; }

			Some(Addr::PointerRelative(val, Ptr::Stack))
		} else {
			// not pointer relative
			s = s.strip_prefix("%")?;

			if s.ends_with("d") {
				// direct page
				s = s.strip_suffix("d")?;

				Some(Addr::DirectPage(u8::from_str_radix(s, 2).ok()?))
			} else if s.ends_with("p") {
				// port
				s = s.strip_suffix("p")?;

				Some(Addr::Port(u16::from_str_radix(s, 2).ok()?))
			} else if s.starts_with("(") && s.ends_with(")") {
				// indirect
				s = s.strip_prefix("(")?.strip_suffix(")")?;

				if let Ok(zb) = u16::from_str_radix(s, 2) {
					// zero bank
					Some(Addr::ZeroBank(zb, true))
				} else if let Ok(a) = u32::from_str_radix(s, 2) {
					// absolute, must fit in 24-bits but rust doesn't have a 24-bit integer type for obvious reasons
					if a <= 0xFFFFFF {
						Some(Addr::Absolute(a, true))
					} else { None }
				} else { None }
			} else {
				// direct

				if let Ok(zb) = u16::from_str_radix(s, 2) {
					// zero bank
					Some(Addr::ZeroBank(zb, false))
				} else if let Ok(a) = u32::from_str_radix(s, 2) {
					// absolute, must fit in 24-bits but rust doesn't have a 24-bit integer type for obvious reasons
					if a <= 0xFFFFFF {
						Some(Addr::Absolute(a, false))
					} else { None }
				} else { None }
			}
		}
	}

	pub fn dec(l: &mut Lexer<Tok>) -> Option<Addr> {
		let mut s = l.slice();

		if s.starts_with("IP") {
			// instruction pointer relative
			s = s.strip_prefix("IP")?;

			Some(Addr::PointerRelative(i16::from_str(s).ok()?, Ptr::Instruction))
		} else if s.starts_with("SP") {
			// stack pointer relative
			s = s.strip_prefix("SP")?;

			Some(Addr::PointerRelative(i16::from_str(s).ok()?, Ptr::Stack))
		} else {
			// not pointer relative

			if s.ends_with("d") {
				// direct page
				s = s.strip_suffix("d")?;

				Some(Addr::DirectPage(u8::from_str(s).ok()?))
			} else if s.ends_with("p") {
				// port
				s = s.strip_suffix("p")?;

				Some(Addr::Port(u16::from_str(s).ok()?))
			} else if s.starts_with("(") && s.ends_with(")") {
				// indirect
				s = s.strip_prefix("(")?.strip_suffix(")")?;

				if let Ok(zb) = u16::from_str(s) {
					// zero bank
					Some(Addr::ZeroBank(zb, true))
				} else if let Ok(a) = u32::from_str(s) {
					// absolute, must fit in 24-bits but rust doesn't have a 24-bit integer type for obvious reasons
					if a <= 0xFFFFFF {
						Some(Addr::Absolute(a, true))
					} else { None }
				} else { None }
			} else {
				// direct

				if let Ok(zb) = u16::from_str(s) {
					// zero bank
					Some(Addr::ZeroBank(zb, false))
				} else if let Ok(a) = u32::from_str(s) {
					// absolute, must fit in 24-bits but rust doesn't have a 24-bit integer type for obvious reasons
					if a <= 0xFFFFFF {
						Some(Addr::Absolute(a, false))
					} else { None }
				} else { None }
			}
		}
	}

	pub fn hex(l: &mut Lexer<Tok>) -> Option<Addr> {
		let mut s = l.slice();

		if s.starts_with("IP") {
			// instruction pointer relative
			s = s.strip_prefix("IP")?;
			
			let negative = if s.starts_with("-") {
				s = s.strip_prefix("-")?;
				true
			} else {
				s = s.strip_prefix("+")?;
				false
			};

			s = s.strip_prefix("$")?;

			let mut val = i16::from_str_radix(s, 16).ok()?;

			if negative { val = -val; }

			Some(Addr::PointerRelative(val, Ptr::Instruction))
		} else if s.starts_with("SP") {
			// stack pointer relative
			s = s.strip_prefix("SP")?;

			let negative = if s.starts_with("-") {
				s = s.strip_prefix("-")?;
				true
			} else {
				s = s.strip_prefix("+")?;
				false
			};

			s = s.strip_prefix("$")?;

			let mut val = i16::from_str_radix(s, 16).ok()?;

			if negative { val = -val; }

			Some(Addr::PointerRelative(val, Ptr::Stack))
		} else {
			// not pointer relative
			s = s.strip_prefix("$")?;

			if s.ends_with("d") {
				// direct page
				s = s.strip_suffix("d")?;

				Some(Addr::DirectPage(u8::from_str_radix(s, 16).ok()?))
			} else if s.ends_with("p") {
				// port
				s = s.strip_suffix("p")?;

				Some(Addr::Port(u16::from_str_radix(s, 16).ok()?))
			} else if s.starts_with("(") && s.ends_with(")") {
				// indirect
				s = s.strip_prefix("(")?.strip_suffix(")")?;

				if let Ok(zb) = u16::from_str_radix(s, 16) {
					// zero bank
					Some(Addr::ZeroBank(zb, true))
				} else if let Ok(a) = u32::from_str_radix(s, 16) {
					// absolute, must fit in 24-bits but rust doesn't have a 24-bit integer type for obvious reasons
					if a <= 0xFFFFFF {
						Some(Addr::Absolute(a, true))
					} else { None }
				} else { None }
			} else {
				// direct

				if let Ok(zb) = u16::from_str_radix(s, 16) {
					// zero bank
					Some(Addr::ZeroBank(zb, false))
				} else if let Ok(a) = u32::from_str_radix(s, 16) {
					// absolute, must fit in 24-bits but rust doesn't have a 24-bit integer type for obvious reasons
					if a <= 0xFFFFFF {
						Some(Addr::Absolute(a, false))
					} else { None }
				} else { None }
			}
		}
	}
}

impl Op {
	pub fn new(l: &mut Lexer<Tok>) -> Option<Op> {
		match l.slice() {
			"!" => Some(Op::Not),
			"#" => Some(Op::Middle),
			"%" => Some(Op::Modulo),
			"^" => Some(Op::ExclOr),
			"&" => Some(Op::And),
			"*" => Some(Op::Multiply),
			"-" => Some(Op::Subtract),
			"+" => Some(Op::Add),
			"|" => Some(Op::Or),
			":" => Some(Op::Colon),
			"<" => Some(Op::Low), // also shift left when doubled (i.e. 'symbol << 4' shifts the contents of the symbol left by 4 bits)
			">" => Some(Op::High), // also shift right when doubled (i.e. 'symbol >> 4' shifts the contents of the symbol right by 4 bits)
			"/" => Some(Op::Divide),
			_ => None
		}
	}
}

impl Wrd {
	pub fn new(l: &mut Lexer<Tok>) -> Wrd {
		let s = l.slice();
		let sl = s.to_lowercase();

		match sl.as_str() {
			// instructions
			"nop" => Wrd::Instruction(Inst::NoOp),
			"mvr" => Wrd::Instruction(Inst::MoveToReg),
			"mvm" => Wrd::Instruction(Inst::MoveToMem),
			"mvrx" => Wrd::Instruction(Inst::MoveToRegX),
			"mvry" => Wrd::Instruction(Inst::MoveToRegY),
			"mvmx" => Wrd::Instruction(Inst::MoveToMemX),
			"mvmy" => Wrd::Instruction(Inst::MoveToMemY),
			"srz" => Wrd::Instruction(Inst::SetRegZero),
			"smz" => Wrd::Instruction(Inst::SetMemZero),
			"smzx" => Wrd::Instruction(Inst::SetMemZeroX),
			"smzy" => Wrd::Instruction(Inst::SetMemZeroY),
			"hacf" => Wrd::Instruction(Inst::HaltCatchFire),
			"trc" => Wrd::Instruction(Inst::TransferRegContents),
			"src" => Wrd::Instruction(Inst::SwapRegContents),
			"sspx" => Wrd::Instruction(Inst::StoreStackPtrInX),
			"gspx" => Wrd::Instruction(Inst::GetStackPtrFromX),
			"ssb" => Wrd::Instruction(Inst::SetStackBank),
			"sdp" => Wrd::Instruction(Inst::SetDirectPage),
			"push" => Wrd::Instruction(Inst::Push),
			"pop" => Wrd::Instruction(Inst::Pop),
			"pshif" => Wrd::Instruction(Inst::PushIntFlags),
			"popif" => Wrd::Instruction(Inst::PopIntFlags),
			"and" => Wrd::Instruction(Inst::And),
			"or" => Wrd::Instruction(Inst::Or),
			"eor" => Wrd::Instruction(Inst::ExclOr),
			"addi" => Wrd::Instruction(Inst::AddInts),
			"adci" => Wrd::Instruction(Inst::AddIntsCarry),
			"subi" => Wrd::Instruction(Inst::SubInts),
			"sbci" => Wrd::Instruction(Inst::SubIntsCarry),
			"not" => Wrd::Instruction(Inst::Not),
			"shl" => Wrd::Instruction(Inst::ShiftLeft),
			"shr" => Wrd::Instruction(Inst::ShiftRight),
			"rol" => Wrd::Instruction(Inst::RotateLeft),
			"ror" => Wrd::Instruction(Inst::RotateRight),
			"pcnt" => Wrd::Instruction(Inst::PopCount),
			"vcnt" => Wrd::Instruction(Inst::VacCount),
			"swiz" => Wrd::Instruction(Inst::Swizzle),
			"set" => Wrd::Instruction(Inst::SetBits),
			"clr" => Wrd::Instruction(Inst::ClearBits),
			"eirq" => Wrd::Instruction(Inst::EnableIRQs),
			"dirq" => Wrd::Instruction(Inst::DisableIRQs),
			"sed" => Wrd::Instruction(Inst::SetDecimal),
			"cld" => Wrd::Instruction(Inst::ClearDecimal),
			"ses" => Wrd::Instruction(Inst::SetSign),
			"cls" => Wrd::Instruction(Inst::ClearSign),
			"sen" => Wrd::Instruction(Inst::SetNegative),
			"cln" => Wrd::Instruction(Inst::ClearNegative),
			"seh" => Wrd::Instruction(Inst::SetHalfCarry),
			"clh" => Wrd::Instruction(Inst::ClearHalfCarry),
			"sec" => Wrd::Instruction(Inst::SetCarry),
			"clc" => Wrd::Instruction(Inst::ClearCarry),
			"sez" => Wrd::Instruction(Inst::SetZero),
			"clz" => Wrd::Instruction(Inst::ClearZero),
			"clo" => Wrd::Instruction(Inst::ClearOverflow),
			"jmp" => Wrd::Instruction(Inst::Jump),
			"jmpx" => Wrd::Instruction(Inst::JumpX),
			"jmpy" => Wrd::Instruction(Inst::JumpY),
			"call" => Wrd::Instruction(Inst::Call),
			"cmpi" => Wrd::Instruction(Inst::CompareInts),
			"beq" => Wrd::Instruction(Inst::BranchEqual),
			"bne" => Wrd::Instruction(Inst::BranchNotEqual),
			"blu" => Wrd::Instruction(Inst::BranchLessUnsigned),
			"bls" => Wrd::Instruction(Inst::BranchLessSigned),
			"bgu" => Wrd::Instruction(Inst::BranchGreaterUnsigned),
			"bgs" => Wrd::Instruction(Inst::BranchGreaterSigned),
			"bleu" => Wrd::Instruction(Inst::BranchLessEqualUnsigned),
			"bles" => Wrd::Instruction(Inst::BranchLessEqualSigned),
			"bgeu" => Wrd::Instruction(Inst::BranchGreaterEqualUnsigned),
			"bges" => Wrd::Instruction(Inst::BranchGreaterEqualSigned),
			"bcs" => Wrd::Instruction(Inst::BranchCarrySet),
			"bcc" => Wrd::Instruction(Inst::BranchCarryClear),
			"bhs" => Wrd::Instruction(Inst::BranchHalfCarrySet),
			"bhc" => Wrd::Instruction(Inst::BranchHalfCarryClear),
			"bos" => Wrd::Instruction(Inst::BranchOverflowSet),
			"boc" => Wrd::Instruction(Inst::BranchOverflowClear),
			"bsf" => Wrd::Instruction(Inst::BranchStackFull),
			"bse" => Wrd::Instruction(Inst::BranchStackEmpty),
			"inc" => Wrd::Instruction(Inst::Increment),
			"ixbne" => Wrd::Instruction(Inst::IncrementXBranchNotEqual),
			"iybne" => Wrd::Instruction(Inst::IncrementYBranchNotEqual),
			"dec" => Wrd::Instruction(Inst::Decrement),
			"dxbnz" => Wrd::Instruction(Inst::DecrementXBranchNotZero),
			"dybnz" => Wrd::Instruction(Inst::DecrementYBranchNotZero),
			"bit" => Wrd::Instruction(Inst::TestBits),
			"bdp" => Wrd::Instruction(Inst::BranchDataParity),
			"cirq" => Wrd::Instruction(Inst::CallIRQs),
			"ret" => Wrd::Instruction(Inst::Return),
			"rti" => Wrd::Instruction(Inst::ReturnInterrupt),
			"brk" => Wrd::Instruction(Inst::Break),
			"wait" => Wrd::Instruction(Inst::Wait),
			// directives
			".org" => Wrd::Directive(Dir::Origin),
			".origin" => Wrd::Directive(Dir::Origin),
			".def" => Wrd::Directive(Dir::Define),
			".define" => Wrd::Directive(Dir::Define),
			".byte" => Wrd::Directive(Dir::Byte),
			".word" => Wrd::Directive(Dir::Word),
			".vec" => Wrd::Directive(Dir::Vector),
			".str" => Wrd::Directive(Dir::String),
			".strz" => Wrd::Directive(Dir::StringZero),
			".incsrc" => Wrd::Directive(Dir::IncludeSource),
			".incbin" => Wrd::Directive(Dir::IncludeBinary),
			// registers
			"ial" => Wrd::Register(Reg::IntALow),
			"iah" => Wrd::Register(Reg::IntAHigh),
			"ia" => Wrd::Register(Reg::IntA),
			"ibl" => Wrd::Register(Reg::IntBLow),
			"ibh" => Wrd::Register(Reg::IntBHigh),
			"ib" => Wrd::Register(Reg::IntB),
			"ix" => Wrd::Register(Reg::IntX),
			"iy" => Wrd::Register(Reg::IntY),
			// identifier
			_ => Wrd::Identifier(String::from(s))
		}
	} 
}

fn char_to_byte(c: char) -> Option<u8> {
	if c.is_ascii() {
		let mut buf: [u8; 1] = [0; 1];
		c.encode_utf8(&mut buf);
		Some(buf[0])
	} else { None }
}