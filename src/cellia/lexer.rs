//--> Imports <--

use std::{
	fmt,
	fs::File,
	io::{
		BufRead,
		BufReader,
		ErrorKind,
	},
	ops::Range,
	path::PathBuf,
};

use crate::{
	Error,
	ErrorList,
	text
};

use logos::{
	Lexer,
	Logos
};

//--> Type Aliases <--

pub type TokenStream = Vec<Token>;

pub(crate) type Result = std::result::Result<TokenStream, ErrorList>;

//--> Structs <--

#[derive(Clone)]
pub struct Token {
	pub inner: TokenInner,
	pub line: usize,
	pub span: Range<usize>,
	pub source: String
}

//--> Enums <--

#[derive(Clone, Logos)]
pub enum TokenInner {
	#[regex(r"[0-9][_0-9]", Imm::decimal)]
	#[regex(r"%[01][_01]", Imm::binary)]
	#[regex(r"0(b|B)[01][_01]", Imm::binary)]
	#[regex(r"[01][_01](b|B)", Imm::binary)]
	#[regex(r"\$[0-9a-fA-F][_0-9a-fA-F]", Imm::hexadecimal)]
	#[regex(r"0(x|X)[0-9a-fA-F][_0-9a-fA-F]", Imm::hexadecimal)]
	#[regex(r"[0-9a-fA-F][_0-9a-fA-F](h|H)", Imm::hexadecimal)]
	#[regex(r"'([\x00-\x7F]+|\\')'", Imm::character)]
	Immediate(Imm),

	#[regex(r#""([\x00-\x7F]|\\")+""#, TokenInner::string)]
	#[regex(r##"r#"([\x00-\x7F]|\\")+"#r"##, TokenInner::raw_string)]
	String(Vec<u8>),

	#[regex(r"@[\u{0}-\u{10FFFE}\u{10FFFF}]*", TokenInner::path)]
	Path(PathBuf),

	#[regex(r"\([0-9][_0-9]\)", Addr::decimal)]
	#[regex(r"\([0-9][_0-9][pP]\)", Addr::decimal)]
	#[regex(r"\([pP][cC][+-][0-9][_0-9]\)", Addr::decimal)]
	#[regex(r"\(%[01][_01]\)", Addr::binary)]
	#[regex(r"\(%[01][_01][pP]\)", Addr::binary)]
	#[regex(r"\([pP][cC][+-]%[01][_01]\)", Addr::binary)]
	#[regex(r"\(0[bB][01][_01]\)", Addr::binary)]
	#[regex(r"\(0[bB][01][_01][pP]\)", Addr::binary)]
	#[regex(r"\([pP][cC][+-]0[bB][01][_01]\)", Addr::binary)]
	#[regex(r"\([01][_01][bB]\)", Addr::binary)]
	#[regex(r"\([01][_01][bB][pP]\)", Addr::binary)]
	#[regex(r"\([pP][cC][+-][01][_01][bB]\)", Addr::binary)]
	#[regex(r"\(\$[0-9a-fA-F][_0-9a-fA-F]\)", Addr::hexadecimal)]
	#[regex(r"\(\$[0-9a-fA-F][_0-9a-fA-F][pP]\)", Addr::hexadecimal)]
	#[regex(r"\([pP][cC][+-]\$[0-9a-fA-F][_0-9a-fA-F]\)", Addr::hexadecimal)]
	#[regex(r"\(0[xX][0-9a-fA-F][_0-9a-fA-F]\)", Addr::hexadecimal)]
	#[regex(r"\(0[xX][0-9a-fA-F][_0-9a-fA-F][pP]\)", Addr::hexadecimal)]
	#[regex(r"\([pP][cC][+-]0[xX][0-9a-fA-F][_0-9a-fA-F]\)", Addr::hexadecimal)]
	#[regex(r"\([0-9a-fA-F][_0-9a-fA-F][hH]\)", Addr::hexadecimal)]
	#[regex(r"\([0-9a-fA-F][_0-9a-fA-F][hH][pP]\)", Addr::hexadecimal)]
	#[regex(r"\([pP][cC][+-][0-9a-fA-F][_0-9a-fA-F][hH]\)", Addr::hexadecimal)]
	#[regex(r"\([rR][pP]?[0-7]\)", Addr::indirect)]
	#[regex(r"\([pP][cC]+[rR][0-7]\)", Addr::indirect)]
	Address(Addr),

	#[regex(r"[~\^&*-=\+|:<>/]+", Op::new)]
	Operator(Op),

	#[regex(r".?[_a-zA-Z][_0-9a-zA-Z]*\$?", Word::new)]
	Keyword(Word),

	Newline,

	#[error]
	#[regex(r"[ \t\r\n\f]+", logos::skip)]
	#[regex(r";[^\n]*", logos::skip)]
	Error
}

#[derive(Clone)]
pub enum Imm {
	Byte(u8),
	Word(u16),
}

#[derive(Clone)]
pub enum Addr {
	Direct(u32),
	Indirect(RegPair),
	DirectPort(u16),
	IndirectPort(FullReg),
	DirectRelative(i16),
	IndirectRelative(FullReg),
}

#[derive(Clone)]
pub enum Op {
	Tilde,
	Caret,
	Ampersand,
	Star,
	Minus,
	Equals,
	Plus,
	Pipe,
	Colon,
	Less,
	LessOrEqual,
	Greater,
	GreaterOrEqual,
	ShiftLeft,
	ShiftRight,
	NotEquals,
	Slash
}

#[derive(Clone)]
pub enum Word {
	Register(Reg),
	Directive(Dir),
	Instruction(Inst),
	Identifier(String),
}

#[derive(Clone)]
pub enum Reg {
	Full(FullReg),
	Short(ShortReg),
}

#[derive(Clone)]
pub enum RegPair {
	RP0,
	RP1,
	RP2,
	RP3,
	RP4,
	RP5,
	RP6,
	RP7
}

#[derive(Clone)]
pub enum FullReg {
	R0,
	R1,
	R2,
	R3,
	R4,
	R5,
	R6,
	R7,
}

#[derive(Clone)]
pub enum ShortReg {
	SR0,
	SR1,
	SR2,
	SR3,
	SR4,
	SR5,
	SR6,
	SR7
}

#[derive(Clone)]
pub enum Dir {
	SetOrigin,
	DefineSymbol,
	DefineExternalSymbol,
	PutByte,
	PutWord,
	PutAddress,
	PutASCIIString,
	PutNullTerminatedASCIIString,
	IncludeSourceFile,
	IncludeBinaryFile,
	AssembleIf,
	AssembleIfNot,
	AssembleIfDefined,
	AssembleIfNotDefined,
	EndIf,
	SelectLowByte,
	SelectLowWord,
	SelectHighByte,
	SelectBankByte,
}

#[derive(Clone)]
pub enum Inst {
	NoOperation,
	ClearZeroFlag,
	SetZeroFlag,
	ClearCarryFlag,
	SetCarryFlag,
	ClearAdjustFlag,
	SetAdjustFlag,
	ClearOverflowFlag,
	SetOverflowFlag,
	ClearNegativeFlag,
	SetNegativeFlag,
	EnableInterrupts,
	DisableInterrupts,
	Load,
	Pull,
	Push,
	PullFlags,
	PushFlags,
	And,
	BitTest,
	Or,
	ExclusiveOr,
	Not,
	ShiftLeft,
	RotateLeft,
	ShiftRight,
	RotateRight,
	Add,
	AddWithCarry,
	Subtract,
	SubtractWithCarry,
	Compare,
	CompareWithCarry,
	Decrement,
	Increment,
	DecimalAddAdjust,
	DecimalSubtractAdjust,
	Jump,
	JumpIfZero,
	JumpIfNotZero,
	JumpIfCarry,
	JumpIfBelow,
	JumpIfNotCarry,
	JumpIfAboveOrEqual,
	JumpIfOverflow,
	JumpIfNotOverflow,
	JumpIfPositive,
	JumpIfNegative,
	JumpIfBelowOrEqual,
	JumpIfAbove,
	JumpIfLess,
	JumpIfLessOrEqual,
	JumpIfGreaterOrEqual,
	JumpIfGreater,
	CallSubroutine,
	CallSubroutineIfZero,
	CallSubroutineIfNotZero,
	CallSubroutineIfCarry,
	CallSubroutineIfBelow,
	CallSubroutineIfNotCarry,
	CallSubroutineIfAboveOrEqual,
	CallSubroutineIfOverflow,
	CallSubroutineIfNotOverflow,
	CallSubroutineIfPositive,
	CallSubroutineIfNegative,
	CallSubroutineIfBelowOrEqual,
	CallSubroutineIfAbove,
	CallSubroutineIfLess,
	CallSubroutineIfLessOrEqual,
	CallSubroutineIfGreaterOrEqual,
	CallSubroutineIfGreater,
	Return,
	ReturnIfZero,
	ReturnIfNotZero,
	ReturnIfCarry,
	ReturnIfBelow,
	ReturnIfNotCarry,
	ReturnIfAboveOrEqual,
	ReturnIfOverflow,
	ReturnIfNotOverflow,
	ReturnIfPositive,
	ReturnIfNegative,
	ReturnIfBelowOrEqual,
	ReturnIfAbove,
	ReturnIfLess,
	ReturnIfLessOrEqual,
	ReturnIfGreaterOrEqual,
	ReturnIfGreater,
	Break,
	DispatchInterruptTable,
	WaitForInterrupt,
	Reset,
	Halt,
}

//--> Functions <--

pub(crate) fn lex(verbose: bool, path: PathBuf) -> Result {
	let mut tokens: TokenStream = Vec::new();
	let mut errs: ErrorList = Vec::new();

	if verbose { println!("INFO: {}: Lexing file...", path.display()) }

	match File::open(path.clone()) {
		Ok(file) => for (line_num, line) in BufReader::new(file).lines().enumerate() {
			match line {
				Ok(line_text) => {
					for (token, span) in TokenInner::lexer(&line_text).spanned() {
						if let TokenInner::Error = token {
							// TODO: better error messages!
							errs.push(Error::new(false, path.clone(), Some(line_num), Some(span.clone()), format!("Couldn't lex this text: {}", &line_text[span])));
						} else {
							if verbose { println!("INFO: {}: {}: {}..{}: Got token from text: {}", path.display(), line_num, span.start, span.end, &line_text[span.clone()]); }
							tokens.push(Token::new(token, line_num, span.clone(), &line_text[span]));
						}
					}

					tokens.push(Token::new(TokenInner::Newline, line_num, line_text.len()-1..line_text.len(), "\n"))
				},
				Err(err) => errs.push(Error::new(false, path.clone(), Some(line_num), None, match err.kind() {
					ErrorKind::InvalidData => format!("Line contained invalid data. (likely not UTF-8 text)"),
					_ => format!("Encountered unexpected I/O error while trying to read the line: {}", err.kind())
				}))
			}
		},
		Err(err) => errs.push(Error::new(false, path, None, None, match err.kind() {
			ErrorKind::NotFound => format!("Couldn't find the file."),
			ErrorKind::PermissionDenied => format!("Wasn't allowed to open the file. (insufficient permissions)"),
			_ => format!("Encountered unexpected I/O error while trying to open the file: {}", err.kind())
		}))
	}

	if errs.is_empty() { Ok(tokens) } else { Err(errs) }
}

impl Token {
	pub fn new(inner: TokenInner, line: usize, span: Range<usize>, slice: &str) -> Token {
		Token { inner, line, span, source: String::from(slice) }
	}
}

impl fmt::Display for Token {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match &self.inner {
			TokenInner::Immediate(imm) => match imm {
				Imm::Byte(b) => write!(f, "an immediate byte {}", b),
				Imm::Word(w) => write!(f, "an immediate word {}", w),
			},
			TokenInner::String(lit) => {
				let mut display = String::new();

				for b in lit {
					display.push(text::byte_to_ascii(*b).ok_or(fmt::Error)?);
				}

				write!(f, "a string literal \"{}\"", display)
			},
			TokenInner::Path(path) => write!(f, "a path to '{}'", path.display()),
			TokenInner::Address(addr) => match addr {
				Addr::Direct(a) => write!(f, "a direct address {}", a),
				Addr::Indirect(rp) => match rp {
					RegPair::RP0 => write!(f, "an indirect address through register pair 0"),
					RegPair::RP1 => write!(f, "an indirect address through register pair 1"),
					RegPair::RP2 => write!(f, "an indirect address through register pair 2"),
					RegPair::RP3 => write!(f, "an indirect address through register pair 3"),
					RegPair::RP4 => write!(f, "an indirect address through register pair 4"),
					RegPair::RP5 => write!(f, "an indirect address through register pair 5"),
					RegPair::RP6 => write!(f, "an indirect address through register pair 6"),
					RegPair::RP7 => write!(f, "an indirect address through register pair 7"),
				},
				Addr::DirectPort(p) => write!(f, "a port address {}", p),
				Addr::IndirectPort(ra) => match ra {
					FullReg::R0 => write!(f, "an indirect port address through register 0"),
					FullReg::R1 => write!(f, "an indirect port address through register 1"),
					FullReg::R2 => write!(f, "an indirect port address through register 2"),
					FullReg::R3 => write!(f, "an indirect port address through register 3"),
					FullReg::R4 => write!(f, "an indirect port address through register 4"),
					FullReg::R5 => write!(f, "an indirect port address through register 5"),
					FullReg::R6 => write!(f, "an indirect port address through register 6"),
					FullReg::R7 => write!(f, "an indirect port address through register 7"),
				},
				Addr::DirectRelative(offset) => write!(f, "a relative address {}", offset),
				Addr::IndirectRelative(ra) => match ra {
					FullReg::R0 => write!(f, "an indirect relative address through register 0"),
					FullReg::R1 => write!(f, "an indirect relative address through register 1"),
					FullReg::R2 => write!(f, "an indirect relative address through register 2"),
					FullReg::R3 => write!(f, "an indirect relative address through register 3"),
					FullReg::R4 => write!(f, "an indirect relative address through register 4"),
					FullReg::R5 => write!(f, "an indirect relative address through register 5"),
					FullReg::R6 => write!(f, "an indirect relative address through register 6"),
					FullReg::R7 => write!(f, "an indirect relative address through register 7"),
				},
			},
			TokenInner::Operator(op) => match op {
				Op::Tilde => write!(f, "operator '~'"),
				Op::Caret => write!(f, "operator '^'"),
				Op::Ampersand => write!(f, "operator '&'"),
				Op::Star => write!(f, "operator '*'"),
				Op::Minus => write!(f, "operator '-'"),
				Op::Equals => write!(f, "operator '='"),
				Op::Plus => write!(f, "operator '+'"),
				Op::Pipe => write!(f, "operator '|'"),
				Op::Colon => write!(f, "operator ':'"),
				Op::Less => write!(f, "operator '<'"),
				Op::ShiftLeft => write!(f, "operator '<<'"),
				Op::LessOrEqual => write!(f, "operator '<='"),
				Op::Greater => write!(f, "operator '>'"),
				Op::ShiftRight => write!(f, "operator '>>'"),
				Op::GreaterOrEqual => write!(f, "operator '>='"),
				Op::NotEquals => write!(f, "operator '<>'"),
				Op::Slash => write!(f, "operator '/'"),
			},
			TokenInner::Keyword(word) => match word {
				Word::Register(r) => match r {
					Reg::Full(ra) => match ra {
						FullReg::R0 => write!(f, "register 0"),
						FullReg::R1 => write!(f, "register 1"),
						FullReg::R2 => write!(f, "register 2"),
						FullReg::R3 => write!(f, "register 3"),
						FullReg::R4 => write!(f, "register 4"),
						FullReg::R5 => write!(f, "register 5"),
						FullReg::R6 => write!(f, "register 6"),
						FullReg::R7 => write!(f, "register 7"),
					},
					Reg::Short(rw) => match rw {
						ShortReg::SR0 => write!(f, "short register 0"),
						ShortReg::SR1 => write!(f, "short register 1"),
						ShortReg::SR2 => write!(f, "short register 2"),
						ShortReg::SR3 => write!(f, "short register 3"),
						ShortReg::SR4 => write!(f, "short register 4"),
						ShortReg::SR5 => write!(f, "short register 5"),
						ShortReg::SR6 => write!(f, "short register 6"),
						ShortReg::SR7 => write!(f, "short register 7"),
					},
				},
				Word::Directive(dir) => match dir {
					Dir::SetOrigin => write!(f, "set origin directive"),
					Dir::DefineSymbol => write!(f, "define symbol directive"),
					Dir::DefineExternalSymbol => write!(f, "define external symbol directive"),
					Dir::PutByte => write!(f, "put byte directive"),
					Dir::PutWord => write!(f, "put word directive"),
					Dir::PutAddress => write!(f, "put address directive"),
					Dir::PutASCIIString => write!(f, "put ASCII string directive"),
					Dir::PutNullTerminatedASCIIString => write!(f, "put null-terminated ASCII string directive"),
					Dir::IncludeSourceFile => write!(f, "include source file directive"),
					Dir::IncludeBinaryFile => write!(f, "include binary file directive"),
					Dir::AssembleIf => write!(f, "assemble if directive"),
					Dir::AssembleIfNot => write!(f, "assemble if not directive"),
					Dir::AssembleIfDefined => write!(f, "assemble if defined directive"),
					Dir::AssembleIfNotDefined => write!(f, "assemble if not defined directive"),
					Dir::EndIf => write!(f, "end if directive"),
					Dir::SelectLowByte => write!(f, "select low byte directive"),
					Dir::SelectLowWord => write!(f, "select low word directive"),
					Dir::SelectHighByte => write!(f, "select high byte directive"),
					Dir::SelectBankByte => write!(f, "select bank byte directive"),
				},
				Word::Instruction(inst) => match inst {
					Inst::NoOperation => write!(f, "no operation instruction"),
					Inst::ClearZeroFlag => write!(f, "clear zero flag instruction"),
					Inst::SetZeroFlag => write!(f, "set zero flag instruction"),
					Inst::ClearCarryFlag => write!(f, "clear carry flag instruction"),
					Inst::SetCarryFlag => write!(f, "set carry flag instruction"),
					Inst::ClearAdjustFlag => write!(f, "clear adjust flag instruction"),
					Inst::SetAdjustFlag => write!(f, "set adjust flag instruction"),
					Inst::ClearOverflowFlag => write!(f, "clear overflow flag instruction"),
					Inst::SetOverflowFlag => write!(f, "set overflow flag instruction"),
					Inst::ClearNegativeFlag => write!(f, "clear negative flag instruction"),
					Inst::SetNegativeFlag => write!(f, "set negative flag instruction"),
					Inst::EnableInterrupts => write!(f, "enable interrupts instruction"),
					Inst::DisableInterrupts => write!(f, "disable interrupts instruction"),
					Inst::Load => write!(f, "load instruction"),
					Inst::Pull => write!(f, "pull instruction"),
					Inst::Push => write!(f, "push instruction"),
					Inst::PullFlags => write!(f, "pull flags instruction"),
					Inst::PushFlags => write!(f, "push flags instruction"),
					Inst::And => write!(f, "and instruction"),
					Inst::BitTest => write!(f, "bit test instruction"),
					Inst::Or => write!(f, "or instruction"),
					Inst::ExclusiveOr => write!(f, "exclusive or instruction"),
					Inst::Not => write!(f, "not instruction"),
					Inst::ShiftLeft => write!(f, "shift left instruction"),
					Inst::RotateLeft => write!(f, "rotate left instruction"),
					Inst::ShiftRight => write!(f, "shift right instruction"),
					Inst::RotateRight => write!(f, "rotate right instruction"),
					Inst::Add => write!(f, "add instruction"),
					Inst::AddWithCarry => write!(f, "add with carry instruction"),
					Inst::Subtract => write!(f, "subtract instruction"),
					Inst::SubtractWithCarry => write!(f, "subtract with carry instruction"),
					Inst::Compare => write!(f, "compare instruction"),
					Inst::CompareWithCarry => write!(f, "compare with carry instruction"),
					Inst::Decrement => write!(f, "decrement instruction"),
					Inst::Increment => write!(f, "increment instruction"),
					Inst::DecimalAddAdjust => write!(f, "decimal add adjust instruction"),
					Inst::DecimalSubtractAdjust => write!(f, "decimal subtract adjust instruction"),
					Inst::Jump => write!(f, "jump instruction"),
					Inst::JumpIfZero => write!(f, "jump if zero instruction"),
					Inst::JumpIfNotZero => write!(f, "jump if not zero instruction"),
					Inst::JumpIfCarry => write!(f, "jump if carry instruction"),
					Inst::JumpIfBelow => write!(f, "jump if below instruction"),
					Inst::JumpIfNotCarry => write!(f, "jump if not carry instruction"),
					Inst::JumpIfAboveOrEqual => write!(f, "jump if above or equal instruction"),
					Inst::JumpIfOverflow => write!(f, "jump if overflow instruction"),
					Inst::JumpIfNotOverflow => write!(f, "jump if not overflow instruction"),
					Inst::JumpIfPositive => write!(f, "jump if positive instruction"),
					Inst::JumpIfNegative => write!(f, "jump if negative instruction"),
					Inst::JumpIfBelowOrEqual => write!(f, "jump if below or equal instruction"),
					Inst::JumpIfAbove => write!(f, "jump if above instruction"),
					Inst::JumpIfLess => write!(f, "jump if less instruction"),
					Inst::JumpIfLessOrEqual => write!(f, "jump if less or equal instruction"),
					Inst::JumpIfGreaterOrEqual => write!(f, "jump if greater or equal instruction"),
					Inst::JumpIfGreater => write!(f, "jump if greater instruction"),
					Inst::CallSubroutine => write!(f, "call subroutine instruction"),
					Inst::CallSubroutineIfZero => write!(f, "call subroutine if zero instruction"),
					Inst::CallSubroutineIfNotZero => write!(f, "call subroutine if not zero instruction"),
					Inst::CallSubroutineIfCarry => write!(f, "call subroutine if carry instruction"),
					Inst::CallSubroutineIfBelow => write!(f, "call subroutine if below instruction"),
					Inst::CallSubroutineIfNotCarry => write!(f, "call subroutine if not carry instruction"),
					Inst::CallSubroutineIfAboveOrEqual => write!(f, "call subroutine if above or equal instruction"),
					Inst::CallSubroutineIfOverflow => write!(f, "call subroutine if overflow instruction"),
					Inst::CallSubroutineIfNotOverflow => write!(f, "call subroutine if not overflow instruction"),
					Inst::CallSubroutineIfPositive => write!(f, "call subroutine if positive instruction"),
					Inst::CallSubroutineIfNegative => write!(f, "call subroutine if negative instruction"),
					Inst::CallSubroutineIfBelowOrEqual => write!(f, "call subroutine if below or equal instruction"),
					Inst::CallSubroutineIfAbove => write!(f, "call subroutine if above instruction"),
					Inst::CallSubroutineIfLess => write!(f, "call subroutine if less instruction"),
					Inst::CallSubroutineIfLessOrEqual => write!(f, "call subroutine if less or equal instruction"),
					Inst::CallSubroutineIfGreaterOrEqual => write!(f, "call subroutine if greater or equal instruction"),
					Inst::CallSubroutineIfGreater => write!(f, "call subroutine if greater instruction"),
					Inst::Return => write!(f, "return instruction"),
					Inst::ReturnIfZero => write!(f, "return if zero instruction"),
					Inst::ReturnIfNotZero => write!(f, "return if not zero instruction"),
					Inst::ReturnIfCarry => write!(f, "return if carry instruction"),
					Inst::ReturnIfBelow => write!(f, "return if below instruction"),
					Inst::ReturnIfNotCarry => write!(f, "return if not carry instruction"),
					Inst::ReturnIfAboveOrEqual => write!(f, "return if above or equal instruction"),
					Inst::ReturnIfOverflow => write!(f, "return if overflow instruction"),
					Inst::ReturnIfNotOverflow => write!(f, "return if not overflow instruction"),
					Inst::ReturnIfPositive => write!(f, "return if positive instruction"),
					Inst::ReturnIfNegative => write!(f, "return if negative instruction"),
					Inst::ReturnIfBelowOrEqual => write!(f, "return if below or equal instruction"),
					Inst::ReturnIfAbove => write!(f, "return if above instruction"),
					Inst::ReturnIfLess => write!(f, "return if less instruction"),
					Inst::ReturnIfLessOrEqual => write!(f, "return if less or equal instruction"),
					Inst::ReturnIfGreaterOrEqual => write!(f, "return if greater or equal instruction"),
					Inst::ReturnIfGreater => write!(f, "return if greater instruction"),
					Inst::Break => write!(f, "break instruction"),
					Inst::DispatchInterruptTable => write!(f, "dispatch interrupt table instruction"),
					Inst::WaitForInterrupt => write!(f, "wait for interrupt instruction"),
					Inst::Reset => write!(f, "reset instruction"),
					Inst::Halt => write!(f, "halt instruction"),
				},
				Word::Identifier(ident) => write!(f, "an identifier '{}'", ident)
			},
			TokenInner::Newline => write!(f, "a newline"),
			TokenInner::Error => write!(f, "an error"),
		}
	}
}

impl TokenInner {
	fn string(l: &mut Lexer<TokenInner>) -> Option<Vec<u8>> {
		let s = l.slice().strip_prefix('"')?.strip_suffix('"')?;

		text::make_ascii_string(s)
	}

	fn raw_string(l: &mut Lexer<TokenInner>) -> Option<Vec<u8>> {
		let s = l.slice().strip_prefix("r#\"")?.strip_suffix("\"#r")?;
		
		text::make_raw_ascii_string(s)
	}

	fn path(l: &mut Lexer<TokenInner>) -> Option<PathBuf> {
		let s = l.slice().strip_prefix("@")?;

		Some(PathBuf::try_from(s).ok()?)
	}
}

impl Imm {
	pub fn binary(l: &mut Lexer<TokenInner>) -> Option<Imm> {
		let mut slice = l.slice().to_lowercase();

		if let Some(s) = slice.strip_prefix('%') { slice = String::from(s); }
		else if let Some(s) = slice.strip_prefix("0b") { slice = String::from(s); }
		else if let Some(s) = slice.strip_suffix("b") { slice = String::from(s); }
		else { unreachable!() }

		if let Ok(b) = u8::from_str_radix(&slice, 2) { Some(Imm::Byte(b)) }
		else if let Ok(w) = u16::from_str_radix(&slice, 2) { Some(Imm::Word(w)) }
		else { None }
	}

	pub fn decimal(l: &mut Lexer<TokenInner>) -> Option<Imm> {
		let slice = l.slice();

		if let Ok(b) = u8::from_str_radix(slice, 10) { Some(Imm::Byte(b)) }
		else if let Ok(w) = u16::from_str_radix(slice, 10) { Some(Imm::Word(w)) }
		else { None }
	}

	pub fn hexadecimal(l: &mut Lexer<TokenInner>) -> Option<Imm> {
		let mut slice = l.slice().to_lowercase();

		if let Some(s) = slice.strip_prefix('$') { slice = String::from(s); }
		else if let Some(s) = slice.strip_prefix("0x") { slice = String::from(s); }
		else if let Some(s) = slice.strip_suffix("h") { slice = String::from(s); }
		else { unreachable!() }

		if let Ok(b) = u8::from_str_radix(&slice, 16) { Some(Imm::Byte(b)) }
		else if let Ok(w) = u16::from_str_radix(&slice, 16) { Some(Imm::Word(w)) }
		else { None }
	}

	pub fn character(l: &mut Lexer<TokenInner>) -> Option<Imm> {
		let s = l.slice().strip_prefix("'")?.strip_suffix("'")?;

		Some(Imm::Byte(text::make_ascii_character(s)?))
	}
}

impl Addr {
	fn try_strip_binary(slice: &String) -> Option<String> {
		if let Some(s) = slice.strip_prefix('%') { Some(String::from(s)) }
		else if let Some(s) = slice.strip_prefix("0b") { Some(String::from(s)) }
		else if let Some(s) = slice.strip_suffix("b") { Some(String::from(s)) }
		else { None }
	}

	pub fn binary(l: &mut Lexer<TokenInner>) -> Option<Addr> {
		let mut slice = l.slice().strip_prefix('(')?.strip_suffix(')')?.to_lowercase();

		if let Some(s) = slice.strip_suffix('p') {
			// Port
			slice = Addr::try_strip_binary(&String::from(s))?;

			if let Ok(p) = u16::from_str_radix(&slice, 2) {
				Some(Addr::DirectPort(p))
			} else { None }
		} else if let Some(s) = slice.strip_prefix("pc") {
			// Relative
			slice = String::from(s);

			let negative = if let Some(s) = slice.strip_prefix('-') {
				slice = Addr::try_strip_binary(&String::from(s))?;
				true
			} else if let Some(s) = slice.strip_prefix('+') {
				slice = Addr::try_strip_binary(&String::from(s))?;
				false
			} else { return None };

			if let Ok(offset) = i16::from_str_radix(&slice, 2) {
				Some(Addr::DirectRelative(if negative { -offset } else { offset }))
			} else { None }
		} else {
			// Standard
			slice = Addr::try_strip_binary(&slice)?;

			if let Ok(a) = u32::from_str_radix(&slice, 2) {
				if a <= 0xFFFFFF { Some(Addr::Direct(a)) } else { None }
			} else { None }
		}
	}

	pub fn decimal(l: &mut Lexer<TokenInner>) -> Option<Addr> {
		let mut slice = l.slice().strip_prefix('(')?.strip_suffix(')')?.to_lowercase();

		if let Some(s) = slice.strip_suffix('p') {
			// Port
			slice = String::from(s);

			if let Ok(p) = u16::from_str_radix(&slice, 10) {
				Some(Addr::DirectPort(p))
			} else { None }
		} else if let Some(s) = slice.strip_prefix("pc") {
			// Relative
			slice = String::from(s);

			let negative = if let Some(s) = slice.strip_prefix('-') {
				slice = String::from(s);
				true
			} else if let Some(s) = slice.strip_prefix('+') {
				slice = String::from(s);
				false
			} else { return None };

			if let Ok(offset) = i16::from_str_radix(&slice, 10) {
				Some(Addr::DirectRelative(if negative { -offset } else { offset }))
			} else { None }
		} else {
			// Standard

			if let Ok(a) = u32::from_str_radix(&slice, 10) {
				if a <= 0xFFFFFF { Some(Addr::Direct(a)) } else { None }
			} else { None }
		}
	}

	fn try_strip_hexadecimal(slice: &String) -> Option<String> {
		if let Some(s) = slice.strip_prefix('$') { Some(String::from(s)) }
		else if let Some(s) = slice.strip_prefix("0x") { Some(String::from(s)) }
		else if let Some(s) = slice.strip_suffix("h") { Some(String::from(s)) }
		else { None }
	}
	
	pub fn hexadecimal(l: &mut Lexer<TokenInner>) -> Option<Addr> {
		let mut slice = l.slice().strip_prefix('(')?.strip_suffix(')')?.to_lowercase();

		if let Some(s) = slice.strip_suffix('p') {
			// Port
			slice = Addr::try_strip_hexadecimal(&String::from(s))?;

			if let Ok(p) = u16::from_str_radix(&slice, 16) {
				Some(Addr::DirectPort(p))
			} else { None }
		} else if let Some(s) = slice.strip_prefix("pc") {
			// Relative
			slice = String::from(s);

			let negative = if let Some(s) = slice.strip_prefix('-') {
				slice = Addr::try_strip_hexadecimal(&String::from(s))?;
				true
			} else if let Some(s) = slice.strip_prefix('+') {
				slice = Addr::try_strip_hexadecimal(&String::from(s))?;
				false
			} else { return None };

			if let Ok(offset) = i16::from_str_radix(&slice, 16) {
				Some(Addr::DirectRelative(if negative { -offset } else { offset }))
			} else { None }
		} else {
			// Standard
			slice = Addr::try_strip_hexadecimal(&slice)?;

			if let Ok(a) = u32::from_str_radix(&slice, 16) {
				if a <= 0xFFFFFF { Some(Addr::Direct(a)) } else { None }
			} else { None }
		}
	}

	pub fn indirect(l: &mut Lexer<TokenInner>) -> Option<Addr> {
		let mut slice = l.slice().strip_prefix('(')?.strip_suffix(')')?.to_lowercase();

		if let Some(s) = slice.strip_suffix('p') {
			// Port
			slice = String::from(s);

			match slice.as_str() {
				"r0" => Some(Addr::IndirectPort(FullReg::R0)),
				"r1" => Some(Addr::IndirectPort(FullReg::R1)),
				"r2" => Some(Addr::IndirectPort(FullReg::R2)),
				"r3" => Some(Addr::IndirectPort(FullReg::R3)),
				"r4" => Some(Addr::IndirectPort(FullReg::R4)),
				"r5" => Some(Addr::IndirectPort(FullReg::R5)),
				"r6" => Some(Addr::IndirectPort(FullReg::R6)),
				"r7" => Some(Addr::IndirectPort(FullReg::R7)),
				_ => None
			}
		} else if let Some(s) = slice.strip_prefix("ip") {
			// Relative
			slice = String::from(s.strip_prefix('+')?);

			match slice.as_str() {
				"r0" => Some(Addr::IndirectRelative(FullReg::R0)),
				"r1" => Some(Addr::IndirectRelative(FullReg::R1)),
				"r2" => Some(Addr::IndirectRelative(FullReg::R2)),
				"r3" => Some(Addr::IndirectRelative(FullReg::R3)),
				"r4" => Some(Addr::IndirectRelative(FullReg::R4)),
				"r5" => Some(Addr::IndirectRelative(FullReg::R5)),
				"r6" => Some(Addr::IndirectRelative(FullReg::R6)),
				"r7" => Some(Addr::IndirectRelative(FullReg::R7)),
				_ => None
			}
		} else {
			// Standard

			match slice.as_str() {
				"rp0" => Some(Addr::Indirect(RegPair::RP0)),
				"rp1" => Some(Addr::Indirect(RegPair::RP1)),
				"rp2" => Some(Addr::Indirect(RegPair::RP2)),
				"rp3" => Some(Addr::Indirect(RegPair::RP3)),
				"rp4" => Some(Addr::Indirect(RegPair::RP4)),
				"rp5" => Some(Addr::Indirect(RegPair::RP5)),
				"rp6" => Some(Addr::Indirect(RegPair::RP6)),
				"rp7" => Some(Addr::Indirect(RegPair::RP7)),
				_ => None
			}
		}
	}
}

impl Op {
	pub fn new(l: &mut Lexer<TokenInner>) -> Op {
		match l.slice() {
			"~" => Op::Tilde,
			"^" => Op::Caret,
			"&" => Op::Ampersand,
			"*" => Op::Star,
			"-" => Op::Minus,
			"=" => Op::Equals,
			"+" => Op::Plus,
			"|" => Op::Pipe,
			":" => Op::Colon,
			"<<" => Op::ShiftLeft,
			"<>" => Op::NotEquals,
			"<" => Op::Less,
			"<=" => Op::LessOrEqual,
			">" => Op::Greater,
			">=" => Op::GreaterOrEqual,
			">>" => Op::ShiftRight,
			"/" => Op::Slash,
			_ => unreachable!()
		}
	}
}

impl Word {
	pub fn new(l: &mut Lexer<TokenInner>) -> Option<Word> {
		let mut s = String::from(l.slice());

		if s.starts_with('.') {
			// Directive
			s = s.strip_prefix('.').unwrap().to_lowercase();

			match s.as_str() {
				"org" | "origin" => Some(Word::Directive(Dir::SetOrigin)),
				"def" | "define" => Some(Word::Directive(Dir::DefineSymbol)),
				"ext" | "extern" => Some(Word::Directive(Dir::DefineExternalSymbol)),
				"db" | "byte" => Some(Word::Directive(Dir::PutByte)),
				"dw" | "word" => Some(Word::Directive(Dir::PutWord)),
				"dv" | "vector" => Some(Word::Directive(Dir::PutAddress)),
				"str" | "ascii" => Some(Word::Directive(Dir::PutASCIIString)),
				"strz" | "asciiz" => Some(Word::Directive(Dir::PutNullTerminatedASCIIString)),
				"incsrc" | "include" => Some(Word::Directive(Dir::IncludeSourceFile)),
				"incbin" | "binary" => Some(Word::Directive(Dir::IncludeBinaryFile)),
				"if" => Some(Word::Directive(Dir::AssembleIf)),
				"ifn" | "ifnot" => Some(Word::Directive(Dir::AssembleIfNot)),
				"ifdef" | "ifdefined" => Some(Word::Directive(Dir::AssembleIfDefined)),
				"ifndef" | "ifnotdef" | "ifndefined" | "ifnotdefined" => Some(Word::Directive(Dir::AssembleIfNotDefined)),
				"endif" => Some(Word::Directive(Dir::EndIf)),
				"lb" | "lobyte" => Some(Word::Directive(Dir::SelectLowByte)),
				"lw" | "loword" => Some(Word::Directive(Dir::SelectLowWord)),
				"hb" | "hibyte" => Some(Word::Directive(Dir::SelectHighByte)),
				"bb" | "bankbyte" => Some(Word::Directive(Dir::SelectBankByte)),
				_ => None
			}
		} else if s.ends_with('$') {
			// Identifier
			Some(Word::Identifier(s))
		} else {
			// Instruction, Register, or Identifier
			Some(match s.to_lowercase().as_str() {
				"nop" => Word::Instruction(Inst::NoOperation),
				"clz" => Word::Instruction(Inst::ClearZeroFlag),
				"sez" => Word::Instruction(Inst::SetZeroFlag),
				"clc" => Word::Instruction(Inst::ClearCarryFlag),
				"sec" => Word::Instruction(Inst::SetCarryFlag),
				"cla" => Word::Instruction(Inst::ClearAdjustFlag),
				"sea" => Word::Instruction(Inst::SetAdjustFlag),
				"clv" => Word::Instruction(Inst::ClearOverflowFlag),
				"sev" => Word::Instruction(Inst::SetOverflowFlag),
				"cln" => Word::Instruction(Inst::ClearNegativeFlag),
				"sen" => Word::Instruction(Inst::SetNegativeFlag),
				"ei" => Word::Instruction(Inst::EnableInterrupts),
				"di" => Word::Instruction(Inst::DisableInterrupts),
				"ld" => Word::Instruction(Inst::Load),
				"pl" => Word::Instruction(Inst::Pull),
				"ph" => Word::Instruction(Inst::Push),
				"plf" => Word::Instruction(Inst::PullFlags),
				"phf" => Word::Instruction(Inst::PushFlags),
				"and" => Word::Instruction(Inst::And),
				"bit" => Word::Instruction(Inst::BitTest),
				"or" => Word::Instruction(Inst::Or),
				"eor" => Word::Instruction(Inst::ExclusiveOr),
				"not" => Word::Instruction(Inst::Not),
				"shl" => Word::Instruction(Inst::ShiftLeft),
				"rol" => Word::Instruction(Inst::RotateLeft),
				"shr" => Word::Instruction(Inst::ShiftRight),
				"ror" => Word::Instruction(Inst::RotateRight),
				"add" => Word::Instruction(Inst::Add),
				"adc" => Word::Instruction(Inst::AddWithCarry),
				"sub" => Word::Instruction(Inst::Subtract),
				"sbc" => Word::Instruction(Inst::SubtractWithCarry),
				"cmp" => Word::Instruction(Inst::Compare),
				"cpc" => Word::Instruction(Inst::CompareWithCarry),
				"de" => Word::Instruction(Inst::Decrement),
				"in" => Word::Instruction(Inst::Increment),
				"daa" => Word::Instruction(Inst::DecimalAddAdjust),
				"dsa" => Word::Instruction(Inst::DecimalSubtractAdjust),
				"jmp" => Word::Instruction(Inst::Jump),
				"jz" => Word::Instruction(Inst::JumpIfZero),
				"jnz" => Word::Instruction(Inst::JumpIfNotZero),
				"jc" => Word::Instruction(Inst::JumpIfCarry),
				"jb" => Word::Instruction(Inst::JumpIfBelow),
				"jnc" => Word::Instruction(Inst::JumpIfNotCarry),
				"jae" => Word::Instruction(Inst::JumpIfAboveOrEqual),
				"jv" => Word::Instruction(Inst::JumpIfOverflow),
				"jnv" => Word::Instruction(Inst::JumpIfNotOverflow),
				"jp" => Word::Instruction(Inst::JumpIfPositive),
				"jn" => Word::Instruction(Inst::JumpIfNegative),
				"jbe" => Word::Instruction(Inst::JumpIfBelowOrEqual),
				"ja" => Word::Instruction(Inst::JumpIfAbove),
				"jl" => Word::Instruction(Inst::JumpIfLess),
				"jle" => Word::Instruction(Inst::JumpIfLessOrEqual),
				"jge" => Word::Instruction(Inst::JumpIfGreaterOrEqual),
				"jg" => Word::Instruction(Inst::JumpIfGreater),
				"csr" => Word::Instruction(Inst::CallSubroutine),
				"cz" => Word::Instruction(Inst::CallSubroutineIfZero),
				"cnz" => Word::Instruction(Inst::CallSubroutineIfNotZero),
				"cc" => Word::Instruction(Inst::CallSubroutineIfCarry),
				"cb" => Word::Instruction(Inst::CallSubroutineIfBelow),
				"cnc" => Word::Instruction(Inst::CallSubroutineIfNotCarry),
				"cae" => Word::Instruction(Inst::CallSubroutineIfAboveOrEqual),
				"cv" => Word::Instruction(Inst::CallSubroutineIfOverflow),
				"cnv" => Word::Instruction(Inst::CallSubroutineIfNotOverflow),
				"cp" => Word::Instruction(Inst::CallSubroutineIfPositive),
				"cn" => Word::Instruction(Inst::CallSubroutineIfNegative),
				"cbe" => Word::Instruction(Inst::CallSubroutineIfBelowOrEqual),
				"ca" => Word::Instruction(Inst::CallSubroutineIfAbove),
				"cl" => Word::Instruction(Inst::CallSubroutineIfLess),
				"cle" => Word::Instruction(Inst::CallSubroutineIfLessOrEqual),
				"cge" => Word::Instruction(Inst::CallSubroutineIfGreaterOrEqual),
				"cg" => Word::Instruction(Inst::CallSubroutineIfGreater),
				"rts" => Word::Instruction(Inst::Return),
				"rsz" => Word::Instruction(Inst::ReturnIfZero),
				"rsnz" => Word::Instruction(Inst::ReturnIfNotZero),
				"rsc" => Word::Instruction(Inst::ReturnIfCarry),
				"rsb" => Word::Instruction(Inst::ReturnIfBelow),
				"rsnc" => Word::Instruction(Inst::ReturnIfNotCarry),
				"rsae" => Word::Instruction(Inst::ReturnIfAboveOrEqual),
				"rsv" => Word::Instruction(Inst::ReturnIfOverflow),
				"rsnv" => Word::Instruction(Inst::ReturnIfNotOverflow),
				"rsp" => Word::Instruction(Inst::ReturnIfPositive),
				"rsn" => Word::Instruction(Inst::ReturnIfNegative),
				"rsbe" => Word::Instruction(Inst::ReturnIfBelowOrEqual),
				"rsa" => Word::Instruction(Inst::ReturnIfAbove),
				"rsl" => Word::Instruction(Inst::ReturnIfLess),
				"rsle" => Word::Instruction(Inst::ReturnIfLessOrEqual),
				"rsge" => Word::Instruction(Inst::ReturnIfGreaterOrEqual),
				"rsg" => Word::Instruction(Inst::ReturnIfGreater),
				"brk" => Word::Instruction(Inst::Break),
				"dit" => Word::Instruction(Inst::DispatchInterruptTable),
				"wait" => Word::Instruction(Inst::WaitForInterrupt),
				"res" => Word::Instruction(Inst::Reset),
				"halt" => Word::Instruction(Inst::Halt),
				"r0" => Word::Register(Reg::Full(FullReg::R0)),
				"r1" => Word::Register(Reg::Full(FullReg::R1)),
				"r2" => Word::Register(Reg::Full(FullReg::R2)),
				"r3" => Word::Register(Reg::Full(FullReg::R3)),
				"r4" => Word::Register(Reg::Full(FullReg::R4)),
				"r5" => Word::Register(Reg::Full(FullReg::R5)),
				"r6" => Word::Register(Reg::Full(FullReg::R6)),
				"r7" => Word::Register(Reg::Full(FullReg::R7)),
				"sr0" => Word::Register(Reg::Short(ShortReg::SR0)),
				"sr1" => Word::Register(Reg::Short(ShortReg::SR1)),
				"sr2" => Word::Register(Reg::Short(ShortReg::SR2)),
				"sr3" => Word::Register(Reg::Short(ShortReg::SR3)),
				"sr4" => Word::Register(Reg::Short(ShortReg::SR4)),
				"sr5" => Word::Register(Reg::Short(ShortReg::SR5)),
				"sr6" => Word::Register(Reg::Short(ShortReg::SR6)),
				"sr7" => Word::Register(Reg::Short(ShortReg::SR7)),
				_ => Word::Identifier(s)
			})
		}
	}
}