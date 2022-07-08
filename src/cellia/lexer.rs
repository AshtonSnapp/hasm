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
	#[regex(r"\([iI][pP][+-][0-9][_0-9]\)", Addr::decimal)]
	#[regex(r"\(%[01][_01]\)", Addr::binary)]
	#[regex(r"\(%[01][_01][pP]\)", Addr::binary)]
	#[regex(r"\([iI][pP][+-]%[01][_01]\)", Addr::binary)]
	#[regex(r"\(0[bB][01][_01]\)", Addr::binary)]
	#[regex(r"\(0[bB][01][_01][pP]\)", Addr::binary)]
	#[regex(r"\([iI][pP][+-]0[bB][01][_01]\)", Addr::binary)]
	#[regex(r"\([01][_01][bB]\)", Addr::binary)]
	#[regex(r"\([01][_01][bB][pP]\)", Addr::binary)]
	#[regex(r"\([iI][pP][+-][01][_01][bB]\)", Addr::binary)]
	#[regex(r"\(\$[0-9a-fA-F][_0-9a-fA-F]\)", Addr::hexadecimal)]
	#[regex(r"\(\$[0-9a-fA-F][_0-9a-fA-F][pP]\)", Addr::hexadecimal)]
	#[regex(r"\([iI][pP][+-]\$[0-9a-fA-F][_0-9a-fA-F]\)", Addr::hexadecimal)]
	#[regex(r"\(0[xX][0-9a-fA-F][_0-9a-fA-F]\)", Addr::hexadecimal)]
	#[regex(r"\(0[xX][0-9a-fA-F][_0-9a-fA-F][pP]\)", Addr::hexadecimal)]
	#[regex(r"\([iI][pP][+-]0[xX][0-9a-fA-F][_0-9a-fA-F]\)", Addr::hexadecimal)]
	#[regex(r"\([0-9a-fA-F][_0-9a-fA-F][hH]\)", Addr::hexadecimal)]
	#[regex(r"\([0-9a-fA-F][_0-9a-fA-F][hH][pP]\)", Addr::hexadecimal)]
	#[regex(r"\([iI][pP][+-][0-9a-fA-F][_0-9a-fA-F][hH]\)", Addr::hexadecimal)]
	#[regex(r"\([abcdefghABCDEFGH]\)", Addr::indirect)]
	#[regex(r"\([abcdefghABCDEFGH][stuvwxyzSTUVWXYZ]\)", Addr::indirect)]
	#[regex(r"\([iI][pP]+[abcdefghABCDEFGH]\)", Addr::indirect)]
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
	AS,
	BT,
	CU,
	DV,
	EW,
	FX,
	GY,
	HZ
}

#[derive(Clone)]
pub enum FullReg {
	A,
	B,
	C,
	D,
	E,
	F,
	G,
	H,
}

#[derive(Clone)]
pub enum ShortReg {
	S,
	T,
	U,
	V,
	W,
	X,
	Y,
	Z
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
	ClearHalfCarryFlag,
	SetHalfCarryFlag,
	ClearOverflowFlag,
	SetOverflowFlag,
	ClearNegativeFlag,
	SetNegativeFlag,
	EnableInterrupts,
	DisableInterrupts,
	Swap,
	Load,
	LoadZero,
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
	ReturnFromSubroutine,
	ReturnFromSubroutineIfZero,
	ReturnFromSubroutineIfNotZero,
	ReturnFromSubroutineIfCarry,
	ReturnFromSubroutineIfBelow,
	ReturnFromSubroutineIfNotCarry,
	ReturnFromSubroutineIfAboveOrEqual,
	ReturnFromSubroutineIfOverflow,
	ReturnFromSubroutineIfNotOverflow,
	ReturnFromSubroutineIfPositive,
	ReturnFromSubroutineIfNegative,
	ReturnFromSubroutineIfBelowOrEqual,
	ReturnFromSubroutineIfAbove,
	ReturnFromSubroutineIfLess,
	ReturnFromSubroutineIfLessOrEqual,
	ReturnFromSubroutineIfGreaterOrEqual,
	ReturnFromSubroutineIfGreater,
	ReturnFromInterrupt,
	ReturnFromInterruptIfZero,
	ReturnFromInterruptIfNotZero,
	ReturnFromInterruptIfCarry,
	ReturnFromInterruptIfBelow,
	ReturnFromInterruptIfNotCarry,
	ReturnFromInterruptIfAboveOrEqual,
	ReturnFromInterruptIfOverflow,
	ReturnFromInterruptIfNotOverflow,
	ReturnFromInterruptIfPositive,
	ReturnFromInterruptIfNegative,
	ReturnFromInterruptIfBelowOrEqual,
	ReturnFromInterruptIfAbove,
	ReturnFromInterruptIfLess,
	ReturnFromInterruptIfLessOrEqual,
	ReturnFromInterruptIfGreaterOrEqual,
	ReturnFromInterruptIfGreater,
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
					RegPair::AS => write!(f, "an indirect address through AS"),
					RegPair::BT => write!(f, "an indirect address through BT"),
					RegPair::CU => write!(f, "an indirect address through CU"),
					RegPair::DV => write!(f, "an indirect address through DV"),
					RegPair::EW => write!(f, "an indirect address through EW"),
					RegPair::FX => write!(f, "an indirect address through FX"),
					RegPair::GY => write!(f, "an indirect address through GY"),
					RegPair::HZ => write!(f, "an indirect address through HZ"),
				},
				Addr::DirectPort(p) => write!(f, "a port address {}", p),
				Addr::IndirectPort(ra) => match ra {
					FullReg::A => write!(f, "an indirect port address through A"),
					FullReg::B => write!(f, "an indirect port address through B"),
					FullReg::C => write!(f, "an indirect port address through C"),
					FullReg::D => write!(f, "an indirect port address through D"),
					FullReg::E => write!(f, "an indirect port address through E"),
					FullReg::F => write!(f, "an indirect port address through F"),
					FullReg::G => write!(f, "an indirect port address through G"),
					FullReg::H => write!(f, "an indirect port address through H"),
				},
				Addr::DirectRelative(offset) => write!(f, "a relative address {}", offset),
				Addr::IndirectRelative(ra) => match ra {
					FullReg::A => write!(f, "an indirect relative address through A"),
					FullReg::B => write!(f, "an indirect relative address through B"),
					FullReg::C => write!(f, "an indirect relative address through C"),
					FullReg::D => write!(f, "an indirect relative address through D"),
					FullReg::E => write!(f, "an indirect relative address through E"),
					FullReg::F => write!(f, "an indirect relative address through F"),
					FullReg::G => write!(f, "an indirect relative address through G"),
					FullReg::H => write!(f, "an indirect relative address through H"),
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
						FullReg::A => write!(f, "A register"),
						FullReg::B => write!(f, "B register"),
						FullReg::C => write!(f, "C register"),
						FullReg::D => write!(f, "D register"),
						FullReg::E => write!(f, "E register"),
						FullReg::F => write!(f, "F register"),
						FullReg::G => write!(f, "G register"),
						FullReg::H => write!(f, "H register"),
					},
					Reg::Short(rw) => match rw {
						ShortReg::S => write!(f, "S register"),
						ShortReg::T => write!(f, "T register"),
						ShortReg::U => write!(f, "U register"),
						ShortReg::V => write!(f, "V register"),
						ShortReg::W => write!(f, "W register"),
						ShortReg::X => write!(f, "X register"),
						ShortReg::Y => write!(f, "Y register"),
						ShortReg::Z => write!(f, "Z register"),
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
					Inst::ClearHalfCarryFlag => write!(f, "clear half-carry flag instruction"),
					Inst::SetHalfCarryFlag => write!(f, "set half-carry flag instruction"),
					Inst::ClearOverflowFlag => write!(f, "clear overflow flag instruction"),
					Inst::SetOverflowFlag => write!(f, "set overflow flag instruction"),
					Inst::ClearNegativeFlag => write!(f, "clear negative flag instruction"),
					Inst::SetNegativeFlag => write!(f, "set negative flag instruction"),
					Inst::EnableInterrupts => write!(f, "enable interrupts instruction"),
					Inst::DisableInterrupts => write!(f, "disable interrupts instruction"),
					Inst::Swap => write!(f, "swap instruction"),
					Inst::Load => write!(f, "load instruction"),
					Inst::LoadZero => write!(f, "load zero instruction"),
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
					Inst::ReturnFromSubroutine => write!(f, "return from subroutine instruction"),
					Inst::ReturnFromSubroutineIfZero => write!(f, "return from subroutine if zero instruction"),
					Inst::ReturnFromSubroutineIfNotZero => write!(f, "return from subroutine if not zero instruction"),
					Inst::ReturnFromSubroutineIfCarry => write!(f, "return from subroutine if carry instruction"),
					Inst::ReturnFromSubroutineIfBelow => write!(f, "return from subroutine if below instruction"),
					Inst::ReturnFromSubroutineIfNotCarry => write!(f, "return from subroutine if not carry instruction"),
					Inst::ReturnFromSubroutineIfAboveOrEqual => write!(f, "return from subroutine if above or equal instruction"),
					Inst::ReturnFromSubroutineIfOverflow => write!(f, "return from subroutine if overflow instruction"),
					Inst::ReturnFromSubroutineIfNotOverflow => write!(f, "return from subroutine if not overflow instruction"),
					Inst::ReturnFromSubroutineIfPositive => write!(f, "return from subroutine if positive instruction"),
					Inst::ReturnFromSubroutineIfNegative => write!(f, "return from subroutine if negative instruction"),
					Inst::ReturnFromSubroutineIfBelowOrEqual => write!(f, "return from subroutine if below or equal instruction"),
					Inst::ReturnFromSubroutineIfAbove => write!(f, "return from subroutine if above instruction"),
					Inst::ReturnFromSubroutineIfLess => write!(f, "return from subroutine if less instruction"),
					Inst::ReturnFromSubroutineIfLessOrEqual => write!(f, "return from subroutine if less or equal instruction"),
					Inst::ReturnFromSubroutineIfGreaterOrEqual => write!(f, "return from subroutine if greater or equal instruction"),
					Inst::ReturnFromSubroutineIfGreater => write!(f, "return from subroutine if greater instruction"),
					Inst::ReturnFromInterrupt => write!(f, "return from interrupt instruction"),
					Inst::ReturnFromInterruptIfZero => write!(f, "return from interrupt if zero instruction"),
					Inst::ReturnFromInterruptIfNotZero => write!(f, "return from interrupt if not zero instruction"),
					Inst::ReturnFromInterruptIfCarry => write!(f, "return from interrupt if carry instruction"),
					Inst::ReturnFromInterruptIfBelow => write!(f, "return from interrupt if below instruction"),
					Inst::ReturnFromInterruptIfNotCarry => write!(f, "return from interrupt if not carry instruction"),
					Inst::ReturnFromInterruptIfAboveOrEqual => write!(f, "return from interrupt if above or equal instruction"),
					Inst::ReturnFromInterruptIfOverflow => write!(f, "return from interrupt if overflow instruction"),
					Inst::ReturnFromInterruptIfNotOverflow => write!(f, "return from interrupt if not overflow instruction"),
					Inst::ReturnFromInterruptIfPositive => write!(f, "return from interrupt if positive instruction"),
					Inst::ReturnFromInterruptIfNegative => write!(f, "return from interrupt if negative instruction"),
					Inst::ReturnFromInterruptIfBelowOrEqual => write!(f, "return from interrupt if below or equal instruction"),
					Inst::ReturnFromInterruptIfAbove => write!(f, "return from interrupt if above instruction"),
					Inst::ReturnFromInterruptIfLess => write!(f, "return from interrupt if less instruction"),
					Inst::ReturnFromInterruptIfLessOrEqual => write!(f, "return from interrupt if less or equal instruction"),
					Inst::ReturnFromInterruptIfGreaterOrEqual => write!(f, "return from interrupt if greater or equal instruction"),
					Inst::ReturnFromInterruptIfGreater => write!(f, "return from interrupt if greater instruction"),
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
		} else if let Some(s) = slice.strip_prefix("ip") {
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
		} else if let Some(s) = slice.strip_prefix("ip") {
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
		} else if let Some(s) = slice.strip_prefix("ip") {
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
				"a" => Some(Addr::IndirectPort(FullReg::A)),
				"b" => Some(Addr::IndirectPort(FullReg::B)),
				"c" => Some(Addr::IndirectPort(FullReg::C)),
				"d" => Some(Addr::IndirectPort(FullReg::D)),
				"e" => Some(Addr::IndirectPort(FullReg::E)),
				"f" => Some(Addr::IndirectPort(FullReg::F)),
				"g" => Some(Addr::IndirectPort(FullReg::G)),
				"h" => Some(Addr::IndirectPort(FullReg::H)),
				_ => None
			}
		} else if let Some(s) = slice.strip_prefix("ip") {
			// Relative
			slice = String::from(s.strip_prefix('+')?);

			match slice.as_str() {
				"a" => Some(Addr::IndirectRelative(FullReg::A)),
				"b" => Some(Addr::IndirectRelative(FullReg::B)),
				"c" => Some(Addr::IndirectRelative(FullReg::C)),
				"d" => Some(Addr::IndirectRelative(FullReg::D)),
				"e" => Some(Addr::IndirectRelative(FullReg::E)),
				"f" => Some(Addr::IndirectRelative(FullReg::F)),
				"g" => Some(Addr::IndirectRelative(FullReg::G)),
				"h" => Some(Addr::IndirectRelative(FullReg::H)),
				_ => None
			}
		} else {
			// Standard

			match slice.as_str() {
				"as" => Some(Addr::Indirect(RegPair::AS)),
				"bt" => Some(Addr::Indirect(RegPair::BT)),
				"cu" => Some(Addr::Indirect(RegPair::CU)),
				"dv" => Some(Addr::Indirect(RegPair::DV)),
				"ew" => Some(Addr::Indirect(RegPair::EW)),
				"fx" => Some(Addr::Indirect(RegPair::FX)),
				"gy" => Some(Addr::Indirect(RegPair::GY)),
				"hz" => Some(Addr::Indirect(RegPair::HZ)),
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
				"clh" => Word::Instruction(Inst::ClearHalfCarryFlag),
				"seh" => Word::Instruction(Inst::SetHalfCarryFlag),
				"clv" => Word::Instruction(Inst::ClearOverflowFlag),
				"sev" => Word::Instruction(Inst::SetOverflowFlag),
				"cln" => Word::Instruction(Inst::ClearNegativeFlag),
				"sen" => Word::Instruction(Inst::SetNegativeFlag),
				"ei" => Word::Instruction(Inst::EnableInterrupts),
				"di" => Word::Instruction(Inst::DisableInterrupts),
				"swp" => Word::Instruction(Inst::Swap),
				"ld" => Word::Instruction(Inst::Load),
				"ldz" => Word::Instruction(Inst::LoadZero),
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
				"rts" => Word::Instruction(Inst::ReturnFromSubroutine),
				"rsz" => Word::Instruction(Inst::ReturnFromSubroutineIfZero),
				"rsnz" => Word::Instruction(Inst::ReturnFromSubroutineIfNotZero),
				"rsc" => Word::Instruction(Inst::ReturnFromSubroutineIfCarry),
				"rsb" => Word::Instruction(Inst::ReturnFromSubroutineIfBelow),
				"rsnc" => Word::Instruction(Inst::ReturnFromSubroutineIfNotCarry),
				"rsae" => Word::Instruction(Inst::ReturnFromSubroutineIfAboveOrEqual),
				"rsv" => Word::Instruction(Inst::ReturnFromSubroutineIfOverflow),
				"rsnv" => Word::Instruction(Inst::ReturnFromSubroutineIfNotOverflow),
				"rsp" => Word::Instruction(Inst::ReturnFromSubroutineIfPositive),
				"rsn" => Word::Instruction(Inst::ReturnFromSubroutineIfNegative),
				"rsbe" => Word::Instruction(Inst::ReturnFromSubroutineIfBelowOrEqual),
				"rsa" => Word::Instruction(Inst::ReturnFromSubroutineIfAbove),
				"rsl" => Word::Instruction(Inst::ReturnFromSubroutineIfLess),
				"rsle" => Word::Instruction(Inst::ReturnFromSubroutineIfLessOrEqual),
				"rsge" => Word::Instruction(Inst::ReturnFromSubroutineIfGreaterOrEqual),
				"rsg" => Word::Instruction(Inst::ReturnFromSubroutineIfGreater),
				"rti" => Word::Instruction(Inst::ReturnFromInterrupt),
				"riz" => Word::Instruction(Inst::ReturnFromInterruptIfZero),
				"rinz" => Word::Instruction(Inst::ReturnFromInterruptIfNotZero),
				"ric" => Word::Instruction(Inst::ReturnFromInterruptIfCarry),
				"rib" => Word::Instruction(Inst::ReturnFromInterruptIfBelow),
				"rinc" => Word::Instruction(Inst::ReturnFromInterruptIfNotCarry),
				"riae" => Word::Instruction(Inst::ReturnFromInterruptIfAboveOrEqual),
				"riv" => Word::Instruction(Inst::ReturnFromInterruptIfOverflow),
				"rinv" => Word::Instruction(Inst::ReturnFromInterruptIfNotOverflow),
				"rip" => Word::Instruction(Inst::ReturnFromInterruptIfPositive),
				"rin" => Word::Instruction(Inst::ReturnFromInterruptIfNegative),
				"ribe" => Word::Instruction(Inst::ReturnFromInterruptIfBelowOrEqual),
				"ria" => Word::Instruction(Inst::ReturnFromInterruptIfAbove),
				"ril" => Word::Instruction(Inst::ReturnFromInterruptIfLess),
				"rile" => Word::Instruction(Inst::ReturnFromInterruptIfLessOrEqual),
				"rige" => Word::Instruction(Inst::ReturnFromInterruptIfGreaterOrEqual),
				"rig" => Word::Instruction(Inst::ReturnFromInterruptIfGreater),
				"brk" => Word::Instruction(Inst::Break),
				"dit" => Word::Instruction(Inst::DispatchInterruptTable),
				"wait" => Word::Instruction(Inst::WaitForInterrupt),
				"res" => Word::Instruction(Inst::Reset),
				"halt" => Word::Instruction(Inst::Halt),
				"a" => Word::Register(Reg::Full(FullReg::A)),
				"b" => Word::Register(Reg::Full(FullReg::B)),
				"c" => Word::Register(Reg::Full(FullReg::C)),
				"d" => Word::Register(Reg::Full(FullReg::D)),
				"e" => Word::Register(Reg::Full(FullReg::E)),
				"f" => Word::Register(Reg::Full(FullReg::F)),
				"g" => Word::Register(Reg::Full(FullReg::G)),
				"h" => Word::Register(Reg::Full(FullReg::H)),
				"s" => Word::Register(Reg::Short(ShortReg::S)),
				"t" => Word::Register(Reg::Short(ShortReg::T)),
				"u" => Word::Register(Reg::Short(ShortReg::U)),
				"v" => Word::Register(Reg::Short(ShortReg::V)),
				"w" => Word::Register(Reg::Short(ShortReg::W)),
				"x" => Word::Register(Reg::Short(ShortReg::X)),
				"y" => Word::Register(Reg::Short(ShortReg::Y)),
				"z" => Word::Register(Reg::Short(ShortReg::Z)),
				_ => Word::Identifier(s)
			})
		}
	}
}