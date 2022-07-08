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
	#[regex(r"#[0-9][_0-9]", TokenInner::decimal)]
	#[regex(r"#%[01][_01]", TokenInner::binary)]
	#[regex(r"#0(b|B)[01][_01]", TokenInner::binary)]
	#[regex(r"#[01][_01](b|B)", TokenInner::binary)]
	#[regex(r"#\$[0-9a-fA-F][_0-9a-fA-F]", TokenInner::hexadecimal)]
	#[regex(r"#0(x|X)[0-9a-fA-F][_0-9a-fA-F]", TokenInner::hexadecimal)]
	#[regex(r"#[0-9a-fA-F][_0-9a-fA-F](h|H)", TokenInner::hexadecimal)]
	#[regex(r"'([\x00-\x7F]+|\\')'", TokenInner::character)]
	Immediate(u8),

	#[regex(r#""([\x00-\x7F]|\\")+""#, TokenInner::string)]
	#[regex(r##"r#"([\x00-\x7F]|\\")+"#r"##, TokenInner::raw_string)]
	String(Vec<u8>),

	#[regex(r"@[\u{0}-\u{10FFFE}\u{10FFFF}]*", TokenInner::path)]
	Path(PathBuf),

	#[regex(r"[0-9][_0-9]", Addr::decimal)]
	#[regex(r"%[01][_01]", Addr::binary)]
	#[regex(r"0(b|B)[01][_01]", Addr::binary)]
	#[regex(r"[01][_01](b|B)", Addr::binary)]
	#[regex(r"\$[0-9a-fA-F][_0-9a-fA-F]", Addr::hexadecimal)]
	#[regex(r"0(x|X)[0-9a-fA-F][_0-9a-fA-F]", Addr::hexadecimal)]
	#[regex(r"[0-9a-fA-F][_0-9a-fA-F](h|H)", Addr::hexadecimal)]
	#[regex(r"\([bdBD][cdCD]\)", Addr::pointer)]
	Address(Addr),

	#[regex(r"[\^&*-=\+|:<>/]", Op::new)]
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
pub enum Addr {
	Regular(u16),
	Pointer,
	ShadowPointer,
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
	A,
	B,
	C,
	PairBC,
	ShadowDD,
	ShadowEE
}

#[derive(Clone)]
pub enum Dir {
	SetOrigin,
	DefineSymbol,
	DefineExternalSymbol,
	PutByte,
	PutWord,
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
	SelectHighByte,
}

#[derive(Clone)]
pub enum Inst {
	NoOperation,
	LoadAZero,
	LoadA,
	PullA,
	CompareA,
	StoreA,
	PushA,
	LoadBZero,
	LoadB,
	PullB,
	CompareB,
	StoreB,
	PushB,
	LoadCZero,
	LoadC,
	PullC,
	CompareC,
	StoreC,
	PushC,
	ExchangeD,
	JumpRelative,
	Jump,
	ExchangeE,
	JumpSubroutineRelative,
	JumpSubroutine,
	JumpNotZero,
	JumpZero,
	JumpSubroutineNotZero,
	JumpSubroutineZero,
	JumpPositive,
	JumpNegative,
	JumpSubroutinePositive,
	JumpSubroutineNegative,
	JumpNotCarry,
	JumpCarry,
	JumpSubroutineNotCarry,
	JumpSubroutineCarry,
	AddZero,
	Add,
	Increment,
	AddZeroWithCarry,
	AddWithCarry,
	SubtractZero,
	Subtract,
	SubtractZeroWithCarry,
	SubtractWithCarry,
	Decrement,
	LogicalShiftRight,
	RotateRight,
	ArithmeticShiftLeft,
	Negate,
	RotateLeft,
	OrZero,
	Or,
	Invert,
	NotOrZero,
	NotOr,
	AndZero,
	And,
	SetDecimal,
	NotAndZero,
	NotAnd,
	ClearDecimal,
	ExclusiveOrZero,
	ExclusiveOr,
	PushProcessor,
	ExclusiveNotOrZero,
	ExclusiveNotOr,
	PullProcessor,
	SetInterrupt,
	ClearInterrupt,
	ReturnFromSubroutine,
	ReturnFromInterrupt,
	Halt,
	Break,
	Reset,
	LoadStackPointer,
	LoadBCWithStackPointer,
	ClearCarry,
	SetCarry,
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
			TokenInner::Immediate(b) => write!(f, "an immediate {}", b),
			TokenInner::String(lit) => {
				let mut display = String::new();

				for b in lit {
					display.push(text::byte_to_ascii(*b).ok_or(fmt::Error)?);
				}

				write!(f, "a string literal \"{}\"", display)
			},
			TokenInner::Path(path) => write!(f, "a path to '{}'", path.display()),
			TokenInner::Address(addr) => match addr {
				Addr::Regular(a) => write!(f, "an address {}", a),
				Addr::Pointer => write!(f, "an indirect address through BC"),
				Addr::ShadowPointer => write!(f, "an indirect address through DD"),
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
					Reg::A => write!(f, "A register"),
					Reg::B => write!(f, "B register"),
					Reg::C => write!(f, "C register"),
					Reg::PairBC => write!(f, "BC register pair"),
					Reg::ShadowDD => write!(f, "DD shadow register"),
					Reg::ShadowEE => write!(f, "EE shadow register"),
				},
				Word::Directive(dir) => match dir {
					Dir::SetOrigin => write!(f, "set origin directive"),
					Dir::DefineSymbol => write!(f, "define symbol directive"),
					Dir::DefineExternalSymbol => write!(f, "define external symbol directive"),
					Dir::PutByte => write!(f, "put byte directive"),
					Dir::PutWord => write!(f, "put word directive"),
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
					Dir::SelectHighByte => write!(f, "select high byte directive"),
				},
				Word::Instruction(inst) => match inst {
					Inst::NoOperation => write!(f, "no operation instruction"),
					Inst::LoadAZero => write!(f, "load A with zero instruction"),
					Inst::LoadA => write!(f, "load A instruction"),
					Inst::PullA => write!(f, "pull A instruction"),
					Inst::CompareA => write!(f, "compare A instruction"),
					Inst::StoreA => write!(f, "store A instruction"),
					Inst::PushA => write!(f, "push A instruction"),
					Inst::LoadBZero => write!(f, "load B with zero instruction"),
					Inst::LoadB => write!(f, "load B instruction"),
					Inst::PullB => write!(f, "pull B instruction"),
					Inst::CompareB => write!(f, "compare B instruction"),
					Inst::StoreB => write!(f, "store B instruction"),
					Inst::PushB => write!(f, "push B instruction"),
					Inst::LoadCZero => write!(f, "load C with zero instruction"),
					Inst::LoadC => write!(f, "load C instruction"),
					Inst::PullC => write!(f, "pull C instruction"),
					Inst::CompareC => write!(f, "compare C instruction"),
					Inst::StoreC => write!(f, "store C instruction"),
					Inst::PushC => write!(f, "push C instruction"),
					Inst::ExchangeD => write!(f, "exchange BC and DD instruction"),
					Inst::JumpRelative => write!(f, "jump relative instruction"),
					Inst::Jump => write!(f, "jump instruction"),
					Inst::ExchangeE => write!(f, "exchange BC and EE instruction"),
					Inst::JumpSubroutineRelative => write!(f, "jump relative to subroutine instruction"),
					Inst::JumpSubroutine => write!(f, "jump to subroutine instruction"),
					Inst::JumpNotZero => write!(f, "jump relative if not zero instruction"),
					Inst::JumpZero => write!(f, "jump if zero instruction"),
					Inst::JumpSubroutineNotZero => write!(f, "jump relative to subroutine if not zero instruction"),
					Inst::JumpSubroutineZero => write!(f, "jump to subroutine if zero instruction"),
					Inst::JumpPositive => write!(f, "jump relative if positive instruction"),
					Inst::JumpNegative => write!(f, "jump if negative instruction"),
					Inst::JumpSubroutinePositive => write!(f, "jump relative to subroutine if positive instruction"),
					Inst::JumpSubroutineNegative => write!(f, "jump to subroutine if negative instruction"),
					Inst::JumpNotCarry => write!(f, "jump relative if not carry instruction"),
					Inst::JumpCarry => write!(f, "jump if carry instruction"),
					Inst::JumpSubroutineNotCarry => write!(f, "jump relative to subroutine if not carry instruction"),
					Inst::JumpSubroutineCarry => write!(f, "jump to subroutine if carry instruction"),
					Inst::AddZero => write!(f, "add zero instruction"),
					Inst::Add => write!(f, "add instruction"),
					Inst::Increment => write!(f, "increment instruction"),
					Inst::AddZeroWithCarry => write!(f, "add zero with carry instruction"),
					Inst::AddWithCarry => write!(f, "add with cary instruction"),
					Inst::SubtractZero => write!(f, "subtract zero instruction"),
					Inst::Subtract => write!(f, "subtract instruction"),
					Inst::SubtractZeroWithCarry => write!(f, "subtract zero with carry instruction"),
					Inst::SubtractWithCarry => write!(f, "subtract with carry instruction"),
					Inst::Decrement => write!(f, "decrement instruction"),
					Inst::LogicalShiftRight => write!(f, "logical shift right instruction"),
					Inst::RotateRight => write!(f, "rotate right instruction"),
					Inst::ArithmeticShiftLeft => write!(f, "arithmetic shift left instruction"),
					Inst::Negate => write!(f, "negate instruction"),
					Inst::RotateLeft => write!(f, "rotate left instruction"),
					Inst::OrZero => write!(f, "or zero instruction"),
					Inst::Or => write!(f, "or instruction"),
					Inst::Invert => write!(f, "invert instruction"),
					Inst::NotOrZero => write!(f, "not or zero instruction"),
					Inst::NotOr => write!(f, "not or instruction"),
					Inst::AndZero => write!(f, "and zero instruction"),
					Inst::And => write!(f, "and instruction"),
					Inst::SetDecimal => write!(f, "set decimal instruction"),
					Inst::NotAndZero => write!(f, "not and zero instruction"),
					Inst::NotAnd => write!(f, "not and instruction"),
					Inst::ClearDecimal => write!(f, "clear decimal instruction"),
					Inst::ExclusiveOrZero => write!(f, "exclusive or zero instruction"),
					Inst::ExclusiveOr => write!(f, "exclusive or instruction"),
					Inst::PushProcessor => write!(f, "push processor instruction"),
					Inst::ExclusiveNotOrZero => write!(f, "exclusive not or zero instruction"),
					Inst::ExclusiveNotOr => write!(f, "exclusive not or instruction"),
					Inst::PullProcessor => write!(f, "pull processor instruction"),
					Inst::SetInterrupt => write!(f, "set interrupt instruction"),
					Inst::ClearInterrupt => write!(f, "clear interrupt instruction"),
					Inst::ReturnFromSubroutine => write!(f, "return from subroutine instruction"),
					Inst::ReturnFromInterrupt => write!(f, "return from interrupt instruction"),
					Inst::Halt => write!(f, "halt instruction"),
					Inst::Break => write!(f, "break instruction"),
					Inst::Reset => write!(f, "reset instruction"),
					Inst::LoadStackPointer => write!(f, "load stack pointer instruction"),
					Inst::LoadBCWithStackPointer => write!(f, "load BC with stack pointer instruction"),
					Inst::ClearCarry => write!(f, "clear carry instruction"),
					Inst::SetCarry => write!(f, "set carry instruction"),
				},
				Word::Identifier(ident) => write!(f, "an identifier '{}'", ident)
			},
			TokenInner::Newline => write!(f, "a newline"),
			TokenInner::Error => write!(f, "an error"),
		}
	}
}

impl TokenInner {
	fn binary(l: &mut Lexer<TokenInner>) -> Option<u8> {
		let mut slice = l.slice().strip_prefix('#')?.to_lowercase();

		if let Some(s) = slice.strip_prefix('%') { slice = String::from(s); }
		else if let Some(s) = slice.strip_prefix("0b") { slice = String::from(s); }
		else if let Some(s) = slice.strip_suffix("b") { slice = String::from(s); }
		else { unreachable!() }

		if let Ok(b) = u8::from_str_radix(&slice, 2) { Some(b) }
		else { None }
	}

	fn decimal(l: &mut Lexer<TokenInner>) -> Option<u8> {
		let slice = l.slice().strip_prefix('#')?;

		if let Ok(b) = u8::from_str_radix(slice, 10) { Some(b) }
		else { None }
	}

	fn hexadecimal(l: &mut Lexer<TokenInner>) -> Option<u8> {
		let mut slice = l.slice().strip_prefix('#')?.to_lowercase();

		if let Some(s) = slice.strip_prefix('$') { slice = String::from(s); }
		else if let Some(s) = slice.strip_prefix("0x") { slice = String::from(s); }
		else if let Some(s) = slice.strip_suffix("h") { slice = String::from(s); }
		else { unreachable!() }

		if let Ok(b) = u8::from_str_radix(&slice, 16) { Some(b) }
		else { None }
	}

	fn character(l: &mut Lexer<TokenInner>) -> Option<u8> {
		let s = l.slice().strip_prefix("'")?.strip_suffix("'")?;

		Some(text::make_ascii_character(s)?)
	}

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

impl Addr {
	pub fn binary(l: &mut Lexer<TokenInner>) -> Option<Addr> {
		let mut slice = l.slice().to_lowercase();

		if let Some(s) = slice.strip_prefix('%') { slice = String::from(s); }
		else if let Some(s) = slice.strip_prefix("0b") { slice = String::from(s); }
		else if let Some(s) = slice.strip_suffix("b") { slice = String::from(s); }
		else { return None }

		Some(Addr::Regular(u16::from_str_radix(&slice, 2).ok()?))
	}

	pub fn decimal(l: &mut Lexer<TokenInner>) -> Option<Addr> {
		let slice = l.slice();

		Some(Addr::Regular(u16::from_str_radix(slice, 10).ok()?))
	}

	pub fn hexadecimal(l: &mut Lexer<TokenInner>) -> Option<Addr> {
		let mut slice = l.slice().to_lowercase();

		if let Some(s) = slice.strip_prefix('$') { slice = String::from(s); }
		else if let Some(s) = slice.strip_prefix("0x") { slice = String::from(s); }
		else if let Some(s) = slice.strip_suffix("h") { slice = String::from(s); }
		else { return None }

		Some(Addr::Regular(u16::from_str_radix(&slice, 16).ok()?))
	}

	pub fn pointer(l: &mut Lexer<TokenInner>) -> Option<Addr> {
		let slice = l.slice().strip_prefix('(')?.strip_suffix(')')?.to_lowercase();

		match slice.as_str() {
			"bc" => Some(Addr::Pointer),
			"dd" => Some(Addr::ShadowPointer),
			_ => None
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
			s = s.strip_prefix('.')?.to_lowercase();

			match s.as_str() {
				"org" | "origin" => Some(Word::Directive(Dir::SetOrigin)),
				"def" | "define" => Some(Word::Directive(Dir::DefineSymbol)),
				"ext" | "extern" => Some(Word::Directive(Dir::DefineExternalSymbol)),
				"db" | "byte" => Some(Word::Directive(Dir::PutByte)),
				"dw" | "word" => Some(Word::Directive(Dir::PutWord)),
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
				"hb" | "hibyte" => Some(Word::Directive(Dir::SelectHighByte)),
				_ => None
			}
		} else if s.ends_with('$') {
			// Identifier
			Some(Word::Identifier(s))
		} else {
			// Instruction, Register, or Identifier
			Some(match s.to_lowercase().as_str() {
				"nop" => Word::Instruction(Inst::NoOperation),
				"ldaz" => Word::Instruction(Inst::LoadAZero),
				"lda" => Word::Instruction(Inst::LoadA),
				"pla" => Word::Instruction(Inst::PullA),
				"cpa" => Word::Instruction(Inst::CompareA),
				"sta" => Word::Instruction(Inst::StoreA),
				"pha" => Word::Instruction(Inst::PushA),
				"ldbz" => Word::Instruction(Inst::LoadBZero),
				"ldb" => Word::Instruction(Inst::LoadB),
				"plb" => Word::Instruction(Inst::PullB),
				"cpb" => Word::Instruction(Inst::CompareB),
				"stb" => Word::Instruction(Inst::StoreB),
				"phb" => Word::Instruction(Inst::PushB),
				"ldcz" => Word::Instruction(Inst::LoadCZero),
				"ldc" => Word::Instruction(Inst::LoadC),
				"plc" => Word::Instruction(Inst::PullC),
				"cpc" => Word::Instruction(Inst::CompareC),
				"stc" => Word::Instruction(Inst::StoreC),
				"phc" => Word::Instruction(Inst::PushC),
				"exd" => Word::Instruction(Inst::ExchangeD),
				"jmpr" => Word::Instruction(Inst::JumpRelative),
				"jmp" => Word::Instruction(Inst::Jump),
				"exe" => Word::Instruction(Inst::ExchangeE),
				"jsrr" => Word::Instruction(Inst::JumpSubroutineRelative),
				"jsr" => Word::Instruction(Inst::JumpSubroutine),
				"jpnz" => Word::Instruction(Inst::JumpNotZero),
				"jmpz" => Word::Instruction(Inst::JumpZero),
				"jsnz" => Word::Instruction(Inst::JumpSubroutineNotZero),
				"jsrz" => Word::Instruction(Inst::JumpSubroutineZero),
				"jmpp" => Word::Instruction(Inst::JumpPositive),
				"jmpn" => Word::Instruction(Inst::JumpNegative),
				"jsrp" => Word::Instruction(Inst::JumpSubroutinePositive),
				"jsrn" => Word::Instruction(Inst::JumpSubroutineNegative),
				"jpnc" => Word::Instruction(Inst::JumpNotCarry),
				"jmpc" => Word::Instruction(Inst::JumpCarry),
				"jsnc" => Word::Instruction(Inst::JumpSubroutineNotCarry),
				"jsrc" => Word::Instruction(Inst::JumpSubroutineCarry),
				"addz" => Word::Instruction(Inst::AddZero),
				"add" => Word::Instruction(Inst::Add),
				"inc" => Word::Instruction(Inst::Increment),
				"adcz" => Word::Instruction(Inst::AddZeroWithCarry),
				"adc" => Word::Instruction(Inst::AddWithCarry),
				"subz" => Word::Instruction(Inst::SubtractZero),
				"sub" => Word::Instruction(Inst::Subtract),
				"sbcz" => Word::Instruction(Inst::SubtractZeroWithCarry),
				"sbc" => Word::Instruction(Inst::SubtractWithCarry),
				"dec" => Word::Instruction(Inst::Decrement),
				"lsr" => Word::Instruction(Inst::LogicalShiftRight),
				"ror" => Word::Instruction(Inst::RotateRight),
				"asl" => Word::Instruction(Inst::ArithmeticShiftLeft),
				"neg" => Word::Instruction(Inst::Negate),
				"rol" => Word::Instruction(Inst::RotateLeft),
				"orz" => Word::Instruction(Inst::OrZero),
				"ora" => Word::Instruction(Inst::Or),
				"inv" => Word::Instruction(Inst::Invert),
				"norz" => Word::Instruction(Inst::NotOrZero),
				"nor" => Word::Instruction(Inst::NotOr),
				"andz" => Word::Instruction(Inst::AndZero),
				"and" => Word::Instruction(Inst::And),
				"sed" => Word::Instruction(Inst::SetDecimal),
				"nndz" => Word::Instruction(Inst::NotAndZero),
				"nnd" => Word::Instruction(Inst::NotAnd),
				"cld" => Word::Instruction(Inst::ClearDecimal),
				"xorz" => Word::Instruction(Inst::ExclusiveOrZero),
				"xor" => Word::Instruction(Inst::ExclusiveOr),
				"php" => Word::Instruction(Inst::PushProcessor),
				"xnrz" => Word::Instruction(Inst::ExclusiveNotOrZero),
				"xnr" => Word::Instruction(Inst::ExclusiveNotOr),
				"plp" => Word::Instruction(Inst::PullProcessor),
				"sei" => Word::Instruction(Inst::SetInterrupt),
				"cli" => Word::Instruction(Inst::ClearInterrupt),
				"rts" => Word::Instruction(Inst::ReturnFromSubroutine),
				"rti" => Word::Instruction(Inst::ReturnFromInterrupt),
				"hlt" => Word::Instruction(Inst::Halt),
				"brk" => Word::Instruction(Inst::Break),
				"rst" => Word::Instruction(Inst::Reset),
				"lds" => Word::Instruction(Inst::LoadStackPointer),
				"lbcs" => Word::Instruction(Inst::LoadBCWithStackPointer),
				"clc" => Word::Instruction(Inst::ClearCarry),
				"sec" => Word::Instruction(Inst::SetCarry),
				"a" => Word::Register(Reg::A),
				"bc" => Word::Register(Reg::PairBC),
				"b" => Word::Register(Reg::B),
				"c" => Word::Register(Reg::C),
				"dd" => Word::Register(Reg::ShadowDD),
				"ee" => Word::Register(Reg::ShadowEE),
				_ => Word::Identifier(s)
			})
		}
	}
}