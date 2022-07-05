//--> Imports <--

use std::{
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
	Caret,
	Ampersand,
	Star,
	Minus,
	Equals,
	Plus,
	Pipe,
	Colon,
	LeftAngle,
	RightAngle,
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

impl TokenInner {
	pub fn binary(l: &mut Lexer<TokenInner>) -> Option<u8> {
		let mut slice = l.slice().strip_prefix('#')?.to_lowercase();

		if let Some(s) = slice.strip_prefix('%') { slice = String::from(s); }
		else if let Some(s) = slice.strip_prefix("0b") { slice = String::from(s); }
		else if let Some(s) = slice.strip_suffix("b") { slice = String::from(s); }
		else { unreachable!() }

		if let Ok(b) = u8::from_str_radix(&slice, 2) { Some(b) }
		else { None }
	}

	pub fn decimal(l: &mut Lexer<TokenInner>) -> Option<u8> {
		let slice = l.slice().strip_prefix('#')?;

		if let Ok(b) = u8::from_str_radix(slice, 10) { Some(b) }
		else { None }
	}

	pub fn hexadecimal(l: &mut Lexer<TokenInner>) -> Option<u8> {
		let mut slice = l.slice().strip_prefix('#')?.to_lowercase();

		if let Some(s) = slice.strip_prefix('$') { slice = String::from(s); }
		else if let Some(s) = slice.strip_prefix("0x") { slice = String::from(s); }
		else if let Some(s) = slice.strip_suffix("h") { slice = String::from(s); }
		else { unreachable!() }

		if let Ok(b) = u8::from_str_radix(&slice, 16) { Some(b) }
		else { None }
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
			"^" => Op::Caret,
			"&" => Op::Ampersand,
			"*" => Op::Star,
			"-" => Op::Minus,
			"=" => Op::Equals,
			"+" => Op::Plus,
			"|" => Op::Pipe,
			":" => Op::Colon,
			"<" => Op::LeftAngle,
			">" => Op::RightAngle,
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
			// Instruction or Identifier
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
				_ => Word::Identifier(s)
			})
		}
	}
}