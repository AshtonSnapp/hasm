//--> Imports <--

use std::{
	collections::HashMap,
	path::PathBuf,
};

use crate::{
	Error,
	ErrorList,
};

use super::lexer::{
	Token,
	TokenStream,
	TokenInner,
	Imm,
	Addr,
	Op,
	Word,
	Reg,
	RegPair,
	FullReg,
	ShortReg,
	Dir,
	Inst,
};

//--> Type Aliases <--

pub(crate) type Result = std::result::Result<(FileTree, ErrorList), ErrorList>;

//--> Structs <--

pub(crate) struct FileTree {
	pub file: PathBuf,
	pub contents: HashMap<usize, SyntaxNode>,
}

pub(crate) struct SyntaxNode {
	pub source: Token,
	pub lefthand: Option<Box<SyntaxNode>>,
	pub righthand: Option<Box<SyntaxNode>>,
}

//--> Functions <--

impl FileTree {
	pub fn new(verbose: bool, path: PathBuf, file_source: TokenStream) -> Result {
		let mut file = FileTree { file: path.clone(), contents: HashMap::new() };
		let mut errs: ErrorList = Vec::new();

		if verbose { println!("INFO: {}: Parsing file...", path.display()); }

		'lines: for line_source in file_source.split(|t| if let TokenInner::Newline = &t.inner { true } else { false }) {
			// We want to use the current line's tokens as a stack.
			let mut tokens = Vec::from(line_source);
			tokens.reverse();

			let mut line_num: usize = 0;
			let mut line: Option<SyntaxNode> = None;

			'tokens: loop {
				if tokens.is_empty() { break; }
			}

			// If we get to this point, line should have a value. So, unwrapping should be safe.
			file.contents.insert(line_num, line.unwrap());
		}

		if errs.is_empty() || errs.iter().all(|e| e.is_warning) { Ok((file, errs)) } else { Err(errs) }
	}
}

impl SyntaxNode {
	pub fn new(source: Token) -> SyntaxNode { SyntaxNode { source, lefthand: None, righthand: None } }

	pub fn with_lefthand(mut self, lefthand: SyntaxNode) -> SyntaxNode { self.lefthand = Some(Box::new(lefthand)); self }

	pub fn with_righthand(mut self, righthand: SyntaxNode) -> SyntaxNode { self.righthand = Some(Box::new(righthand)); self }

	pub fn add_lefthand(&mut self, lefthand: SyntaxNode) { self.lefthand = Some(Box::new(lefthand)) }

	pub fn add_righthand(&mut self, righthand: SyntaxNode) { self.righthand = Some(Box::new(righthand)) }
}