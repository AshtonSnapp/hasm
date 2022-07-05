//--> Imports <--

use std::{
	collections::HashMap,
	path::PathBuf,
};

use super::lexer::{
	Token,
	TokenInner,
	TokenStream,
	Imm,
	Addr,
	Ptr,
	Op,
	Word,
	Reg,
	RegPair,
	FullReg,
	ShortReg,
	Dir,
	Inst,
};

use crate::{
	Error,
	ErrorList,
};

//--> Type Aliases <--

pub type Result = std::result::Result<(FileTree, ErrorList), ErrorList>;

//--> Structs <--

pub struct FileTree {
	pub path: PathBuf,
	pub contents: HashMap<usize, SyntaxNode>,
}

pub struct SyntaxNode {
	pub source: Token,
	pub lefthand: Option<Box<SyntaxNode>>,
	pub righthand: Option<Box<SyntaxNode>>,
}

//--> Functions <--

impl FileTree {
	pub fn new(verbose: bool, path: PathBuf, source: TokenStream) -> Result {}
}