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

//--> Type Aliases <--

pub(crate) type Result = std::result::Result<(FileTree, ErrorList), ErrorList>;

//--> Structs <--

pub(crate) struct FileTree {}

pub(crate) struct SyntaxNode {}

//--> Functions <--

impl FileTree {
	pub fn new(verbose: bool, path: PathBuf, file_source: TokenStream) -> Result {}
}