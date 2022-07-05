//--> Imports <--

mod lexer;

use std::{
	collections::HashMap,
	path::PathBuf,
};

use super::{
	Arch,
	AssembleResult as Result,
	ErrorList,
};

use lexer::TokenStream;

//--> Constants <--

const ARCH: Arch = Arch::Cellia;

//--> Functions <--

pub(crate) fn assemble(verbose: bool, listing_path: Option<PathBuf>, output_path: PathBuf, input_paths: Vec<PathBuf>) -> Result {
	let mut errs: ErrorList = Vec::new();

	let input_file_paths = input_paths.iter().filter(|p| p.is_file()).map(|p| p.clone()).collect::<Vec<PathBuf>>();
	let input_directory_paths = input_paths.iter().filter(|p| p.is_dir()).map(|p| p.clone()).collect::<Vec<PathBuf>>();

	let mut tokenized_files: HashMap<PathBuf, TokenStream> = HashMap::new();

	for input_file_path in input_file_paths {
		match lexer::lex(verbose, input_file_path.clone()) {
			Ok(tokens) => { tokenized_files.insert(input_file_path, tokens); },
			Err(mut file_errs) => { errs.append(&mut file_errs); }
		}
	}

	// If we encountered errors in lexing, we want to return early so we don't generate even more errors.
	if !errs.is_empty() { return Err((ARCH, errs)) }

	if errs.is_empty() | errs.iter().all(|e| e.is_warning) { Ok((ARCH, errs)) } else { Err((ARCH, errs)) }
}