//--> Imports <--

// Handles assembly and disassembly of code targeting the 16-bit Cellia instruction set architecture.
mod cellia;

// Handles assembly and disassembly of code targeting the 8-bit ROCKET88 instruction set architecture.
mod rocket88;

// Handles strings and characters going into assembly.
mod text;

use std::{
	collections::HashMap,
	fmt,
	ops::Range,
	path::PathBuf,
	process,
};

use clap::{
	Arg,
	PossibleValue,
	ValueEnum,
};

//--> Type Aliases <--

type ErrorList = Vec<Error>;

type AssembleResult = Result<(Arch, ErrorList), (Arch, ErrorList)>;

type DetectResult = Result<(Arch, ErrorList), HashMap<Arch, ErrorList>>;

//--> Structs <--

struct Error {
	is_warning: bool,
	file: PathBuf,
	line: Option<usize>,
	span: Option<Range<usize>>,
	message: String,
}

//--> Enums <--

#[derive(Clone, Eq, Hash, PartialEq)]
enum Arch {
	Cellia,
	Rocket88,
}

//--> Functions <--

fn main() {
	let args = {
		clap::command!()
		.long_about(
			"The Homebrew Assembler (hasm) is a cross-assembler targeting homebrew processor architectures. \
			For the purpose of semi-unnecessary definitions, something is 'homebrew' if it is retro but created in modern times.\n\n\
			hasm currently supports the following architectures:\n\t\
			- Cellia, a 16-bit architecture created by Ashton Snapp.\n\t\
			- ROCKET88, an 8-bit architecture created by Matt Heffernan.\n\n\
			By default, hasm will attempt to detect what architecture you are targeting. \
			However, this is not advised - it is recommended you explicitly state your target architecture as an argument."
		)
		.arg_required_else_help(true)
		.trailing_var_arg(true)
		.args([
			{
				Arg::new("verbose")
				.short('v')
				.long("verbose")
				.help("Tells the assembler to output additional information while assembling, rather than just outputting errors.")
			},
			{
				Arg::new("output")
				.short('o')
				.long("out")
				.value_name("OUTPATH")
				.value_parser(clap::value_parser!(PathBuf))
				.help("Specifies a custom path for the output binary. By default, the binary will be placed in the current working directory as 'a.out'.")
			},
			{
				Arg::new("listing")
				.short('l')
				.long("list")
				.value_name("LISTPATH")
				.value_parser(clap::value_parser!(PathBuf))
				.help("Specifies a path to generate a listing file at. By default, no listing file is generated.")
			},
			{
				Arg::new("architecture")
				.short('a')
				.long("arch")
				.value_name("ARCHNAME")
				.value_parser(clap::value_parser!(Arch))
				.help("Specifies what architecture to assemble for. By default, tries to detect the architecture.")
			},
			{
				Arg::new("infiles")
				.value_name("INPATHS")
				.value_parser(clap::value_parser!(PathBuf))
				.required(true)
				.multiple_values(true)
				.help("Paths to source files. All must exist.")
			}
		])
		.get_matches()
	};

	let verbose = args.contains_id("verbose");

	let listing_path = args.get_one::<PathBuf>("listing").map(|p| p.clone());

	let output_path = match args.get_one::<PathBuf>("output") {
		Some(path) => path.clone(),
		None => PathBuf::from("a.out")
	};

	let target_arch = args.get_one::<Arch>("architecture").map(|a| a.clone());

	// This is a required argument, so unwrapping is okay here.
	let input_paths = args.get_many::<PathBuf>("infiles").unwrap().map(|p| p.clone()).collect::<Vec<PathBuf>>();

	if !input_paths.iter().all(|p| p.is_file()) {
		eprintln!("ERR: All paths given as inputs must point to existing files.");
		process::exit(1);
	}

	match target_arch {
		Some(arch) => {
			if verbose { println!("INFO: Assembling for {}...", arch); }

			match arch.get_assembler()(verbose, listing_path.clone(), output_path.clone(), input_paths) {
				Ok((_, warns)) => {
					let warn_count = warns.len();

					for warn in warns {
						eprintln!("{}", warn);
					}

					println!("INFO: Successfully assembled for {}, with {} warnings.\nINFO: The binary was output at '{}'.", arch, warn_count, output_path.display());

					if let Some(path) = listing_path {
						println!("INFO: The listing file was output at '{}'.", path.display());
					}
				}
				Err((_, errs)) => {
					let err_count = errs.iter().filter(|e| !e.is_warning).count();
					let warn_count = errs.iter().filter(|e| e.is_warning).count();

					for err in errs {
						eprintln!("{}", err);
					}

					eprintln!("ERR: Failed to assemble for {}, with {} errors and {} warnings.", arch, err_count, warn_count);
					process::exit(2);
				}
			}
		},
		None => {
			eprintln!("WARN: hasm doesn't know what architecture you want to target. It'll try to detect it, but this is not recommended.");

			match detect_assemble(verbose, listing_path.clone(), output_path.clone(), input_paths) {
				Ok((arch, warns)) => {
					let warn_count = warns.len();

					for warn in warns {
						eprintln!("{}", warn);
					}

					println!("INFO: Successfully assembled for {}, with {} warnings.\nINFO: The binary was output at '{}'.", arch, warn_count, output_path.display());

					if let Some(path) = listing_path {
						println!("INFO: The listing file was output at '{}'.", path.display());
					}
				},
				Err(err_map) => {
					for (arch, errs) in err_map {
						let err_count = errs.iter().filter(|e| !e.is_warning).count();
						let warn_count = errs.iter().filter(|e| e.is_warning).count();

						for err in errs {
							eprintln!("{}", err);
						}

						eprintln!("ERR: Failed to assemble for {}, with {} errors and {} warnings.", arch, err_count, warn_count);
					}

					process::exit(2);
				}
			}
		}
	}
}

fn detect_assemble(verbose: bool, listing_path: Option<PathBuf>, output_path: PathBuf, input_paths: Vec<PathBuf>) -> DetectResult {
	let mut err_map = HashMap::<Arch, ErrorList>::new();

	for arch in Arch::value_variants() {
		if verbose { println!("INFO: Attempting to assemble for {}...", arch); }

		match arch.get_assembler()(verbose, listing_path.clone(), output_path.clone(), input_paths.clone()) {
			Ok((_, warns)) => { return Ok((arch.clone(), warns)); },
			Err((_, errs)) => {
				err_map.insert(arch.clone(), errs);
			},
		}
	}

	Err(err_map)
}

impl Error {
	pub fn new(is_warning: bool, file: PathBuf, line: Option<usize>, span: Option<Range<usize>>, message: String) -> Error {
		Error { is_warning, file, line, span, message }
	}
}

impl fmt::Display for Error {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		let start = if self.is_warning { "WARN" } else { "ERR" };
		
		match self.line {
			Some(l) => match &self.span {
				Some(s) => write!(f, "{}: {}: {}: {}..{}: {}", start, self.file.display(), l, s.start, s.end, self.message),
				None => write!(f, "{}: {}: {}: {}", start, self.file.display(), l, self.message)
			},
			None => write!(f, "{}: {}: {}", start, self.file.display(), self.message)
		}
	}
}

impl Arch {
	pub fn get_assembler(&self) -> fn(bool, Option<PathBuf>, PathBuf, Vec<PathBuf>) -> AssembleResult {
		match self {
			Arch::Cellia => cellia::assemble,
			Arch::Rocket88 => rocket88::assemble,
		}
	}
}

impl ValueEnum for Arch {
	fn to_possible_value<'a>(&self) -> Option<PossibleValue<'a>> {
		match self {
			Arch::Cellia => Some(PossibleValue::new("cellia")),
			Arch::Rocket88 => Some(PossibleValue::new("rocket88")),
		}
	}

	fn value_variants<'a>() -> &'a [Self] {
		&[
			Arch::Cellia,
			Arch::Rocket88,
		]
	}
}

impl fmt::Display for Arch {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match self {
			Arch::Cellia => write!(f, "Cellia"),
			Arch::Rocket88 => write!(f, "ROCKET88"),
		}
	}
}