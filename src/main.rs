//--> Imports <--

mod lex;

use lex::{Token, TokenStream};
use clap::{Arg, command, ValueHint};
use std::path::Path;
use std::fs::{File, ReadDir, read_dir};
use std::io;
use std::io::{ErrorKind, Write};
use std::collections::HashMap;
use std::process::exit;

//--> Type Aliases <--

pub type StringList = Vec<String>;

//--> Functions <--

fn main() {
    let args = command!()
        .trailing_var_arg(true)
        .arg(Arg::new("output")
            .short('o')
            .long("output")
            .value_hint(ValueHint::FilePath)
            .value_name("OUTFILE")
            .allow_invalid_utf8(true)
            .help("File to write the assembled binary to.")
        )
        .arg(Arg::new("INFILES")
            .value_hint(ValueHint::FilePath)
            .allow_invalid_utf8(true)
            .multiple_values(true)
            .help("Files to assemble."))
        .get_matches();
    
    // Get the names of our input files. At least one is required, so we can safely unwrap here.
    let infiles = args.values_of_os("INFILES").unwrap().map(|x| Path::new(x)).collect::<Vec<&Path>>();

    // Next, get our output file's name. The extension will be replaced in the assemble function.
    let outfile = if let Some(o) = args.value_of_os("output") {
        Path::new(o)
    } else {
        Path::new(infiles[0])
    };

    match assemble(infiles, outfile) {
        Ok(wrns) => {
            let wrn_count = wrns.len();
            
            for wrn in wrns { eprintln!("{}", wrn); }

            println!("Assembly succeeded with {} warnings.", wrn_count);

            exit(0);
        },
        Err((errs, wrns)) => {
            let wrn_count = wrns.len();
            
            for wrn in wrns { eprintln!("{}", wrn); }

            let err_count = errs.len();

            for err in errs { eprintln!("{}", err); }

            println!("Failed to assemble, with {} warnings and {} errors.", wrn_count, err_count);

            exit(1);
        }
    }
}

fn assemble(sources: Vec<&Path>, output: &Path) -> Result<StringList, (StringList, StringList)> {
    let mut errs = Vec::new();
    let mut wrns = Vec::new();

    let wrapped_dirs = unique_parent_dirs(sources.clone());

    // dirs we can get look for source files in.
    let mut dirs: Vec<ReadDir> = Vec::new();

    for wrapped_dir in wrapped_dirs {
        match wrapped_dir {
            Ok(d) => dirs.push(d),
            Err(e) => errs.push(format!("<ERR!> Encountered I/O Error trying to get parent directories of source files: {}", e))
        }
    }

    let mut toks: HashMap<&Path, TokenStream> = HashMap::new();

    for source in sources {
        match Token::lex(source) {
            Ok(t) => { toks.insert(source, t); },
            Err(mut e) => {
                errs.append(&mut e);
                return Err((errs, wrns))
            }
        }
    }

    // TODO

    if errs.is_empty() {
        Ok(wrns)
    } else {
        Err((errs, wrns))
    }
}

fn unique_parent_dirs(paths: Vec<&Path>) -> Vec<io::Result<ReadDir>> {
    let mut parents = paths.iter().map(|p| p.parent().unwrap()).collect::<Vec<&Path>>();
    
    // ensure we only have our unique parent directories
    parents.sort();
    parents.dedup();

    parents.iter().map(|p| read_dir(p)).collect::<Vec<io::Result<ReadDir>>>()
}