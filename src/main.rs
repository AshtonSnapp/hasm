//--> Imports <--

mod lex;
mod parse;

use lex::{Token, TokenStream};
use clap::{Arg, command, ValueHint};
use std::path::Path;
use std::fs::{File, ReadDir, read_dir};
use std::io;
use std::io::{ErrorKind, Write};
use std::collections::HashMap;
use parse::{FileAsm, ProgAsm};
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

    let mut file_asms: HashMap<&Path, FileAsm> = HashMap::new();

    for (source, tok) in toks {
        match FileAsm::parse(source, tok) {
            Ok((file_asm, mut w)) => {
                file_asms.insert(source, file_asm);
                wrns.append(&mut w);
            },
            Err((mut e, mut w)) => {
                errs.append(&mut e);
                wrns.append(&mut w);
                return Err((errs, wrns))
            }
        }
    }

    let prg_asm = match ProgAsm::link(file_asms) {
        Ok((p, mut w)) => {
            wrns.append(&mut w);
            p
        },
        Err((mut e, mut w)) => {
            errs.append(&mut e);
            wrns.append(&mut w);
            return Err((errs, wrns))
        }
    };

    let prg_bytes = prg_asm.to_bytes();
    let outfile_name = output.with_extension("bin");
    let ofn = outfile_name.clone();

    match File::create(outfile_name) {
        Ok(mut f) => match f.write_all(prg_bytes) {
            Ok(()) => match f.flush() {
                Ok(()) => {},
                Err(e) => errs.push(format!("<ERR! {}> Unexpected I/O error encountered trying to flush bytes to file: {}", ofn.display(), e))
            },
            Err(e) => errs.push(format!("<ERR! {}> Unexpected I/O error encountered trying to write bytes to file: {}", ofn.display(), e))
        },
        Err(e) => match e.kind() {
            ErrorKind::PermissionDenied => errs.push(format!("<ERR! {}> Denied permission to create or open the file for writing.", ofn.display())),
            _ => errs.push(format!("<ERR! {}> Unepected I/O error encountered trying to create or open the file for writing: {}", ofn.display(), e))
        }
    }

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