extern crate clap;
use clap::*;
use std::path::Path;
use std::process::exit;

mod lex;

fn main() {
    let yaml = load_yaml!("cli.yml");
    let matches = clap::App::from_yaml(yaml).get_matches();

    match matches.subcommand_name() {
        None => {
            eprintln!("ERR: No sub-command! What am I supposed do do with NOTHING?");
            exit(1);
        }
        Some("assemble") => {
            let asm_matches = matches.subcommand_matches("assemble").unwrap();
            let mut v = false;
            if asm_matches.is_present("verbose") {
                v = true;
            }

            let infile = Path::new(asm_matches.value_of("INPUT").unwrap());
            let spt = usize::from_str_radix(asm_matches.value_of("spt").unwrap(), 10).unwrap();
            let tokens = lex::lex(infile, v, spt);
        }
        _ => {}
    }
}
