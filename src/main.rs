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
            let poss_tokens = lex::lex(infile, v, spt);
            match poss_tokens {
                Ok(vt) => {
                    todo!("Congratulations, your code was tokenized successfully! Unfortunately, neither the parser nor the tokens-to-JSON converter are implemented yet.");
                }
                Err(ve) => {
                    if v {
                        exit(1);
                    } else {
                        for err in ve {
                            eprintln!("ERR: {}", err);
                        }
                        exit(1);
                    }
                }
            }
        }
        _ => {}
    }
}
