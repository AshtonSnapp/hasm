mod lex;
use clap::*;
use std::path::Path;
use std::fs::File;
use std::process::exit;

fn main() {

    // Load cli.yml and use it to do command-line matches
    let yaml = load_yaml!("cli.yml");
    let matches = App::from_yaml(yaml).get_matches();
    let verbose: bool;

    // Check for verbose mode
    if matches.is_present("verbose") {
        verbose = true;
    } else {
        verbose = false;
    }

    // Check for custom outfile name
    if matches.is_present("output") {
        let outfile_name = Path::new(matches.value_of("output").unwrap());
    }

    // Infile set as required in cli.yml, so there's no need to check if it's present.
    // If it wasn't present, the program would have exited already.
    let infile_name = Path::new(matches.value_of("INPUT").unwrap());

    let file = File::open(infile_name);
    match file {
        Ok(&mut f) => {
            let poss_tokens = lex::tok(f, verbose);
            match poss_tokens {
                Ok(t) => {},
                Err(e) => {
                    if verbose {
                        exit(1);
                    } else {
                        for err in e {
                            eprintln!("{}", err);
                        }
                        exit(1);
                    }
                }
            }
        },
        Err(e) => {
            eprintln!("[ERR] Cannot open input file. Please check to make sure that the file exists!");
            exit(1);
        }
    }
}
