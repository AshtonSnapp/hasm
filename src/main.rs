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

    // Infile set as required in cli.yml, so there's no need to check if it's present.
    // If it wasn't present, the program would have exited already.
    let infile_name = Path::new(matches.value_of("INPUT").unwrap());

    // Outfile name is also required.
    let outfile_name = Path::new(matches.value_of("output").unwrap());

    // Mode isn't techincally required, but it has a default value of "full".
    let mode = matches.value_of("mode").unwrap();

    // Let's open the file.
    let file = File::open(infile_name);

    // Let's check if the file actually opened.
    match file {

        // The file did indeed open!
        Ok(f) => {
            if mode == "full" || mode == "lex" {
                let poss_tokens = lex::tok(f, verbose);
                match poss_tokens {
                    Ok(t) => {
                        if mode == "lex" {
                            let outfile = File::create(outfile_name);

                            match outfile {
                                Ok(of) => {
                                    if let Ok(_t) = serde_json::to_writer_pretty(of, &t) {
                                        if verbose {
                                            println!("[INFO] Successfully wrote JSON to outfile.");
                                        }
                                    } else {
                                        eprintln!("[ERR!] Failed to write JSON to outfile.");
                                        exit(1);
                                    }
                                },
                                Err(_e) => {
                                    eprintln!("[ERR!] Failed to create or access outfile.");
                                    exit(1);
                                }
                            }
                        }
                    },
                    Err(e) => {
                        if verbose {
                            exit(1);
                        } else {
                            for err in &e {
                                eprintln!("{}", err);
                            }

                            eprintln!("[INFO] {} errors encountered.", e.len());

                            exit(1);
                        }
                    }
                }
            } else if mode == "parse" {
                eprintln!("[ERR!] Parser not currently implemented.");
                exit(1);
            } else {
                eprintln!("[ERR!] Invalid mode parameter.");
                exit(1);
            }
        },

        // The file didn't actually open, crap!
        Err(_e) => {
            eprintln!("[ERR!] Cannot open input file. Please check to make sure that the file actually exists.");
            exit(1);
        }
    }
}
