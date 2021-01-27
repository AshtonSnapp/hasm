extern crate clap;
use clap::*;
use std::path::Path;
use std::fs::File;
use std::process::exit;
use std::io::Write;

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
                    if asm_matches.is_present("tokens-only") {
                        // Is -o set?
                        if let Some(o) = asm_matches.value_of("output") {
                            let mut outfile_name: String = String::from(o);

                            if !outfile_name.contains(".") {
                                outfile_name.push_str("-tokens.json");
                            }

                            let outpath = Path::new(&outfile_name);

                            let out = serde_json::to_vec_pretty(&vt);
                            match out {
                                Ok(t) => {
                                    let outfile = File::create(outpath);
                                    match outfile {
                                        Ok(mut f) => {
                                            let res = f.write(&t);
                                            match res {
                                                Ok(b) => {
                                                    println!("INFO: Successfully written {} bytes to file \"{}\".", b, outfile_name);
                                                }
                                                Err(e) => {
                                                    eprintln!("ERR: Failed to write to file \"{}\".", outfile_name);
                                                    exit(1);
                                                }
                                            }
                                        }
                                        Err(e) => {
                                            eprintln!("ERR: Failed to create or open file \"{}\".", outfile_name);
                                            exit(1);
                                        }
                                    }
                                }
                                Err(e) => {
                                    eprintln!("ERR: Failed to make list of list of tokens into JSON. serde failed us :(");
                                    exit(1);
                                }
                            }
                        } else {}
                    } else {
                        todo!("My apologies, but the parser is not implemented yet. Try using -t. Otherwise, no errors occurred during lexical analysis.");
                    }
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
