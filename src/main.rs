extern crate clap;
use clap::*;
use std::path::Path;

mod lex;

fn main() {
    let yaml = load_yaml!("cli.yml");
    let matches = clap::App::from_yaml(yaml).get_matches();
}
