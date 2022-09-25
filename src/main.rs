
#![feature(let_chains)]

use std::path::PathBuf;
use clap::Parser;

use crate::{interface::{run_file, RunOptions}};

pub mod ast;
pub mod parser;
pub mod comptime;
pub mod interface;
pub mod error;
pub mod type_;
pub mod utils;
pub mod output;

#[derive(Parser, Debug)]
#[clap(author, version, about)]
struct Args {
    /// Files to compile
    files: Vec<PathBuf>,

    #[clap(short, long)]
    debug: bool,

    #[clap(short, long)]
    pretty_debug: bool,
}

fn main() {
    let args = Args::parse();

    let mut errors = false;
    for file in args.files {
        match run_file(&file, &RunOptions {
            print_debug: args.debug,
            print_ast: args.debug,
            pretty_print: args.pretty_debug,
            ..Default::default()
        }) {
            Ok(_) => (),
            Err(_) => errors = true
        }
    }

    println!("---\n{}", if errors { "Finished with errors :(" } else { "Finished :)" });
}
