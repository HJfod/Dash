use clap::Parser;
use gdml_compiler::{src::Src, compiler::{TypeCheck, TypeChecker}};
use std::path::PathBuf;

/// Simple program to greet a person
#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
    /// Files to compile
    files: Vec<PathBuf>,
}

fn main() {
    let args = Args::parse();
    if args.files.is_empty() {
        println!("No files provided");
    }
    for file in args.files {
        match Src::from_file(&file) {
            Ok(src) => match src.parse() {
                Ok(ast) => {
                    println!("ast: {ast:#?}");
                    let mut checker = TypeChecker::new();
                    ast.typecheck(&mut checker);
                }
                Err(e) => {
                    println!("Unable to parse src {}: {e}", src.name());
                }
            },
            Err(e) => {
                println!("Unable to parse src {}: {e}", file.display());
            }
        }
    }
}
