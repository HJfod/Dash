use clap::Parser;
use gdml_compiler::{shared::src::Src, compiler::typecheck::{TypeChecker, TypeCheck}, parser::stream::{TokenStream, SrcReader, Token}};
use std::path::PathBuf;

/// Simple program to greet a person
#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
    /// Files to compile
    files: Vec<PathBuf>,

    #[clap(long)]
    debug_tokens: bool,

    #[clap(long)]
    debug_ast: bool,
}

fn main() {
    let args = Args::parse();
    if args.files.is_empty() {
        println!("No files provided");
    }
    for file in args.files {
        match Src::from_file(&file) {
            Ok(src) => {
                if args.debug_tokens {
                    let mut stream = TokenStream::new(&src, SrcReader::new(&src));
                    while match stream.next() {
                        Token::EOF(_, _) => false,
                        token => {
                            println!("Token: {token}");
                            true
                        }
                    } {}
                }
                match src.parse() {
                    Ok(ast) => {
                        if args.debug_ast {
                            println!("AST for {}: {ast:#?}", src.name());
                        }
                        let mut checker = TypeChecker::new();
                        ast.typecheck(&mut checker);
                    }
                    Err(e) => {
                        println!("Unable to parse src {}: {e}", src.name());
                    }
                }
            }
            Err(e) => {
                println!("Unable to parse src {}: {e}", file.display());
            }
        }
    }
}
