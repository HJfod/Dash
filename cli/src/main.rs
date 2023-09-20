use clap::Parser;
use gs_compiler::{
    shared::{src::Src, logging::ConsoleLogger},
    compiler::typecheck::{TypeChecker, TypeCheck},
    parser::stream::{TokenStream, SrcReader, Token}
};
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
                let logger = ConsoleLogger::new();
                if args.debug_tokens {
                    let mut stream = TokenStream::new(&src, SrcReader::new(&src, logger.clone()));
                    while match stream.next() {
                        Token::EOF(_, _) => false,
                        token => {
                            println!("Token: {token}");
                            true
                        }
                    } {}
                }
                match src.parse(logger.clone()) {
                    Ok(ast) => {
                        if args.debug_ast {
                            println!("AST for {}: {ast:#?}", src.name());
                        }
                        let mut checker = TypeChecker::new(logger.clone());
                        ast.typecheck(&mut checker);
                        let ref_logger = logger.lock().unwrap();
                        println!(
                            "Finished with {} errors and {} warnings",
                            ref_logger.error_count(),
                            ref_logger.warn_count()
                        );
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
