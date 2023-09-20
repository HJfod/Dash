use clap::{Parser, builder::OsStr};
use gs_compiler::{
    shared::{src::Src, logging::ConsoleLogger},
    compiler::typecheck::{TypeChecker, TypeCheck},
    parser::stream::{TokenStream, SrcReader, Token}
};
use std::{path::PathBuf, fs::FileType, process::exit};

/// Simple program to greet a person
#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
    /// Project directory. Uses current working directory if not provided
    dir: Option<PathBuf>,

    #[clap(long)]
    debug_tokens: bool,

    #[clap(long)]
    debug_ast: bool,
}

fn find_src_files(dir: PathBuf) -> Vec<PathBuf> {
    let mut res = vec![];
    if let Ok(entries) = std::fs::read_dir(dir) { 
        for entry in entries {
            let file = entry.unwrap();
            if let Ok(ty) = file.file_type() {
                if ty.is_dir() {
                    res.extend(find_src_files(file.path()));
                }
                else if file.path().extension() == Some(&OsStr::from("gs")) {
                    res.push(file.path());
                }
            }
        }
    }
    res
}

fn main() {
    let args = Args::parse();
    let files = find_src_files(args.dir.unwrap_or(std::env::current_dir().unwrap()));
    if files.is_empty() {
        println!("Error: No source files found");
        exit(1);
    }
    for file in files {
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
