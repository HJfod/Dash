use clap::Parser;
use gs_compiler::{
    shared::{src::SrcPool, logging::ConsoleLogger},
    compiler::{typecheck::TypeVisitor, pool::ASTPool, visitor::Visitors},
    parser::{stream::Token, node::ASTNode}
};
use normalize_path::NormalizePath;
use std::path::PathBuf;

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

fn main() {
    let args = Args::parse();
    let cur_dir = std::env::current_dir().expect("Unable to get current directory");
    let src_pool = SrcPool::new_from_dir(
        args.dir
            .map(|d| cur_dir.join(d).normalize())
            .unwrap_or(cur_dir)
    ).expect("Unable to find sources");
    
    let logger = ConsoleLogger::new();
    if args.debug_tokens {
        for src in &src_pool {
            println!("Tokens for {src}");
            let mut stream = src.tokenize(logger.clone());
            while match stream.next() {
                Token::EOF(_, _) => false,
                token => {
                    println!(" + {token}");
                    true
                }
            } {}
        }
    }
    let ast_pool = ASTPool::parse_src_pool(&src_pool, logger.clone());

    if args.debug_ast {
        for ast in &ast_pool {
            println!("AST for {}", ast.src());
            println!("{ast:#?}");
        }
    }

    let mut visitor = TypeVisitor::new(logger.clone());
    for ast in &ast_pool {
        ast.visit_coherency(&mut visitor);
    }

    let ref_logger = logger.lock().unwrap();
    println!(
        "Finished with {} errors and {} warnings",
        ref_logger.error_count(),
        ref_logger.warn_count()
    );
    
    if ref_logger.error_count() > 0 {
        std::process::exit(1);
    }
}
