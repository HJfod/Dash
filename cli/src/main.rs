
use clap::Parser;
use dash_compiler::{
    shared::{logger::Logger, src::ArcSpan},
    shared::src::SrcPool,
    parser::parse::Parse,
    tokenize,
    checker::pool::ASTPool,
    // check_coherency
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
    no_ast: bool,

    #[clap(long)]
    debug_ast: bool,

    #[clap(long)]
    debug_log_matches: bool,
}

fn main() {
    let args = Args::parse();
    let cur_dir = std::env::current_dir().expect("Unable to get current directory");

    let logger = Logger::default();
    let src_dir = args.dir.map(|d| cur_dir.join(d).normalize()).unwrap_or(cur_dir);
    let src_pool = SrcPool::new_from_dir(src_dir).expect("Unable to find sources");
    
    if args.debug_tokens {
        for src in &src_pool {
            println!(":: Tokens for {src} ::");
            for t in tokenize(src.as_ref(), logger.clone()) {
                println!("{t:#?}");
            }
        }
    }
    if args.no_ast {
        return;
    }
    let ast_pool = ASTPool::parse_src_pool(
        &src_pool, logger.clone(),
    );

    if args.debug_ast {
        for ast in &ast_pool {
            println!("AST for {}", ast.span().unwrap_or(ArcSpan::builtin()).0);
            println!("{ast:#?}");
        }
    }

    for ast in &mut ast_pool {
        check_coherency(ast, logger.clone());
    }

    let ref_logger = logger.lock().unwrap();
    println!(
        "Finished with {} errors and {} warnings",
        ref_logger.errors(),
        ref_logger.warnings()
    );
    
    if ref_logger.errors() > 0 {
        std::process::exit(1);
    }
}
