
use clap::Parser;
use dash_compiler::{
    shared::logger::Logger,
    shared::src::SrcPool,
    parser::parse::{Node, NodeList},
    tokenize,
    checker::pool::ASTPool, check_coherency,
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
    let mut node_list = NodeList::new();
    let mut ast_pool = ASTPool::parse_src_pool(&mut node_list, &src_pool, logger.clone());

    if args.debug_ast {
        for ast in &ast_pool {
            println!("AST for {}", ast.span_or_builtin(&node_list).0);
            println!("{ast:#?}");
        }
    }

    for ast in &mut ast_pool {
        check_coherency(ast, &mut node_list, logger.clone());
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
