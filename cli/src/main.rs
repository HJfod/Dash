
use clap::Parser;
use dash_compiler_v2::{logger::Logger, src::SrcPool, default_grammar, tokenize, ast::ASTPool};
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
}

fn main() {
    let args = Args::parse();
    let cur_dir = std::env::current_dir().expect("Unable to get current directory");

    let logger = Logger::default();
    let src_dir = args.dir.map(|d| cur_dir.join(d).normalize()).unwrap_or(cur_dir);
    let src_pool = SrcPool::new_from_dir(src_dir).expect("Unable to find sources");
    let grammar = default_grammar();
    
    if args.debug_tokens {
        for src in &src_pool {
            println!("Tokens for {src}");
            for t in tokenize(src.as_ref(), &grammar, logger.clone()) {
                println!(" . {t:#?}");
            }
        }
    }
    if args.no_ast {
        return;
    }
    let ast_pool = ASTPool::parse_src_pool(&src_pool, &grammar, logger.clone());

    if args.debug_ast {
        for ast in &ast_pool {
            println!("AST for {}", ast.span().0);
            println!("{ast:#?}");
        }
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
