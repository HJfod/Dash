
use std::{path::{PathBuf}};
use crate::{parser::dash_grammar, comptime, error::ErrorCode};
use crate::output::Output;
use crate::error::{LineError};
use crate::utils::File;

pub struct RunOptions {
    pub print_debug: bool,
    pub print_ast: bool,
    pub pretty_print: bool,
    pub channel: Output,
}

impl Default for RunOptions {
    fn default() -> Self {
        RunOptions {
            print_debug: false,
            print_ast: false,
            pretty_print: false,
            channel: Default::default(),
        }
    }
}

pub fn run_file(path: &PathBuf, options: &RunOptions) -> Result<(), ErrorCode> {
    let mut comptime = comptime::SharedComptime::new(&options.channel);

    if options.print_debug {
        println!("Working with {}", path.to_str().unwrap());
    }

    let file = File::new(path, &options.channel)?;

    let ast = dash_grammar::parse(file.data.as_str(), &file, &options.channel)
        .map_err(|e|
            options.channel.print(&LineError::from(&file, &e))
        )?;

    if options.print_ast {
        if options.pretty_print {
            println!("AST: {:#?}", ast);
        } else {
            println!("AST: {:?}", ast);
        }
    }

    let mut file_comptime = comptime.new_file(&file);
    ast.walk(&mut file_comptime)?;

    // ast.eval(&mut runtime).map_err(|e| e.to_error())?;

    Ok(())
}
