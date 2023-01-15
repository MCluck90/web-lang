mod asts;
mod code_gen;
mod errors;
mod lexer;
mod parser;
mod passes;
mod program;
mod types;

use std::process;

use lexer::lexer;
use program::Program;

fn main() {
    let file_path_arg = std::env::args().nth(1).unwrap();
    let (program, has_errors) = Program::from_entry_point(file_path_arg);

    if has_errors {
        process::exit(1);
    }
}
