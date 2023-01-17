mod asts;
mod code_gen;
mod errors;
mod lexer;
mod parser;
mod passes;
mod program;
mod types;

use std::process;

use errors::print_error_report;
use lexer::lexer;
use program::Program;

fn main() {
    let file_path_arg = std::env::args().nth(1).unwrap();
    let (program, mut has_errors) = Program::from_entry_point(file_path_arg);
    let modules = program
        .modules_in_order
        .iter()
        .map(|path| program.module_by_path.get(path).unwrap())
        .collect();
    let modules = passes::name_resolution::resolve_names(modules);
    for module in modules {
        if !module.errors.is_empty() {
            has_errors = true;
            print_error_report(&module.path, &module.errors);
        }
    }

    if has_errors {
        process::exit(1);
    }
}
