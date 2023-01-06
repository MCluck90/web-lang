use crate::parser::Program;

use self::generate_symbols::{generate_symbols, Context};

mod generate_symbols;

pub fn transform_ast(program: &Program) -> (Program, Context) {
    generate_symbols(program)
}
