use chumsky::prelude::Simple;

use crate::parser::Program;

use self::generate_symbols::generate_symbols;

mod generate_symbols;

pub fn transform_ast(program: Program) -> Result<Program, Vec<Simple<String>>> {
    generate_symbols(program)
}
