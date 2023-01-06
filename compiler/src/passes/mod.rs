use chumsky::prelude::Simple;

mod generate_symbols;
pub mod shared;
mod validate_ast;

use crate::parser::Program;

use self::generate_symbols::generate_symbols;
use self::validate_ast::validate_ast;

pub fn transform_ast(program: Program) -> Result<Program, Vec<Simple<String>>> {
    validate_ast(program).and_then(generate_symbols)
}
