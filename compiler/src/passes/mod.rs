use chumsky::prelude::Simple;

mod generate_symbols;
pub mod shared;
mod type_inference;
mod validate_ast;

use crate::parser::Program;

use self::generate_symbols::generate_symbols;
use self::type_inference::infer_types;
use self::validate_ast::validate_ast;

pub fn transform_ast(program: Program) -> Result<Program, Vec<Simple<String>>> {
    generate_symbols(program)
        .and_then(validate_ast)
        .and_then(infer_types)
        .map(|(program, _)| program)
}
