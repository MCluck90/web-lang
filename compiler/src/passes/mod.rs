use chumsky::prelude::Simple;

mod generate_symbols;
pub mod name_resolution;
pub mod shared;
mod type_inference;
mod validate_ast;

use crate::asts::source::ModuleAST;

use self::generate_symbols::generate_symbols;
use self::type_inference::infer_types;
use self::validate_ast::validate_ast;

pub fn transform_ast(program: ModuleAST) -> Result<ModuleAST, Vec<Simple<String>>> {
    generate_symbols(program)
        .and_then(validate_ast)
        .and_then(infer_types)
        .map(|(program, _)| program)
}
