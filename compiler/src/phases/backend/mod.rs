use self::code_gen::{generate_code, CodeGenOutput};

use super::middle_end::Program;

mod code_gen;
mod ir;

pub fn run_backend(program: Program) -> CodeGenOutput {
    let statements = ir::from_middle_end(program);
    generate_code(statements)
}
