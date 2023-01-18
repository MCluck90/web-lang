use self::code_gen::{generate_code, CodeGenOutput};

use super::middle_end::Program;

mod code_gen;

pub fn run_backend(program: Program) -> CodeGenOutput {
    generate_code(program)
}
