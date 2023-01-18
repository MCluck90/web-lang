mod errors;
mod module_paths;
mod phases;
mod types;

use std::process;

use phases::{backend, frontend, middle_end};

fn main() {
    let file_path_arg = std::env::args().nth(1).unwrap();
    let (program, has_frontend_errors) = frontend::run_frontend(&file_path_arg);
    let (program, has_middle_end_errors) = middle_end::run_middle_end(program);
    let has_errors = has_frontend_errors || has_middle_end_errors;
    if has_errors {
        process::exit(1);
    }

    let output = backend::run_backend(program);
    if let Some(be_js) = output.js {
        println!("{}", be_js);
    }
}
