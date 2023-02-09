mod errors;
mod phases;
mod types;

use clap::Parser;
use std::{path::Path, process};

use phases::{backend, frontend, middle_end};

#[derive(Parser)]
#[command(version)]
struct Cli {
    /// Entry point for the application (Ex: main.nux)
    entry_point: String,

    /// Output directory
    #[arg(short, long)]
    output: String,
}

fn main() {
    let cli = Cli::parse();

    match frontend::run_frontend(&cli.entry_point) {
        Ok((program, has_frontend_errors)) => {
            let (program, has_middle_end_errors) = middle_end::run_middle_end(program);
            let has_errors = has_frontend_errors || has_middle_end_errors;
            if has_errors {
                process::exit(1);
            }

            let output_dir = Path::new(&cli.output);
            if !output_dir.exists() {
                std::fs::create_dir(&output_dir).expect(&format!(
                    "Failed to generate output directory: {}",
                    cli.output
                ));
            }
            let output = backend::run_backend(program);
            if let Some(be_js) = output.js {
                let js_path = output_dir.join("main.js");
                std::fs::write(&js_path, be_js)
                    .expect(&format!("Failed to write to {}", js_path.display()));
            }
        }
        Err(error_message) => {
            eprintln!("{}", error_message);
            process::exit(1);
        }
    }
}
