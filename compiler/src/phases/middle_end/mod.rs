use crate::errors::print_error_report;

use self::{name_resolution::resolve_names, type_checking::check_types};

use super::frontend;

pub mod ir;
mod name_resolution;
mod type_checking;

pub fn run_middle_end(program: frontend::Program) -> (Program, bool) {
    let (mut modules, mut symbol_table) = resolve_names(
        program
            .modules_in_order
            .iter()
            .map(|path| program.module_by_path.get(path).unwrap())
            .collect(),
    );

    let mut has_errors = false;
    for module in &mut modules {
        if !module.errors.is_empty() {
            has_errors = true;
            print_error_report(&module.path, &module.errors);
            // Empty out the errors since these have already been reported
            module.errors.clear();
        }
    }

    check_types(&mut modules, &mut symbol_table);
    for module in &mut modules {
        if !module.errors.is_empty() {
            has_errors = true;
            print_error_report(&module.path, &module.errors);
            // Empty out the errors since these have already been reported
            module.errors.clear();
        }
    }

    (Program { modules }, has_errors)
}

pub struct Program {
    pub modules: Vec<ir::Module>,
}
