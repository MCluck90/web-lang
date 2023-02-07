mod ast;
pub mod ir;
pub mod lexer;
mod parser;

use std::{
    collections::{HashMap, HashSet},
    path::Path,
};

pub use lexer::{BuiltInTypeToken, Span, Token};

use crate::errors::{print_error_report, CompilerError};

use self::parser::module_parser;

pub fn run_frontend(file_path: &str) -> Result<(Program, bool), String> {
    Program::from_entry_point(file_path.to_string())
}

type ModulePath = String;
pub struct Program {
    // Collection of module paths.
    // Contents of the module can be found in `module_by_path`
    pub modules_in_order: Vec<ModulePath>,
    pub module_by_path: HashMap<ModulePath, ir::Module>,
}

/// Used when handling importing modules.
/// Allows us to determine who asked for a module to be imported so we can report errors correctly
struct ModuleContext {
    pub module_to_parse: ModulePath,
    pub module_who_imported: ModulePath,
    pub span_of_import: Span,
}

impl Program {
    pub fn from_entry_point(entry_path: String) -> Result<(Self, bool), String> {
        let path = Path::new(&entry_path);
        if !path.exists() {
            return Err(format!("Could not find file: {}", entry_path));
        }

        let mut module_paths: Vec<ModuleContext> = vec![ModuleContext {
            module_to_parse: entry_path.clone(),
            module_who_imported: "".into(),
            span_of_import: 0..0,
        }];
        let mut visited_modules = HashSet::<String>::new();
        let mut has_errors = false;
        let mut program = Program {
            modules_in_order: Vec::new(),
            module_by_path: HashMap::new(),
        };

        while !module_paths.is_empty() {
            let context = module_paths.pop().unwrap();
            let module_path = context.module_to_parse;
            let requester_path = context.module_who_imported;
            let import_span = context.span_of_import;
            match program.parse_module(
                std::path::Path::new(&module_path),
                &requester_path,
                &import_span,
            ) {
                Ok(mut module) => {
                    visited_modules.insert(module_path.clone());
                    program.modules_in_order.push(module_path.clone());
                    if !module.errors.is_empty() {
                        has_errors = true;
                        print_error_report(&module_path, &module.errors);
                        // Empty out the errors since these have already been reported
                        module.errors.clear();
                    }

                    match &module.ast {
                        Some(ast) => {
                            for import in ast.imports.iter().rev() {
                                let next_module_path = import.to_path();
                                if !visited_modules.contains(&next_module_path) {
                                    module_paths.push(ModuleContext {
                                        module_to_parse: next_module_path,
                                        module_who_imported: module_path.clone(),
                                        span_of_import: import.span.clone(),
                                    });
                                }
                            }
                        }
                        None => {}
                    };
                    program.module_by_path.insert(module_path, module.into());
                }
                Err((path, err)) => {
                    print_error_report(&path, &vec![err]);
                }
            }
        }

        program.modules_in_order.reverse();

        Ok((program, has_errors))
    }

    fn parse_module(
        &self,
        file_path: &Path,
        requester_path: &String,
        import_span: &Span,
    ) -> Result<ast::Module, (String, CompilerError)> {
        if !file_path.exists() {
            return Err((
                requester_path.clone(),
                CompilerError::could_not_find_module(import_span, &file_path.display().to_string()),
            ));
        }

        let file_name = file_path.file_stem().unwrap().to_str().unwrap().to_string();
        let src = std::fs::read_to_string(&file_path).unwrap();
        let lexer = lexer::create_lexer(src.as_str());
        let (ast, errors) = module_parser(file_name, lexer);

        Ok(ast::Module {
            path: file_path.to_str().unwrap().to_string(),
            ast: Some(ast), // TODO: Change type to not be an Option
            errors,
        })
    }
}
