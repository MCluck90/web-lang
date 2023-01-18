pub mod ast;
pub mod lexer;
mod parser;

use std::{
    collections::{HashMap, HashSet},
    path::Path,
};

use chumsky::{Parser, Stream};
pub use lexer::{BinaryOperator, BuiltInTypeToken, Span, Token};

use crate::{
    errors::{print_error_report, CompilerError},
    module_paths::from_package_import,
};

use self::parser::module_parser;

pub fn run_frontend(file_path: &str) -> (Program, bool) {
    Program::from_entry_point(file_path.to_string())
}

type ModulePath = String;
pub struct Program {
    // Collection of module paths.
    // Contents of the module can be found in `module_by_path`
    pub modules_in_order: Vec<ModulePath>,
    pub module_by_path: HashMap<ModulePath, ast::Module>,
}

impl Program {
    pub fn from_entry_point(entry_path: String) -> (Self, bool) {
        let mut module_paths = vec![entry_path.clone()];
        let mut visited_modules = HashSet::<String>::new();
        let mut has_errors = false;
        let mut program = Program {
            modules_in_order: Vec::new(),
            module_by_path: HashMap::new(),
        };

        while !module_paths.is_empty() {
            let module_path = module_paths.pop().unwrap();
            let mut module = program.parse_module(std::path::Path::new(&module_path));
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
                        match &import.kind {
                            ast::ImportKind::Package {
                                scope,
                                package,
                                path,
                                ..
                            } => {
                                let next_module_path = from_package_import(
                                    &scope.name,
                                    &package.name,
                                    &path.iter().map(|ident| ident.name.clone()).collect(),
                                );
                                if !visited_modules.contains(&next_module_path) {
                                    module_paths.push(next_module_path);
                                }
                            }
                        }
                    }
                }
                None => {}
            };
            program.module_by_path.insert(module_path, module);
        }

        program.modules_in_order.reverse();

        (program, has_errors)
    }

    fn parse_module(&self, file_path: &Path) -> ast::Module {
        let file_name = file_path.file_stem().unwrap().to_str().unwrap().to_string();
        let src = std::fs::read_to_string(&file_path).unwrap();
        let (tokens, lex_errs) = lexer::lexer().parse_recovery(src.as_str());
        let mut errors = lex_errs
            .clone()
            .into_iter()
            .map(|e| e.map(|c| c.to_string()))
            .collect::<Vec<CompilerError>>();

        let ast = tokens.and_then(|tokens| {
            let len = src.chars().count();
            let (module, parse_errs) = module_parser(file_name)
                .parse_recovery(Stream::from_iter(len..len + 1, tokens.into_iter()));

            let mut parse_errs = parse_errs
                .into_iter()
                .map(|e| e.map(|token| token.to_string()))
                .collect::<Vec<CompilerError>>();

            errors.append(&mut parse_errs);
            module
        });

        ast::Module {
            path: file_path.to_str().unwrap().to_string(),
            ast,
            errors,
        }
    }
}
