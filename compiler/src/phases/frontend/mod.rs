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

pub fn run_frontend(file_path: &str) -> Result<(Program, bool), String> {
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
    pub fn from_entry_point(entry_path: String) -> Result<(Self, bool), String> {
        let path = Path::new(&entry_path);
        if !path.exists() {
            return Err(format!("Could not find file: {}", entry_path));
        }

        // TODO: Make this a struct
        // (path_of_module_to_parse, (path_of_module_who_imported, span_of_import_statement))
        let mut module_paths: Vec<(String, (String, Span))> =
            vec![(entry_path.clone(), ("".into(), 0..0))];
        let mut visited_modules = HashSet::<String>::new();
        let mut has_errors = false;
        let mut program = Program {
            modules_in_order: Vec::new(),
            module_by_path: HashMap::new(),
        };

        while !module_paths.is_empty() {
            let (module_path, (requester_path, import_span)) = module_paths.pop().unwrap();
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
                                            module_paths.push((
                                                next_module_path,
                                                (module_path.clone(), import.span.clone()),
                                            ));
                                        }
                                    }
                                }
                            }
                        }
                        None => {}
                    };
                    program.module_by_path.insert(module_path, module);
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
            println!("{} vs. {}", file_path.display(), requester_path);
            return Err((
                requester_path.clone(),
                CompilerError::could_not_find_module(import_span, requester_path),
            ));
        }

        let file_name = file_path.file_stem().unwrap().to_str().unwrap().to_string();
        let src = std::fs::read_to_string(&file_path).unwrap();
        let (tokens, mut errors) = lexer::lexer().parse_recovery(src.as_str());

        let ast = tokens.and_then(|tokens| {
            let len = src.chars().count();
            let (module, mut parse_errs) = module_parser(file_name)
                .parse_recovery(Stream::from_iter(len..len + 1, tokens.into_iter()));

            errors.append(&mut parse_errs);
            module
        });

        Ok(ast::Module {
            path: file_path.to_str().unwrap().to_string(),
            ast,
            errors,
        })
    }
}
