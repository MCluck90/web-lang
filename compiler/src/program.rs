use std::{
    collections::{HashMap, HashSet},
    path::Path,
};

use chumsky::{prelude::*, Stream};

use crate::{
    asts::source::{ImportKind, Module},
    errors::{print_error_report, CompilerError},
    lexer,
    parser::module_parser,
};

pub struct Program {
    // Collection of module paths.
    // Contents of the module can be found in `module_by_path`
    // TODO: Expose the modules in the order that they were imported
    pub modules_in_order: Vec<String>,
    pub module_by_path: HashMap<String, Module>,
}

impl Program {
    pub fn from_entry_point(entry_path: String) -> (Program, bool) {
        let mut module_paths = vec![entry_path.clone()];
        let mut visited_modules = HashSet::<String>::new();
        let mut program = Program {
            modules_in_order: Vec::new(),
            module_by_path: HashMap::new(),
        };
        let mut has_errors = false;

        while !module_paths.is_empty() {
            let module_path = module_paths.pop().unwrap();
            let module = program.parse_module(std::path::Path::new(&module_path));
            visited_modules.insert(module_path.clone());
            program.modules_in_order.push(module_path.clone());
            if !module.errors.is_empty() {
                has_errors = true;
                print_error_report(&module.path, &module.errors);
            }

            match &module.ast {
                Some(ast) => {
                    for import in ast.imports.iter().rev() {
                        match &import.kind {
                            ImportKind::Package {
                                scope,
                                package,
                                path,
                                ..
                            } => {
                                let inner_path = path
                                    .iter()
                                    .map(|ident| ident.name.clone())
                                    .collect::<Vec<_>>()
                                    .join("/");
                                let next_module_path = if inner_path.is_empty() {
                                    format!("./{}/{}.nux", scope.name, package.name)
                                } else {
                                    format!("./{}/{}/{}.nux", scope.name, package.name, inner_path)
                                };
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

    fn parse_module(&self, file_path: &Path) -> Module {
        let file_name = file_path.file_stem().unwrap().to_str().unwrap().to_string();
        let src = std::fs::read_to_string(&file_path).unwrap();
        let (tokens, lex_errs) = lexer().parse_recovery(src.as_str());
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

        Module {
            path: file_path.to_str().unwrap().to_string(),
            ast,
            errors,
        }
    }
}
