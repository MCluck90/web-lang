use crate::errors::CompilerError;

pub use super::ast::{
    Block, EnvironmentType, Expression, ExpressionKind, Identifier, Import, ImportKind,
    ImportSelector, ImportSelectorKind, ModuleItem, ModuleItemKind, Parameter, Statement,
    StatementKind,
};

use super::ast;

pub struct Module {
    pub path: String,
    pub ast: Option<ModuleAST>,
    pub errors: Vec<CompilerError>,
}
impl From<ast::Module> for Module {
    fn from(module: ast::Module) -> Self {
        Module {
            path: module.path,
            errors: module.errors,
            ast: module.ast.map(ModuleAST::from),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ModuleAST {
    pub path: String,
    pub imports: Vec<ast::Import>,
    pub items: Vec<ast::ModuleItem>,
    pub exports: Vec<ast::Identifier>,
}
impl From<ast::ModuleAST> for ModuleAST {
    fn from(module_ast: ast::ModuleAST) -> Self {
        ModuleAST {
            path: module_ast.path,
            imports: module_ast.imports,
            items: module_ast.items,
            exports: Vec::new(),
        }
    }
}
