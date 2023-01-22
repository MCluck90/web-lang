use core::fmt;

use crate::{
    errors::CompilerError,
    phases::{
        frontend,
        frontend::lexer::{BinaryOperator, Span},
        shared::Type,
    },
};

pub struct Module {
    pub path: String,
    pub ast: ModuleAST,
    pub errors: Vec<CompilerError>,
}

/// An AST that contains information from the name resolution stage.
#[derive(Clone, Debug)]
pub struct ModuleAST {
    pub path: String,
    pub imports: Vec<Import>,
    pub statements: Vec<TopLevelStatement>,
}

#[derive(Clone, Debug)]
pub struct Import {
    pub span: Span,
    pub kind: ImportKind,
}
impl Import {
    pub fn from_source(import: &frontend::ir::Import) -> Self {
        Import {
            span: import.span.clone(),
            kind: ImportKind::from_source(&import.kind),
        }
    }

    pub fn to_path(&self) -> String {
        match &self.kind {
            ImportKind::Package {
                scope,
                package,
                path,
                selectors: _,
            } => if path.is_empty() {
                format!("./{}/{}.nux", scope, package)
            } else {
                format!("./{}/{}/{}.nux", scope, package, path)
            }
            .to_string(),
        }
    }

    pub fn to_identifiers(&self) -> Vec<(String, Span)> {
        match &self.kind {
            ImportKind::Package {
                scope: _,
                package: _,
                path: _,
                selectors,
            } => selectors
                .iter()
                .map(|selector| match &selector.kind {
                    ImportSelectorKind::Name(name) => (name.clone(), selector.span.clone()),
                })
                .collect(),
        }
    }
}

#[derive(Clone, Debug)]
pub enum ImportKind {
    Package {
        scope: Identifier,
        package: Identifier,
        path: String,
        selectors: Vec<ImportSelector>,
    },
}
impl ImportKind {
    pub fn from_source(kind: &frontend::ir::ImportKind) -> Self {
        match kind {
            frontend::ir::ImportKind::Package {
                scope,
                package,
                path,
                selectors,
            } => ImportKind::Package {
                scope: Identifier::from_source(scope, scope.name.clone()),
                package: Identifier::from_source(package, package.name.clone()),
                path: path
                    .iter()
                    .map(|i| i.name.clone())
                    .collect::<Vec<_>>()
                    .join("/"),
                selectors: selectors.iter().map(ImportSelector::from_source).collect(),
            },
        }
    }
}

#[derive(Clone, Debug)]
pub struct ImportSelector {
    pub span: Span,
    pub kind: ImportSelectorKind,
}
impl ImportSelector {
    pub fn from_source(selector: &frontend::ir::ImportSelector) -> Self {
        ImportSelector {
            span: selector.span.clone(),
            kind: match &selector.kind {
                frontend::ir::ImportSelectorKind::Name(name) => {
                    ImportSelectorKind::Name(name.clone())
                }
            },
        }
    }
}

#[derive(Clone, Debug)]
pub enum ImportSelectorKind {
    Name(String),
}

#[derive(Clone, Debug)]
pub struct TopLevelStatement {
    pub span: Span,
    pub kind: TopLevelStatementKind,
}

#[derive(Clone, Debug)]
pub enum TopLevelStatementKind {
    VariableDeclaration {
        is_public: bool,
        is_mutable: bool,
        identifier: Identifier,
        initializer: Box<Expression>,
    },
    FunctionDefinition {
        is_public: bool,
        name: Identifier,
        parameters: Vec<Parameter>,
        return_type: Type,
        body: Vec<Statement>,
    },
    Expression(Expression),
}

#[derive(Clone, Debug)]
pub struct Statement {
    pub span: Span,
    pub kind: StatementKind,
}

#[derive(Clone, Debug)]
pub enum StatementKind {
    VariableDeclaration {
        is_mutable: bool,
        identifier: Identifier,
        initializer: Box<Expression>,
    },
    FunctionDefinition {
        name: Identifier,
        parameters: Vec<Parameter>,
        return_type: Type,
        body: Vec<Statement>,
    },
    Expression(Expression),
    Return(Option<Expression>),
}

#[derive(Clone, Debug)]
pub struct Parameter {
    pub span: Span,
    pub identifier: Identifier,
    pub type_: Type,
}

#[derive(Clone, Debug)]
pub struct Expression {
    pub kind: ExpressionKind,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub enum ExpressionKind {
    // Primitives
    Boolean(bool),
    Identifier(Identifier),
    Integer(i64),
    String(String),
    Block(Box<Block>),
    JsBlock(Type, Vec<Expression>),

    BinaryExpression(Box<Expression>, BinaryOperator, Box<Expression>),
    PropertyAccess(Box<Expression>, Identifier),
    FunctionCall {
        callee: Box<Expression>,
        arguments: Vec<Expression>,
    },
    If {
        condition: Box<Expression>,
        body: Box<Expression>,
        else_: Option<Box<Expression>>,
    },
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Identifier {
    pub name: String,
    pub original_name: String,
    pub span: Span,
}
impl fmt::Display for Identifier {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name)
    }
}
impl Identifier {
    pub fn from_source(identifier: &frontend::ir::Identifier, new_name: String) -> Identifier {
        Identifier {
            name: new_name,
            original_name: identifier.name.clone(),
            span: identifier.span.clone(),
        }
    }
}

#[derive(Clone, Debug)]
pub struct Block {
    pub span: Span,
    pub statements: Vec<Statement>,
    pub return_expression: Option<Expression>,
}
