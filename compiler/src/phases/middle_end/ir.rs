use core::fmt;

use crate::{
    errors::CompilerError,
    phases::{
        frontend::Span,
        frontend::{
            self,
            ir::{BinaryOperator, EnvironmentType, PreUnaryOperator},
        },
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
    pub path: String,
    pub selectors: Vec<ImportSelector>,
}
impl Import {
    pub fn from_source(import: &frontend::ir::Import) -> Self {
        Import {
            span: import.span.clone(),
            path: import.to_path(),
            selectors: match &import.kind {
                frontend::ir::ImportKind::Package {
                    scope: _,
                    package: _,
                    path: _,
                    selectors,
                } => selectors
                    .iter()
                    .map(ImportSelector::from_source)
                    .collect::<Vec<_>>(),
            },
        }
    }

    pub fn to_identifiers(&self) -> Vec<(String, Span)> {
        self.selectors
            .iter()
            .map(|selector| match &selector.kind {
                ImportSelectorKind::Name(name) => (name.clone(), selector.span.clone()),
            })
            .collect()
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
        type_: Option<Type>,
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
    Loop(Vec<Statement>),
    ForLoop {
        initializer: Option<Statement>,
        condition: Option<Expression>,
        post_loop: Option<Expression>,
        body: Vec<Statement>,
    },
    EnvironmentBlock(EnvironmentType, Vec<Statement>),
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
        type_: Option<Type>,
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
    Loop(Vec<Statement>),
    ForLoop {
        initializer: Option<Box<Statement>>,
        condition: Option<Box<Expression>>,
        post_loop: Option<Box<Expression>>,
        body: Vec<Statement>,
    },
    Break,
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
    List(Vec<Expression>),

    Parenthesized(Box<Expression>),

    BinaryExpression(Box<Expression>, BinaryOperator, Box<Expression>),
    PreUnaryExpression(PreUnaryOperator, Box<Expression>),
    PropertyAccess(Box<Expression>, Identifier),
    ArrayAccess(Box<Expression>, Box<Expression>),
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
