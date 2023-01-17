use core::fmt;

use crate::{
    errors::CompilerError,
    lexer::{BinaryOperator, Span},
    passes::shared::Type,
};

use super::source;

pub struct Module {
    pub path: String,
    pub ast: ModuleAST,
    pub errors: Vec<CompilerError>,
}

/// An AST that contains information from the name resolution stage.
pub struct ModuleAST {
    pub path: String,
    pub imports: Vec<Import>,
    pub statements: Vec<Statement>,
}

pub struct Import {
    pub span: Span,
    pub kind: ImportKind,
}

pub enum ImportKind {
    Package {
        scope: Identifier,
        package: Identifier,
        selectors: Vec<ImportSelector>,
    },
}

pub struct ImportSelector {
    pub span: Span,
    pub kind: ImportSelectorKind,
}

pub enum ImportSelectorKind {
    Name(String),
    // TODO: Aliased { original: String, alias: String },
    // TODO: All(String), // Alias
}

pub struct Statement {
    pub span: Span,
    pub kind: StatementKind,
}

pub enum StatementKind {
    FunctionDefinition {
        name: Identifier,
        parameters: Vec<Parameter>,
        return_type: Type,
        body: Box<Expression>,
    },
    Expression(Expression),
    JsBlock(Vec<Expression>),
    Return(Option<Expression>),
}

pub struct Parameter {
    pub span: Span,
    pub identifier: Identifier,
    pub type_: Type,
}

pub struct Expression {
    pub kind: ExpressionKind,
    pub span: Span,
}

pub enum ExpressionKind {
    // Primitives
    Boolean(bool),
    Identifier(Identifier),
    Integer(i64),
    String(String),
    Block(Box<Block>),

    // Ex: `Todo { title: "Write a compiler" }`
    // ObjectLiteral(String, HashMap<String, Expression>),
    VariableDeclaration {
        is_mutable: bool,
        identifier: Identifier,
        initializer: Box<Expression>,
    },

    // UnaryExpression(Operator, Box<Expression>),
    BinaryExpression(Box<Expression>, BinaryOperator, Box<Expression>),
    PropertyAccess(Box<Expression>, Identifier),
    FunctionCall {
        callee: Box<Expression>,
        arguments: Vec<Expression>,
    },
    // AnonymousFunction {
    //     parameters: Vec<Parameter>,
    //     body: Box<Expression>,
    // },
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
    pub fn from_source(identifier: &source::Identifier, new_name: String) -> Identifier {
        Identifier {
            name: new_name,
            original_name: identifier.name.clone(),
            span: identifier.span.clone(),
        }
    }
}

pub struct Block {
    pub span: Span,
    pub statements: Vec<Statement>,
    pub return_expression: Option<Expression>,
}
