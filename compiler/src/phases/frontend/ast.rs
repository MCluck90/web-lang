use core::fmt;

use crate::{
    errors::CompilerError,
    phases::{
        frontend::lexer::{BinaryOperator, Span},
        shared::Type,
    },
};

pub struct Module {
    pub path: String,
    pub ast: Option<ModuleAST>,
    pub errors: Vec<CompilerError>,
}

/// The original source AST.
/// Directly taken from the source code.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ModuleAST {
    pub path: String,
    pub imports: Vec<Import>,
    pub statements: Vec<Statement>,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Import {
    pub span: Span,
    pub kind: ImportKind,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum ImportKind {
    Package {
        scope: Identifier,
        package: Identifier,
        path: Vec<Identifier>,
        selectors: Vec<ImportSelector>,
    },
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct ImportSelector {
    pub span: Span,
    pub kind: ImportSelectorKind,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum ImportSelectorKind {
    Name(String),
    // TODO: Aliased { original: String, alias: String },
    // TODO: All(String), // Alias
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Block {
    pub span: Span,
    pub statements: Vec<Statement>,
    pub return_expression: Option<Expression>,
}
impl From<Block> for Expression {
    fn from(block: Block) -> Self {
        Expression {
            span: block.span.clone(),
            kind: ExpressionKind::Block(Box::new(block)),
        }
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Expression {
    pub kind: ExpressionKind,
    pub span: Span,
}

impl Expression {
    pub fn new(kind: ExpressionKind, span: Span) -> Expression {
        Expression { kind, span }
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum ExpressionKind {
    // Primitives
    Boolean(bool),
    Identifier(Identifier),
    Integer(i64),
    String(String),
    Block(Box<Block>),

    // Ex: `Todo { title: "Write a compiler" }`
    // ObjectLiteral(String, HashMap<String, Expression>),

    // TODO: Lift out in to a statement
    // TODO: It doesn't make sense to allow a variable declaration to be the result for a block, for example
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

    Error,
}

impl ExpressionKind {
    pub fn to_human_readable_name(&self) -> &str {
        match self {
            ExpressionKind::Boolean(_) => "a boolean",
            ExpressionKind::Identifier(_) => "an identifier",
            ExpressionKind::Integer(_) => "an integer",
            ExpressionKind::String(_) => "a string",
            ExpressionKind::Block(_) => "a block",
            ExpressionKind::VariableDeclaration { .. } => "a variable declaration",
            ExpressionKind::BinaryExpression(_, op, _) => match op {
                BinaryOperator::Add => "an addition expression",
                BinaryOperator::Sub => "a subtraction expression",
                BinaryOperator::Mul => "a multiplication expression",
                BinaryOperator::Div => "a division operation",
                BinaryOperator::Dot => "a property access",
                BinaryOperator::NotEqual => "an equality expression",
                BinaryOperator::Equal => "an equality expression",
                BinaryOperator::LessThan => "a comparison expression",
                BinaryOperator::LessThanOrEqual => "a comparison expression",
                BinaryOperator::GreaterThan => "a comparison expression",
                BinaryOperator::GreaterThanOrEqual => "a comparison expression",
                BinaryOperator::And => "a comparison expression",
                BinaryOperator::Or => "a comparison expression",
                BinaryOperator::Assignment => "an assignment expression",
            },
            ExpressionKind::PropertyAccess(_, _) => "a property access",
            ExpressionKind::FunctionCall { .. } => "a function call",
            ExpressionKind::If { .. } => "an if expression",
            ExpressionKind::Error => "an error",
        }
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Parameter {
    pub span: Span,
    pub identifier: Identifier,
    pub type_: Type,
}

impl Parameter {
    pub fn new(span: Span, identifier: Identifier, type_: Type) -> Parameter {
        Parameter {
            span,
            identifier,
            type_,
        }
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Identifier {
    pub name: String,
    pub span: Span,
}
impl fmt::Display for Identifier {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name)
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Statement {
    pub span: Span,
    pub kind: StatementKind,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
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

impl From<Expression> for Statement {
    fn from(expression: Expression) -> Self {
        let clone = expression.clone();
        Statement {
            span: expression.span,
            kind: StatementKind::Expression(clone),
        }
    }
}
