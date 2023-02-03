use core::fmt;

use crate::{
    errors::CompilerError,
    phases::{frontend::lexer::Span, shared::Type},
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
    pub statements: Vec<TopLevelStatement>,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Import {
    pub span: Span,
    pub kind: ImportKind,
}
impl Import {
    pub fn to_path(&self) -> String {
        match &self.kind {
            ImportKind::Package {
                scope,
                package,
                path,
                selectors: _,
            } => {
                let inner_path = path
                    .iter()
                    .map(|i| i.name.clone())
                    .collect::<Vec<_>>()
                    .join("/");
                if inner_path.is_empty() {
                    format!("./{}/{}.nux", scope, package)
                } else {
                    format!("./{}/{}/{}.nux", scope, package, inner_path)
                }
                .to_string()
            }
        }
    }
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
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TopLevelStatement {
    pub span: Span,
    pub kind: TopLevelStatementKind,
}

#[derive(Clone, Debug, PartialEq, Eq)]
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
        body: Block,
    },
    Expression(Expression),
    Loop(Block),
    ForLoop {
        initializer: Option<Box<Statement>>,
        condition: Option<Expression>,
        post_loop: Option<Expression>,
        body: Vec<Statement>,
    },
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
    pub span: Span,
    pub kind: ExpressionKind,
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
    List(Vec<Expression>),

    BinaryExpression(Box<Expression>, BinaryOperator, Box<Expression>),
    PreUnaryExpression(PreUnaryOperator, Box<Expression>),
    PropertyAccess(Box<Expression>, Identifier),
    ArrayAccess(Box<Expression>, Box<Expression>),
    FunctionCall {
        callee: Box<Expression>,
        arguments: Vec<Expression>,
    },
    JsBlock(Type, Vec<Expression>),
    If {
        condition: Box<Expression>,
        body: Box<Expression>,
        else_: Option<Box<Expression>>,
    },

    Error,
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
        body: Block,
    },
    Expression(Expression),
    Return(Option<Expression>),
    Loop(Block),
    ForLoop {
        initializer: Option<Box<Statement>>,
        condition: Option<Expression>,
        post_loop: Option<Expression>,
        body: Vec<Statement>,
    },
    Break,
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

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum BinaryOperator {
    Add,
    Sub,
    Mul,
    Div,
    Modulus,
    NotEqual,
    Equal,
    LessThan,
    LessThanOrEqual,
    GreaterThan,
    GreaterThanOrEqual,
    And,
    Or,
    Assignment,
}

impl fmt::Display for BinaryOperator {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            BinaryOperator::Add => write!(f, "+"),
            BinaryOperator::Sub => write!(f, "-"),
            BinaryOperator::Mul => write!(f, "*"),
            BinaryOperator::Div => write!(f, "/"),
            BinaryOperator::Modulus => write!(f, "%"),
            BinaryOperator::NotEqual => write!(f, "!="),
            BinaryOperator::Equal => write!(f, "=="),
            BinaryOperator::LessThan => write!(f, "<"),
            BinaryOperator::LessThanOrEqual => write!(f, "<="),
            BinaryOperator::GreaterThan => write!(f, ">"),
            BinaryOperator::GreaterThanOrEqual => write!(f, ">="),
            BinaryOperator::And => write!(f, "&&"),
            BinaryOperator::Or => write!(f, "||"),
            BinaryOperator::Assignment => write!(f, "="),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum PreUnaryOperator {
    Not,
    Increment,
    Decrement,
}

impl fmt::Display for PreUnaryOperator {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            PreUnaryOperator::Not => write!(f, "!"),
            PreUnaryOperator::Increment => write!(f, "++"),
            PreUnaryOperator::Decrement => write!(f, "--"),
        }
    }
}
