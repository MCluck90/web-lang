use core::fmt;

use crate::{
    lexer::*,
    passes::shared::{NodeId, Type, DUMMY_NODE_ID},
};
use chumsky::prelude::*;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Program {
    pub statements: Vec<Statement>,
}

pub fn main_parser() -> impl Parser<Token, Program, Error = Simple<Token>> + Clone {
    statement_parser()
        .repeated()
        .then_ignore(end())
        .map(|statements| Program { statements })
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Parameter {
    pub id: NodeId,
    pub span: Span,
    pub identifier: Identifier,
    pub type_: Type,
}

impl Parameter {
    fn new(id: NodeId, span: Span, identifier: Identifier, type_: Type) -> Parameter {
        Parameter {
            id,
            span,
            identifier,
            type_,
        }
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Identifier {
    pub id: NodeId,
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
    pub id: NodeId,
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
            id: expression.id,
            span: expression.span,
            kind: StatementKind::Expression(clone),
        }
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Block {
    pub id: NodeId,
    pub span: Span,
    pub statements: Vec<Statement>,
    pub return_expression: Option<Expression>,
}
impl From<Block> for Expression {
    fn from(block: Block) -> Self {
        Expression {
            id: block.id.clone(),
            span: block.span.clone(),
            kind: ExpressionKind::Block(Box::new(block)),
        }
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Expression {
    pub id: NodeId,
    pub kind: ExpressionKind,
    pub span: Span,
}

impl Expression {
    fn new(id: NodeId, kind: ExpressionKind, span: Span) -> Expression {
        Expression { id, kind, span }
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
    VariableDeclaration {
        is_mutable: bool,
        identifier: Identifier,
        initializer: Box<Expression>,
    },

    // UnaryExpression(Operator, Box<Expression>),
    BinaryExpression(Box<Expression>, BinaryOperator, Box<Expression>),
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
            ExpressionKind::FunctionCall { .. } => "a function call",
            ExpressionKind::If { .. } => "an if expression",
            ExpressionKind::Error => "an error",
        }
    }
}

fn identifier_parser() -> impl Parser<Token, Identifier, Error = Simple<Token>> + Clone {
    select! {
        Token::Identifier(i) => i
    }
    .labelled("identifier")
    .map_with_span(|name, span| Identifier {
        id: DUMMY_NODE_ID,
        name,
        span,
    })
}

fn type_parser() -> impl Parser<Token, Type, Error = Simple<Token>> + Clone {
    recursive(|type_parser| {
        let simple_type = select! {
            Token::Identifier(i) => Type::Custom(i),
            Token::BuiltInType(t) =>
                match t {
                    BuiltInTypeToken::Bool => Type::Bool,
                    BuiltInTypeToken::Int => Type::Int,
                    BuiltInTypeToken::String => Type::String
                }
        };
        let function_type = type_parser
            .clone()
            .separated_by(just(Token::ListSeparator))
            .delimited_by(just(Token::OpenParen), just(Token::CloseParen))
            .then_ignore(just(Token::FunctionArrow))
            .then(type_parser.clone())
            .map(|(parameters, return_type)| Type::Function {
                parameters,
                return_type: Box::new(return_type),
            });

        simple_type.or(function_type)
    })
}

fn statement_parser() -> impl Parser<Token, Statement, Error = Simple<Token>> + Clone {
    recursive(|statement| {
        let block = just(Token::OpenBlock)
            .ignore_then(statement.clone().repeated())
            .then(expression_parser(statement.clone()).or_not())
            .then_ignore(just(Token::CloseBlock))
            .map_with_span(|(statements, return_expression), span| {
                Expression::new(
                    DUMMY_NODE_ID,
                    ExpressionKind::Block(Box::new(Block {
                        id: DUMMY_NODE_ID,
                        span: span.clone(),
                        statements,
                        return_expression,
                    })),
                    span,
                )
            });

        let parameters = identifier_parser()
            .then_ignore(just(Token::KeyValueSeparator))
            .then(type_parser())
            .map(|(identifier, type_)| {
                Parameter::new(DUMMY_NODE_ID, identifier.clone().span, identifier, type_)
            })
            .labelled("parameter")
            .separated_by(just(Token::ListSeparator))
            .delimited_by(just(Token::OpenParen), just(Token::CloseParen))
            .map_with_span(|parameters, span| (parameters, span));

        let function_definition = just(Token::Let)
            .ignore_then(identifier_parser())
            .then(parameters)
            .then(
                just(Token::KeyValueSeparator)
                    .ignore_then(type_parser())
                    .or_not(),
            )
            .then(block)
            .map_with_span(
                |(((name, parameters), return_type), body), span| Statement {
                    id: DUMMY_NODE_ID,
                    span,
                    kind: StatementKind::FunctionDefinition {
                        name,
                        parameters: parameters.0,
                        return_type: return_type.unwrap_or(Type::Void),
                        body: Box::new(body.into()),
                    },
                },
            );

        let return_statement = just(Token::Return)
            .ignore_then(expression_parser(statement.clone()).or_not())
            .then_ignore(just(Token::Terminator))
            .map_with_span(|expression, span| Statement {
                id: DUMMY_NODE_ID,
                span,
                kind: StatementKind::Return(expression),
            });

        let js_block = just(Token::StartJsBlock)
            .ignore_then(
                expression_parser(statement.clone())
                    .repeated()
                    .delimited_by(just(Token::OpenBlock), just(Token::CloseBlock)),
            )
            .map_with_span(|expressions, span| Statement {
                id: DUMMY_NODE_ID,
                span,
                kind: StatementKind::JsBlock(expressions),
            });

        let expression = expression_parser(statement)
            .then_ignore(just(Token::Terminator))
            .map(|expression| Statement {
                id: DUMMY_NODE_ID,
                span: expression.span.clone(),
                kind: StatementKind::Expression(expression),
            });

        function_definition
            .or(return_statement)
            .or(js_block)
            .or(expression)
    })
}

fn expression_parser<'a>(
    statement: Recursive<'a, Token, Statement, Simple<Token>>,
) -> impl Parser<Token, Expression, Error = Simple<Token>> + Clone + 'a {
    recursive(|expr| {
        let parenthesized_expr: chumsky::combinator::DelimitedBy<
            Recursive<Token, Expression, Simple<Token>>,
            chumsky::primitive::Just<Token, Token, Simple<Token>>,
            chumsky::primitive::Just<Token, Token, Simple<Token>>,
            Token,
            Token,
        > = expr
            .clone()
            .delimited_by(just(Token::OpenParen), just(Token::CloseParen));

        let value = select! {
            Token::Boolean(b) => ExpressionKind::Boolean(b),
            Token::Integer(n) => ExpressionKind::Integer(n.parse().unwrap()),
            Token::String(s) => ExpressionKind::String(s)
        }
        .labelled("value")
        .map_with_span(|kind, span| Expression::new(DUMMY_NODE_ID, kind, span));

        let block = just(Token::OpenBlock)
            .ignore_then(statement.repeated())
            .then(expr.clone().or_not())
            .then_ignore(just(Token::CloseBlock))
            .map_with_span(|(statements, return_expression), span| {
                Expression::new(
                    DUMMY_NODE_ID,
                    ExpressionKind::Block(Box::new(Block {
                        id: DUMMY_NODE_ID,
                        span: span.clone(),
                        statements,
                        return_expression,
                    })),
                    span,
                )
            });

        let variable_declaration = just(Token::Let)
            .to(false)
            .or(just(Token::Mut).to(true))
            .then(identifier_parser())
            .then_ignore(just(Token::Operator(BinaryOperator::Assignment)))
            .then(expr.clone())
            .map_with_span(|((is_mutable, identifier), initializer), span| {
                Expression::new(
                    DUMMY_NODE_ID,
                    ExpressionKind::VariableDeclaration {
                        is_mutable,
                        identifier,
                        initializer: Box::new(initializer),
                    },
                    span,
                )
            });

        let if_ = just(Token::If)
            .ignore_then(expr.clone())
            .then(expr.clone())
            .then(
                just(Token::Else)
                    .ignore_then(expr.clone())
                    .map(Box::new)
                    .or_not(),
            )
            .map_with_span(|((condition, body), else_), span| {
                Expression::new(
                    DUMMY_NODE_ID,
                    ExpressionKind::If {
                        condition: Box::new(condition),
                        body: Box::new(body),
                        else_,
                    },
                    span,
                )
            });

        let atom = value
            .or(block)
            .or(identifier_parser().map(|ident| {
                Expression::new(
                    DUMMY_NODE_ID,
                    ExpressionKind::Identifier(ident.clone()),
                    ident.span,
                )
            }))
            .or(variable_declaration)
            .or(if_)
            // Attempt to recover anything that looks like a list but contains errors
            .recover_with(nested_delimiters(
                Token::OpenList,
                Token::CloseList,
                [
                    (Token::OpenParen, Token::CloseParen),
                    (Token::OpenBlock, Token::CloseBlock),
                ],
                |span| Expression::new(DUMMY_NODE_ID, ExpressionKind::Error, span),
            ))
            .or(parenthesized_expr);

        let operator = just(Token::Operator(BinaryOperator::Dot)).to(BinaryOperator::Dot);
        let dot_member = atom
            .clone()
            .then(operator.then(identifier_parser()).repeated())
            .foldl(|left, (op, right)| {
                let span = left.span.start..right.span.end;
                Expression::new(
                    DUMMY_NODE_ID,
                    ExpressionKind::BinaryExpression(
                        Box::new(left),
                        op,
                        Box::new(Expression::new(
                            DUMMY_NODE_ID,
                            ExpressionKind::Identifier(right.clone()),
                            right.span,
                        )),
                    ),
                    span,
                )
            });

        let arguments = expr
            .clone()
            .separated_by(just(Token::ListSeparator))
            .delimited_by(just(Token::OpenParen), just(Token::CloseParen))
            .map_with_span(|args, span| (args, span));

        let fn_call =
            dot_member
                .clone()
                .then(arguments.repeated())
                .foldl(|callee, (arguments, arg_span)| {
                    let span = callee.span.start..arg_span.end;
                    Expression::new(
                        DUMMY_NODE_ID,
                        ExpressionKind::FunctionCall {
                            callee: Box::new(callee),
                            arguments: arguments,
                        },
                        span,
                    )
                });

        let operator = just(Token::Operator(BinaryOperator::Mul))
            .to(BinaryOperator::Mul)
            .or(just(Token::Operator(BinaryOperator::Div)).to(BinaryOperator::Div));
        let factor = fn_call
            .clone()
            .then(operator.then(fn_call).repeated())
            .foldl(|left, (op, right)| {
                let span = left.span.start..right.span.end;
                Expression::new(
                    DUMMY_NODE_ID,
                    ExpressionKind::BinaryExpression(Box::new(left), op, Box::new(right)),
                    span,
                )
            });

        let operator = just(Token::Operator(BinaryOperator::Add))
            .to(BinaryOperator::Add)
            .or(just(Token::Operator(BinaryOperator::Sub)).to(BinaryOperator::Sub));
        let sum =
            factor
                .clone()
                .then(operator.then(factor).repeated())
                .foldl(|left, (op, right)| {
                    let span = left.span.start..right.span.end;
                    Expression::new(
                        DUMMY_NODE_ID,
                        ExpressionKind::BinaryExpression(Box::new(left), op, Box::new(right)),
                        span,
                    )
                });

        let operator = just(Token::Operator(BinaryOperator::LessThan))
            .to(BinaryOperator::LessThan)
            .or(just(Token::Operator(BinaryOperator::LessThanOrEqual))
                .to(BinaryOperator::LessThanOrEqual))
            .or(just(Token::Operator(BinaryOperator::GreaterThan)).to(BinaryOperator::GreaterThan))
            .or(just(Token::Operator(BinaryOperator::GreaterThanOrEqual))
                .to(BinaryOperator::GreaterThanOrEqual));
        let comparison =
            sum.clone()
                .then(operator.then(sum).repeated())
                .foldl(|left, (op, right)| {
                    let span = left.span.start..right.span.end;
                    Expression::new(
                        DUMMY_NODE_ID,
                        ExpressionKind::BinaryExpression(Box::new(left), op, Box::new(right)),
                        span,
                    )
                });

        let operator = just(Token::Operator(BinaryOperator::Equal))
            .to(BinaryOperator::Equal)
            .or(just(Token::Operator(BinaryOperator::NotEqual)).to(BinaryOperator::NotEqual));
        let equality = comparison
            .clone()
            .then(operator.then(comparison).repeated())
            .foldl(|left, (op, right)| {
                let span = left.span.start..right.span.end;
                Expression::new(
                    DUMMY_NODE_ID,
                    ExpressionKind::BinaryExpression(Box::new(left), op, Box::new(right)),
                    span,
                )
            });

        let operator = just(Token::Operator(BinaryOperator::And))
            .to(BinaryOperator::And)
            .or(just(Token::Operator(BinaryOperator::Or)).to(BinaryOperator::Or));
        let logical = equality
            .clone()
            .then(operator.then(equality.clone()).repeated())
            .foldl(|left, (op, right)| {
                let span = left.span.start..right.span.end;
                Expression::new(
                    DUMMY_NODE_ID,
                    ExpressionKind::BinaryExpression(Box::new(left), op, Box::new(right)),
                    span,
                )
            });

        let operator =
            just(Token::Operator(BinaryOperator::Assignment)).to(BinaryOperator::Assignment);
        let assignment = logical
            .clone()
            .then(operator.then(equality).repeated())
            .foldl(|left, (op, right)| {
                let span = left.span.start..right.span.end;
                Expression::new(
                    DUMMY_NODE_ID,
                    ExpressionKind::BinaryExpression(Box::new(left), op, Box::new(right)),
                    span,
                )
            });

        assignment
    })
}
