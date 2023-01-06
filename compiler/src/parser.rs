use core::fmt;

use crate::{
    lexer::*,
    passes::shared::{NodeId, DUMMY_NODE_ID},
};
use chumsky::prelude::*;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Program {
    pub expressions: Vec<Expression>,
}

pub fn main_parser() -> impl Parser<Token, Program, Error = Simple<Token>> + Clone {
    expression_parser()
        .then_ignore(just(Token::Terminator).or_not())
        .repeated()
        .then_ignore(end())
        .map(|expressions| Program { expressions })
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Parameter {
    pub id: NodeId,
    pub span: Span,
    pub identifier: Identifier,
    pub type_: String,
}

impl Parameter {
    fn new(id: NodeId, span: Span, identifier: Identifier, type_: String) -> Parameter {
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
    Block(Vec<Expression>),

    // Ex: `Todo { title: "Write a compiler" }`
    // ObjectLiteral(String, HashMap<String, Expression>),
    VariableDeclaration {
        is_mutable: bool,
        identifier: Identifier,
        initializer: Box<Expression>,
    },
    FunctionDefinition {
        name: Identifier,
        parameters: Vec<Parameter>,
        body: Box<Expression>,
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
            ExpressionKind::FunctionDefinition { .. } => "a function definition",
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

fn expression_parser() -> impl Parser<Token, Expression, Error = Simple<Token>> + Clone {
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

        let identifier = select! {
            Token::Identifier(i) => i
        }
        .labelled("identifier")
        .map_with_span(|name, span| Identifier {
            id: DUMMY_NODE_ID,
            name,
            span,
        });

        let parameters = identifier
            .clone()
            .then_ignore(just(Token::KeyValueSeparator))
            .then(select! {
                Token::Identifier(i) => i,
                Token::BuiltInType(t) => format!("{}", t)
            })
            .map(|(identifier, type_)| {
                Parameter::new(DUMMY_NODE_ID, identifier.clone().span, identifier, type_)
            })
            .labelled("parameter")
            .separated_by(just(Token::ListSeparator))
            .delimited_by(just(Token::OpenParen), just(Token::CloseParen))
            .map_with_span(|parameters, span| (parameters, span));

        let block = expr
            .clone()
            .repeated()
            .delimited_by(just(Token::OpenBlock), just(Token::CloseBlock))
            .map_with_span(|e, span| {
                Expression::new(DUMMY_NODE_ID, ExpressionKind::Block(e), span)
            });

        let function_definition = just(Token::Let)
            .ignore_then(identifier.clone())
            .then(parameters)
            .then(block.clone())
            .map_with_span(|((name, parameters), body), span| {
                Expression::new(
                    DUMMY_NODE_ID,
                    ExpressionKind::FunctionDefinition {
                        name,
                        parameters: parameters.0,
                        body: Box::new(body),
                    },
                    span,
                )
            });

        let variable_declaration = just(Token::Let)
            .to(false)
            .or(just(Token::Mut).to(true))
            .then(identifier)
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
            .or(block.clone())
            .or(identifier.clone().map(|ident| {
                Expression::new(
                    DUMMY_NODE_ID,
                    ExpressionKind::Identifier(ident.clone()),
                    ident.span,
                )
            }))
            .or(function_definition)
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
            .then(operator.then(identifier.clone()).repeated())
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

        let operator =
            just(Token::Operator(BinaryOperator::Assignment)).to(BinaryOperator::Assignment);
        let assignment = equality
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

        assignment.then_ignore(just(Token::Terminator).or_not())
    })
}
