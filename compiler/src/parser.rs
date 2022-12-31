use crate::lexer::*;
use chumsky::prelude::*;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct NodeId {
    value: u32,
}

impl NodeId {
    pub const fn from_u32(value: u32) -> NodeId {
        NodeId { value }
    }
}

// We use this just for the initial construction. We need to go over the tree later and generate unique IDs.
// We don't generate them as we parse the tree because, for example, repeated use of the same identifier should
// give the same ID.
const DUMMY_NODE_ID: NodeId = NodeId::from_u32(u32::MAX);

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

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Parameter {
    pub id: NodeId,
    pub span: Span,
    pub name: String,
    pub type_: String,
}

impl Parameter {
    fn new(id: NodeId, span: Span, name: String, type_: String) -> Parameter {
        Parameter {
            id,
            span,
            name,
            type_,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
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

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ExpressionKind {
    // Primitives
    Boolean(bool),
    Identifier(String),
    Integer(i64),
    String(String),
    Block(Vec<Expression>),

    // Ex: `Todo { title: "Write a compiler" }`
    // ObjectLiteral(String, HashMap<String, Expression>),
    VariableDeclaration {
        is_mutable: bool,
        identifier: String,
        initializer: Box<Expression>,
    },
    FunctionDefinition {
        name: String,
        parameters: Vec<Parameter>,
        body: Box<Expression>,
    },

    // UnaryExpression(Operator, Box<Expression>),
    BinaryExpression(Box<Expression>, Operator, Box<Expression>),
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

fn get_identifier_from_expression_kind(kind: &ExpressionKind) -> String {
    match kind {
        ExpressionKind::Identifier(i) => i.to_string(),
        _ => unreachable!("If you called this with a non-identifier, you messed up"),
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
            Token::Identifier(i) => ExpressionKind::Identifier(i),
        }
        .labelled("identifier")
        .map_with_span(|kind, span| Expression::new(DUMMY_NODE_ID, kind, span));

        let parameters = identifier
            .clone()
            .map(|ident| (ident.kind, ident.span))
            .then_ignore(just(Token::KeyValueSeparator))
            .then(select! {
                Token::Identifier(i) => i,
                Token::BuiltInType(t) => format!("{}", t)
            })
            .map(|((kind, span), type_)| {
                Parameter::new(
                    DUMMY_NODE_ID,
                    span,
                    get_identifier_from_expression_kind(&kind),
                    type_,
                )
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
            .ignore_then(
                identifier
                    .clone()
                    .map(|expr| (get_identifier_from_expression_kind(&expr.kind), expr.span)),
            )
            .then(parameters)
            .then(block.clone())
            .map_with_span(|((name, parameters), body), span| {
                Expression::new(
                    DUMMY_NODE_ID,
                    ExpressionKind::FunctionDefinition {
                        name: name.0,
                        parameters: parameters.0,
                        body: Box::new(body),
                    },
                    span,
                )
            });

        let variable_declaration = just(Token::Let)
            .to(false)
            .or(just(Token::Mut).to(true))
            .then(
                identifier.map(|expr| (get_identifier_from_expression_kind(&expr.kind), expr.span)),
            )
            .then_ignore(just(Token::Operator(Operator::Assignment)))
            .then(expr.clone())
            .map_with_span(|((is_mutable, identifier), initializer), span| {
                Expression::new(
                    DUMMY_NODE_ID,
                    ExpressionKind::VariableDeclaration {
                        is_mutable,
                        identifier: identifier.0,
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
            .or(identifier.clone())
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

        let operator = just(Token::Operator(Operator::Dot)).to(Operator::Dot);
        let dot_member = atom
            .clone()
            .then(operator.then(identifier.clone()).repeated())
            .foldl(|left, (op, right)| {
                let span = left.span.start..right.span.end;
                Expression::new(
                    DUMMY_NODE_ID,
                    ExpressionKind::BinaryExpression(Box::new(left), op, Box::new(right)),
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

        let operator = just(Token::Operator(Operator::Mul))
            .to(Operator::Mul)
            .or(just(Token::Operator(Operator::Div)).to(Operator::Div));
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

        let operator = just(Token::Operator(Operator::Add))
            .to(Operator::Add)
            .or(just(Token::Operator(Operator::Sub)).to(Operator::Sub));
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

        let operator = just(Token::Operator(Operator::LessThan))
            .to(Operator::LessThan)
            .or(just(Token::Operator(Operator::LessThanOrEqual)).to(Operator::LessThanOrEqual))
            .or(just(Token::Operator(Operator::GreaterThan)).to(Operator::GreaterThan))
            .or(just(Token::Operator(Operator::GreaterThanOrEqual))
                .to(Operator::GreaterThanOrEqual));
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

        let operator = just(Token::Operator(Operator::Equal))
            .to(Operator::Equal)
            .or(just(Token::Operator(Operator::NotEqual)).to(Operator::NotEqual));
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

        let operator = just(Token::Operator(Operator::Assignment)).to(Operator::Assignment);
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

        assignment
    })
}
