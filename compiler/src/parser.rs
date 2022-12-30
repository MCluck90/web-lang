use crate::lexer::*;
use chumsky::prelude::*;

pub fn main_parser() -> impl Parser<Token, Spanned<Expression>, Error = Simple<Token>> + Clone {
    expression_parser().then_ignore(end())
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Parameter {
    name: String,
    type_: String,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Expression {
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

fn expression_parser() -> impl Parser<Token, Spanned<Expression>, Error = Simple<Token>> + Clone {
    recursive(|expr| {
        let parenthesized_expr: chumsky::combinator::DelimitedBy<
            Recursive<Token, (Expression, std::ops::Range<usize>), Simple<Token>>,
            chumsky::primitive::Just<Token, Token, Simple<Token>>,
            chumsky::primitive::Just<Token, Token, Simple<Token>>,
            Token,
            Token,
        > = expr
            .clone()
            .delimited_by(just(Token::OpenParen), just(Token::CloseParen));

        let value = select! {
            Token::Boolean(b) => Expression::Boolean(b),
            Token::Integer(n) => Expression::Integer(n.parse().unwrap()),
            Token::String(s) => Expression::String(s)
        }
        .labelled("value");

        let identifier = select! {
            Token::Identifier(i) => Expression::Identifier(i),
        }
        .labelled("identifier");

        let parameters = identifier
            .clone()
            .map(|i| match i {
                Expression::Identifier(i) => i,
                _ => unreachable!(),
            })
            .then_ignore(just(Token::KeyValueSeparator))
            .then(select! {
                Token::Identifier(i) => i,
                Token::BuiltInType(t) => format!("{}", t)
            })
            .map(|(name, type_)| Parameter { name, type_ })
            .labelled("parameter")
            .separated_by(just(Token::ListSeparator))
            .delimited_by(just(Token::OpenParen), just(Token::CloseParen))
            .map_with_span(|parameters, span| (parameters, span));

        let block = expr
            .clone()
            .repeated()
            .delimited_by(just(Token::OpenBlock), just(Token::CloseBlock))
            .map(|e| Expression::Block(e.into_iter().map(|(e, _)| e).collect()));

        let function_definition = just(Token::Let)
            .ignore_then(identifier.clone().map(|e| match e {
                Expression::Identifier(i) => i,
                _ => unreachable!(),
            }))
            .then(parameters)
            .then(block.clone())
            .map(
                |((name, parameters), body)| Expression::FunctionDefinition {
                    name,
                    parameters: parameters.0,
                    body: Box::new(body),
                },
            );

        let variable_declaration = just(Token::Let)
            .to(false)
            .or(just(Token::Mut).to(true))
            .then(identifier.map(|exp| match exp {
                Expression::Identifier(i) => i,
                _ => unreachable!(),
            }))
            .then_ignore(just(Token::Operator(Operator::Assignment)))
            .then(expr.clone())
            .map(
                |((is_mutable, identifier), (initializer, _))| Expression::VariableDeclaration {
                    is_mutable,
                    identifier,
                    initializer: Box::new(initializer),
                },
            );

        let if_ = just(Token::If)
            .ignore_then(expr.clone())
            .then(expr.clone())
            .then(
                just(Token::Else)
                    .ignore_then(expr.clone())
                    .map(|(e, _)| Box::new(e))
                    .or_not(),
            )
            .map(|((condition, body), else_)| Expression::If {
                condition: Box::new(condition.0),
                body: Box::new(body.0),
                else_,
            });

        let atom = value
            .or(block.clone())
            .or(identifier)
            .or(function_definition)
            .or(variable_declaration)
            .or(if_)
            .map_with_span(|expr, span| (expr, span))
            // Attempt to recover anything that looks like a list but contains errors
            .recover_with(nested_delimiters(
                Token::OpenList,
                Token::CloseList,
                [
                    (Token::OpenParen, Token::CloseParen),
                    (Token::OpenBlock, Token::CloseBlock),
                ],
                |span| (Expression::Error, span),
            ))
            .or(parenthesized_expr);

        let arguments = expr
            .clone()
            .separated_by(just(Token::ListSeparator))
            .delimited_by(just(Token::OpenParen), just(Token::CloseParen))
            .map_with_span(|args, span| (args, span));

        let fn_call = atom
            .clone()
            .then(arguments.repeated())
            .foldl(|callee, arguments| {
                let span = callee.1.start..arguments.1.end;
                (
                    Expression::FunctionCall {
                        callee: Box::new(callee.0),
                        arguments: arguments.0.into_iter().map(|(e, _)| e).collect(),
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
            .foldl(|a, (op, b)| {
                let span = a.1.start..b.1.end;
                (
                    Expression::BinaryExpression(Box::new(a.0), op, Box::new(b.0)),
                    span,
                )
            });

        let operator = just(Token::Operator(Operator::Add))
            .to(Operator::Add)
            .or(just(Token::Operator(Operator::Sub)).to(Operator::Sub));
        let sum = factor
            .clone()
            .then(operator.then(factor).repeated())
            .foldl(|a, (op, b)| {
                let span = a.1.start..b.1.end;
                (
                    Expression::BinaryExpression(Box::new(a.0), op, Box::new(b.0)),
                    span,
                )
            });

        let operator = just(Token::Operator(Operator::LessThan))
            .to(Operator::LessThan)
            .or(just(Token::Operator(Operator::LessThanOrEqual)).to(Operator::LessThanOrEqual))
            .or(just(Token::Operator(Operator::GreaterThan)).to(Operator::GreaterThan))
            .or(just(Token::Operator(Operator::GreaterThanOrEqual))
                .to(Operator::GreaterThanOrEqual));
        let comparison = sum
            .clone()
            .then(operator.then(sum).repeated())
            .foldl(|a, (op, b)| {
                let span = a.1.start..b.1.end;
                (
                    Expression::BinaryExpression(Box::new(a.0), op, Box::new(b.0)),
                    span,
                )
            });

        let operator = just(Token::Operator(Operator::Equal))
            .to(Operator::Equal)
            .or(just(Token::Operator(Operator::NotEqual)).to(Operator::NotEqual));
        let equality = comparison
            .clone()
            .then(operator.then(comparison).repeated())
            .foldl(|a, (op, b)| {
                let span = a.1.start..b.1.end;
                (
                    Expression::BinaryExpression(Box::new(a.0), op, Box::new(b.0)),
                    span,
                )
            });

        let operator = just(Token::Operator(Operator::Assignment)).to(Operator::Assignment);
        let assignment = equality
            .clone()
            .then(operator.then(equality).repeated())
            .foldl(|a, (op, b)| {
                let span = a.1.start..b.1.end;
                (
                    Expression::BinaryExpression(Box::new(a.0), op, Box::new(b.0)),
                    span,
                )
            });

        assignment
    })
}

#[test]
fn can_parse_basic_values() {
    let expected = Expression::Boolean(true);
    let actual = expression_parser()
        .parse(vec![Token::Boolean(true)])
        .unwrap()
        .0;
    assert_eq!(expected, actual, "expected to parse a boolean");

    let expected = Expression::Integer(23);
    let actual = expression_parser()
        .parse(vec![Token::Integer("23".into())])
        .unwrap()
        .0;
    assert_eq!(expected, actual, "expected to parse an integer");

    let expected = Expression::String("hello".into());
    let actual = expression_parser()
        .parse(vec![Token::String("hello".into())])
        .unwrap()
        .0;
    assert_eq!(expected, actual, "expected to parse a string");
}

#[test]
fn can_parse_an_identifier() {
    let expected = Expression::Identifier("foo".into());
    let actual = expression_parser()
        .parse(vec![Token::Identifier("foo".into())])
        .unwrap()
        .0;
    assert_eq!(expected, actual, "expected to parse an identifier");

    let expected = Expression::Identifier("foo".into());
    let actual = expression_parser()
        .parse(vec![
            Token::OpenParen,
            Token::Identifier("foo".into()),
            Token::CloseParen,
        ])
        .unwrap()
        .0;
    assert_eq!(expected, actual, "expected to parse an identifier");
}

#[test]
fn can_parse_function_definitions() {
    let expected = Expression::FunctionDefinition {
        name: "add".into(),
        parameters: vec![
            Parameter {
                name: "x".into(),
                type_: "int".into(),
            },
            Parameter {
                name: "y".into(),
                type_: "int".into(),
            },
        ],
        body: Box::new(Expression::Block(vec![Expression::BinaryExpression(
            Box::new(Expression::Identifier("x".into())),
            Operator::Add,
            Box::new(Expression::Identifier("y".into())),
        )])),
    };
    let actual = expression_parser()
        .parse(vec![
            Token::Let,
            Token::Identifier("add".into()),
            Token::OpenParen,
            Token::Identifier("x".into()),
            Token::KeyValueSeparator,
            Token::Identifier("int".into()),
            Token::ListSeparator,
            Token::Identifier("y".into()),
            Token::KeyValueSeparator,
            Token::Identifier("int".into()),
            Token::CloseParen,
            Token::OpenBlock,
            Token::Identifier("x".into()),
            Token::Operator(Operator::Add),
            Token::Identifier("y".into()),
            Token::CloseBlock,
        ])
        .unwrap()
        .0;
    assert_eq!(expected, actual, "expected to parse a function definition");
}

#[test]
fn can_parse_variable_declarations() {
    let expected = Expression::VariableDeclaration {
        is_mutable: false,
        identifier: "foo".into(),
        initializer: Box::new(Expression::Boolean(true)),
    };
    let actual = expression_parser()
        .parse(vec![
            Token::Let,
            Token::Identifier("foo".into()),
            Token::Operator(Operator::Assignment),
            Token::Boolean(true),
        ])
        .unwrap()
        .0;
    assert_eq!(expected, actual, "expected to parse a variable declaration");
}

#[test]
fn can_parse_function_calls() {
    let expected = Expression::FunctionCall {
        callee: Box::new(Expression::Identifier("log".into())),
        arguments: vec![Expression::String("hello".into()), Expression::Integer(10)],
    };
    let actual = expression_parser()
        .parse(vec![
            Token::Identifier("log".into()),
            Token::OpenParen,
            Token::String("hello".into()),
            Token::ListSeparator,
            Token::Integer("10".into()),
            Token::CloseParen,
        ])
        .unwrap()
        .0;
    assert_eq!(expected, actual, "expected to parse a function call");

    let expected = Expression::FunctionCall {
        callee: Box::new(Expression::FunctionCall {
            callee: Box::new(Expression::Identifier("add".into())),
            arguments: vec![Expression::Integer(2)],
        }),
        arguments: vec![Expression::Integer(3)],
    };
    let actual = expression_parser()
        .parse(vec![
            Token::Identifier("add".into()),
            Token::OpenParen,
            Token::Integer("2".into()),
            Token::CloseParen,
            Token::OpenParen,
            Token::Integer("3".into()),
            Token::CloseParen,
        ])
        .unwrap()
        .0;
    assert_eq!(expected, actual, "expected to parse a function call");
}

#[test]
fn can_parse_multiplication() {
    let expected = Expression::BinaryExpression(
        Box::new(Expression::Integer(2)),
        Operator::Mul,
        Box::new(Expression::Integer(3)),
    );
    let actual = expression_parser()
        .parse(vec![
            Token::Integer("2".into()),
            Token::Operator(Operator::Mul),
            Token::Integer("3".into()),
        ])
        .unwrap()
        .0;
    assert_eq!(expected, actual, "expected to parse multiplication");
}

#[test]
fn can_parse_division() {
    let expected = Expression::BinaryExpression(
        Box::new(Expression::Integer(10)),
        Operator::Div,
        Box::new(Expression::Integer(2)),
    );
    let actual = expression_parser()
        .parse(vec![
            Token::Integer("10".into()),
            Token::Operator(Operator::Div),
            Token::Integer("2".into()),
        ])
        .unwrap()
        .0;
    assert_eq!(expected, actual, "expected to parse division");
}

#[test]
fn can_parse_addition() {
    let expected = Expression::BinaryExpression(
        Box::new(Expression::Integer(10)),
        Operator::Add,
        Box::new(Expression::Integer(2)),
    );
    let actual = expression_parser()
        .parse(vec![
            Token::Integer("10".into()),
            Token::Operator(Operator::Add),
            Token::Integer("2".into()),
        ])
        .unwrap()
        .0;
    assert_eq!(expected, actual, "expected to parse addition");
}

#[test]
fn can_parse_subtraction() {
    let expected = Expression::BinaryExpression(
        Box::new(Expression::Integer(10)),
        Operator::Sub,
        Box::new(Expression::Integer(2)),
    );
    let actual = expression_parser()
        .parse(vec![
            Token::Integer("10".into()),
            Token::Operator(Operator::Sub),
            Token::Integer("2".into()),
        ])
        .unwrap()
        .0;
    assert_eq!(expected, actual, "expected to parse subtraction");
}

#[test]
fn can_parse_less_than() {
    let expected = Expression::BinaryExpression(
        Box::new(Expression::Integer(10)),
        Operator::LessThan,
        Box::new(Expression::Integer(2)),
    );
    let actual = expression_parser()
        .parse(vec![
            Token::Integer("10".into()),
            Token::Operator(Operator::LessThan),
            Token::Integer("2".into()),
        ])
        .unwrap()
        .0;
    assert_eq!(expected, actual, "expected to parse less than");
}

#[test]
fn can_parse_less_than_or_equal() {
    let expected = Expression::BinaryExpression(
        Box::new(Expression::Integer(10)),
        Operator::LessThanOrEqual,
        Box::new(Expression::Integer(2)),
    );
    let actual = expression_parser()
        .parse(vec![
            Token::Integer("10".into()),
            Token::Operator(Operator::LessThanOrEqual),
            Token::Integer("2".into()),
        ])
        .unwrap()
        .0;
    assert_eq!(expected, actual, "expected to parse less than or equal");
}

#[test]
fn can_parse_greater_than() {
    let expected = Expression::BinaryExpression(
        Box::new(Expression::Integer(10)),
        Operator::GreaterThan,
        Box::new(Expression::Integer(2)),
    );
    let actual = expression_parser()
        .parse(vec![
            Token::Integer("10".into()),
            Token::Operator(Operator::GreaterThan),
            Token::Integer("2".into()),
        ])
        .unwrap()
        .0;
    assert_eq!(expected, actual, "expected to parse greater than");
}

#[test]
fn can_parse_greater_than_or_equal() {
    let expected = Expression::BinaryExpression(
        Box::new(Expression::Integer(10)),
        Operator::GreaterThanOrEqual,
        Box::new(Expression::Integer(2)),
    );
    let actual = expression_parser()
        .parse(vec![
            Token::Integer("10".into()),
            Token::Operator(Operator::GreaterThanOrEqual),
            Token::Integer("2".into()),
        ])
        .unwrap()
        .0;
    assert_eq!(expected, actual, "expected to parse greater than or equal");
}

#[test]
fn can_parse_equality() {
    let expected = Expression::BinaryExpression(
        Box::new(Expression::Integer(10)),
        Operator::Equal,
        Box::new(Expression::Integer(2)),
    );
    let actual = expression_parser()
        .parse(vec![
            Token::Integer("10".into()),
            Token::Operator(Operator::Equal),
            Token::Integer("2".into()),
        ])
        .unwrap()
        .0;
    assert_eq!(expected, actual, "expected to parse equality");

    let expected = Expression::BinaryExpression(
        Box::new(Expression::Integer(10)),
        Operator::NotEqual,
        Box::new(Expression::Integer(2)),
    );
    let actual = expression_parser()
        .parse(vec![
            Token::Integer("10".into()),
            Token::Operator(Operator::NotEqual),
            Token::Integer("2".into()),
        ])
        .unwrap()
        .0;
    assert_eq!(expected, actual, "expected to parse equality");
}

#[test]
fn can_parse_assignment() {
    let expected = Expression::BinaryExpression(
        Box::new(Expression::Identifier("x".into())),
        Operator::Assignment,
        Box::new(Expression::Integer(2)),
    );
    let actual = expression_parser()
        .parse(vec![
            Token::Identifier("x".into()),
            Token::Operator(Operator::Assignment),
            Token::Integer("2".into()),
        ])
        .unwrap()
        .0;
    assert_eq!(expected, actual, "expected to parse assignment");
}

#[test]
fn can_parse_if_expressions() {
    let expected = Expression::If {
        condition: Box::new(Expression::Boolean(true)),
        body: Box::new(Expression::Block(vec![Expression::Integer(1)])),
        else_: Some(Box::new(Expression::Block(vec![Expression::Integer(2)]))),
    };
    let actual = expression_parser()
        .parse(vec![
            Token::If,
            Token::Boolean(true),
            Token::OpenBlock,
            Token::Integer("1".into()),
            Token::CloseBlock,
            Token::Else,
            Token::OpenBlock,
            Token::Integer("2".into()),
            Token::CloseBlock,
        ])
        .unwrap()
        .0;
    assert_eq!(expected, actual, "expected to parse if expression");
}
