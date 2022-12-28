use core::fmt;

use ariadne::{Color, Fmt, Label, Report, ReportKind, Source};
use chumsky::prelude::*;

pub type Span = std::ops::Range<usize>;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
enum BuiltInTypeToken {
    Bool,
    Int,
    String,
}

impl fmt::Display for BuiltInTypeToken {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BuiltInTypeToken::Bool => write!(f, "bool"),
            BuiltInTypeToken::Int => write!(f, "int"),
            BuiltInTypeToken::String => write!(f, "string"),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
enum Operator {
    Add,
    Sub,
    Mul,
    Div,
    Dot,
    Not,
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

impl fmt::Display for Operator {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Operator::Add => write!(f, "+"),
            Operator::Sub => write!(f, "-"),
            Operator::Mul => write!(f, "*"),
            Operator::Div => write!(f, "/"),
            Operator::Dot => write!(f, "."),
            Operator::Not => write!(f, "!"),
            Operator::NotEqual => write!(f, "!="),
            Operator::Equal => write!(f, "=="),
            Operator::LessThan => write!(f, "<"),
            Operator::LessThanOrEqual => write!(f, "<="),
            Operator::GreaterThan => write!(f, ">"),
            Operator::GreaterThanOrEqual => write!(f, ">="),
            Operator::And => write!(f, "&&"),
            Operator::Or => write!(f, "||"),
            Operator::Assignment => write!(f, "="),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
enum Token {
    // Grouping and delimiting markers
    FunctionArrow,      // ->
    OpenBlock,          // {
    CloseBlock,         // }
    OpenList,           // [
    CloseList,          // ]
    ListSeparator,      // ,
    OpenParen,          // (
    CloseParen,         // )
    KeyValueSeparator,  // :
    Terminator,         // ;
    AbsolutePathMarker, // ~
    PackagePathMarker,  // @

    // Keywords
    Back,
    Else,
    Front,
    If,
    Let,
    Mut,
    Pub,
    Struct,
    Use,

    // Values and types
    Boolean(bool),
    BuiltInType(BuiltInTypeToken),
    Identifier(String),
    Integer(String),
    Operator(Operator),
    String(String),
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Token::FunctionArrow => write!(f, "->"),
            Token::OpenBlock => write!(f, "{{"),
            Token::CloseBlock => write!(f, "}}"),
            Token::OpenList => write!(f, "["),
            Token::CloseList => write!(f, "]"),
            Token::ListSeparator => write!(f, ","),
            Token::OpenParen => write!(f, "("),
            Token::CloseParen => write!(f, ")"),
            Token::KeyValueSeparator => write!(f, ":"),
            Token::Terminator => write!(f, ";"),
            Token::AbsolutePathMarker => write!(f, "~"),
            Token::PackagePathMarker => write!(f, "@"),
            Token::Back => write!(f, "back"),
            Token::Else => write!(f, "else"),
            Token::Front => write!(f, "front"),
            Token::If => write!(f, "if"),
            Token::Let => write!(f, "let"),
            Token::Mut => write!(f, "mut"),
            Token::Pub => write!(f, "pub"),
            Token::Struct => write!(f, "struct"),
            Token::Use => write!(f, "use"),
            Token::Boolean(b) => write!(f, "{}", b),
            Token::BuiltInType(t) => write!(f, "{}", t),
            Token::Identifier(i) => write!(f, "{}", i),
            Token::Integer(i) => write!(f, "{}", i),
            Token::Operator(o) => write!(f, "{}", o),
            Token::String(s) => write!(f, "{}", s),
        }
    }
}

fn lexer() -> impl Parser<char, Vec<(Token, Span)>, Error = Simple<char>> {
    let num = text::int::<char, Simple<char>>(10).map(Token::Integer);

    // Grouping or delimiting marker
    let marker = choice::<_, Simple<char>>((
        just("->").to(Token::FunctionArrow),
        just("{").to(Token::OpenBlock),
        just("}").to(Token::CloseBlock),
        just("[").to(Token::OpenList),
        just("]").to(Token::CloseList),
        just(",").to(Token::ListSeparator),
        just("(").to(Token::OpenParen),
        just(")").to(Token::CloseParen),
        just(":").to(Token::KeyValueSeparator),
        just(";").to(Token::Terminator),
        just("@").to(Token::AbsolutePathMarker),
        just("~").to(Token::PackagePathMarker),
    ));

    let string_ = just('\'')
        .ignore_then(filter(|c| *c != '\'').repeated())
        .then_ignore(just('\''))
        .collect::<String>()
        .map(Token::String);

    let operator = choice::<_, Simple<char>>((
        just::<char, _, Simple<char>>('+').to(Token::Operator(Operator::Add)),
        just::<char, _, Simple<char>>('-').to(Token::Operator(Operator::Sub)),
        just::<char, _, Simple<char>>('*').to(Token::Operator(Operator::Mul)),
        just::<char, _, Simple<char>>('/').to(Token::Operator(Operator::Div)),
        just::<char, _, Simple<char>>('.').to(Token::Operator(Operator::Dot)),
        just::<char, _, Simple<char>>("==").to(Token::Operator(Operator::Equal)),
        just::<char, _, Simple<char>>("!=").to(Token::Operator(Operator::NotEqual)),
        just::<char, _, Simple<char>>("!").to(Token::Operator(Operator::Not)),
        just::<char, _, Simple<char>>("<=").to(Token::Operator(Operator::LessThanOrEqual)),
        just::<char, _, Simple<char>>("<").to(Token::Operator(Operator::LessThan)),
        just::<char, _, Simple<char>>(">=").to(Token::Operator(Operator::GreaterThanOrEqual)),
        just::<char, _, Simple<char>>(">").to(Token::Operator(Operator::GreaterThan)),
        just::<char, _, Simple<char>>("&&").to(Token::Operator(Operator::And)),
        just::<char, _, Simple<char>>("||").to(Token::Operator(Operator::Or)),
        just::<char, _, Simple<char>>("=").to(Token::Operator(Operator::Assignment)),
    ));

    let ident = text::ident().map(|ident: String| match ident.as_str() {
        "back" => Token::Back,
        "else" => Token::Else,
        "front" => Token::Front,
        "if" => Token::If,
        "let" => Token::Let,
        "mut" => Token::Mut,
        "pub" => Token::Pub,
        "struct" => Token::Struct,
        "use" => Token::Use,

        // Types
        "bool" => Token::BuiltInType(BuiltInTypeToken::Bool),
        "int" => Token::BuiltInType(BuiltInTypeToken::Int),
        "string" => Token::BuiltInType(BuiltInTypeToken::String),

        // Values
        "true" => Token::Boolean(true),
        "false" => Token::Boolean(false),
        _ => Token::Identifier(ident),
    });

    let token = num
        .or(string_)
        .or(marker)
        .or(operator)
        .or(ident)
        .recover_with(skip_then_retry_until([]));

    let comment = just("//").then(take_until(just('\n'))).padded();

    token
        .map_with_span(|tok, span| (tok, span))
        .padded_by(comment.repeated())
        .padded()
        .repeated()
}

fn main() {
    let src = std::fs::read_to_string(std::env::args().nth(1).unwrap()).unwrap();
    let (tokens, tokenization_errors) = lexer().parse_recovery(src.as_str());
    if let Some(tokens) = tokens {
        for token in tokens {
            println!("Token: {:?}", token)
        }
    }

    tokenization_errors
        .into_iter()
        .map(|e| e.map(|c| c.to_string()))
        .for_each(|e| {
            let report = Report::build(ReportKind::Error, (), e.span().start);

            let report = match e.reason() {
                chumsky::error::SimpleReason::Unclosed { span, delimiter } => report
                    .with_message(format!(
                        "Unclosed delimiter {}",
                        delimiter.fg(Color::Yellow)
                    ))
                    .with_label(
                        Label::new(span.clone())
                            .with_message(format!(
                                "Unclosed delimiter {}",
                                delimiter.fg(Color::Yellow)
                            ))
                            .with_color(Color::Yellow),
                    )
                    .with_label(
                        Label::new(e.span())
                            .with_message(format!(
                                "Must be closed before this {}",
                                e.found()
                                    .unwrap_or(&"end of file".to_string())
                                    .fg(Color::Red)
                            ))
                            .with_color(Color::Red),
                    ),
                chumsky::error::SimpleReason::Unexpected => report
                    .with_message(format!(
                        "{}, expected {}",
                        if e.found().is_some() {
                            "Unexpected token in input"
                        } else {
                            "Unexpected end of input"
                        },
                        if e.expected().len() == 0 {
                            "something else".to_string()
                        } else {
                            e.expected()
                                .map(|expected| match expected {
                                    Some(expected) => expected.to_string(),
                                    None => "end of input".to_string(),
                                })
                                .collect::<Vec<_>>()
                                .join(", ")
                        }
                    ))
                    .with_label(
                        Label::new(e.span())
                            .with_message(format!(
                                "Unexpected token {}",
                                e.found()
                                    .unwrap_or(&"end of file".to_string())
                                    .fg(Color::Red)
                            ))
                            .with_color(Color::Red),
                    ),
                chumsky::error::SimpleReason::Custom(msg) => report.with_message(msg).with_label(
                    Label::new(e.span())
                        .with_message(format!("{}", msg.fg(Color::Red)))
                        .with_color(Color::Red),
                ),
            };

            report.finish().print(Source::from(&src)).unwrap();
        });
}
