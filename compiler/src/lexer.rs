use chumsky::prelude::*;
use core::fmt;

pub type Span = std::ops::Range<usize>;
pub type Spanned<T> = (T, Span);

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum BuiltInTypeToken {
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
pub enum Operator {
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
pub enum Token {
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

pub fn lexer() -> impl Parser<char, Vec<Spanned<Token>>, Error = Simple<char>> {
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

    let single_quote_string = just('\'')
        .ignore_then(filter(|c| *c != '\'').repeated())
        .then_ignore(just('\''))
        .collect::<String>()
        .map(Token::String);

    let double_quote_string = just('"')
        .ignore_then(filter(|c| *c != '"').repeated())
        .then_ignore(just('"'))
        .collect::<String>()
        .map(Token::String);

    let string_ = single_quote_string.or(double_quote_string);

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
