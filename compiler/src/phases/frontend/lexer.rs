use chumsky::prelude::*;
use core::fmt;

use crate::errors::CompilerError;

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
    PropertyAccessOp,   // .

    // Keywords
    Back,
    Break,
    Else,
    Front,
    If,
    Let,
    Loop,
    Mut,
    Pub,
    Return,
    Struct,
    Use,

    // JS Interop
    StartJsBlock, // #js

    // Values and types
    Boolean(bool),
    BuiltInType(BuiltInTypeToken),
    Identifier(String),
    Integer(String),
    Operator(BinaryOperator),
    String(String),
}
impl Token {
    pub fn to_unexpected_error_message(&self) -> String {
        match self {
            Token::FunctionArrow => "function arrow",
            Token::OpenBlock => "start of block",
            Token::CloseBlock => "end of block",
            Token::OpenList => "start of list",
            Token::CloseList => "end of list",
            Token::ListSeparator => "comma",
            Token::OpenParen => "open parentheses",
            Token::CloseParen => "close parentheses",
            Token::KeyValueSeparator => "colon",
            Token::Terminator => "semicolon",
            Token::AbsolutePathMarker => "absolute path marker",
            Token::PackagePathMarker => "package path marker",
            Token::PropertyAccessOp => "property access operator",
            Token::Back => "back keyword",
            Token::Break => "break keyword",
            Token::Else => "else keyword",
            Token::Front => "front keyword",
            Token::If => "if keyword",
            Token::Let => "let keyword",
            Token::Loop => "loop keyword",
            Token::Mut => "mut keyword",
            Token::Pub => "pub keyword",
            Token::Return => "return keyword",
            Token::Struct => "struct keyword",
            Token::Use => "use keyword",
            Token::StartJsBlock => "start of #js block",
            Token::Boolean(_) => "boolean",
            Token::BuiltInType(_) => "type",
            Token::Identifier(_) => "identifier",
            Token::Integer(_) => "integer",
            Token::Operator(_) => "operator",
            Token::String(_) => "string",
        }
        .to_string()
    }
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
            Token::PropertyAccessOp => write!(f, "."),
            Token::Back => write!(f, "back"),
            Token::Break => write!(f, "break"),
            Token::Else => write!(f, "else"),
            Token::Front => write!(f, "front"),
            Token::If => write!(f, "if"),
            Token::Let => write!(f, "let"),
            Token::Loop => write!(f, "loop"),
            Token::Mut => write!(f, "mut"),
            Token::Pub => write!(f, "pub"),
            Token::Return => write!(f, "return"),
            Token::Struct => write!(f, "struct"),
            Token::Use => write!(f, "use"),
            Token::StartJsBlock => write!(f, "#js"),
            Token::Boolean(b) => write!(f, "{}", b),
            Token::BuiltInType(t) => write!(f, "{}", t),
            Token::Identifier(i) => write!(f, "{}", i),
            Token::Integer(i) => write!(f, "{}", i),
            Token::Operator(o) => write!(f, "{}", o),
            Token::String(s) => write!(f, "{}", s),
        }
    }
}

pub fn lexer() -> impl Parser<char, Vec<Spanned<Token>>, Error = CompilerError> {
    let num = text::int::<char, CompilerError>(10).map(Token::Integer);

    // Grouping or delimiting marker
    let marker = choice::<_, CompilerError>((
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
        just("~").to(Token::AbsolutePathMarker),
        just("@").to(Token::PackagePathMarker),
        just("#js").to(Token::StartJsBlock),
    ));

    let escape_sequence = just('\\').ignore_then(choice::<_, CompilerError>((
        just('r').to('\r'),
        just('n').to('\n'),
        just('t').to('\t'),
    )));
    let escaped_single_quote = just('\\').ignore_then(just('\''));
    let single_quote_string = just('\'')
        .ignore_then(
            escape_sequence
                .or(escaped_single_quote)
                .or(filter(|c| *c != '\''))
                .repeated(),
        )
        .then_ignore(just('\''))
        .collect::<String>()
        .map(Token::String);

    let escaped_double_quote = just('\\').ignore_then(just('"'));
    let double_quote_string = just('"')
        .ignore_then(
            escape_sequence
                .or(escaped_double_quote)
                .or(filter(|c| *c != '"'))
                .repeated(),
        )
        .then_ignore(just('"'))
        .collect::<String>()
        .map(Token::String);

    let string_ = single_quote_string.or(double_quote_string);

    let operator = choice::<_, CompilerError>((
        just::<char, _, CompilerError>('+').to(Token::Operator(BinaryOperator::Add)),
        just::<char, _, CompilerError>('-').to(Token::Operator(BinaryOperator::Sub)),
        just::<char, _, CompilerError>('*').to(Token::Operator(BinaryOperator::Mul)),
        just::<char, _, CompilerError>('/').to(Token::Operator(BinaryOperator::Div)),
        just::<char, _, CompilerError>('%').to(Token::Operator(BinaryOperator::Modulus)),
        just::<char, _, CompilerError>("==").to(Token::Operator(BinaryOperator::Equal)),
        just::<char, _, CompilerError>("!=").to(Token::Operator(BinaryOperator::NotEqual)),
        just::<char, _, CompilerError>("<=").to(Token::Operator(BinaryOperator::LessThanOrEqual)),
        just::<char, _, CompilerError>("<").to(Token::Operator(BinaryOperator::LessThan)),
        just::<char, _, CompilerError>(">=")
            .to(Token::Operator(BinaryOperator::GreaterThanOrEqual)),
        just::<char, _, CompilerError>(">").to(Token::Operator(BinaryOperator::GreaterThan)),
        just::<char, _, CompilerError>("&&").to(Token::Operator(BinaryOperator::And)),
        just::<char, _, CompilerError>("||").to(Token::Operator(BinaryOperator::Or)),
        just::<char, _, CompilerError>("=").to(Token::Operator(BinaryOperator::Assignment)),
        just::<char, _, CompilerError>(".").to(Token::PropertyAccessOp),
    ));

    let identifier = filter::<char, _, CompilerError>(|c| c.is_ascii_alphabetic() || *c == '_')
        .map(Some)
        .chain::<char, Vec<_>, _>(
            filter(|c: &char| c.is_ascii_alphanumeric() || *c == '_' || *c == '-').repeated(),
        )
        .collect();
    let ident = identifier.map(|ident: String| match ident.as_str() {
        "back" => Token::Back,
        "break" => Token::Break,
        "else" => Token::Else,
        "front" => Token::Front,
        "if" => Token::If,
        "let" => Token::Let,
        "loop" => Token::Loop,
        "mut" => Token::Mut,
        "pub" => Token::Pub,
        "return" => Token::Return,
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

    let comment = just("//")
        .then(take_until(choice((just('\n').ignored(), end()))))
        .padded()
        .ignored();

    token
        .map_with_span(|tok, span| (tok, span))
        .padded_by(comment.repeated())
        .padded()
        .repeated()
}
