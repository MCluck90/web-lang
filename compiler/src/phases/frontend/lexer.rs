use chumsky::prelude::*;
use core::fmt;
use std::collections::VecDeque;

use crate::errors::CompilerError;

use super::ast::Identifier;

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
    Modulus,
    Increment,
    Decrement,
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
            Operator::Modulus => write!(f, "%"),
            Operator::Increment => write!(f, "++"),
            Operator::Decrement => write!(f, "--"),
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
    PropertyAccessOp,   // .

    // Keywords
    Back,
    Break,
    Else,
    For,
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
    Operator(Operator),
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
            Token::For => "for keyword",
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
            Token::For => write!(f, "for"),
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

pub struct Lexer {
    tokens: VecDeque<Spanned<Token>>,
    errors: Vec<CompilerError>,
    end: usize,
}
impl Lexer {
    fn new(tokens: Option<Vec<Spanned<Token>>>, errors: Vec<CompilerError>, end: usize) -> Self {
        Self {
            tokens: tokens.map(|t| t.into()).unwrap_or(VecDeque::new()),
            errors,
            end,
        }
    }

    pub fn to_errors(self) -> Vec<CompilerError> {
        self.errors
    }

    pub fn span(&self) -> Span {
        self.tokens
            .front()
            .map(|t| t.1.clone())
            .unwrap_or(self.end..self.end)
    }

    pub fn peek(&self) -> Option<&Token> {
        self.tokens.front().map(|(t, _)| t)
    }

    pub fn peek_slice(&self, length: usize) -> VecDeque<Option<&Token>> {
        let mut slice: VecDeque<Option<&Token>> = VecDeque::new();
        let mut i = 0;
        loop {
            if i == length {
                break;
            }

            slice.push_back(self.tokens.get(i).map(|(t, _)| t));

            i += 1;
        }
        slice
    }

    pub fn peek_with_span(&self) -> Option<Spanned<Token>> {
        self.tokens.front().map(|t| t.clone())
    }

    pub fn consume(&mut self) -> Option<Spanned<Token>> {
        self.tokens.pop_front()
    }

    pub fn expect(&mut self, token: Token) -> Result<Spanned<Token>, ()> {
        match self.peek_with_span() {
            Some((t, span)) => {
                if t == token {
                    Ok(self.consume().unwrap())
                } else {
                    self.errors.push(CompilerError::unexpected_token(
                        &span,
                        vec![Some(token)],
                        Some(t.clone()),
                    ));
                    Err(())
                }
            }
            None => {
                self.errors.push(CompilerError::unexpected_token(
                    &(self.end..self.end),
                    vec![Some(token)],
                    None,
                ));
                Err(())
            }
        }
    }

    pub fn expect_identifier(&mut self) -> Result<Identifier, ()> {
        match self.peek_with_span() {
            Some((Token::Identifier(ident), span)) => {
                self.consume();
                Ok(Identifier {
                    name: ident.clone(),
                    span: span.clone(),
                })
            }
            Some((token, span)) => {
                self.errors.push(CompilerError::unexpected_token(
                    &span,
                    vec![Some(Token::Identifier(String::new()))],
                    Some(token),
                ));
                Err(())
            }
            None => {
                self.errors.push(CompilerError::unexpected_token(
                    &self.span(),
                    vec![Some(Token::Identifier(String::new()))],
                    None,
                ));
                Err(())
            }
        }
    }

    pub fn expect_eof(&mut self) {
        match self.peek_with_span() {
            None => {}
            Some((token, span)) => {
                self.errors.push(CompilerError::unexpected_token(
                    &span,
                    vec![None],
                    Some(token),
                ));
            }
        }
    }

    pub fn expect_or_end_statement(&mut self, token: Token) -> Result<Spanned<Token>, ()> {
        match self.expect(token) {
            Ok(res) => Ok(res),
            Err(err) => {
                self.consume_until_end_of_statement();
                Err(err)
            }
        }
    }

    pub fn consume_until_end_of_statement(&mut self) {
        loop {
            let next = self.peek();
            if next != None && next != Some(&Token::Terminator) {
                self.consume();
            } else {
                break;
            }
        }
    }

    pub fn expected_one_of(&mut self, tokens: Vec<Token>) {
        self.errors.push(CompilerError::unexpected_token(
            &self.span(),
            tokens.into_iter().map(Some).collect(),
            self.peek().map(|t| t.clone()),
        ))
    }
}

pub fn create_lexer(source: &str) -> Lexer {
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
        just::<char, _, CompilerError>("++").to(Token::Operator(Operator::Increment)),
        just::<char, _, CompilerError>("--").to(Token::Operator(Operator::Decrement)),
        just::<char, _, CompilerError>('+').to(Token::Operator(Operator::Add)),
        just::<char, _, CompilerError>('-').to(Token::Operator(Operator::Sub)),
        just::<char, _, CompilerError>('*').to(Token::Operator(Operator::Mul)),
        just::<char, _, CompilerError>('/').to(Token::Operator(Operator::Div)),
        just::<char, _, CompilerError>('%').to(Token::Operator(Operator::Modulus)),
        just::<char, _, CompilerError>("==").to(Token::Operator(Operator::Equal)),
        just::<char, _, CompilerError>("!=").to(Token::Operator(Operator::NotEqual)),
        just::<char, _, CompilerError>("!").to(Token::Operator(Operator::Not)),
        just::<char, _, CompilerError>("<=").to(Token::Operator(Operator::LessThanOrEqual)),
        just::<char, _, CompilerError>("<").to(Token::Operator(Operator::LessThan)),
        just::<char, _, CompilerError>(">=").to(Token::Operator(Operator::GreaterThanOrEqual)),
        just::<char, _, CompilerError>(">").to(Token::Operator(Operator::GreaterThan)),
        just::<char, _, CompilerError>("&&").to(Token::Operator(Operator::And)),
        just::<char, _, CompilerError>("||").to(Token::Operator(Operator::Or)),
        just::<char, _, CompilerError>("=").to(Token::Operator(Operator::Assignment)),
        just::<char, _, CompilerError>(".").to(Token::PropertyAccessOp),
    ));

    fn is_start_or_end_of_identifier(c: &char) -> bool {
        c.is_alphabetic() || *c == '_'
    }
    fn is_middle_of_identifier(c: &char) -> bool {
        c.is_alphanumeric() || *c == '_'
    }
    fn is_dash(c: &char) -> bool {
        *c == '-'
    }
    let identifier = filter::<char, _, CompilerError>(is_start_or_end_of_identifier)
        .map(|c| Some(c.to_string()))
        .chain::<String, Vec<_>, _>(
            filter(|c: &char| is_dash(c))
                .then(filter(is_middle_of_identifier))
                .map(|(a, b)| format!("{}{}", a, b))
                .or(filter(is_middle_of_identifier).map(|c| c.to_string()))
                .repeated(),
        )
        .collect();
    let ident = identifier.map(|ident: String| match ident.as_str() {
        "back" => Token::Back,
        "break" => Token::Break,
        "else" => Token::Else,
        "for" => Token::For,
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

    let (tokens, errors) = token
        .map_with_span(|tok, span| (tok, span))
        .padded_by(comment.repeated())
        .padded()
        .repeated()
        .parse_recovery(source);
    Lexer::new(tokens, errors, source.chars().count())
}
