use super::{ast::*, lexer::Lexer, Token};
use crate::phases::frontend::lexer::Spanned;
use crate::{
    errors::CompilerError,
    phases::{
        frontend::{lexer::Operator, Span},
        shared::Type,
    },
};

type Parser<T, E> = fn(&mut Lexer) -> Result<T, E>;
type Peek<T, E> = fn(&Lexer) -> Option<Parser<T, E>>;

pub fn module_parser(path: String, mut lexer: Lexer) -> (ModuleAST, Vec<CompilerError>) {
    let imports = zero_or_more(import::peek, &mut lexer);
    let statements = zero_or_more(top_level_statement::peek, &mut lexer);
    lexer.expect_eof();

    (
        ModuleAST {
            path,
            imports,
            statements,
        },
        lexer.to_errors(),
    )
}

mod import {
    use super::*;

    pub(super) fn peek(lexer: &Lexer) -> Option<Parser<Import, ()>> {
        if lexer.peek() == Some(&Token::Use) {
            Some(parse)
        } else {
            None
        }
    }

    pub(super) fn parse(lexer: &mut Lexer) -> Result<Import, ()> {
        fn path_parser(lexer: &mut Lexer) -> Result<Vec<Identifier>, ()> {
            lexer.expect(Token::Operator(Operator::Div))?;
            let mut path_parts: Vec<Identifier> = Vec::new();
            loop {
                if let Some(parser) = identifier::peek(lexer) {
                    let ident = parser(lexer)?;
                    path_parts.push(ident);

                    lexer.expect(Token::Operator(Operator::Div))?;
                } else if lexer.peek() == Some(&Token::OpenBlock) {
                    break;
                } else {
                    lexer.expected_one_of(vec![Token::Identifier(String::new()), Token::OpenBlock]);
                    return Err(());
                }
            }
            Ok(path_parts)
        }

        fn named_selector_parser(lexer: &mut Lexer) -> Result<ImportSelector, ()> {
            let ident = identifier::parse(lexer)?;
            Ok(ImportSelector {
                span: ident.span,
                kind: ImportSelectorKind::Name(ident.name),
            })
        }

        fn selector_parser(lexer: &mut Lexer) -> Result<Vec<ImportSelector>, ()> {
            lexer.expect(Token::OpenBlock)?;
            let mut selectors: Vec<ImportSelector> = Vec::new();
            if lexer.peek() == Some(&Token::CloseBlock) {
                lexer.consume();
                return Ok(selectors);
            }

            loop {
                let selector = named_selector_parser(lexer)?;
                selectors.push(selector);
                if lexer.peek() == Some(&Token::ListSeparator) {
                    lexer.consume();
                    continue;
                } else if lexer.peek() == Some(&Token::CloseBlock) {
                    lexer.consume();
                    break;
                } else {
                    lexer.expected_one_of(vec![Token::ListSeparator, Token::CloseBlock]);
                }
            }

            Ok(selectors)
        }

        fn package_parser(lexer: &mut Lexer) -> Result<(ImportKind, Span), ()> {
            let start = lexer.span().start;
            lexer.expect(Token::PackagePathMarker)?;
            let scope = identifier::parse(lexer)?;
            lexer.expect(Token::KeyValueSeparator)?;
            let package = identifier::parse(lexer)?;
            let path = path_parser(lexer)?;
            let selectors = selector_parser(lexer)?;
            let (_, terminator_span) = lexer.expect(Token::Terminator)?;
            Ok((
                ImportKind::Package {
                    scope,
                    package,
                    path,
                    selectors,
                },
                start..terminator_span.end,
            ))
        }

        let (_, start) = lexer.expect(Token::Use)?;
        let (package, package_span) = package_parser(lexer)?;
        Ok(Import {
            span: start.start..package_span.end,
            kind: package,
        })
    }
}

mod top_level_statement {
    use super::*;

    pub(super) fn peek(lexer: &Lexer) -> Option<Parser<TopLevelStatement, ()>> {
        match top_level_variable_declaration::peek(lexer) {
            Some(_) => Some(parse),
            None => None,
        }
    }

    pub(super) fn parse(lexer: &mut Lexer) -> Result<TopLevelStatement, ()> {
        match top_level_variable_declaration::peek(lexer) {
            Some(parse) => parse(lexer),
            None => Err(()),
        }
    }

    mod top_level_variable_declaration {
        use super::*;

        pub(super) fn peek(lexer: &Lexer) -> Option<Parser<TopLevelStatement, ()>> {
            let mut slice = lexer.peek_slice(4);
            let token = *slice.get(0).unwrap();
            if token == Some(&Token::Pub) {
                slice.pop_front();
            }

            let token = slice.pop_front().unwrap();
            if token != Some(&Token::Let) && token != Some(&Token::Mut) {
                return None;
            }

            let token = slice.pop_front().unwrap();
            if let Some(&Token::Identifier(_)) = token {
            } else {
                return None;
            }

            let token = slice.pop_front().unwrap();
            if token != Some(&Token::KeyValueSeparator)
                && token != Some(&Token::Operator(Operator::Assignment))
            {
                return None;
            }

            Some(parse)
        }

        pub(super) fn parse(lexer: &mut Lexer) -> Result<TopLevelStatement, ()> {
            let is_public = if lexer.peek() == Some(&Token::Pub) {
                lexer.consume();
                true
            } else {
                false
            };

            let variable_declaration = variable_declaration::parse(lexer)?;
            Ok(TopLevelStatement {
                span: Span::default(),
                kind: TopLevelStatementKind::VariableDeclaration {
                    is_public,
                    is_mutable: variable_declaration.is_mutable,
                    type_: variable_declaration.type_,
                    identifier: variable_declaration.identifier,
                    initializer: variable_declaration.initializer,
                },
            })
        }
    }
}

mod statement {
    use super::*;

    pub(super) fn peek(lexer: &Lexer) -> Option<Parser<Statement, ()>> {
        todo!()
    }

    pub(super) fn parse(lexer: &mut Lexer) -> Result<Statement, ()> {
        todo!()
    }
}

mod type_ {
    use super::*;
    use crate::phases::frontend::BuiltInTypeToken;

    pub(super) fn peek(lexer: &Lexer) -> Option<Parser<Type, ()>> {
        match lexer.peek() {
            Some(&Token::Identifier(_))
            | Some(&Token::BuiltInType(_))
            | Some(&Token::OpenList)
            | Some(&Token::OpenParen) => Some(parse),
            _ => None,
        }
    }

    pub(super) fn parse(lexer: &mut Lexer) -> Result<Type, ()> {
        Ok(match lexer.consume() {
            Some((Token::Identifier(name), _)) => Type::Custom(name.clone()),
            Some((Token::BuiltInType(typ), _)) => match typ {
                BuiltInTypeToken::Bool => Type::Bool,
                BuiltInTypeToken::Int => Type::Int,
                BuiltInTypeToken::String => Type::String,
            },

            // List types
            Some((Token::OpenList, _)) => {
                let inner_type = type_::parse(lexer)?;
                lexer.expect(Token::CloseList)?;
                Type::List(Box::new(inner_type))
            }

            // Function types
            Some((Token::OpenParen, _)) => {
                let mut parameters: Vec<Type> = Vec::new();
                loop {
                    if !parameters.is_empty() {
                        lexer.expect(Token::ListSeparator)?;
                    }
                    match type_::peek(lexer) {
                        None => break,
                        Some(parse) => {
                            parameters.push(parse(lexer)?);
                        }
                    }
                }

                // Allow trailing commas
                if lexer.peek() == Some(&Token::ListSeparator) {
                    lexer.consume();
                }

                lexer.expect(Token::CloseParen)?;
                lexer.expect(Token::FunctionArrow)?;
                let return_type = Box::new(type_::parse(lexer)?);
                Type::Function {
                    parameters,
                    return_type,
                }
            }
            _ => {
                return Err(());
            }
        })
    }
}

struct VariableDeclaration {
    is_mutable: bool,
    type_: Option<Type>,
    identifier: Identifier,
    initializer: Box<Expression>,
}
impl Default for VariableDeclaration {
    fn default() -> Self {
        Self {
            is_mutable: false,
            type_: None,
            identifier: Identifier {
                name: String::new(),
                span: 0usize..0usize,
            },
            initializer: Box::new(Expression {
                span: 0usize..0usize,
                kind: ExpressionKind::Error,
            }),
        }
    }
}

mod variable_declaration {
    use super::*;

    pub(super) fn peek(lexer: &Lexer) -> Option<Parser<VariableDeclaration, ()>> {
        let token = lexer.peek();
        if token == Some(&Token::Let) || token == Some(&Token::Mut) {
            Some(parse)
        } else {
            None
        }
    }

    pub(super) fn parse(lexer: &mut Lexer) -> Result<VariableDeclaration, ()> {
        let is_mutable = if lexer.peek() == Some(&Token::Let) {
            lexer.consume();
            false
        } else if lexer.peek() == Some(&Token::Mut) {
            lexer.consume();
            true
        } else {
            lexer.expected_one_of(vec![Token::Let, Token::Mut]);
            lexer.consume_until_end_of_statement();
            return Err(());
        };

        let identifier = identifier::parse(lexer)?;
        let type_ = if lexer.peek() == Some(&Token::KeyValueSeparator) {
            lexer.consume();
            Some(type_::parse(lexer)?)
        } else {
            None
        };

        lexer.expect_or_end_statement(Token::Operator(Operator::Assignment))?;
        let initializer = Box::new(expression::parse(lexer)?);
        lexer.expect_or_end_statement(Token::Terminator)?;
        Ok(VariableDeclaration {
            is_mutable,
            type_,
            identifier,
            initializer,
        })
    }
}

mod expression {
    use super::*;

    pub(super) fn peek(lexer: &Lexer) -> Option<Parser<Expression, ()>> {
        match non_block_expression::peek(lexer) {
            Some(_) => Some(parse),
            None => None,
        }
    }

    pub(super) fn parse(lexer: &mut Lexer) -> Result<Expression, ()> {
        match non_block_expression::peek(lexer).or(block_based_expression::peek(lexer)) {
            Some(parse) => parse(lexer),
            None => Err(()),
        }
    }
}

mod non_block_expression {
    use super::*;

    pub(super) fn peek(lexer: &Lexer) -> Option<Parser<Expression, ()>> {
        match assignment::peek(lexer) {
            Some(_) => Some(parse),
            None => None,
        }
    }

    pub(super) fn parse(lexer: &mut Lexer) -> Result<Expression, ()> {
        assignment::parse(lexer)
    }

    mod parenthesized {
        use super::*;

        pub(super) fn peek(lexer: &Lexer) -> Option<Parser<Expression, ()>> {
            match lexer.peek() {
                Some(&Token::OpenParen) => Some(parse),
                _ => None,
            }
        }

        pub(super) fn parse(lexer: &mut Lexer) -> Result<Expression, ()> {
            let (_, start) = lexer.expect(Token::OpenParen)?;
            let result = expression::parse(lexer)?;
            let (_, end) = lexer.expect(Token::CloseParen)?;
            Ok(Expression::new(
                ExpressionKind::Parenthesized(Box::new(result)),
                start.start..end.end,
            ))
        }
    }

    mod value {
        use super::*;

        pub(super) fn peek(lexer: &Lexer) -> Option<Parser<Expression, ()>> {
            match lexer.peek() {
                Some(&Token::Boolean(_)) | Some(&Token::Integer(_)) | Some(&Token::String(_)) => {
                    Some(parse)
                }
                _ => None,
            }
        }

        pub(super) fn parse(lexer: &mut Lexer) -> Result<Expression, ()> {
            match lexer.consume() {
                Some((Token::Boolean(value), span)) => {
                    Ok(Expression::new(ExpressionKind::Boolean(value), span))
                }
                Some((Token::Integer(value), span)) => Ok(Expression::new(
                    ExpressionKind::Integer(value.parse().unwrap()),
                    span,
                )),
                Some((Token::String(value), span)) => {
                    Ok(Expression::new(ExpressionKind::String(value), span))
                }
                _ => Err(()),
            }
        }
    }

    mod ident {
        use super::*;

        pub(super) fn peek(lexer: &Lexer) -> Option<Parser<Expression, ()>> {
            match identifier::peek(lexer) {
                Some(_) => Some(parse),
                None => None,
            }
        }

        pub(super) fn parse(lexer: &mut Lexer) -> Result<Expression, ()> {
            identifier::parse(lexer).map(|ident| {
                Expression::new(
                    ExpressionKind::Identifier(ident.clone()),
                    ident.span.clone(),
                )
            })
        }
    }

    mod list {
        use super::*;

        pub(super) fn peek(lexer: &Lexer) -> Option<Parser<Expression, ()>> {
            match lexer.peek() {
                Some(&Token::OpenList) => Some(parse),
                _ => None,
            }
        }

        pub(super) fn parse(lexer: &mut Lexer) -> Result<Expression, ()> {
            let (_, open_list_span) = lexer.expect(Token::OpenList)?;
            let mut elements: Vec<Expression> = Vec::new();

            loop {
                if !elements.is_empty() && lexer.peek() != Some(&Token::CloseList) {
                    lexer.expect(Token::ListSeparator)?;
                }

                match expression::peek(lexer) {
                    Some(parser) => {
                        elements.push(parser(lexer)?);
                    }
                    None => break,
                }
            }

            // Allow trailing commas
            if lexer.peek() == Some(&Token::ListSeparator) {
                lexer.consume();
            }
            let (_, close_list_span) = lexer.expect(Token::CloseList)?;
            let span = open_list_span.start..close_list_span.end;
            Ok(Expression::new(ExpressionKind::List(elements), span))
        }
    }

    mod atom {
        use super::*;

        pub(super) fn peek(lexer: &Lexer) -> Option<Parser<Expression, ()>> {
            match value::peek(lexer)
                .or(ident::peek(lexer))
                .or(list::peek(lexer))
                .or(parenthesized::peek(lexer))
            {
                Some(_) => Some(parse),
                None => None,
            }
        }

        pub(super) fn parse(lexer: &mut Lexer) -> Result<Expression, ()> {
            if let Some(parser) = value::peek(lexer) {
                return parser(lexer);
            }
            if let Some(parser) = ident::peek(lexer) {
                return parser(lexer);
            }
            if let Some(parser) = list::peek(lexer) {
                return parser(lexer);
            }
            if let Some(parser) = parenthesized::peek(lexer) {
                return parser(lexer);
            }

            Err(())
        }
    }

    mod member_access_or_args {
        use super::*;

        pub(super) enum MemberAccessOrArgs {
            Prop(Identifier),
            ArrayAccess(Expression),
            Args((Vec<Expression>, Span)),
        }

        pub(super) fn peek(lexer: &Lexer) -> Option<Parser<MemberAccessOrArgs, ()>> {
            match lexer.peek() {
                Some(&Token::OpenList)
                | Some(&Token::PropertyAccessOp)
                | Some(&Token::OpenParen) => Some(parse),
                _ => None,
            }
        }

        pub(super) fn parse(lexer: &mut Lexer) -> Result<MemberAccessOrArgs, ()> {
            match lexer.consume() {
                Some((Token::OpenList, _)) => {
                    let result = MemberAccessOrArgs::ArrayAccess(expression::parse(lexer)?);
                    lexer.expect(Token::CloseList)?;
                    Ok(result)
                }
                Some((Token::PropertyAccessOp, _)) => {
                    Ok(MemberAccessOrArgs::Prop(identifier::parse(lexer)?))
                }
                Some((Token::OpenParen, open_paren_span)) => {
                    let mut arguments: Vec<Expression> = Vec::new();
                    loop {
                        if !arguments.is_empty() && lexer.peek() != Some(&Token::CloseParen) {
                            lexer.expect(Token::ListSeparator)?;
                        }
                        match expression::peek(&lexer) {
                            None => break,
                            Some(parser) => {
                                let expr = parser(lexer)?;
                                arguments.push(expr);
                            }
                        }
                    }

                    // Allow trailing commas
                    if lexer.peek() == Some(&Token::ListSeparator) {
                        lexer.consume();
                    }

                    let (_, close_paren_span) = lexer.expect(Token::CloseParen)?;
                    let span = open_paren_span.start..close_paren_span.end;
                    Ok(MemberAccessOrArgs::Args((arguments, span)))
                }
                _ => Err(()),
            }
        }
    }

    mod prop_or_fn_call {
        use std::collections::VecDeque;

        use super::{member_access_or_args::MemberAccessOrArgs, *};

        const PEEK_LEFT: Peek<Expression, ()> = atom::peek;
        const PARSE_LEFT: Parser<Expression, ()> = atom::parse;
        const PEEK_RIGHT: Peek<MemberAccessOrArgs, ()> = member_access_or_args::peek;

        pub(super) fn peek(lexer: &Lexer) -> Option<Parser<Expression, ()>> {
            match PEEK_LEFT(lexer) {
                Some(_) => Some(parse),
                None => None,
            }
        }

        pub(super) fn parse(lexer: &mut Lexer) -> Result<Expression, ()> {
            let mut left = PARSE_LEFT(lexer)?;
            let rights = zero_or_more(PEEK_RIGHT, lexer);
            let mut rights = VecDeque::<MemberAccessOrArgs>::from(rights);
            loop {
                match rights.pop_front() {
                    None => break,
                    Some(right) => {
                        left = match right {
                            MemberAccessOrArgs::Prop(right) => {
                                let span = left.span.start..right.span.end;
                                Expression::new(
                                    ExpressionKind::PropertyAccess(Box::new(left), right.clone()),
                                    span,
                                )
                            }
                            MemberAccessOrArgs::ArrayAccess(index) => {
                                let span = left.span.start..index.span.end;
                                Expression::new(
                                    ExpressionKind::ArrayAccess(Box::new(left), Box::new(index)),
                                    span,
                                )
                            }
                            MemberAccessOrArgs::Args((arguments, arg_span)) => {
                                let span = left.span.start..arg_span.end;
                                Expression::new(
                                    ExpressionKind::FunctionCall {
                                        callee: Box::new(left),
                                        arguments,
                                    },
                                    span,
                                )
                            }
                        }
                    }
                }
            }
            Ok(left)
        }
    }

    mod pre_unary {
        use std::collections::VecDeque;

        use super::*;

        const PEEK_RIGHT: Peek<Expression, ()> = prop_or_fn_call::peek;
        const PARSE_RIGHT: Parser<Expression, ()> = prop_or_fn_call::parse;

        pub(super) fn peek(lexer: &Lexer) -> Option<Parser<Expression, ()>> {
            if PEEK_RIGHT(lexer).is_some() {
                return Some(parse);
            }

            match lexer.peek() {
                Some(&Token::Operator(Operator::Increment))
                | Some(&Token::Operator(Operator::Decrement)) => Some(parse),
                _ => None,
            }
        }

        pub(super) fn parse(lexer: &mut Lexer) -> Result<Expression, ()> {
            let mut operators: VecDeque<Spanned<PreUnaryOperator>> = VecDeque::new();
            loop {
                match lexer.peek() {
                    Some(&Token::Operator(Operator::Increment)) => {
                        let (_, span) = lexer.consume().unwrap();
                        operators.push_back((PreUnaryOperator::Increment, span));
                    }
                    Some(&Token::Operator(Operator::Decrement)) => {
                        let (_, span) = lexer.consume().unwrap();
                        operators.push_back((PreUnaryOperator::Decrement, span));
                    }
                    _ => break,
                }
            }

            let mut expr = PARSE_RIGHT(lexer)?;
            loop {
                match operators.pop_front() {
                    Some((operator, span)) => {
                        expr = Expression::new(
                            ExpressionKind::PreUnaryExpression(operator, Box::new(expr)),
                            span,
                        );
                    }
                    None => break,
                }
            }
            Ok(expr)
        }
    }

    mod factor {
        use super::*;

        const PEEK_LEFT: Peek<Expression, ()> = pre_unary::peek;
        const PARSE_LEFT: Parser<Expression, ()> = pre_unary::parse;
        const PARSE_RIGHT: Parser<Expression, ()> = pre_unary::parse;

        pub(super) fn peek(lexer: &Lexer) -> Option<Parser<Expression, ()>> {
            match PEEK_LEFT(lexer) {
                Some(_) => Some(parse),
                None => None,
            }
        }

        pub(super) fn parse(lexer: &mut Lexer) -> Result<Expression, ()> {
            let mut left = PARSE_LEFT(lexer)?;
            loop {
                let operator = lexer.peek().and_then(|t| match t {
                    &Token::Operator(Operator::Mul) => Some(BinaryOperator::Mul),
                    &Token::Operator(Operator::Div) => Some(BinaryOperator::Div),
                    _ => None,
                });
                if let Some(operator) = operator {
                    lexer.consume();

                    let right = PARSE_RIGHT(lexer)?;
                    let span = left.span.start..right.span.end;
                    left = Expression::new(
                        ExpressionKind::BinaryExpression(Box::new(left), operator, Box::new(right)),
                        span,
                    );
                } else {
                    break;
                }
            }
            Ok(left)
        }
    }

    mod sum {
        use super::*;

        const PEEK_LEFT: Peek<Expression, ()> = factor::peek;
        const PARSE_LEFT: Parser<Expression, ()> = factor::parse;
        const PARSE_RIGHT: Parser<Expression, ()> = factor::parse;

        pub(super) fn peek(lexer: &Lexer) -> Option<Parser<Expression, ()>> {
            match PEEK_LEFT(lexer) {
                Some(_) => Some(parse),
                None => None,
            }
        }

        pub(super) fn parse(lexer: &mut Lexer) -> Result<Expression, ()> {
            let mut left = PARSE_LEFT(lexer)?;
            loop {
                let operator = lexer.peek().and_then(|t| match t {
                    &Token::Operator(Operator::Add) => Some(BinaryOperator::Add),
                    &Token::Operator(Operator::Sub) => Some(BinaryOperator::Sub),
                    _ => None,
                });
                if let Some(operator) = operator {
                    lexer.consume();

                    let right = PARSE_RIGHT(lexer)?;
                    let span = left.span.start..right.span.end;
                    left = Expression::new(
                        ExpressionKind::BinaryExpression(Box::new(left), operator, Box::new(right)),
                        span,
                    );
                } else {
                    break;
                }
            }
            Ok(left)
        }
    }

    mod comparison {
        use super::*;

        const PEEK_LEFT: Peek<Expression, ()> = sum::peek;
        const PARSE_LEFT: Parser<Expression, ()> = sum::parse;
        const PARSE_RIGHT: Parser<Expression, ()> = sum::parse;

        pub(super) fn peek(lexer: &Lexer) -> Option<Parser<Expression, ()>> {
            match PEEK_LEFT(lexer) {
                Some(_) => Some(parse),
                None => None,
            }
        }

        pub(super) fn parse(lexer: &mut Lexer) -> Result<Expression, ()> {
            let mut left = PARSE_LEFT(lexer)?;
            loop {
                let operator = lexer.peek().and_then(|t| match t {
                    &Token::Operator(Operator::LessThan) => Some(BinaryOperator::LessThan),
                    &Token::Operator(Operator::LessThanOrEqual) => {
                        Some(BinaryOperator::LessThanOrEqual)
                    }
                    &Token::Operator(Operator::GreaterThan) => Some(BinaryOperator::GreaterThan),
                    &Token::Operator(Operator::GreaterThanOrEqual) => {
                        Some(BinaryOperator::GreaterThanOrEqual)
                    }
                    _ => None,
                });
                if let Some(operator) = operator {
                    lexer.consume();

                    let right = PARSE_RIGHT(lexer)?;
                    let span = left.span.start..right.span.end;
                    left = Expression::new(
                        ExpressionKind::BinaryExpression(Box::new(left), operator, Box::new(right)),
                        span,
                    );
                } else {
                    break;
                }
            }
            Ok(left)
        }
    }

    mod equality {
        use super::*;

        const PEEK_LEFT: Peek<Expression, ()> = comparison::peek;
        const PARSE_LEFT: Parser<Expression, ()> = comparison::parse;
        const PARSE_RIGHT: Parser<Expression, ()> = comparison::parse;

        pub(super) fn peek(lexer: &Lexer) -> Option<Parser<Expression, ()>> {
            match PEEK_LEFT(lexer) {
                Some(_) => Some(parse),
                None => None,
            }
        }

        pub(super) fn parse(lexer: &mut Lexer) -> Result<Expression, ()> {
            let mut left = PARSE_LEFT(lexer)?;
            loop {
                let operator = lexer.peek().and_then(|t| match t {
                    &Token::Operator(Operator::Equal) => Some(BinaryOperator::Equal),
                    &Token::Operator(Operator::NotEqual) => Some(BinaryOperator::NotEqual),
                    _ => None,
                });
                if let Some(operator) = operator {
                    lexer.consume();

                    let right = PARSE_RIGHT(lexer)?;
                    let span = left.span.start..right.span.end;
                    left = Expression::new(
                        ExpressionKind::BinaryExpression(Box::new(left), operator, Box::new(right)),
                        span,
                    );
                } else {
                    break;
                }
            }
            Ok(left)
        }
    }

    mod logical {
        use super::*;

        const PEEK_LEFT: Peek<Expression, ()> = equality::peek;
        const PARSE_LEFT: Parser<Expression, ()> = equality::parse;
        const PARSE_RIGHT: Parser<Expression, ()> = equality::parse;

        pub(super) fn peek(lexer: &Lexer) -> Option<Parser<Expression, ()>> {
            match PEEK_LEFT(lexer) {
                Some(_) => Some(parse),
                None => None,
            }
        }

        pub(super) fn parse(lexer: &mut Lexer) -> Result<Expression, ()> {
            let mut left = PARSE_LEFT(lexer)?;
            loop {
                let operator = lexer.peek().and_then(|t| match t {
                    &Token::Operator(Operator::And) => Some(BinaryOperator::And),
                    &Token::Operator(Operator::Or) => Some(BinaryOperator::Or),
                    _ => None,
                });
                if let Some(operator) = operator {
                    lexer.consume();

                    let right = PARSE_RIGHT(lexer)?;
                    let span = left.span.start..right.span.end;
                    left = Expression::new(
                        ExpressionKind::BinaryExpression(Box::new(left), operator, Box::new(right)),
                        span,
                    );
                } else {
                    break;
                }
            }
            Ok(left)
        }
    }

    mod assignment {
        use super::*;

        const PEEK_LEFT: Peek<Expression, ()> = logical::peek;
        const PARSE_LEFT: Parser<Expression, ()> = logical::parse;
        const PARSE_RIGHT: Parser<Expression, ()> = logical::parse;

        pub(super) fn peek(lexer: &Lexer) -> Option<Parser<Expression, ()>> {
            match PEEK_LEFT(lexer) {
                Some(_) => Some(parse),
                None => None,
            }
        }

        pub(super) fn parse(lexer: &mut Lexer) -> Result<Expression, ()> {
            let mut left = PARSE_LEFT(lexer)?;
            loop {
                if lexer.peek() != Some(&Token::Operator(Operator::Assignment)) {
                    break;
                }
                lexer.consume();

                let right = PARSE_RIGHT(lexer)?;
                let span = left.span.start..right.span.end;
                left = Expression::new(
                    ExpressionKind::BinaryExpression(
                        Box::new(left),
                        BinaryOperator::Assignment,
                        Box::new(right),
                    ),
                    span,
                );
            }
            Ok(left)
        }
    }
}

mod block_based_expression {
    use super::*;

    pub(super) fn peek(lexer: &Lexer) -> Option<Parser<Expression, ()>> {
        match block::peek(lexer)
            .or(if_::peek(lexer))
            .or(js_block::peek(lexer))
        {
            Some(_) => Some(parse),
            None => None,
        }
    }

    pub(super) fn parse(lexer: &mut Lexer) -> Result<Expression, ()> {
        match block::peek(lexer)
            .or(if_::peek(lexer))
            .or(js_block::peek(lexer))
        {
            Some(parse) => parse(lexer),
            None => Err(()),
        }
    }

    mod block {
        use crate::phases::frontend::ast::Expression;

        use super::*;

        pub(super) fn peek(lexer: &Lexer) -> Option<Parser<Expression, ()>> {
            match lexer.peek() {
                Some(&Token::OpenBlock) => Some(parse),
                _ => None,
            }
        }

        pub(super) fn parse(lexer: &mut Lexer) -> Result<Expression, ()> {
            let (_, open_block_span) = lexer.expect(Token::OpenBlock)?;
            let statements = zero_or_more(statement::peek, lexer);
            let return_expression = match expression::peek(lexer) {
                Some(parse) => Some(parse(lexer)?),
                None => None,
            };
            let (_, close_block_span) = lexer.expect(Token::CloseBlock)?;
            let span = open_block_span.start..close_block_span.end;
            let block = Block {
                span: span.clone(),
                statements,
                return_expression,
            };
            Ok(Expression::new(
                ExpressionKind::Block(Box::new(block)),
                span,
            ))
        }
    }

    mod if_ {
        use super::*;

        pub(super) fn peek(lexer: &Lexer) -> Option<Parser<Expression, ()>> {
            match lexer.peek() {
                Some(&Token::If) => Some(parse),
                _ => None,
            }
        }

        pub(super) fn parse(lexer: &mut Lexer) -> Result<Expression, ()> {
            let (_, if_span) = lexer.expect(Token::If)?;
            let condition = expression::parse(lexer)?;
            let body = expression::parse(lexer)?;
            let else_ = if let Some(&Token::Else) = lexer.peek() {
                lexer.consume();
                Some(Box::new(expression::parse(lexer)?))
            } else {
                None
            };
            let end = else_
                .clone()
                .map(|e| e.span.end)
                .unwrap_or(condition.span.end);
            let span = if_span.start..end;
            Ok(Expression::new(
                ExpressionKind::If {
                    condition: Box::new(condition),
                    body: Box::new(body),
                    else_,
                },
                span,
            ))
        }
    }

    mod js_block {
        use super::*;

        pub(super) fn peek(lexer: &Lexer) -> Option<Parser<Expression, ()>> {
            match lexer.peek() {
                Some(&Token::StartJsBlock) => Some(parse),
                _ => None,
            }
        }

        pub(super) fn parse(lexer: &mut Lexer) -> Result<Expression, ()> {
            let (_, start_span) = lexer.expect(Token::StartJsBlock)?;
            let type_ = if lexer.peek() == Some(&Token::KeyValueSeparator) {
                lexer.consume();
                Some(type_::parse(lexer)?)
            } else {
                None
            };

            lexer.expect(Token::OpenBlock)?;
            let expressions = zero_or_more(expression::peek, lexer);
            let (_, end_span) = lexer.expect(Token::CloseBlock)?;
            let span = start_span.start..end_span.end;
            Ok(Expression::new(
                ExpressionKind::JsBlock(type_.unwrap_or(Type::Void), expressions),
                span,
            ))
        }
    }
}

mod identifier {
    use super::*;

    pub(super) fn peek(lexer: &Lexer) -> Option<Parser<Identifier, ()>> {
        match lexer.peek() {
            Some(&Token::Identifier(_)) => Some(parse),
            _ => None,
        }
    }

    pub(super) fn parse(lexer: &mut Lexer) -> Result<Identifier, ()> {
        lexer.expect_identifier()
    }
}

fn zero_or_more<TOk, TErr>(peek: Peek<TOk, TErr>, lexer: &mut Lexer) -> Vec<TOk> {
    let mut results: Vec<TOk> = Vec::new();

    loop {
        match peek(lexer) {
            Some(parser) => match parser(lexer) {
                Ok(res) => {
                    results.push(res);
                }
                Err(_) => break,
            },
            None => break,
        }
    }

    results
}

// fn top_level_statement_parser(
// ) -> impl Parser<Token, TopLevelStatement, Error = CompilerError> + Clone {
//     let variable_declaration = just(Token::Pub)
//         .or_not()
//         .then(just(Token::Let).to(false).or(just(Token::Mut).to(true)))
//         .then(identifier_parser())
//         .then(
//             just(Token::KeyValueSeparator)
//                 .ignore_then(type_parser())
//                 .or_not(),
//         )
//         .then_ignore(just(Token::Operator(Operator::Assignment)))
//         .then(expression_parser(statement_parser()))
//         .then_ignore(just(Token::Terminator))
//         .map_with_span(
//             |((((is_public, is_mutable), identifier), type_), initializer), span| {
//                 TopLevelStatement {
//                     span,
//                     kind: TopLevelStatementKind::VariableDeclaration {
//                         is_public: is_public.is_some(),
//                         is_mutable,
//                         type_,
//                         identifier,
//                         initializer: Box::new(initializer),
//                     },
//                 }
//             },
//         );

//     let block = just(Token::OpenBlock)
//         .ignore_then(statement_parser().repeated())
//         .then(expression_parser(statement_parser()).or_not())
//         .then_ignore(just(Token::CloseBlock))
//         .map_with_span(|(statements, return_expression), span| Block {
//             span: span.clone(),
//             statements,
//             return_expression,
//         });

//     let parameters = identifier_parser()
//         .then_ignore(just(Token::KeyValueSeparator))
//         .then(type_parser())
//         .map(|(identifier, type_)| Parameter::new(identifier.clone().span, identifier, type_))
//         .separated_by(just(Token::ListSeparator))
//         .delimited_by(just(Token::OpenParen), just(Token::CloseParen))
//         .map_with_span(|parameters, span| (parameters, span));

//     let function_definition = just(Token::Pub)
//         .or_not()
//         .then_ignore(just(Token::Let))
//         .then(identifier_parser())
//         .then(parameters)
//         .then(
//             just(Token::KeyValueSeparator)
//                 .ignore_then(type_parser())
//                 .or_not(),
//         )
//         .then(block.clone())
//         .map_with_span(
//             |((((is_public, name), parameters), return_type), body), span| TopLevelStatement {
//                 span,
//                 kind: TopLevelStatementKind::FunctionDefinition {
//                     is_public: is_public.is_some(),
//                     name,
//                     parameters: parameters.0,
//                     return_type: return_type.unwrap_or(Type::Void),
//                     body,
//                 },
//             },
//         );

//     let loop_ = just(Token::Loop)
//         .ignore_then(block)
//         .map_with_span(|block, span| TopLevelStatement {
//             span,
//             kind: TopLevelStatementKind::Loop(block),
//         });

//     let for_loop = just(Token::For)
//         .then(just(Token::OpenParen))
//         .ignore_then(
//             // Pre-loop
//             statement_parser().or_not(),
//         )
//         .then(
//             // Condition
//             expression_parser(statement_parser()).or_not(),
//         )
//         .then_ignore(just(Token::Terminator))
//         .then(
//             // Post-loop
//             expression_parser(statement_parser()).or_not(),
//         )
//         .then_ignore(just(Token::CloseParen))
//         .then(
//             // Body
//             statement_parser()
//                 .repeated()
//                 .delimited_by(just(Token::OpenBlock), just(Token::CloseBlock)),
//         )
//         .map_with_span(
//             |(((initializer, condition), post_loop), body), span| TopLevelStatement {
//                 span,
//                 kind: TopLevelStatementKind::ForLoop {
//                     initializer: initializer.map(Box::new),
//                     condition,
//                     post_loop,
//                     body,
//                 },
//             },
//         );

//     let expression = expression_parser(statement_parser().clone())
//         .then_ignore(just(Token::Terminator))
//         .map(|expression| TopLevelStatement {
//             span: expression.span.clone(),
//             kind: TopLevelStatementKind::Expression(expression),
//         });

//     variable_declaration
//         .or(function_definition)
//         .or(loop_)
//         .or(for_loop)
//         .or(expression)
// }

// fn statement_parser() -> impl Parser<Token, Statement, Error = CompilerError> + Clone {
//     recursive(|statement| {
//         let variable_declaration = just(Token::Let)
//             .to(false)
//             .or(just(Token::Mut).to(true))
//             .then(identifier_parser())
//             .then(
//                 just(Token::KeyValueSeparator)
//                     .ignore_then(type_parser())
//                     .or_not(),
//             )
//             .then_ignore(just(Token::Operator(Operator::Assignment)))
//             .then(expression_parser(statement.clone()))
//             .then_ignore(just(Token::Terminator))
//             .map_with_span(
//                 |(((is_mutable, identifier), type_), initializer), span| Statement {
//                     span,
//                     kind: StatementKind::VariableDeclaration {
//                         is_mutable,
//                         type_,
//                         identifier,
//                         initializer: Box::new(initializer),
//                     },
//                 },
//             );

//         let block = just(Token::OpenBlock)
//             .ignore_then(statement.clone().repeated())
//             .then(expression_parser(statement.clone()).or_not())
//             .then_ignore(just(Token::CloseBlock))
//             .map_with_span(|(statements, return_expression), span| Block {
//                 span: span.clone(),
//                 statements,
//                 return_expression,
//             });

//         let parameters = identifier_parser()
//             .then_ignore(just(Token::KeyValueSeparator))
//             .then(type_parser())
//             .map(|(identifier, type_)| Parameter::new(identifier.clone().span, identifier, type_))
//             .separated_by(just(Token::ListSeparator))
//             .delimited_by(just(Token::OpenParen), just(Token::CloseParen))
//             .map_with_span(|parameters, span| (parameters, span));

//         let function_definition = just(Token::Let)
//             .ignore_then(identifier_parser())
//             .then(parameters)
//             .then(
//                 just(Token::KeyValueSeparator)
//                     .ignore_then(type_parser())
//                     .or_not(),
//             )
//             .then(block.clone())
//             .map_with_span(
//                 |(((name, parameters), return_type), body), span| Statement {
//                     span,
//                     kind: StatementKind::FunctionDefinition {
//                         name,
//                         parameters: parameters.0,
//                         return_type: return_type.unwrap_or(Type::Void),
//                         body,
//                     },
//                 },
//             );

//         let loop_ = just(Token::Loop)
//             .ignore_then(block)
//             .map_with_span(|block, span| Statement {
//                 span,
//                 kind: StatementKind::Loop(block),
//             });

//         let for_loop = just(Token::For)
//             .then(just(Token::OpenParen))
//             .ignore_then(
//                 // Pre-loop
//                 statement.clone().or_not(),
//             )
//             .then(
//                 // Condition
//                 expression_parser(statement.clone()).or_not(),
//             )
//             .then_ignore(just(Token::Terminator))
//             .then(
//                 // Post-loop
//                 expression_parser(statement.clone()).or_not(),
//             )
//             .then_ignore(just(Token::CloseParen))
//             .then(
//                 // Body
//                 statement
//                     .clone()
//                     .repeated()
//                     .delimited_by(just(Token::OpenBlock), just(Token::CloseBlock)),
//             )
//             .map_with_span(
//                 |(((initializer, condition), post_loop), body), span| Statement {
//                     span,
//                     kind: StatementKind::ForLoop {
//                         initializer: initializer.map(Box::new),
//                         condition,
//                         post_loop,
//                         body,
//                     },
//                 },
//             );

//         let break_ = just(Token::Break)
//             .ignore_then(just(Token::Terminator))
//             .map_with_span(|_, span| Statement {
//                 span,
//                 kind: StatementKind::Break,
//             });

//         let return_statement = just(Token::Return)
//             .ignore_then(expression_parser(statement.clone()).or_not())
//             .then_ignore(just(Token::Terminator))
//             .map_with_span(|expression, span| Statement {
//                 span,
//                 kind: StatementKind::Return(expression),
//             });

//         let expression = expression_parser(statement)
//             .then_ignore(just(Token::Terminator))
//             .map(|expression| Statement {
//                 span: expression.span.clone(),
//                 kind: StatementKind::Expression(expression),
//             });

//         variable_declaration
//             .or(function_definition)
//             .or(loop_)
//             .or(for_loop)
//             .or(break_)
//             .or(return_statement)
//             .or(expression)
//     })
// }

// fn expression_parser<'a>(
//     statement: impl Parser<Token, Statement, Error = CompilerError> + Clone + 'a,
// ) -> impl Parser<Token, Expression, Error = CompilerError> + Clone + 'a {
//     recursive(|expr| {
//         let parenthesized_expr: chumsky::combinator::DelimitedBy<
//             Recursive<Token, Expression, CompilerError>,
//             chumsky::primitive::Just<Token, Token, CompilerError>,
//             chumsky::primitive::Just<Token, Token, CompilerError>,
//             Token,
//             Token,
//         > = expr
//             .clone()
//             .delimited_by(just(Token::OpenParen), just(Token::CloseParen));

//         let value = select! {
//             Token::Boolean(b) => ExpressionKind::Boolean(b),
//             Token::Integer(n) => ExpressionKind::Integer(n.parse().unwrap()),
//             Token::String(s) => ExpressionKind::String(s)
//         }
//         .map_with_span(|kind, span| Expression::new(kind, span));

//         let block = just(Token::OpenBlock)
//             .ignore_then(statement.repeated())
//             .then(expr.clone().or_not())
//             .then_ignore(just(Token::CloseBlock))
//             .map_with_span(|(statements, return_expression), span| {
//                 Expression::new(
//                     ExpressionKind::Block(Box::new(Block {
//                         span: span.clone(),
//                         statements,
//                         return_expression,
//                     })),
//                     span,
//                 )
//             });

//         let if_ = just(Token::If)
//             .ignore_then(expr.clone())
//             .then(expr.clone())
//             .then(
//                 just(Token::Else)
//                     .ignore_then(expr.clone())
//                     .map(Box::new)
//                     .or_not(),
//             )
//             .map_with_span(|((condition, body), else_), span| {
//                 Expression::new(
//                     ExpressionKind::If {
//                         condition: Box::new(condition),
//                         body: Box::new(body),
//                         else_,
//                     },
//                     span,
//                 )
//             });

//         let js_block = just(Token::StartJsBlock)
//             .ignore_then(
//                 just(Token::KeyValueSeparator)
//                     .ignore_then(type_parser())
//                     .or_not(),
//             )
//             .then(
//                 expr.clone()
//                     .repeated()
//                     .delimited_by(just(Token::OpenBlock), just(Token::CloseBlock)),
//             )
//             .map_with_span(|(type_, expressions), span| Expression {
//                 span,
//                 kind: ExpressionKind::JsBlock(type_.unwrap_or(Type::Void), expressions),
//             });

//         let list = just(Token::OpenList)
//             .ignore_then(
//                 expr.clone()
//                     .separated_by(just(Token::ListSeparator))
//                     .allow_trailing(),
//             )
//             .then_ignore(just(Token::CloseList))
//             .map_with_span(|expressions, span| Expression {
//                 span,
//                 kind: ExpressionKind::List(expressions),
//             });

//         let atom = js_block
//             .or(list)
//             .or(value)
//             .or(block)
//             .or(identifier_parser().map(|ident| {
//                 Expression::new(ExpressionKind::Identifier(ident.clone()), ident.span)
//             }))
//             .or(if_)
//             // Attempt to recover anything that looks like a list but contains errors
//             .recover_with(nested_delimiters(
//                 Token::OpenList,
//                 Token::CloseList,
//                 [
//                     (Token::OpenParen, Token::CloseParen),
//                     (Token::OpenBlock, Token::CloseBlock),
//                 ],
//                 |span| Expression::new(ExpressionKind::Error, span),
//             ))
//             .or(parenthesized_expr);

//         /*
//          * atom -> identifier | int | string | ...
//          * prop_or_fn_call -> ( '.' identifier | '(' argument_list? ')' )*
//          * dot_member -> atom ('.' identifier)*
//          * fn_call -> dot_member ('(' argument_list? ')')*
//          */
//         enum MemberAccessOrArgs {
//             Prop(Identifier),
//             ArrayAccess(Expression),
//             Args((Vec<Expression>, Span)),
//         }
//         let arguments = expr
//             .clone()
//             .separated_by(just(Token::ListSeparator))
//             .delimited_by(just(Token::OpenParen), just(Token::CloseParen))
//             .map_with_span(|args, span| (args, span));

//         let member_access_or_args = choice::<_, CompilerError>((
//             just(Token::OpenList)
//                 .ignore_then(expr.clone().map(MemberAccessOrArgs::ArrayAccess))
//                 .then_ignore(just(Token::CloseList)),
//             just(Token::PropertyAccessOp)
//                 .ignore_then(identifier_parser().map(MemberAccessOrArgs::Prop)),
//             arguments.map(MemberAccessOrArgs::Args),
//         ))
//         .repeated();

//         let prop_or_fn_call = atom
//             .clone()
//             .then(member_access_or_args)
//             .foldl(|left, right| match right {
//                 MemberAccessOrArgs::Prop(right) => {
//                     let span = left.span.start..right.span.end;
//                     Expression::new(
//                         ExpressionKind::PropertyAccess(Box::new(left), right.clone()),
//                         span,
//                     )
//                 }
//                 MemberAccessOrArgs::Args((arguments, arg_span)) => {
//                     let span = left.span.start..arg_span.end;
//                     Expression::new(
//                         ExpressionKind::FunctionCall {
//                             callee: Box::new(left),
//                             arguments: arguments,
//                         },
//                         span,
//                     )
//                 }
//                 MemberAccessOrArgs::ArrayAccess(index) => {
//                     let span = left.span.start..index.span.end;
//                     Expression::new(
//                         ExpressionKind::ArrayAccess(Box::new(left), Box::new(index)),
//                         span,
//                     )
//                 }
//             });

//         let operator = just(Token::Operator(Operator::Not))
//             .to(PreUnaryOperator::Not)
//             .or(just(Token::Operator(Operator::Increment)).to(PreUnaryOperator::Increment))
//             .or(just(Token::Operator(Operator::Decrement)).to(PreUnaryOperator::Decrement));
//         let pre_unary = operator
//             .repeated()
//             .then(prop_or_fn_call.clone())
//             .map_with_span(|(ops, expr): (Vec<PreUnaryOperator>, Expression), span| {
//                 ops.into_iter().fold(expr, |acc, op| {
//                     Expression::new(
//                         ExpressionKind::PreUnaryExpression(op, Box::new(acc)),
//                         span.clone(),
//                     )
//                 })
//             });

//         let operator = just(Token::Operator(Operator::Mul))
//             .to(BinaryOperator::Mul)
//             .or(just(Token::Operator(Operator::Div)).to(BinaryOperator::Div))
//             .or(just(Token::Operator(Operator::Modulus)).to(BinaryOperator::Modulus));
//         let factor = pre_unary
//             .clone()
//             .then(operator.then(prop_or_fn_call).repeated())
//             .foldl(|left, (op, right)| {
//                 let span = left.span.start..right.span.end;
//                 Expression::new(
//                     ExpressionKind::BinaryExpression(Box::new(left), op, Box::new(right)),
//                     span,
//                 )
//             });

//         let operator = just(Token::Operator(Operator::Add))
//             .to(BinaryOperator::Add)
//             .or(just(Token::Operator(Operator::Sub)).to(BinaryOperator::Sub));
//         let sum =
//             factor
//                 .clone()
//                 .then(operator.then(factor).repeated())
//                 .foldl(|left, (op, right)| {
//                     let span = left.span.start..right.span.end;
//                     Expression::new(
//                         ExpressionKind::BinaryExpression(Box::new(left), op, Box::new(right)),
//                         span,
//                     )
//                 });

//         let operator = just(Token::Operator(Operator::LessThan))
//             .to(BinaryOperator::LessThan)
//             .or(just(Token::Operator(Operator::LessThanOrEqual))
//                 .to(BinaryOperator::LessThanOrEqual))
//             .or(just(Token::Operator(Operator::GreaterThan)).to(BinaryOperator::GreaterThan))
//             .or(just(Token::Operator(Operator::GreaterThanOrEqual))
//                 .to(BinaryOperator::GreaterThanOrEqual));
//         let comparison =
//             sum.clone()
//                 .then(operator.then(sum).repeated())
//                 .foldl(|left, (op, right)| {
//                     let span = left.span.start..right.span.end;
//                     Expression::new(
//                         ExpressionKind::BinaryExpression(Box::new(left), op, Box::new(right)),
//                         span,
//                     )
//                 });

//         let operator = just(Token::Operator(Operator::Equal))
//             .to(BinaryOperator::Equal)
//             .or(just(Token::Operator(Operator::NotEqual)).to(BinaryOperator::NotEqual));
//         let equality = comparison
//             .clone()
//             .then(operator.then(comparison).repeated())
//             .foldl(|left, (op, right)| {
//                 let span = left.span.start..right.span.end;
//                 Expression::new(
//                     ExpressionKind::BinaryExpression(Box::new(left), op, Box::new(right)),
//                     span,
//                 )
//             });

//         let operator = just(Token::Operator(Operator::And))
//             .to(BinaryOperator::And)
//             .or(just(Token::Operator(Operator::Or)).to(BinaryOperator::Or));
//         let logical = equality
//             .clone()
//             .then(operator.then(equality.clone()).repeated())
//             .foldl(|left, (op, right)| {
//                 let span = left.span.start..right.span.end;
//                 Expression::new(
//                     ExpressionKind::BinaryExpression(Box::new(left), op, Box::new(right)),
//                     span,
//                 )
//             });

//         let operator = just(Token::Operator(Operator::Assignment)).to(BinaryOperator::Assignment);
//         let assignment = logical
//             .clone()
//             .then(operator.then(equality).repeated())
//             .foldl(|left, (op, right)| {
//                 let span = left.span.start..right.span.end;
//                 Expression::new(
//                     ExpressionKind::BinaryExpression(Box::new(left), op, Box::new(right)),
//                     span,
//                 )
//             });

//         assignment
//     })
// }
