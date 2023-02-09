use super::{ast::*, lexer::Lexer, Token};
use crate::phases::frontend::lexer::Spanned;
use crate::{
    errors::CompilerError,
    phases::{
        frontend::{lexer::Operator, Span},
        shared::Type,
    },
};

type Parser<T> = fn(&mut Lexer) -> Result<T, ()>;
type Peek<T> = fn(&Lexer) -> Option<Parser<T>>;

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

    pub(super) fn peek(lexer: &Lexer) -> Option<Parser<Import>> {
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

    pub(super) fn peek(lexer: &Lexer) -> Option<Parser<TopLevelStatement>> {
        match loop_statement::peek(lexer)
            .or_else(|| for_loop_statement::peek(lexer))
            .or_else(|| environment_block::peek(lexer))
            .or_else(|| top_level_variable_declaration::peek(lexer))
            .or_else(|| top_level_function_definition::peek(lexer))
            .or_else(|| top_level_expression::peek(lexer))
        {
            Some(_) => Some(parse),
            None => None,
        }
    }

    pub(super) fn parse(lexer: &mut Lexer) -> Result<TopLevelStatement, ()> {
        match loop_statement::peek(lexer)
            .or_else(|| for_loop_statement::peek(lexer))
            .or_else(|| environment_block::peek(lexer))
            .or_else(|| top_level_variable_declaration::peek(lexer))
            .or_else(|| top_level_function_definition::peek(lexer))
            .or_else(|| top_level_expression::peek(lexer))
        {
            Some(parse) => {
                let result = parse(lexer);
                lexer.hasta_la_vista();
                result
            }
            None => Err(()),
        }
    }

    mod top_level_variable_declaration {
        use super::*;

        pub(super) fn peek(lexer: &Lexer) -> Option<Parser<TopLevelStatement>> {
            // TODO: Change this so that we check if this is potentially an "Item" (i.e. it starts with `pub`)
            // then we can just use `variable_declaration::peek`. Same with function definitions.
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

    mod top_level_function_definition {
        use super::*;

        pub(super) fn peek(lexer: &Lexer) -> Option<Parser<TopLevelStatement>> {
            let mut slice = lexer.peek_slice(4);
            let token = *slice.get(0).unwrap();
            if token == Some(&Token::Pub) {
                slice.pop_front();
            }

            let token = slice.pop_front().unwrap();
            if token != Some(&Token::Let) {
                return None;
            }

            let token = slice.pop_front().unwrap();
            if let Some(&Token::Identifier(_)) = token {
            } else {
                return None;
            }

            let token = slice.pop_front().unwrap();
            if token != Some(&Token::OpenParen) {
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

            let function_definition = function_definition::parse(lexer)?;
            Ok(TopLevelStatement {
                span: function_definition.span,
                kind: TopLevelStatementKind::FunctionDefinition {
                    is_public,
                    name: function_definition.name,
                    parameters: function_definition.parameters,
                    return_type: function_definition.return_type,
                    body: function_definition.body,
                },
            })
        }
    }

    mod top_level_expression {
        use super::*;

        pub(super) fn peek(lexer: &Lexer) -> Option<Parser<TopLevelStatement>> {
            match expression::peek(lexer) {
                Some(_) => Some(parse),
                None => None,
            }
        }

        pub(super) fn parse(lexer: &mut Lexer) -> Result<TopLevelStatement, ()> {
            if let Some(parse) = non_block_expression::peek(lexer) {
                let expr = parse(lexer)?;
                lexer.expect(Token::Terminator)?;
                Ok(TopLevelStatement {
                    span: expr.span.clone(),
                    kind: TopLevelStatementKind::Expression(expr),
                })
            } else if let Some(parse) = block_based_expression::peek(lexer) {
                let expr = parse(lexer)?;
                Ok(TopLevelStatement {
                    span: expr.span.clone(),
                    kind: TopLevelStatementKind::Expression(expr),
                })
            } else {
                Err(())
            }
        }
    }

    mod loop_statement {
        use super::*;

        pub(super) fn peek(lexer: &Lexer) -> Option<Parser<TopLevelStatement>> {
            match lexer.peek() {
                Some(&Token::Loop) => Some(parse),
                _ => None,
            }
        }

        pub(super) fn parse(lexer: &mut Lexer) -> Result<TopLevelStatement, ()> {
            let (_, start_span) = lexer.expect(Token::Loop)?;
            let block = void_block::parse(lexer)?;
            let span = start_span.start..block.span.end;
            Ok(TopLevelStatement {
                span,
                kind: TopLevelStatementKind::Loop(block),
            })
        }
    }

    mod for_loop_statement {
        use super::*;

        pub(super) fn peek(lexer: &Lexer) -> Option<Parser<TopLevelStatement>> {
            match lexer.peek() {
                Some(&Token::For) => Some(parse),
                _ => None,
            }
        }

        pub(super) fn parse(lexer: &mut Lexer) -> Result<TopLevelStatement, ()> {
            let (_, start_span) = lexer.expect(Token::For)?;
            lexer.expect(Token::OpenParen)?;
            let initializer = match statement::peek(lexer) {
                Some(parse) => {
                    let result = parse(lexer)?;
                    match result {
                        StatementWithTerminationStatus::Terminated(statement) => {
                            Some(Box::new(statement))
                        }
                        StatementWithTerminationStatus::Block(statement) => {
                            lexer.expect(Token::Terminator)?;
                            Some(Box::new(statement))
                        }
                        StatementWithTerminationStatus::NotTerminated(_) => return Err(()),
                    }
                }
                None => None,
            };

            let condition = match expression::peek(lexer) {
                Some(parse) => Some(parse(lexer)?),
                None => None,
            };
            lexer.expect(Token::Terminator)?;

            let post_loop = match expression::peek(lexer) {
                Some(parse) => Some(parse(lexer)?),
                None => None,
            };
            lexer.expect(Token::CloseParen)?;

            let body = void_block::parse(lexer)?;
            let span = start_span.start..body.span.end;

            Ok(TopLevelStatement {
                span,
                kind: TopLevelStatementKind::ForLoop {
                    initializer,
                    condition,
                    post_loop,
                    body: body.statements,
                },
            })
        }
    }

    mod environment_block {
        use super::*;

        pub(super) fn peek(lexer: &Lexer) -> Option<Parser<TopLevelStatement>> {
            match lexer.peek() {
                Some(&Token::Front) | Some(&Token::Back) => Some(parse),
                _ => None,
            }
        }

        pub(super) fn parse(lexer: &mut Lexer) -> Result<TopLevelStatement, ()> {
            let (environment_type, start_span) = match lexer.consume() {
                Some((Token::Back, span)) => (EnvironmentType::Backend, span),
                Some((Token::Front, span)) => (EnvironmentType::Frontend, span),
                _ => {
                    lexer.expected_one_of(vec![Token::Back, Token::Front]);
                    return Err(());
                }
            };

            lexer.expect(Token::OpenBlock)?;
            let mut statements: Vec<Statement> = Vec::new();
            loop {
                match statement::peek(lexer) {
                    Some(parse) => match parse(lexer)? {
                        StatementWithTerminationStatus::Terminated(stmt)
                        | StatementWithTerminationStatus::Block(stmt) => {
                            statements.push(stmt);
                        }
                        StatementWithTerminationStatus::NotTerminated(_) => {
                            lexer.expect(Token::Terminator)?;
                        }
                    },
                    None => break,
                }
            }
            let (_, end_span) = lexer.expect(Token::CloseBlock)?;
            let span = start_span.start..end_span.end;
            Ok(TopLevelStatement {
                span,
                kind: TopLevelStatementKind::EnvironmentBlock(environment_type, statements),
            })
        }
    }
}

enum StatementWithTerminationStatus {
    Terminated(Statement),
    NotTerminated(Statement),
    Block(Statement),
}
mod statement {
    use super::*;

    pub(super) fn peek(lexer: &Lexer) -> Option<Parser<StatementWithTerminationStatus>> {
        match loop_statement::peek(lexer)
            .or_else(|| for_loop_statement::peek(lexer))
            .or_else(|| break_statement::peek(lexer))
            .or_else(|| return_statement::peek(lexer))
            .or_else(|| variable_declaration_statement::peek(lexer))
            .or_else(|| function_definition_statement::peek(lexer))
            .or_else(|| expression_statement::peek(lexer))
        {
            Some(_) => Some(parse),
            None => None,
        }
    }

    pub(super) fn parse(lexer: &mut Lexer) -> Result<StatementWithTerminationStatus, ()> {
        match loop_statement::peek(lexer)
            .or_else(|| for_loop_statement::peek(lexer))
            .or_else(|| break_statement::peek(lexer))
            .or_else(|| return_statement::peek(lexer))
            .or_else(|| variable_declaration_statement::peek(lexer))
            .or_else(|| function_definition_statement::peek(lexer))
            .or_else(|| expression_statement::peek(lexer))
        {
            Some(parse) => {
                let result = parse(lexer);
                lexer.hasta_la_vista();
                result
            }
            None => Err(()),
        }
    }

    mod variable_declaration_statement {
        use super::*;

        pub(super) fn peek(lexer: &Lexer) -> Option<Parser<StatementWithTerminationStatus>> {
            match variable_declaration::peek(lexer) {
                Some(_) => Some(parse),
                None => None,
            }
        }

        pub(super) fn parse(lexer: &mut Lexer) -> Result<StatementWithTerminationStatus, ()> {
            let decl = variable_declaration::parse(lexer)?;
            Ok(StatementWithTerminationStatus::Terminated(Statement {
                span: decl.span,
                kind: StatementKind::VariableDeclaration {
                    is_mutable: decl.is_mutable,
                    type_: decl.type_,
                    identifier: decl.identifier,
                    initializer: decl.initializer,
                },
            }))
        }
    }

    mod function_definition_statement {
        use super::*;

        pub(super) fn peek(lexer: &Lexer) -> Option<Parser<StatementWithTerminationStatus>> {
            match function_definition::peek(lexer) {
                Some(_) => Some(parse),
                None => None,
            }
        }

        pub(super) fn parse(lexer: &mut Lexer) -> Result<StatementWithTerminationStatus, ()> {
            let function = function_definition::parse(lexer)?;
            Ok(StatementWithTerminationStatus::Block(Statement {
                span: function.span,
                kind: StatementKind::FunctionDefinition {
                    name: function.name,
                    parameters: function.parameters,
                    return_type: function.return_type,
                    body: function.body,
                },
            }))
        }
    }

    mod expression_statement {
        use super::*;

        pub(super) fn peek(lexer: &Lexer) -> Option<Parser<StatementWithTerminationStatus>> {
            match expression::peek(lexer) {
                Some(_) => Some(parse),
                None => None,
            }
        }

        pub(super) fn parse(lexer: &mut Lexer) -> Result<StatementWithTerminationStatus, ()> {
            if let Some(parse) = non_block_expression::peek(lexer) {
                let expr = parse(lexer)?;
                let to_status = if lexer.peek() == Some(&Token::Terminator) {
                    lexer.consume();
                    StatementWithTerminationStatus::Terminated
                } else {
                    StatementWithTerminationStatus::NotTerminated
                };
                Ok(to_status(Statement {
                    span: expr.span.clone(),
                    kind: StatementKind::Expression(expr),
                }))
            } else if let Some(parse) = block_based_expression::peek(lexer) {
                let expr = parse(lexer)?;
                Ok(StatementWithTerminationStatus::Block(Statement {
                    span: expr.span.clone(),
                    kind: StatementKind::Expression(expr),
                }))
            } else {
                Err(())
            }
        }
    }

    mod loop_statement {
        use super::*;

        pub(super) fn peek(lexer: &Lexer) -> Option<Parser<StatementWithTerminationStatus>> {
            match lexer.peek() {
                Some(&Token::Loop) => Some(parse),
                _ => None,
            }
        }

        pub(super) fn parse(lexer: &mut Lexer) -> Result<StatementWithTerminationStatus, ()> {
            let (_, start_span) = lexer.expect(Token::Loop)?;
            let block = void_block::parse(lexer)?;
            let span = start_span.start..block.span.end;
            Ok(StatementWithTerminationStatus::Block(Statement {
                span,
                kind: StatementKind::Loop(block),
            }))
        }
    }

    mod for_loop_statement {
        use super::*;

        pub(super) fn peek(lexer: &Lexer) -> Option<Parser<StatementWithTerminationStatus>> {
            match lexer.peek() {
                Some(&Token::For) => Some(parse),
                _ => None,
            }
        }

        pub(super) fn parse(lexer: &mut Lexer) -> Result<StatementWithTerminationStatus, ()> {
            let (_, start_span) = lexer.expect(Token::For)?;
            lexer.expect(Token::OpenParen)?;
            let initializer = match statement::peek(lexer) {
                Some(parse) => {
                    let result = parse(lexer)?;
                    match result {
                        StatementWithTerminationStatus::Terminated(statement) => {
                            Some(Box::new(statement))
                        }
                        StatementWithTerminationStatus::Block(statement) => {
                            lexer.expect(Token::Terminator)?;
                            Some(Box::new(statement))
                        }
                        StatementWithTerminationStatus::NotTerminated(_) => return Err(()),
                    }
                }
                None => None,
            };

            let condition = match expression::peek(lexer) {
                Some(parse) => Some(parse(lexer)?),
                None => None,
            };
            lexer.expect(Token::Terminator)?;

            let post_loop = match expression::peek(lexer) {
                Some(parse) => Some(parse(lexer)?),
                None => None,
            };
            lexer.expect(Token::CloseParen)?;

            let body = void_block::parse(lexer)?;
            let span = start_span.start..body.span.end;
            let body = body.statements;

            Ok(StatementWithTerminationStatus::Block(Statement {
                span,
                kind: StatementKind::ForLoop {
                    initializer,
                    condition,
                    post_loop,
                    body,
                },
            }))
        }
    }

    mod break_statement {
        use super::*;

        pub(super) fn peek(lexer: &Lexer) -> Option<Parser<StatementWithTerminationStatus>> {
            match lexer.peek() {
                Some(&Token::Break) => Some(parse),
                _ => None,
            }
        }

        pub(super) fn parse(lexer: &mut Lexer) -> Result<StatementWithTerminationStatus, ()> {
            let (_, span) = lexer.expect(Token::Break)?;
            lexer.expect(Token::Terminator)?;
            Ok(StatementWithTerminationStatus::Terminated(Statement {
                span,
                kind: StatementKind::Break,
            }))
        }
    }

    mod return_statement {
        use super::*;

        pub(super) fn peek(lexer: &Lexer) -> Option<Parser<StatementWithTerminationStatus>> {
            match lexer.peek() {
                Some(&Token::Return) => Some(parse),
                _ => None,
            }
        }

        pub(super) fn parse(lexer: &mut Lexer) -> Result<StatementWithTerminationStatus, ()> {
            let (_, span) = lexer.expect(Token::Return)?;
            let expression = match expression::peek(lexer) {
                Some(parse) => Some(parse(lexer)?),
                None => None,
            };
            lexer.expect(Token::Terminator)?;
            Ok(StatementWithTerminationStatus::Terminated(Statement {
                span,
                kind: StatementKind::Return(expression),
            }))
        }
    }
}

mod type_ {
    use super::*;
    use crate::phases::frontend::BuiltInTypeToken;

    pub(super) fn peek(lexer: &Lexer) -> Option<Parser<Type>> {
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
                    if !parameters.is_empty() && lexer.peek() != Some(&Token::CloseParen) {
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
    span: Span,
    is_mutable: bool,
    type_: Option<Type>,
    identifier: Identifier,
    initializer: Box<Expression>,
}

mod variable_declaration {
    use super::*;

    pub(super) fn peek(lexer: &Lexer) -> Option<Parser<VariableDeclaration>> {
        let tokens = lexer.peek_slice(3);
        let first_token = *tokens.get(0).unwrap();
        let second_token = *tokens.get(1).unwrap();
        let third_token = *tokens.get(2).unwrap();
        if first_token != Some(&Token::Let) && first_token != Some(&Token::Mut) {
            return None;
        }
        if let Some(&Token::Identifier(_)) = second_token {
        } else {
            return None;
        }

        if third_token != Some(&Token::KeyValueSeparator)
            && third_token != Some(&Token::Operator(Operator::Assignment))
        {
            return None;
        }
        Some(parse)
    }

    pub(super) fn parse(lexer: &mut Lexer) -> Result<VariableDeclaration, ()> {
        let (is_mutable, start_span) = if lexer.peek() == Some(&Token::Let) {
            let (_, span) = lexer.consume().unwrap();
            (false, span)
        } else if lexer.peek() == Some(&Token::Mut) {
            let (_, span) = lexer.consume().unwrap();
            (true, span)
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
        let (_, end_span) = lexer.expect_or_end_statement(Token::Terminator)?;
        Ok(VariableDeclaration {
            span: start_span.start..end_span.end,
            is_mutable,
            type_,
            identifier,
            initializer,
        })
    }
}

struct FunctionDefinition {
    span: Span,
    name: Identifier,
    parameters: Vec<Parameter>,
    return_type: Type,
    body: Block,
}
mod function_definition {
    use super::*;

    pub(super) fn peek(lexer: &Lexer) -> Option<Parser<FunctionDefinition>> {
        let tokens = lexer.peek_slice(3);
        let first_token = *tokens.get(0).unwrap();
        let second_token = *tokens.get(1).unwrap();
        let third_token = *tokens.get(2).unwrap();
        if first_token != Some(&Token::Let) && first_token != Some(&Token::Mut) {
            return None;
        }
        if let Some(&Token::Identifier(_)) = second_token {
        } else {
            return None;
        }

        if third_token != Some(&Token::OpenParen) {
            return None;
        }
        Some(parse)
    }

    pub(super) fn parse(lexer: &mut Lexer) -> Result<FunctionDefinition, ()> {
        let (_, start_span) = lexer.expect(Token::Let)?;
        let name = identifier::parse(lexer)?;
        let parameters = parameters::parse(lexer)?;
        let return_type = match lexer.peek() {
            Some(&Token::KeyValueSeparator) => {
                lexer.consume();
                type_::parse(lexer)?
            }
            _ => Type::Void,
        };
        let body = block::parse(lexer)?;
        let span = start_span.start..body.span.end;
        Ok(FunctionDefinition {
            span,
            name,
            parameters,
            return_type,
            body,
        })
    }

    mod parameters {
        use super::*;

        pub(super) fn parse(lexer: &mut Lexer) -> Result<Vec<Parameter>, ()> {
            lexer.expect(Token::OpenParen)?;
            let mut parameters: Vec<Parameter> = Vec::new();
            loop {
                if !parameters.is_empty() && lexer.peek() != Some(&Token::CloseParen) {
                    lexer.expect(Token::ListSeparator)?;
                }
                match identifier::peek(&lexer) {
                    None => break,
                    Some(parse) => {
                        let ident = parse(lexer)?;
                        lexer.expect(Token::KeyValueSeparator)?;
                        let type_ = type_::parse(lexer)?;
                        parameters.push(Parameter::new(ident.clone().span, ident, type_));
                    }
                }
            }

            // Allow trailing commas
            if lexer.peek() == Some(&Token::ListSeparator) {
                lexer.consume();
            }
            lexer.expect(Token::CloseParen)?;
            Ok(parameters)
        }
    }
}

mod expression {
    use super::*;

    pub(super) fn peek(lexer: &Lexer) -> Option<Parser<Expression>> {
        match non_block_expression::peek(lexer).or_else(|| block_based_expression::peek(lexer)) {
            Some(_) => Some(parse),
            None => None,
        }
    }

    pub(super) fn parse(lexer: &mut Lexer) -> Result<Expression, ()> {
        match non_block_expression::peek(lexer).or_else(|| block_based_expression::peek(lexer)) {
            Some(parse) => parse(lexer),
            None => Err(()),
        }
    }
}

mod non_block_expression {
    use super::*;

    pub(super) fn peek(lexer: &Lexer) -> Option<Parser<Expression>> {
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

        pub(super) fn peek(lexer: &Lexer) -> Option<Parser<Expression>> {
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

        pub(super) fn peek(lexer: &Lexer) -> Option<Parser<Expression>> {
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

        pub(super) fn peek(lexer: &Lexer) -> Option<Parser<Expression>> {
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

        pub(super) fn peek(lexer: &Lexer) -> Option<Parser<Expression>> {
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

        pub(super) fn peek(lexer: &Lexer) -> Option<Parser<Expression>> {
            match value::peek(lexer)
                .or_else(|| ident::peek(lexer))
                .or_else(|| list::peek(lexer))
                .or_else(|| parenthesized::peek(lexer))
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

        pub(super) fn peek(lexer: &Lexer) -> Option<Parser<MemberAccessOrArgs>> {
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

        const PEEK_LEFT: Peek<Expression> = atom::peek;
        const PARSE_LEFT: Parser<Expression> = atom::parse;
        const PEEK_RIGHT: Peek<MemberAccessOrArgs> = member_access_or_args::peek;

        pub(super) fn peek(lexer: &Lexer) -> Option<Parser<Expression>> {
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

        const PEEK_RIGHT: Peek<Expression> = prop_or_fn_call::peek;
        const PARSE_RIGHT: Parser<Expression> = prop_or_fn_call::parse;

        pub(super) fn peek(lexer: &Lexer) -> Option<Parser<Expression>> {
            if PEEK_RIGHT(lexer).is_some() {
                return Some(parse);
            }

            match lexer.peek() {
                Some(&Token::Operator(Operator::Not))
                | Some(&Token::Operator(Operator::Increment))
                | Some(&Token::Operator(Operator::Decrement)) => Some(parse),
                _ => None,
            }
        }

        pub(super) fn parse(lexer: &mut Lexer) -> Result<Expression, ()> {
            let mut operators: VecDeque<Spanned<PreUnaryOperator>> = VecDeque::new();
            loop {
                match lexer.peek() {
                    Some(&Token::Operator(Operator::Not)) => {
                        let (_, span) = lexer.consume().unwrap();
                        operators.push_back((PreUnaryOperator::Not, span));
                    }
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

        const PEEK_LEFT: Peek<Expression> = pre_unary::peek;
        const PARSE_LEFT: Parser<Expression> = pre_unary::parse;
        const PARSE_RIGHT: Parser<Expression> = pre_unary::parse;

        pub(super) fn peek(lexer: &Lexer) -> Option<Parser<Expression>> {
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
                    &Token::Operator(Operator::Modulus) => Some(BinaryOperator::Modulus),
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

        const PEEK_LEFT: Peek<Expression> = factor::peek;
        const PARSE_LEFT: Parser<Expression> = factor::parse;
        const PARSE_RIGHT: Parser<Expression> = factor::parse;

        pub(super) fn peek(lexer: &Lexer) -> Option<Parser<Expression>> {
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

        const PEEK_LEFT: Peek<Expression> = sum::peek;
        const PARSE_LEFT: Parser<Expression> = sum::parse;
        const PARSE_RIGHT: Parser<Expression> = sum::parse;

        pub(super) fn peek(lexer: &Lexer) -> Option<Parser<Expression>> {
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

        const PEEK_LEFT: Peek<Expression> = comparison::peek;
        const PARSE_LEFT: Parser<Expression> = comparison::parse;
        const PARSE_RIGHT: Parser<Expression> = comparison::parse;

        pub(super) fn peek(lexer: &Lexer) -> Option<Parser<Expression>> {
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

        const PEEK_LEFT: Peek<Expression> = equality::peek;
        const PARSE_LEFT: Parser<Expression> = equality::parse;
        const PARSE_RIGHT: Parser<Expression> = equality::parse;

        pub(super) fn peek(lexer: &Lexer) -> Option<Parser<Expression>> {
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

        const PEEK_LEFT: Peek<Expression> = logical::peek;
        const PARSE_LEFT: Parser<Expression> = logical::parse;
        const PARSE_RIGHT: Parser<Expression> = logical::parse;

        pub(super) fn peek(lexer: &Lexer) -> Option<Parser<Expression>> {
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

    pub(super) fn peek(lexer: &Lexer) -> Option<Parser<Expression>> {
        match block_expr::peek(lexer)
            .or_else(|| if_::peek(lexer))
            .or_else(|| js_block::peek(lexer))
        {
            Some(_) => Some(parse),
            None => None,
        }
    }

    pub(super) fn parse(lexer: &mut Lexer) -> Result<Expression, ()> {
        match block_expr::peek(lexer)
            .or_else(|| if_::peek(lexer))
            .or_else(|| js_block::peek(lexer))
        {
            Some(parse) => parse(lexer),
            None => Err(()),
        }
    }

    mod block_expr {
        use crate::phases::frontend::ast::Expression;

        use super::*;

        pub(super) fn peek(lexer: &Lexer) -> Option<Parser<Expression>> {
            match block::peek(lexer) {
                Some(_) => Some(parse),
                None => None,
            }
        }

        pub(super) fn parse(lexer: &mut Lexer) -> Result<Expression, ()> {
            let block = block::parse(lexer)?;
            let span = block.span.clone();
            Ok(Expression::new(
                ExpressionKind::Block(Box::new(block)),
                span,
            ))
        }
    }

    mod if_ {
        use super::*;

        pub(super) fn peek(lexer: &Lexer) -> Option<Parser<Expression>> {
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

        pub(super) fn peek(lexer: &Lexer) -> Option<Parser<Expression>> {
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

mod block {
    use super::*;

    pub(super) fn peek(lexer: &Lexer) -> Option<Parser<Block>> {
        match lexer.peek() {
            Some(&Token::OpenBlock) => Some(parse),
            _ => None,
        }
    }

    pub(super) fn parse(lexer: &mut Lexer) -> Result<Block, ()> {
        let (_, start_span) = lexer.expect(Token::OpenBlock)?;
        let mut statements: Vec<Statement> = Vec::new();
        let mut return_expression: Option<Expression> = None;
        let mut has_hit_non_terminated = false;
        loop {
            if let Some(parse) = statement::peek(lexer) {
                if has_hit_non_terminated {
                    lexer.expect(Token::Terminator)?;
                }

                let statement = parse(lexer)?;
                if let Some(expr) = return_expression {
                    // Moves things like `if` expressions in to the statements list
                    statements.push(Statement {
                        span: expr.span.clone(),
                        kind: StatementKind::Expression(expr),
                    });
                    return_expression = None;
                }
                match statement {
                    StatementWithTerminationStatus::Terminated(statement) => {
                        statements.push(statement);
                    }
                    StatementWithTerminationStatus::NotTerminated(statement) => {
                        has_hit_non_terminated = true;
                        match statement.kind {
                            StatementKind::Expression(expr) => {
                                return_expression = Some(expr);
                            }
                            StatementKind::FunctionDefinition { .. }
                            | StatementKind::ForLoop { .. }
                            | StatementKind::Loop(_) => {
                                statements.push(statement);
                            }
                            StatementKind::VariableDeclaration { .. }
                            | StatementKind::Return(_)
                            | StatementKind::Break => unreachable!(),
                        }
                    }
                    StatementWithTerminationStatus::Block(statement) => match statement.kind {
                        StatementKind::Expression(expr) => {
                            return_expression = Some(expr);
                        }
                        StatementKind::FunctionDefinition { .. }
                        | StatementKind::ForLoop { .. }
                        | StatementKind::Loop(_) => {
                            statements.push(statement);
                        }
                        StatementKind::VariableDeclaration { .. }
                        | StatementKind::Return(_)
                        | StatementKind::Break => unreachable!(),
                    },
                }
            } else {
                break;
            }
        }
        let (_, end_span) = lexer.expect(Token::CloseBlock)?;
        let span = start_span.start..end_span.end;
        Ok(Block {
            span,
            statements,
            return_expression,
        })
    }
}

// A block but must evaluate to a `void` value. Useful for loops
mod void_block {
    use super::*;

    pub(super) fn parse(lexer: &mut Lexer) -> Result<Block, ()> {
        let (_, start_span) = lexer.expect(Token::OpenBlock)?;
        let mut statements: Vec<Statement> = Vec::new();
        loop {
            if let Some(parse) = statement::peek(lexer) {
                let statement = parse(lexer)?;
                match statement {
                    StatementWithTerminationStatus::Terminated(statement) => {
                        statements.push(statement);
                    }
                    StatementWithTerminationStatus::NotTerminated(_) => {
                        lexer.expect(Token::Terminator)?;
                    }
                    StatementWithTerminationStatus::Block(statement) => match statement.kind {
                        StatementKind::Expression(expr) => {
                            statements.push(Statement {
                                span: expr.span.clone(),
                                kind: StatementKind::Expression(expr),
                            });
                        }
                        StatementKind::ForLoop { .. } | StatementKind::Loop(_) => {
                            statements.push(statement);
                        }
                        StatementKind::VariableDeclaration { .. }
                        | StatementKind::FunctionDefinition { .. }
                        | StatementKind::Return(_)
                        | StatementKind::Break => unreachable!(),
                    },
                }
            } else {
                break;
            }
        }
        let (_, end_span) = lexer.expect(Token::CloseBlock)?;
        let span = start_span.start..end_span.end;
        Ok(Block {
            span,
            statements,
            return_expression: None,
        })
    }
}

mod identifier {
    use super::*;

    pub(super) fn peek(lexer: &Lexer) -> Option<Parser<Identifier>> {
        match lexer.peek() {
            Some(&Token::Identifier(_)) => Some(parse),
            _ => None,
        }
    }

    pub(super) fn parse(lexer: &mut Lexer) -> Result<Identifier, ()> {
        lexer.expect_identifier()
    }
}

fn zero_or_more<TOk>(peek: Peek<TOk>, lexer: &mut Lexer) -> Vec<TOk> {
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
