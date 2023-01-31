use chumsky::prelude::*;

use crate::{errors::CompilerError, phases::shared::Type};

use super::{ast::*, BinaryOperator, BuiltInTypeToken, Span, Token};

pub fn module_parser(path: String) -> impl Parser<Token, ModuleAST, Error = CompilerError> + Clone {
    import_parser()
        .repeated()
        .then(top_level_statement_parser().repeated().then_ignore(end()))
        .map(move |(imports, statements)| ModuleAST {
            path: path.clone(),
            imports,
            statements,
        })
}

fn identifier_parser() -> impl Parser<Token, Identifier, Error = CompilerError> + Clone {
    select! {
        Token::Identifier(i) => i
    }
    .map_with_span(|name, span| Identifier { name, span })
}

fn type_parser() -> impl Parser<Token, Type, Error = CompilerError> + Clone {
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
        let list_type = just(Token::OpenList)
            .ignore_then(type_parser.clone())
            .then_ignore(just(Token::CloseList))
            .map(|t| Type::List(Box::new(t)));

        simple_type.or(function_type).or(list_type)
    })
}

fn import_parser() -> impl Parser<Token, Import, Error = CompilerError> + Clone {
    let path_parser = just(Token::Operator(BinaryOperator::Div))
        .map(|_| Vec::<Identifier>::new())
        .or(identifier_parser()
            .separated_by(just(Token::ListSeparator))
            .then_ignore(just(Token::Operator(BinaryOperator::Div))))
        .or_not();

    let named_selector_parser = identifier_parser().map_with_span(|ident, span| ImportSelector {
        span,
        kind: ImportSelectorKind::Name(ident.name),
    });
    let selector_parser = named_selector_parser
        .separated_by(just(Token::ListSeparator))
        .delimited_by(just(Token::OpenBlock), just(Token::CloseBlock));

    let package_parser = just(Token::PackagePathMarker)
        .ignore_then(identifier_parser())
        .then_ignore(just(Token::KeyValueSeparator))
        .then(identifier_parser())
        .then_ignore(just(Token::Operator(BinaryOperator::Div)))
        .then(path_parser)
        .then(selector_parser)
        .map_with_span(|(((scope, package), path), selectors), span| Import {
            span,
            kind: ImportKind::Package {
                scope,
                package,
                path: path.unwrap_or(Vec::new()),
                selectors,
            },
        });

    just(Token::Use)
        .ignore_then(package_parser)
        .then_ignore(just(Token::Terminator))
}

fn top_level_statement_parser(
) -> impl Parser<Token, TopLevelStatement, Error = CompilerError> + Clone {
    let variable_declaration = just(Token::Pub)
        .or_not()
        .then(just(Token::Let).to(false).or(just(Token::Mut).to(true)))
        .then(identifier_parser())
        .then(
            just(Token::KeyValueSeparator)
                .ignore_then(type_parser())
                .or_not(),
        )
        .then_ignore(just(Token::Operator(BinaryOperator::Assignment)))
        .then(expression_parser(statement_parser()))
        .then_ignore(just(Token::Terminator))
        .map_with_span(
            |((((is_public, is_mutable), identifier), type_), initializer), span| {
                TopLevelStatement {
                    span,
                    kind: TopLevelStatementKind::VariableDeclaration {
                        is_public: is_public.is_some(),
                        is_mutable,
                        type_,
                        identifier,
                        initializer: Box::new(initializer),
                    },
                }
            },
        );

    let block = just(Token::OpenBlock)
        .ignore_then(statement_parser().repeated())
        .then(expression_parser(statement_parser()).or_not())
        .then_ignore(just(Token::CloseBlock))
        .map_with_span(|(statements, return_expression), span| Block {
            span: span.clone(),
            statements,
            return_expression,
        });

    let parameters = identifier_parser()
        .then_ignore(just(Token::KeyValueSeparator))
        .then(type_parser())
        .map(|(identifier, type_)| Parameter::new(identifier.clone().span, identifier, type_))
        .separated_by(just(Token::ListSeparator))
        .delimited_by(just(Token::OpenParen), just(Token::CloseParen))
        .map_with_span(|parameters, span| (parameters, span));

    let function_definition = just(Token::Pub)
        .or_not()
        .then_ignore(just(Token::Let))
        .then(identifier_parser())
        .then(parameters)
        .then(
            just(Token::KeyValueSeparator)
                .ignore_then(type_parser())
                .or_not(),
        )
        .then(block.clone())
        .map_with_span(
            |((((is_public, name), parameters), return_type), body), span| TopLevelStatement {
                span,
                kind: TopLevelStatementKind::FunctionDefinition {
                    is_public: is_public.is_some(),
                    name,
                    parameters: parameters.0,
                    return_type: return_type.unwrap_or(Type::Void),
                    body,
                },
            },
        );

    let loop_ = just(Token::Loop)
        .ignore_then(block)
        .map_with_span(|block, span| TopLevelStatement {
            span,
            kind: TopLevelStatementKind::Loop(block),
        });

    let for_loop = just(Token::For)
        .then(just(Token::OpenParen))
        .ignore_then(
            // Pre-loop
            statement_parser().or_not(),
        )
        .then(
            // Condition
            expression_parser(statement_parser()).or_not(),
        )
        .then_ignore(just(Token::Terminator))
        .then(
            // Post-loop
            expression_parser(statement_parser()).or_not(),
        )
        .then_ignore(just(Token::CloseParen))
        .then(
            // Body
            statement_parser()
                .repeated()
                .delimited_by(just(Token::OpenBlock), just(Token::CloseBlock)),
        )
        .map_with_span(
            |(((initializer, condition), post_loop), body), span| TopLevelStatement {
                span,
                kind: TopLevelStatementKind::ForLoop {
                    initializer: initializer.map(Box::new),
                    condition,
                    post_loop,
                    body,
                },
            },
        );

    let expression = expression_parser(statement_parser().clone())
        .then_ignore(just(Token::Terminator))
        .map(|expression| TopLevelStatement {
            span: expression.span.clone(),
            kind: TopLevelStatementKind::Expression(expression),
        });

    variable_declaration
        .or(function_definition)
        .or(loop_)
        .or(for_loop)
        .or(expression)
}

fn statement_parser() -> impl Parser<Token, Statement, Error = CompilerError> + Clone {
    recursive(|statement| {
        let variable_declaration = just(Token::Let)
            .to(false)
            .or(just(Token::Mut).to(true))
            .then(identifier_parser())
            .then_ignore(just(Token::Operator(BinaryOperator::Assignment)))
            .then(expression_parser(statement.clone()))
            .then_ignore(just(Token::Terminator))
            .map_with_span(|((is_mutable, identifier), initializer), span| Statement {
                span,
                kind: StatementKind::VariableDeclaration {
                    is_mutable,
                    identifier,
                    initializer: Box::new(initializer),
                },
            });

        let block = just(Token::OpenBlock)
            .ignore_then(statement.clone().repeated())
            .then(expression_parser(statement.clone()).or_not())
            .then_ignore(just(Token::CloseBlock))
            .map_with_span(|(statements, return_expression), span| Block {
                span: span.clone(),
                statements,
                return_expression,
            });

        let parameters = identifier_parser()
            .then_ignore(just(Token::KeyValueSeparator))
            .then(type_parser())
            .map(|(identifier, type_)| Parameter::new(identifier.clone().span, identifier, type_))
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
            .then(block.clone())
            .map_with_span(
                |(((name, parameters), return_type), body), span| Statement {
                    span,
                    kind: StatementKind::FunctionDefinition {
                        name,
                        parameters: parameters.0,
                        return_type: return_type.unwrap_or(Type::Void),
                        body,
                    },
                },
            );

        let loop_ = just(Token::Loop)
            .ignore_then(block)
            .map_with_span(|block, span| Statement {
                span,
                kind: StatementKind::Loop(block),
            });

        let for_loop = just(Token::For)
            .then(just(Token::OpenParen))
            .ignore_then(
                // Pre-loop
                statement.clone().or_not(),
            )
            .then(
                // Condition
                expression_parser(statement.clone()).or_not(),
            )
            .then_ignore(just(Token::Terminator))
            .then(
                // Post-loop
                expression_parser(statement.clone()).or_not(),
            )
            .then_ignore(just(Token::CloseParen))
            .then(
                // Body
                statement
                    .clone()
                    .repeated()
                    .delimited_by(just(Token::OpenBlock), just(Token::CloseBlock)),
            )
            .map_with_span(
                |(((initializer, condition), post_loop), body), span| Statement {
                    span,
                    kind: StatementKind::ForLoop {
                        initializer: initializer.map(Box::new),
                        condition,
                        post_loop,
                        body,
                    },
                },
            );

        let break_ = just(Token::Break)
            .ignore_then(just(Token::Terminator))
            .map_with_span(|_, span| Statement {
                span,
                kind: StatementKind::Break,
            });

        let return_statement = just(Token::Return)
            .ignore_then(expression_parser(statement.clone()).or_not())
            .then_ignore(just(Token::Terminator))
            .map_with_span(|expression, span| Statement {
                span,
                kind: StatementKind::Return(expression),
            });

        let expression = expression_parser(statement)
            .then_ignore(just(Token::Terminator))
            .map(|expression| Statement {
                span: expression.span.clone(),
                kind: StatementKind::Expression(expression),
            });

        variable_declaration
            .or(function_definition)
            .or(loop_)
            .or(for_loop)
            .or(break_)
            .or(return_statement)
            .or(expression)
    })
}

fn expression_parser<'a>(
    statement: impl Parser<Token, Statement, Error = CompilerError> + Clone + 'a,
) -> impl Parser<Token, Expression, Error = CompilerError> + Clone + 'a {
    recursive(|expr| {
        let parenthesized_expr: chumsky::combinator::DelimitedBy<
            Recursive<Token, Expression, CompilerError>,
            chumsky::primitive::Just<Token, Token, CompilerError>,
            chumsky::primitive::Just<Token, Token, CompilerError>,
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
        .map_with_span(|kind, span| Expression::new(kind, span));

        let block = just(Token::OpenBlock)
            .ignore_then(statement.repeated())
            .then(expr.clone().or_not())
            .then_ignore(just(Token::CloseBlock))
            .map_with_span(|(statements, return_expression), span| {
                Expression::new(
                    ExpressionKind::Block(Box::new(Block {
                        span: span.clone(),
                        statements,
                        return_expression,
                    })),
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
                    ExpressionKind::If {
                        condition: Box::new(condition),
                        body: Box::new(body),
                        else_,
                    },
                    span,
                )
            });

        let js_block = just(Token::StartJsBlock)
            .ignore_then(
                just(Token::KeyValueSeparator)
                    .ignore_then(type_parser())
                    .or_not(),
            )
            .then(
                expr.clone()
                    .repeated()
                    .delimited_by(just(Token::OpenBlock), just(Token::CloseBlock)),
            )
            .map_with_span(|(type_, expressions), span| Expression {
                span,
                kind: ExpressionKind::JsBlock(type_.unwrap_or(Type::Void), expressions),
            });

        let list = just(Token::OpenList)
            .ignore_then(
                expr.clone()
                    .separated_by(just(Token::ListSeparator))
                    .allow_trailing(),
            )
            .then_ignore(just(Token::CloseList))
            .map_with_span(|expressions, span| Expression {
                span,
                kind: ExpressionKind::List(expressions),
            });

        let atom = js_block
            .or(list)
            .or(value)
            .or(block)
            .or(identifier_parser().map(|ident| {
                Expression::new(ExpressionKind::Identifier(ident.clone()), ident.span)
            }))
            .or(if_)
            // Attempt to recover anything that looks like a list but contains errors
            .recover_with(nested_delimiters(
                Token::OpenList,
                Token::CloseList,
                [
                    (Token::OpenParen, Token::CloseParen),
                    (Token::OpenBlock, Token::CloseBlock),
                ],
                |span| Expression::new(ExpressionKind::Error, span),
            ))
            .or(parenthesized_expr);

        /*
         * atom -> identifier | int | string | ...
         * prop_or_fn_call -> ( '.' identifier | '(' argument_list? ')' )*
         * dot_member -> atom ('.' identifier)*
         * fn_call -> dot_member ('(' argument_list? ')')*
         */

        enum PropOrArgs {
            Prop(Identifier),
            Args((Vec<Expression>, Span)),
        }
        let arguments = expr
            .clone()
            .separated_by(just(Token::ListSeparator))
            .delimited_by(just(Token::OpenParen), just(Token::CloseParen))
            .map_with_span(|args, span| (args, span));

        let dot_or_args = choice::<_, CompilerError>((
            just(Token::PropertyAccessOp).ignore_then(identifier_parser().map(PropOrArgs::Prop)),
            arguments.map(PropOrArgs::Args),
        ))
        .repeated();

        let prop_or_fn_call = atom
            .clone()
            .then(dot_or_args)
            .foldl(|left, right| match right {
                PropOrArgs::Prop(right) => {
                    let span = left.span.start..right.span.end;
                    Expression::new(
                        ExpressionKind::PropertyAccess(Box::new(left), right.clone()),
                        span,
                    )
                }
                PropOrArgs::Args((arguments, arg_span)) => {
                    let span = left.span.start..arg_span.end;
                    Expression::new(
                        ExpressionKind::FunctionCall {
                            callee: Box::new(left),
                            arguments: arguments,
                        },
                        span,
                    )
                }
            });

        let operator = just(Token::Operator(BinaryOperator::Mul))
            .to(BinaryOperator::Mul)
            .or(just(Token::Operator(BinaryOperator::Div)).to(BinaryOperator::Div))
            .or(just(Token::Operator(BinaryOperator::Modulus)).to(BinaryOperator::Modulus));
        let factor = prop_or_fn_call
            .clone()
            .then(operator.then(prop_or_fn_call).repeated())
            .foldl(|left, (op, right)| {
                let span = left.span.start..right.span.end;
                Expression::new(
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
                    ExpressionKind::BinaryExpression(Box::new(left), op, Box::new(right)),
                    span,
                )
            });

        assignment
    })
}
