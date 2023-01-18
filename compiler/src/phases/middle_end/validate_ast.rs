use chumsky::{prelude::Simple, Error};

use crate::{errors::CompilerError, phases::shared::SymbolTable};

use super::{
    asts::name_resolved::{Expression, ExpressionKind, Module, Statement, StatementKind},
    lexer::BinaryOperator,
};

/// Validate essential semantics about the shape of the AST.
/// This step includes things like checking for invalid assignments.
/// Ex: `1 = 2`
/// This step does not handle things like type checking.
pub fn validate_ast(ctx: (&mut Module, SymbolTable)) {
    let (module, mut symbol_table) = ctx;
    let mut errors = module
        .ast
        .statements
        .iter()
        .map(|s| validate_statement(s, &mut symbol_table))
        .filter(|r| r.is_err())
        .map(|r| r.unwrap_err())
        .collect::<Vec<CompilerError>>();
    module.errors.append(&mut errors);
}

fn validate_statement(
    statement: &Statement,
    symbol_table: &mut SymbolTable,
) -> Result<(), CompilerError> {
    match &statement.kind {
        StatementKind::Expression(expr) => validate_expression(expr, symbol_table),
        StatementKind::FunctionDefinition { body, .. } => validate_expression(&body, symbol_table),
        StatementKind::JsBlock(expressions) => {
            for expr in expressions {
                validate_expression(expr, symbol_table)?;
            }
            Ok(())
        }
        StatementKind::Return(expr) => {
            if let Some(expr) = expr {
                validate_expression(expr, symbol_table)?;
            }
            Ok(())
        }
    }
}

fn validate_expression(
    expression: &Expression,
    symbol_table: &mut SymbolTable,
) -> Result<(), CompilerError> {
    match &expression.kind {
        ExpressionKind::Boolean(_) => Ok(()),
        ExpressionKind::Identifier(_) => Ok(()),
        ExpressionKind::Integer(_) => Ok(()),
        ExpressionKind::String(_) => Ok(()),
        ExpressionKind::Block(block) => {
            for statement in &block.statements {
                validate_statement(statement, symbol_table)?;
            }

            if let Some(expr) = &block.return_expression {
                if let Err(err) = validate_expression(&expr, symbol_table) {
                    return Err(err);
                }
            }

            Ok(())
        }
        ExpressionKind::VariableDeclaration { initializer, .. } => {
            validate_expression(initializer, symbol_table)
        }
        ExpressionKind::BinaryExpression(left, op, _) => match &op {
            BinaryOperator::Assignment => {
                if is_property_access_or_identifier(left) {
                    // TODO: Check if value is mutable
                    // let symbol = symbol_table.get(&left.id).unwrap();
                    // if !symbol.is_mutable {
                    //     Err(Simple::custom(
                    //         expression.span.clone(),
                    //         "Assignment to constant variable",
                    //     ))
                    // } else {
                    //     Ok(())
                    // }
                    eprintln!("TODO: Cannot verify if assigning to an immutable variable");
                    Ok(())
                } else {
                    Err(Simple::custom(
                        expression.span.clone(),
                        "Invalid left-hand side in assignment",
                    )
                    .merge(Simple::expected_input_found(
                        expression.span.clone(),
                        vec![Some(format!(
                            "an identifier or property access, found {}",
                            left.kind.to_human_readable_name().to_string()
                        ))],
                        None,
                    )))
                }
            }
            _ => Ok(()),
        },
        ExpressionKind::PropertyAccess(left, _) => validate_expression(left, symbol_table),
        ExpressionKind::FunctionCall { callee, arguments } => {
            validate_expression(&callee, symbol_table)?;
            for arg in arguments {
                if let Err(err) = validate_expression(arg, symbol_table) {
                    return Err(err);
                }
            }
            Ok(())
        }
        ExpressionKind::If {
            condition,
            body,
            else_,
        } => {
            validate_expression(condition, symbol_table)
                .and_then(|()| validate_expression(&body, symbol_table))?;
            if let Some(e) = else_ {
                validate_expression(e, symbol_table)
            } else {
                Ok(())
            }
        }
    }
}

fn is_property_access_or_identifier(expression: &Expression) -> bool {
    match &expression.kind {
        ExpressionKind::BinaryExpression(_, op, right) => match &right.kind {
            ExpressionKind::BinaryExpression(_, _, _) => is_property_access_or_identifier(right),
            ExpressionKind::Identifier(_) => op == &BinaryOperator::Dot,
            _ => false,
        },
        ExpressionKind::Identifier(_) => true,
        _ => false,
    }
}
