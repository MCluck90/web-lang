use chumsky::{prelude::Simple, Error};

use crate::{
    lexer::BinaryOperator,
    parser::{Expression, ExpressionKind, Program, Statement, StatementKind},
};

/// Validate essential semantics about the shape of the AST.
/// This step includes things like checking for invalid assignments.
/// Ex: `1 = 2`
/// This step does not handle things like type checking.
pub fn validate_ast(program: Program) -> Result<Program, Vec<Simple<String>>> {
    let errors = program
        .statements
        .iter()
        .map(validate_statement)
        .filter(|r| r.is_err())
        .map(|r| r.unwrap_err())
        .collect::<Vec<Simple<String>>>();
    if !errors.is_empty() {
        Err(errors)
    } else {
        Ok(program)
    }
}

fn validate_statement(statement: &Statement) -> Result<(), Simple<String>> {
    match &statement.kind {
        StatementKind::Expression(expr) => validate_expression(expr),
        StatementKind::FunctionDefinition { body, .. } => validate_expression(&body),
    }
}

fn validate_expression(expression: &Expression) -> Result<(), Simple<String>> {
    match &expression.kind {
        ExpressionKind::Boolean(_) => Ok(()),
        ExpressionKind::Identifier(_) => Ok(()),
        ExpressionKind::Integer(_) => Ok(()),
        ExpressionKind::String(_) => Ok(()),
        ExpressionKind::Block(block) => {
            for statement in &block.statements {
                validate_statement(statement)?;
            }

            if let Some(expr) = &block.return_expression {
                if let Err(err) = validate_expression(&expr) {
                    return Err(err);
                }
            }

            Ok(())
        }
        ExpressionKind::VariableDeclaration { initializer, .. } => validate_expression(initializer),
        ExpressionKind::BinaryExpression(left, op, _) => match &op {
            BinaryOperator::Assignment => {
                if is_property_access_or_identifier(left) {
                    Ok(())
                } else {
                    Err(Simple::custom(
                        expression.span.clone(),
                        "Invalid left-hand side in assignement",
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
        ExpressionKind::FunctionCall { callee, arguments } => {
            validate_expression(&callee)?;
            for arg in arguments {
                if let Err(err) = validate_expression(arg) {
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
            validate_expression(condition).and_then(|()| validate_expression(&body))?;
            if let Some(e) = else_ {
                validate_expression(e)
            } else {
                Ok(())
            }
        }
        ExpressionKind::Error => unreachable!(),
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
