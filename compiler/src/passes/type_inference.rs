use chumsky::{prelude::Simple, Error};

use crate::{
    lexer::{Operator, Span},
    parser::{Expression, ExpressionKind, Program},
};

use super::shared::{SymbolTable, Type};

pub fn infer_types(
    ctx: (Program, SymbolTable),
) -> Result<(Program, SymbolTable), Vec<Simple<String>>> {
    let (program, symbol_table) = ctx;
    visit_program(program, symbol_table)
}

fn visit_program(
    program: Program,
    mut symbol_table: SymbolTable,
) -> Result<(Program, SymbolTable), Vec<Simple<String>>> {
    let expression_results = program
        .expressions
        .iter()
        .map(|expr| visit_expression(expr, &mut symbol_table))
        .collect::<Vec<_>>();
    let expression_errors = expression_results
        .iter()
        .filter_map(|res| res.clone().err())
        .collect::<Vec<_>>();
    if !expression_errors.is_empty() {
        Err(expression_errors)
    } else {
        Ok((program, symbol_table))
    }
}

fn to_missing_node_error(expression: &Expression) -> Result<(), Simple<String>> {
    Err(Simple::custom(
        expression.span.clone(),
        "Node is not available in the symbol table",
    ))
}

fn create_type_mismatch_error(
    span: &Span,
    expected_type: &Type,
    found_type: &Type,
) -> Result<(), Simple<String>> {
    Err(
        Simple::custom(span.clone(), "Type mismatch").merge(Simple::expected_input_found(
            span.clone(),
            vec![Some(format!("{}, found {}", expected_type, found_type))],
            None,
        )),
    )
}

fn visit_expression(
    expression: &Expression,
    symbol_table: &mut SymbolTable,
) -> Result<(), Simple<String>> {
    match &expression.kind {
        ExpressionKind::Boolean(_) => {
            if !symbol_table.set_type(&expression.id, Type::Bool) {
                to_missing_node_error(expression)
            } else {
                Ok(())
            }
        }
        ExpressionKind::Integer(_) => {
            if !symbol_table.set_type(&expression.id, Type::Int) {
                to_missing_node_error(expression)
            } else {
                Ok(())
            }
        }
        ExpressionKind::String(_) => {
            if !symbol_table.set_type(&expression.id, Type::String) {
                to_missing_node_error(expression)
            } else {
                Ok(())
            }
        }

        ExpressionKind::Identifier(_) => Ok(()),
        ExpressionKind::Block(expressions) => {
            for expr in expressions {
                visit_expression(expr, symbol_table)?;
            }
            let output_type = expressions.last().map_or(Type::Void, |expr| {
                symbol_table.get(&expr.id).unwrap().type_.clone()
            });
            symbol_table.set_type(&expression.id, output_type);
            Ok(())
        }
        ExpressionKind::VariableDeclaration {
            identifier,
            initializer,
            ..
        } => {
            visit_expression(initializer, symbol_table)?;
            let initializer_symbol = symbol_table.get(&initializer.id).unwrap();
            symbol_table.set_type(&identifier.id, initializer_symbol.type_.clone());
            Ok(())
        }
        ExpressionKind::FunctionDefinition {
            name,
            parameters,
            body,
        } => todo!(),
        ExpressionKind::BinaryExpression(left, op, right) => {
            visit_expression(left, symbol_table)?;
            visit_expression(right, symbol_table)?;

            let right_type = symbol_table.get_mut(&right.id).unwrap().type_.clone();
            let mut left = symbol_table.get_mut(&left.id).unwrap();
            match &op {
                Operator::Assignment => {
                    if left.type_ == Type::Unknown {
                        left.type_ = right_type.clone();
                    } else {
                        return create_type_mismatch_error(&right.span, &left.type_, &right_type);
                    }
                    symbol_table.set_type(&expression.id, right_type);
                    Ok(())
                }
                Operator::Add => todo!(),
                Operator::Sub => todo!(),
                Operator::Mul => todo!(),
                Operator::Div => todo!(),
                Operator::Dot => todo!(),
                Operator::Not => todo!(),
                Operator::NotEqual => todo!(),
                Operator::Equal => todo!(),
                Operator::LessThan => todo!(),
                Operator::LessThanOrEqual => todo!(),
                Operator::GreaterThan => todo!(),
                Operator::GreaterThanOrEqual => todo!(),
                Operator::And => todo!(),
                Operator::Or => todo!(),
            }
        }
        ExpressionKind::FunctionCall { callee, arguments } => todo!(),
        ExpressionKind::If {
            condition,
            body,
            else_,
        } => {
            visit_expression(condition, symbol_table)?;
            let condition_sym = symbol_table.get(&condition.id).unwrap();
            if condition_sym.type_ != Type::Bool {
                return create_type_mismatch_error(
                    &condition.span,
                    &Type::Bool,
                    &condition_sym.type_,
                );
            }
            visit_expression(body, symbol_table)?;
            let body_type = &symbol_table.get(&body.id).unwrap().type_.clone();
            let mut else_type = &Type::Void;
            if let Some(else_) = else_ {
                visit_expression(else_, symbol_table)?;
                else_type = &symbol_table.get(&else_.id).unwrap().type_;
            }
            if body_type != else_type {
                Err(
                    Simple::custom(expression.span.clone(), "Branch type mismatch").merge(
                        Simple::expected_input_found(
                            expression.span.clone(),
                            vec![Some(format!(
                                "else branch to have type {}, found {}",
                                body_type, else_type
                            ))],
                            None,
                        ),
                    ),
                )
            } else {
                Ok(())
            }
        }
        ExpressionKind::Error => todo!(),
    }
}

#[cfg(test)]
mod tests {
    use chumsky::{Parser, Stream};

    use crate::{
        lexer::lexer,
        parser::{main_parser, ExpressionKind},
        passes::{
            generate_symbols::generate_symbols,
            shared::{NodeId, Type},
            type_inference::infer_types,
        },
    };

    #[test]
    fn should_identify_booleans() {
        let src = "true";
        let len = src.chars().count();
        let (tokens, _) = lexer().parse_recovery(src);
        let (program, _) = crate::parser::main_parser()
            .parse_recovery(Stream::from_iter(len..len + 1, tokens.unwrap().into_iter()));
        let (_, symbol_table) = infer_types(generate_symbols(program.unwrap()).unwrap()).unwrap();
        let symbol = symbol_table.get(&NodeId::from_u32(0)).unwrap();
        assert_eq!(symbol.type_, Type::Bool);
    }

    #[test]
    fn should_identify_integers() {
        let src = "123";
        let len = src.chars().count();
        let (tokens, _) = lexer().parse_recovery(src);
        let (program, _) = main_parser()
            .parse_recovery(Stream::from_iter(len..len + 1, tokens.unwrap().into_iter()));
        let (_, symbol_table) = infer_types(generate_symbols(program.unwrap()).unwrap()).unwrap();
        let symbol = symbol_table.get(&NodeId::from_u32(0)).unwrap();
        assert_eq!(symbol.type_, Type::Int);
    }

    #[test]
    fn should_identify_strings() {
        let src = "'string'";
        let len = src.chars().count();
        let (tokens, _) = lexer().parse_recovery(src);
        let (program, _) = main_parser()
            .parse_recovery(Stream::from_iter(len..len + 1, tokens.unwrap().into_iter()));
        let (_, symbol_table) = infer_types(generate_symbols(program.unwrap()).unwrap()).unwrap();
        let symbol = symbol_table.get(&NodeId::from_u32(0)).unwrap();
        assert_eq!(symbol.type_, Type::String);
    }

    #[test]
    fn should_infer_for_assignments() {
        let src = "a = 1";
        let len = src.chars().count();
        let (tokens, _) = lexer().parse_recovery(src);
        let (program, _) = main_parser()
            .parse_recovery(Stream::from_iter(len..len + 1, tokens.unwrap().into_iter()));
        let (program, symbol_table) =
            infer_types(generate_symbols(program.unwrap()).unwrap()).unwrap();
        let binary_expression = program.expressions.first().unwrap();
        match &binary_expression.kind {
            ExpressionKind::BinaryExpression(left, _, right) => {
                let bin_exp_symbol = symbol_table.get(&binary_expression.id).unwrap();
                let identifier = symbol_table.get(&left.id).unwrap();
                let integer = symbol_table.get(&right.id).unwrap();
                assert_eq!(integer.type_, Type::Int);
                assert_eq!(identifier.type_, Type::Int);
                assert_eq!(bin_exp_symbol.type_, Type::Int);
            }
            _ => unreachable!(),
        }
    }

    #[test]
    fn inference_of_identifiers_should_carry_forward() {
        let src = "a = 1; a";
        let len = src.chars().count();
        let (tokens, _) = lexer().parse_recovery(src);
        let (program, _) = main_parser()
            .parse_recovery(Stream::from_iter(len..len + 1, tokens.unwrap().into_iter()));
        let (program, symbol_table) =
            infer_types(generate_symbols(program.unwrap()).unwrap()).unwrap();
        let identifier = program.expressions.last().unwrap();
        match &identifier.kind {
            ExpressionKind::Identifier(_) => {
                let symbol = symbol_table.get(&identifier.id).unwrap();
                assert_eq!(symbol.type_, Type::Int);
            }
            _ => unreachable!(),
        }
    }

    #[test]
    fn infer_types_of_identifiers_from_declarations() {
        let src = "
            let x = true;
            x
        ";
        let len = src.chars().count();
        let (tokens, _) = lexer().parse_recovery(src);
        let (program, _) = main_parser()
            .parse_recovery(Stream::from_iter(len..len + 1, tokens.unwrap().into_iter()));
        let (program, symbol_table) =
            infer_types(generate_symbols(program.unwrap()).unwrap()).unwrap();
        let identifier = program.expressions.last().unwrap();
        match &identifier.kind {
            ExpressionKind::Identifier(_) => {
                let symbol = symbol_table.get(&identifier.id).unwrap();
                assert_eq!(symbol.type_, Type::Bool);
            }
            _ => unreachable!(),
        }
    }
}
