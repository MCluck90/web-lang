use std::collections::HashMap;

use crate::{
    errors::CompilerError,
    phases::{
        frontend::{BinaryOperator, Span},
        shared::{ObjectType, Type},
    },
    types::{
        primitives::build_primitive_types,
        symbol_table::{SymbolTable, TypeSymbol, ValueId, ValueSymbol},
    },
};

use super::ast::{Expression, ExpressionKind, Module, Parameter, Statement, StatementKind};

struct TypeContext {
    pub primitive_types: HashMap<Type, ObjectType>,
}

/// Infers and checks types.
pub fn check_types(modules: &mut Vec<Module>, symbol_table: &mut SymbolTable) {
    let mut type_context = &mut TypeContext {
        primitive_types: build_primitive_types(),
    };
    for module in modules {
        visit_module(module, symbol_table, &mut type_context);
    }
}

fn visit_module(
    module: &mut Module,
    symbol_table: &mut SymbolTable,
    type_context: &mut TypeContext,
) {
    for statement in module.ast.statements.iter() {
        match visit_statement(&statement, symbol_table, type_context) {
            Err(mut errors) => {
                module.errors.append(&mut errors);
            }
            _ => {}
        }
    }
}

fn visit_statement(
    statement: &Statement,
    symbol_table: &mut SymbolTable,
    type_context: &mut TypeContext,
) -> Result<(), Vec<CompilerError>> {
    match &statement.kind {
        StatementKind::VariableDeclaration {
            identifier,
            initializer,
            is_mutable,
        } => {
            let initializer_type = visit_expression(initializer, symbol_table, type_context)?;
            symbol_table.set_value(
                ValueId(identifier.name.clone()),
                ValueSymbol::new()
                    .with_type(initializer_type.type_)
                    .with_mutability(*is_mutable),
            );
            Ok(())
        }
        StatementKind::FunctionDefinition {
            name,
            parameters,
            return_type,
            body,
        } => {
            let parameter_types = parameters
                .iter()
                .map(|p| visit_parameter(p, symbol_table))
                .collect::<Vec<_>>();
            let function_type = Type::Function {
                parameters: parameter_types,
                return_type: Box::new(return_type.clone()),
            };
            symbol_table.set_value(
                name.name.clone().into(),
                ValueSymbol::new().with_type(function_type.clone()),
            );

            let body_type = visit_expression(body, symbol_table, type_context)?;

            if *return_type != body_type.type_ {
                let span = match &body.kind {
                    ExpressionKind::Block(block) => match &block.return_expression {
                        Some(expr) => expr.span.clone(),
                        None => block.span.clone(),
                    },
                    _ => body.span.clone(),
                };
                Err(vec![CompilerError::invalid_return_value(
                    &span,
                    &return_type.clone().into(),
                    &body_type,
                )])
            } else {
                Ok(())
            }
        }
        StatementKind::Expression(expression) => {
            visit_expression(&expression, symbol_table, type_context).map(|_| ())
        }
        StatementKind::Return(expr) => match &expr {
            Some(expr) => visit_expression(expr, symbol_table, type_context).map(|_| ()),
            None => Ok(()),
        },
    }
}

fn visit_parameter(parameter: &Parameter, symbol_table: &mut SymbolTable) -> Type {
    symbol_table.set_value(
        parameter.identifier.name.clone().into(),
        ValueSymbol::new().with_type(parameter.type_.clone()),
    );
    parameter.type_.clone()
}

fn visit_expression(
    expression: &Expression,
    symbol_table: &mut SymbolTable,
    type_context: &mut TypeContext,
) -> Result<TypeSymbol, Vec<CompilerError>> {
    match &expression.kind {
        ExpressionKind::Boolean(_) => Ok(TypeSymbol::bool()),
        ExpressionKind::Identifier(identifier) => {
            match symbol_table.get_value(&identifier.name.clone().into()) {
                Some(value) => Ok(match &value.type_id {
                    Some(type_id) => symbol_table.get_type(&type_id).unwrap().clone(),
                    None => value.type_.clone().into(),
                }),
                None => Ok(TypeSymbol::from(Type::Unknown)),
            }
        }
        ExpressionKind::Integer(_) => Ok(TypeSymbol::int()),
        ExpressionKind::String(_) => Ok(TypeSymbol::string()),
        ExpressionKind::Block(block) => {
            for statement in &block.statements {
                visit_statement(statement, symbol_table, type_context)
                    .map(|_| TypeSymbol::from(Type::Void))?;
            }

            let mut output_type = TypeSymbol::from(Type::Void);
            if let Some(expr) = &block.return_expression {
                output_type = visit_expression(&expr, symbol_table, type_context)?;
            }

            Ok(output_type)
        }
        ExpressionKind::JsBlock(type_, _) => Ok(type_.clone().into()),
        ExpressionKind::BinaryExpression(left, op, right) => {
            let left_type = visit_expression(left, symbol_table, type_context);
            let right_type = visit_expression(right, symbol_table, type_context);
            if left_type.is_err() || right_type.is_err() {
                match (left_type, right_type) {
                    (Err(mut a), Err(mut b)) => {
                        a.append(&mut b);
                        Err(a)
                    }
                    (Err(errs), Ok(_)) | (Ok(_), Err(errs)) => Err(errs),
                    _ => unreachable!(),
                }
            } else {
                let left_type_symbol = left_type.unwrap();
                let right_type_symbol = right_type.unwrap();
                match op {
                    BinaryOperator::Add
                    | BinaryOperator::Sub
                    | BinaryOperator::Mul
                    | BinaryOperator::Div => {
                        let mut errors: Vec<CompilerError> = Vec::new();
                        if left_type_symbol.type_ != Type::Int {
                            errors.push(CompilerError::binary_operator_not_supported_on_type(
                                &left.span,
                                op,
                                &left_type_symbol,
                            ));
                        }
                        if right_type_symbol.type_ != Type::Int {
                            errors.push(CompilerError::binary_operator_not_supported_on_type(
                                &right.span,
                                op,
                                &right_type_symbol,
                            ));
                        }

                        if errors.is_empty() {
                            Ok(left_type_symbol)
                        } else {
                            Err(errors)
                        }
                    }
                    BinaryOperator::NotEqual
                    | BinaryOperator::Equal
                    | BinaryOperator::LessThan
                    | BinaryOperator::LessThanOrEqual
                    | BinaryOperator::GreaterThan
                    | BinaryOperator::GreaterThanOrEqual => {
                        let mut errors: Vec<CompilerError> = Vec::new();
                        if left_type_symbol.type_ != Type::Int {
                            errors.push(CompilerError::binary_operator_not_supported_on_type(
                                &left.span,
                                op,
                                &left_type_symbol,
                            ));
                        }
                        if right_type_symbol.type_ != Type::Int {
                            errors.push(CompilerError::binary_operator_not_supported_on_type(
                                &right.span,
                                op,
                                &right_type_symbol,
                            ));
                        }

                        if errors.is_empty() {
                            Ok(TypeSymbol::from(Type::Bool))
                        } else {
                            Err(errors)
                        }
                    }
                    BinaryOperator::Dot => todo!(),
                    BinaryOperator::And => todo!(),
                    BinaryOperator::Or => todo!(),
                    BinaryOperator::Assignment => match &left.kind {
                        ExpressionKind::PropertyAccess(_, _) => todo!(),
                        ExpressionKind::Identifier(identifier) => {
                            let left_value_symbol = symbol_table
                                .get_value(&identifier.name.clone().into())
                                .unwrap();
                            if !left_value_symbol.is_mutable {
                                Err(vec![CompilerError::assignment_to_immutable_variable(
                                    &left.span,
                                    &identifier.name,
                                )])
                            } else if left_type_symbol.type_ == Type::Unknown {
                                symbol_table.set_value(
                                    ValueId(identifier.name.clone()),
                                    ValueSymbol::new().with_type(right_type_symbol.type_.clone()),
                                );
                                Ok(right_type_symbol)
                            } else if left_type_symbol.type_ != right_type_symbol.type_ {
                                Err(vec![CompilerError::mismatched_types(
                                    &right.span,
                                    &left_type_symbol,
                                    &right_type_symbol,
                                )])
                            } else {
                                Ok(right_type_symbol)
                            }
                        }
                        ExpressionKind::Boolean(_) => todo!(),
                        ExpressionKind::Integer(_) => todo!(),
                        ExpressionKind::String(_) => todo!(),
                        ExpressionKind::Block(_) => todo!(),
                        ExpressionKind::JsBlock(type_, _) => {
                            if &left_type_symbol.type_ != type_ {
                                Err(vec![CompilerError::mismatched_types(
                                    &right.span,
                                    &left_type_symbol,
                                    &type_.clone().into(),
                                )])
                            } else {
                                Ok(left_type_symbol)
                            }
                        }
                        ExpressionKind::BinaryExpression(_, _, _) => todo!(),
                        ExpressionKind::FunctionCall { .. } => todo!(),
                        ExpressionKind::If { .. } => todo!(),
                    },
                }
            }
        }
        ExpressionKind::PropertyAccess(left, right) => {
            let left_type_symbol = visit_expression(left, symbol_table, type_context)?;

            match &left_type_symbol.type_ {
                Type::Object(ObjectType { key_to_type }) => {
                    if let Some(type_) = key_to_type.get(&right.name) {
                        Ok(TypeSymbol::from(*type_.clone()))
                    } else {
                        Err(vec![CompilerError::no_field_on_type(
                            &expression.span,
                            &right.name,
                            &left_type_symbol,
                        )])
                    }
                }

                // Access properties on primitive types
                Type::Bool | Type::Int | Type::String => {
                    match type_context.primitive_types.get(&left_type_symbol.type_) {
                        None => unreachable!(),
                        Some(left_type) => {
                            if let Some(type_) = left_type.key_to_type.get(&right.name) {
                                Ok(TypeSymbol::from(*type_.clone()))
                            } else {
                                Err(vec![CompilerError::no_field_on_type(
                                    &expression.span,
                                    &right.name,
                                    &left_type_symbol,
                                )])
                            }
                        }
                    }
                }
                _ => Err(vec![CompilerError::no_field_on_type(
                    &expression.span,
                    &right.name,
                    &left_type_symbol,
                )]),
            }
        }
        ExpressionKind::FunctionCall { callee, arguments } => {
            let callee_type = visit_expression(callee, symbol_table, type_context)?;
            let (parameter_types, return_type) = match &callee_type.type_ {
                Type::Function {
                    parameters,
                    return_type,
                } => (
                    parameters.iter().map(TypeSymbol::from).collect::<Vec<_>>(),
                    return_type.clone(),
                ),
                _ => {
                    return Err(vec![CompilerError::type_cannot_be_called_as_a_function(
                        &expression.span,
                        &callee_type,
                    )]);
                }
            };

            let mut argument_types = Vec::<(TypeSymbol, Span)>::new();
            for arg in arguments {
                let arg_type = visit_expression(arg, symbol_table, type_context)?;
                argument_types.push((arg_type, arg.span.clone()));
            }

            if argument_types.len() != parameter_types.len() {
                return Err(vec![CompilerError::invalid_arguments(
                    &expression.span,
                    argument_types.iter().map(|(t, _)| t.clone()).collect(),
                    parameter_types,
                )]);
            }

            let mut argument_errors = Vec::<CompilerError>::new();
            for i in 0..argument_types.len() {
                let (argument_type, arg_span) = argument_types.get(i).unwrap();
                let parameter_type = parameter_types.get(i).unwrap();
                if argument_type.type_ != parameter_type.type_ {
                    argument_errors.push(CompilerError::mismatched_types(
                        arg_span,
                        parameter_type,
                        argument_type,
                    ));
                }
            }

            if !argument_errors.is_empty() {
                Err(argument_errors)
            } else {
                Ok(TypeSymbol::from(*return_type))
            }
        }
        ExpressionKind::If {
            condition,
            body,
            else_,
        } => {
            let mut errors: Vec<CompilerError> = Vec::new();
            let condition_type = visit_expression(condition, symbol_table, type_context)?;
            if condition_type.type_ != Type::Bool {
                errors.push(CompilerError::mismatched_types(
                    &condition.span,
                    &Type::Bool.into(),
                    &condition_type.into(),
                ));
            }

            let body_type = visit_expression(body, symbol_table, type_context)?;
            let mut else_type = TypeSymbol::from(Type::Void);
            if let Some(else_) = else_ {
                else_type = visit_expression(else_, symbol_table, type_context)?;
            }
            if body_type.type_ != else_type.type_ {
                match else_ {
                    Some(else_) => {
                        errors.push(CompilerError::if_branch_incompatiable_types(
                            &else_.span,
                            &body_type,
                            &else_type,
                        ));
                    }
                    None => {
                        errors.push(CompilerError::if_branch_incompatiable_types(
                            &body.span, &body_type, &else_type,
                        ));
                    }
                }
            }

            if !errors.is_empty() {
                Err(errors)
            } else {
                Ok(body_type)
            }
        }
    }
}

// struct TypeContext {
//     pub return_expressions: HashMap<NodeId, Vec<NodeId>>,
//     pub primitive_types: HashMap<Type, ObjectType>,
// }

// fn visit_module(
//     module: ModuleAST,
//     mut symbol_table: SymbolTable,
//     mut type_context: TypeContext,
// ) -> Result<(ModuleAST, SymbolTable), Vec<Simple<String>>> {
//     let statement_results = module
//         .statements
//         .iter()
//         .map(|statement| visit_statement(statement, &mut symbol_table, &mut type_context))
//         .collect::<Vec<_>>();
//     let statement_errors = statement_results
//         .iter()
//         .filter_map(|res| res.clone().err())
//         .collect::<Vec<_>>();
//     if !statement_errors.is_empty() {
//         Err(statement_errors)
//     } else {
//         Ok((module, symbol_table))
//     }
// }

// fn visit_statement(
//     statement: &Statement,
//     symbol_table: &mut SymbolTable,
//     type_context: &mut TypeContext,
// ) -> Result<(), Simple<String>> {
//     match &statement.kind {
//         StatementKind::Expression(expression) => {
//             visit_expression(expression, symbol_table, type_context).map(|_| ())
//         }
//         StatementKind::FunctionDefinition {
//             name,
//             parameters,
//             return_type,
//             body,
//         } => {
//             todo!()
//             let parameter_types = parameters
//                 .iter()
//                 .map(|p| visit_parameter(p, symbol_table))
//                 .collect::<Vec<_>>();
//             let function_type = Type::Function {
//                 parameters: parameter_types,
//                 return_type: Box::new(return_type.clone()),
//             };
//             symbol_table.set_type(&name.id, function_type.clone());
//             type_context
//                 .return_expressions
//                 .insert(name.id.clone(), Vec::new());

//             let body_type = visit_expression(body, symbol_table, type_context)?;

//             if *return_type != body_type {
//                 return Err(Simple::custom(
//                     body.span.clone(),
//                     format!(
//                         "Function expected to return type {} but instead returned {}",
//                         return_type, body_type
//                     ),
//                 ));
//             }
//             Ok(())
//         }
//         StatementKind::JsBlock(expressions) => {
//             for expr in expressions {
//                 visit_expression(expr, symbol_table, type_context)?;
//             }
//             Ok(())
//         }
//         StatementKind::Return(maybe_expression) => {
//             // TODO: Add current function scope so we can check if this is valid
//             if let Some(expr) = maybe_expression {
//                 visit_expression(expr, symbol_table, type_context)?;
//             }
//             Ok(())
//         }
//     }
// }

// fn to_missing_node_error(expression: &Expression) -> Result<Type, Simple<String>> {
//     Err(Simple::custom(
//         expression.span.clone(),
//         "Node is not available in the symbol table",
//     ))
// }

// fn visit_expression(
//     expression: &Expression,
//     symbol_table: &mut SymbolTable,
//     type_context: &mut TypeContext,
// ) -> Result<Type, Simple<String>> {
//     let type_ = match &expression.kind {
//         ExpressionKind::Boolean(_) => {
//             if !symbol_table.set_type(&expression.id, Type::Bool) {
//                 return to_missing_node_error(expression);
//             }

//             Type::Bool
//         }
//         ExpressionKind::Integer(_) => {
//             if !symbol_table.set_type(&expression.id, Type::Int) {
//                 return to_missing_node_error(expression);
//             }

//             Type::Int
//         }
//         ExpressionKind::String(_) => {
//             if !symbol_table.set_type(&expression.id, Type::String) {
//                 return to_missing_node_error(expression);
//             }

//             Type::String
//         }

//         ExpressionKind::Identifier(_) => symbol_table
//             .get(&expression.id)
//             .map(|i| i.type_.clone())
//             .unwrap_or(Type::Unknown),

//         ExpressionKind::Block(block) => {
//             for statement in &block.statements {
//                 visit_statement(statement, symbol_table, type_context)?;
//             }

//             let mut output_type = Type::Void;
//             if let Some(expr) = &block.return_expression {
//                 output_type = visit_expression(&expr, symbol_table, type_context)?;
//             }

//             output_type
//         }

//         ExpressionKind::VariableDeclaration {
//             identifier,
//             initializer,
//             ..
//         } => {
//             let initializer_type = visit_expression(initializer, symbol_table, type_context)?;
//             symbol_table.set_type(&identifier.id, initializer_type.clone());
//             initializer_type
//         }

//         ExpressionKind::BinaryExpression(left, op, right) => {
//             visit_expression(left, symbol_table, type_context)?;
//             visit_expression(right, symbol_table, type_context)?;

//             let right_type = symbol_table.get_mut(&right.id).unwrap().type_.clone();
//             let mut left_sym = symbol_table.get_mut(&left.id).unwrap();
//             // TODO: Generally handle when types are unknown
//             let result_type = match &op {
//                 BinaryOperator::Assignment => {
//                     if left_sym.type_ == Type::Unknown {
//                         left_sym.type_ = right_type.clone();
//                     } else if left_sym.type_ != right_type {
//                         return create_type_mismatch_error(
//                             &right.span,
//                             &left_sym.type_,
//                             &right_type,
//                         );
//                     }

//                     right_type
//                 }
//                 BinaryOperator::Add
//                 | BinaryOperator::Sub
//                 | BinaryOperator::Mul
//                 | BinaryOperator::Div => {
//                     // TODO: Infer when either are `Unknown`
//                     // TODO: Add support for string concatenation
//                     if left_sym.type_ != Type::Int {
//                         return create_type_mismatch_error(&left.span, &Type::Int, &left_sym.type_);
//                     } else if right_type != Type::Int {
//                         return create_type_mismatch_error(&right.span, &Type::Int, &right_type);
//                     }

//                     left_sym.type_.clone()
//                 }
//                 BinaryOperator::Dot => todo!(),
//                 BinaryOperator::NotEqual | BinaryOperator::Equal => {
//                     if left_sym.type_ != right_type {
//                         return Err(
//                             Simple::custom(expression.span.clone(), "Invalid comparison").merge(
//                                 Simple::expected_input_found(
//                                     expression.span.clone(),
//                                     vec![Some(format!(
//                                         "both sides to be {} or {}",
//                                         left_sym.type_, right_type
//                                     ))],
//                                     None,
//                                 ),
//                             ),
//                         );
//                     }

//                     Type::Bool
//                 }
//                 BinaryOperator::LessThan
//                 | BinaryOperator::LessThanOrEqual
//                 | BinaryOperator::GreaterThan
//                 | BinaryOperator::GreaterThanOrEqual => {
//                     // TODO: Infer when either are `Unknown`
//                     if left_sym.type_ != Type::Int {
//                         return create_type_mismatch_error(&left.span, &Type::Int, &left_sym.type_);
//                     } else if right_type != Type::Int {
//                         return create_type_mismatch_error(&right.span, &Type::Int, &right_type);
//                     }

//                     Type::Bool
//                 }

//                 BinaryOperator::And | BinaryOperator::Or => {
//                     // TODO: Infer when either are `Unknown`
//                     if left_sym.type_ != Type::Bool {
//                         return create_type_mismatch_error(
//                             &left.span,
//                             &Type::Bool,
//                             &left_sym.type_,
//                         );
//                     } else if right_type != Type::Bool {
//                         return create_type_mismatch_error(&right.span, &Type::Bool, &right_type);
//                     }

//                     Type::Bool
//                 }
//             };
//             result_type
//         }

//         ExpressionKind::PropertyAccess(left, right) => {
//             visit_expression(left, symbol_table, type_context)?;
//             let left_type = &symbol_table.get(&left.id).unwrap().type_;

//             match left_type {
//                 Type::Object(ObjectType { key_to_type }) => {
//                     if let Some(type_) = key_to_type.get(&right.name) {
//                         *type_.clone()
//                     } else {
//                         return Err(Simple::custom(
//                             expression.span.clone(),
//                             format!("TODO: Unrecognized property `{}`", right.name),
//                         ));
//                     }
//                 }

//                 // Access properties on primitive types
//                 Type::Bool | Type::Int | Type::String => {
//                     match type_context.primitive_types.get(&left_type) {
//                         None => {
//                             return Err(Simple::custom(
//                                 left.span.clone(),
//                                 format!("TODO: No primitive type entry for {}", left_type),
//                             ))
//                         }
//                         Some(left_type) => {
//                             if let Some(type_) = left_type.key_to_type.get(&right.name) {
//                                 *type_.clone()
//                             } else {
//                                 return Err(Simple::custom(
//                                     expression.span.clone(),
//                                     format!("TODO: Unrecognized property `{}`", right.name),
//                                 ));
//                             }
//                         }
//                     }
//                 }
//                 _ => {
//                     return Err(Simple::custom(
//                         expression.span.clone(),
//                         "TODO: Improve message. Cannot access property of non-object type",
//                     ))
//                 }
//             }
//         }

//         ExpressionKind::FunctionCall { callee, arguments } => {
//             visit_expression(callee, symbol_table, type_context)?;
//             let callee_type = symbol_table.get(&callee.id).unwrap().type_.clone();
//             let (parameter_types, return_type) = match callee_type {
//                 Type::Function {
//                     parameters,
//                     return_type,
//                 } => (parameters, return_type.clone()),
//                 _ => {
//                     return Err(Simple::custom(
//                         callee.span.clone(),
//                         format!("Type {} is not callable", callee_type),
//                     ));
//                 }
//             };

//             let mut argument_types = Vec::<(Type, Span)>::new();
//             for arg in arguments {
//                 let arg_type = visit_expression(arg, symbol_table, type_context)?;
//                 argument_types.push((arg_type, arg.span.clone()));
//             }

//             if argument_types.len() != parameter_types.len() {
//                 return Err(Simple::custom(
//                     expression.span.clone(),
//                     format!(
//                         "Expected arguments to be: ({}), received: ({})",
//                         parameter_types
//                             .iter()
//                             .map(|param| format!("{}", param))
//                             .collect::<Vec<_>>()
//                             .join(", "),
//                         argument_types
//                             .iter()
//                             .map(|(arg, _)| format!("{}", arg))
//                             .collect::<Vec<_>>()
//                             .join(", ")
//                     ),
//                 ));
//             }

//             for i in 0..argument_types.len() {
//                 let (argument_type, arg_span) = argument_types.get(i).unwrap();
//                 let parameter_type = parameter_types.get(i).unwrap();
//                 if argument_type != parameter_type {
//                     return Err(Simple::custom(
//                         arg_span.clone(),
//                         format!("Expected {}, received {}", parameter_type, argument_type),
//                     ));
//                 }
//             }

//             *return_type
//         }

//         ExpressionKind::If {
//             condition,
//             body,
//             else_,
//         } => {
//             visit_expression(condition, symbol_table, type_context)?;
//             let condition_sym = symbol_table.get(&condition.id).unwrap();
//             if condition_sym.type_ != Type::Bool {
//                 return create_type_mismatch_error(
//                     &condition.span,
//                     &Type::Bool,
//                     &condition_sym.type_,
//                 );
//             }
//             visit_expression(body, symbol_table, type_context)?;
//             let body_type = &symbol_table.get(&body.id).unwrap().type_.clone();
//             let mut else_type = &Type::Void;
//             if let Some(else_) = else_ {
//                 visit_expression(else_, symbol_table, type_context)?;
//                 else_type = &symbol_table.get(&else_.id).unwrap().type_;
//             }
//             if body_type != else_type {
//                 return Err(
//                     Simple::custom(expression.span.clone(), "Branch type mismatch").merge(
//                         Simple::expected_input_found(
//                             expression.span.clone(),
//                             vec![Some(format!(
//                                 "else branch to have type {}, found {}",
//                                 body_type, else_type
//                             ))],
//                             None,
//                         ),
//                     ),
//                 );
//             }

//             body_type.clone()
//         }
//         ExpressionKind::Error => unreachable!(),
//     };

//     symbol_table.set_type(&expression.id, type_.clone());
//     Ok(type_)
// }

// fn visit_parameter(parameter: &Parameter, symbol_table: &mut SymbolTable) -> Type {
//     symbol_table.set_type(&parameter.id, parameter.type_.clone());
//     parameter.type_.clone()
// }

// #[cfg(test)]
// mod tests {
//     use chumsky::{Parser, Stream};

//     use crate::{
//         frontend::asts::source::{ExpressionKind, StatementKind},
//         frontend::module_parser,
//         phases::{
//             frontend::lexer,
//             middle_end::type_inference::infer_types,
//             shared::{NodeId, Type},
//         },
//     };

//     #[test]
//     fn should_identify_booleans() {
//         let src = "true";
//         let len = src.chars().count();
//         let (tokens, _) = lexer().parse_recovery(src);
//         let (module, _) = module_parser("test".into())
//             .parse_recovery(Stream::from_iter(len..len + 1, tokens.unwrap().into_iter()));
//         let (_, symbol_table) = infer_types(generate_symbols(module.unwrap()).unwrap()).unwrap();
//         let symbol = symbol_table.get(&NodeId::from_u32(0)).unwrap();
//         assert_eq!(symbol.type_, Type::Bool);
//     }

//     #[test]
//     fn should_identify_integers() {
//         let src = "123";
//         let len = src.chars().count();
//         let (tokens, _) = lexer().parse_recovery(src);
//         let (module, _) = module_parser("test".into())
//             .parse_recovery(Stream::from_iter(len..len + 1, tokens.unwrap().into_iter()));
//         let (_, symbol_table) = infer_types(generate_symbols(module.unwrap()).unwrap()).unwrap();
//         let symbol = symbol_table.get(&NodeId::from_u32(0)).unwrap();
//         assert_eq!(symbol.type_, Type::Int);
//     }

//     #[test]
//     fn should_identify_strings() {
//         let src = "'string'";
//         let len = src.chars().count();
//         let (tokens, _) = lexer().parse_recovery(src);
//         let (module, _) = module_parser("test".into())
//             .parse_recovery(Stream::from_iter(len..len + 1, tokens.unwrap().into_iter()));
//         let (_, symbol_table) = infer_types(generate_symbols(module.unwrap()).unwrap()).unwrap();
//         let symbol = symbol_table.get(&NodeId::from_u32(0)).unwrap();
//         assert_eq!(symbol.type_, Type::String);
//     }

//     #[test]
//     fn should_infer_for_assignments() {
//         let src = "a = 1";
//         let len = src.chars().count();
//         let (tokens, _) = lexer().parse_recovery(src);
//         let (module, _) = module_parser("test".into())
//             .parse_recovery(Stream::from_iter(len..len + 1, tokens.unwrap().into_iter()));
//         let (module, symbol_table) =
//             infer_types(generate_symbols(module.unwrap()).unwrap()).unwrap();
//         let statement = module.statements.first().unwrap();
//         match &statement.kind {
//             StatementKind::Expression(expr) => match &expr.kind {
//                 ExpressionKind::BinaryExpression(left, _, right) => {
//                     let bin_exp_symbol = symbol_table.get(&statement.id).unwrap();
//                     let identifier = symbol_table.get(&left.id).unwrap();
//                     let integer = symbol_table.get(&right.id).unwrap();
//                     assert_eq!(integer.type_, Type::Int);
//                     assert_eq!(identifier.type_, Type::Int);
//                     assert_eq!(bin_exp_symbol.type_, Type::Int);
//                 }
//                 _ => unreachable!(),
//             },
//             _ => unreachable!(),
//         }
//     }

//     #[test]
//     fn inference_of_identifiers_should_carry_forward() {
//         let src = "a = 1; a";
//         let len = src.chars().count();
//         let (tokens, _) = lexer().parse_recovery(src);
//         let (module, _) = module_parser("test".into())
//             .parse_recovery(Stream::from_iter(len..len + 1, tokens.unwrap().into_iter()));
//         let (module, symbol_table) =
//             infer_types(generate_symbols(module.unwrap()).unwrap()).unwrap();
//         let statement = module.statements.last().unwrap();
//         match &statement.kind {
//             StatementKind::Expression(expr) => match &expr.kind {
//                 ExpressionKind::Identifier(_) => {
//                     let symbol = symbol_table.get(&expr.id).unwrap();
//                     assert_eq!(symbol.type_, Type::Int);
//                 }
//                 _ => unreachable!(),
//             },
//             _ => unreachable!(),
//         }
//     }

//     #[test]
//     fn infer_types_of_identifiers_from_declarations() {
//         let src = "
//             let x = true;
//             x
//         ";
//         let len = src.chars().count();
//         let (tokens, _) = lexer().parse_recovery(src);
//         let (module, _) = module_parser("test".into())
//             .parse_recovery(Stream::from_iter(len..len + 1, tokens.unwrap().into_iter()));
//         let (module, symbol_table) =
//             infer_types(generate_symbols(module.unwrap()).unwrap()).unwrap();
//         let statement = module.statements.last().unwrap();
//         match &statement.kind {
//             StatementKind::Expression(expr) => match &expr.kind {
//                 ExpressionKind::Identifier(_) => {
//                     let symbol = symbol_table.get(&expr.id).unwrap();
//                     assert_eq!(symbol.type_, Type::Bool);
//                 }
//                 _ => unreachable!(),
//             },
//             _ => unreachable!(),
//         }
//     }
// }
