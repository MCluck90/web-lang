use std::collections::HashMap;

use crate::{
    errors::CompilerError,
    phases::{
        frontend::{
            ir::{BinaryOperator, PreUnaryOperator},
            Span,
        },
        shared::{ObjectType, Type},
    },
    types::{
        built_ins::{build_built_in_types, build_list_type},
        symbol_table::{SymbolTable, TypeSymbol, ValueId, ValueSymbol},
    },
};

use super::ir::{
    Expression, ExpressionKind, Module, Parameter, Statement, StatementKind, TopLevelStatement,
    TopLevelStatementKind,
};

struct TypeContext {
    pub type_to_properties: HashMap<Type, ObjectType>,

    /// A stack of return types found within a function.
    ///
    /// Why a stack? Because we support nested function definitions. Each element on the top-level stack correlates
    /// with a new function scope. Each element inside of the inner stack tells us what type we're attempting to return
    /// and where that return statement is. This allows us to both identify any potential type issues and report where
    /// those type issues occur.
    pub found_return_types: Vec<Vec<(TypeSymbol, Span)>>,
}

/// Infers and checks types.
pub fn check_types(modules: &mut Vec<Module>, symbol_table: &mut SymbolTable) {
    let mut type_context = &mut TypeContext {
        type_to_properties: build_built_in_types(),
        found_return_types: Vec::new(),
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
        match visit_top_level_statement(&statement, symbol_table, type_context) {
            Err(mut errors) => {
                module.errors.append(&mut errors);
            }
            _ => {}
        }
    }
}

fn visit_top_level_statement(
    statement: &TopLevelStatement,
    symbol_table: &mut SymbolTable,
    type_context: &mut TypeContext,
) -> Result<(), Vec<CompilerError>> {
    match &statement.kind {
        TopLevelStatementKind::VariableDeclaration {
            identifier,
            initializer,
            is_mutable,
            type_,
            is_public: _,
        } => {
            let initializer_type = visit_expression(initializer, symbol_table, type_context);
            let (final_type, res) = match (type_, initializer_type) {
                (Some(type_), Ok(initializer_type)) => {
                    if &initializer_type.type_ != &Type::Unknown && &initializer_type.type_ != type_
                    {
                        (
                            type_.clone(),
                            Err(vec![CompilerError::mismatched_types(
                                &statement.span,
                                &TypeSymbol::from(type_),
                                &initializer_type,
                            )]),
                        )
                    } else {
                        (type_.clone(), Ok(()))
                    }
                }
                (None, Ok(initializer_type)) => (initializer_type.type_, Ok(())),
                (None, Err(errors)) => (Type::Unknown, Err(errors)),
                (Some(type_), Err(errors)) => (type_.clone(), Err(errors)),
            };

            symbol_table.set_value(
                ValueId(identifier.name.clone()),
                ValueSymbol::new()
                    .with_type(final_type)
                    .with_mutability(*is_mutable),
            );

            res
        }
        TopLevelStatementKind::FunctionDefinition {
            name,
            parameters,
            return_type,
            body,
            is_public: _,
        } => {
            type_context.found_return_types.push(Vec::new());
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

            for stmt in body {
                visit_statement(stmt, symbol_table, type_context)?;
            }

            let return_type = TypeSymbol::from(return_type);
            let found_returns = type_context.found_return_types.pop().unwrap();
            let mut errors: Vec<CompilerError> = Vec::new();
            for (return_statement_type, span) in found_returns {
                if return_statement_type.type_ != return_type.type_ {
                    errors.push(CompilerError::invalid_return_value(
                        &span,
                        &return_type,
                        &return_statement_type,
                    ));
                }
            }

            if !errors.is_empty() {
                Err(errors)
            } else {
                Ok(())
            }
        }
        TopLevelStatementKind::Expression(expression) => {
            visit_expression(&expression, symbol_table, type_context).map(|_| ())
        }
        TopLevelStatementKind::Loop(body) => {
            for stmt in body {
                visit_statement(stmt, symbol_table, type_context)?;
            }
            Ok(())
        }
        TopLevelStatementKind::ForLoop {
            initializer,
            condition,
            post_loop,
            body,
        } => {
            if let Some(initializer) = initializer {
                visit_statement(initializer, symbol_table, type_context)?;
            }
            if let Some(condition) = condition {
                visit_expression(condition, symbol_table, type_context)?;
            }
            if let Some(post_loop) = post_loop {
                visit_expression(post_loop, symbol_table, type_context)?;
            }
            for stmt in body {
                visit_statement(stmt, symbol_table, type_context)?;
            }
            Ok(())
        }
        TopLevelStatementKind::EnvironmentBlock(_, statements) => {
            for stmt in statements {
                visit_statement(stmt, symbol_table, type_context)?;
            }
            Ok(())
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
            type_,
            identifier,
            initializer,
            is_mutable,
        } => {
            let initializer_type = visit_expression(initializer, symbol_table, type_context);
            let (final_type, res) = match (type_, initializer_type) {
                (Some(type_), Ok(initializer_type)) => {
                    if &initializer_type.type_ != &Type::Unknown && &initializer_type.type_ != type_
                    {
                        (
                            type_.clone(),
                            Err(vec![CompilerError::mismatched_types(
                                &statement.span,
                                &TypeSymbol::from(type_),
                                &initializer_type,
                            )]),
                        )
                    } else {
                        (type_.clone(), Ok(()))
                    }
                }
                (None, Ok(initializer_type)) => (initializer_type.type_, Ok(())),
                (None, Err(errors)) => (Type::Unknown, Err(errors)),
                (Some(type_), Err(errors)) => (type_.clone(), Err(errors)),
            };

            symbol_table.set_value(
                ValueId(identifier.name.clone()),
                ValueSymbol::new()
                    .with_type(final_type)
                    .with_mutability(*is_mutable),
            );

            res
        }
        StatementKind::FunctionDefinition {
            name,
            parameters,
            return_type,
            body,
        } => {
            type_context.found_return_types.push(Vec::new());
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

            for stmt in body {
                visit_statement(stmt, symbol_table, type_context)?;
            }

            let return_type = TypeSymbol::from(return_type);
            let found_returns = type_context.found_return_types.pop().unwrap();
            let mut errors: Vec<CompilerError> = Vec::new();
            for (return_statement_type, span) in found_returns {
                if return_statement_type.type_ != return_type.type_ {
                    errors.push(CompilerError::invalid_return_value(
                        &span,
                        &return_type,
                        &return_statement_type,
                    ));
                }
            }

            if !errors.is_empty() {
                Err(errors)
            } else {
                Ok(())
            }
        }
        StatementKind::Expression(expression) => {
            visit_expression(&expression, symbol_table, type_context).map(|_| ())
        }
        StatementKind::Return(expr) => match &expr {
            Some(expr) => {
                let expr_type = visit_expression(expr, symbol_table, type_context)?;
                type_context
                    .found_return_types
                    .last_mut()
                    .unwrap()
                    .push((expr_type, expr.span.clone()));
                Ok(())
            }
            None => Ok(()),
        },
        StatementKind::Loop(body) => {
            for stmt in body {
                visit_statement(stmt, symbol_table, type_context)?;
            }
            Ok(())
        }
        StatementKind::Break => Ok(()),
        StatementKind::ForLoop {
            initializer,
            condition,
            post_loop,
            body,
        } => {
            if let Some(initializer) = initializer {
                visit_statement(initializer, symbol_table, type_context)?;
            }
            if let Some(condition) = condition {
                visit_expression(condition, symbol_table, type_context)?;
            }
            if let Some(post_loop) = post_loop {
                visit_expression(post_loop, symbol_table, type_context)?;
            }
            for stmt in body {
                visit_statement(stmt, symbol_table, type_context)?;
            }
            Ok(())
        }
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
        ExpressionKind::Parenthesized(expr) => visit_expression(expr, symbol_table, type_context),
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
                    | BinaryOperator::Div
                    | BinaryOperator::Modulus => {
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
                    BinaryOperator::NotEqual | BinaryOperator::Equal => {
                        let mut errors: Vec<CompilerError> = Vec::new();
                        if !left_type_symbol.type_.is_primitive() {
                            errors.push(CompilerError::binary_operator_not_supported_on_type(
                                &left.span,
                                op,
                                &left_type_symbol,
                            ));
                        }
                        if !right_type_symbol.type_.is_primitive() {
                            errors.push(CompilerError::binary_operator_not_supported_on_type(
                                &right.span,
                                op,
                                &right_type_symbol,
                            ));
                        }
                        if left_type_symbol.type_ != right_type_symbol.type_ {
                            errors.push(CompilerError::mismatched_types(
                                &right.span,
                                &left_type_symbol,
                                &right_type_symbol,
                            ));
                        }

                        if errors.is_empty() {
                            Ok(TypeSymbol::from(Type::Bool))
                        } else {
                            Err(errors)
                        }
                    }
                    BinaryOperator::LessThan
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
                    BinaryOperator::And | BinaryOperator::Or => {
                        let mut errors: Vec<CompilerError> = Vec::new();
                        if left_type_symbol.type_ != Type::Bool {
                            errors.push(CompilerError::binary_operator_not_supported_on_type(
                                &left.span,
                                op,
                                &left_type_symbol,
                            ));
                        }
                        if right_type_symbol.type_ != Type::Bool {
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
                    BinaryOperator::Assignment => match &left.kind {
                        ExpressionKind::PropertyAccess(_, _) => todo!(),
                        ExpressionKind::ArrayAccess(_, _) => {
                            // TODO: Don't allow assigning to immutable arrays
                            if right_type_symbol.type_ == left_type_symbol.type_ {
                                Ok(right_type_symbol)
                            } else {
                                Err(vec![CompilerError::mismatched_types(
                                    &right.span,
                                    &left_type_symbol,
                                    &right_type_symbol,
                                )])
                            }
                        }
                        ExpressionKind::Identifier(identifier) => {
                            let left_value_symbol = symbol_table
                                .get_value(&identifier.name.clone().into())
                                .unwrap();
                            if !left_value_symbol.is_mutable {
                                Err(vec![CompilerError::assignment_to_immutable_variable(
                                    &left.span,
                                    &identifier.original_name,
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
                        ExpressionKind::Boolean(_)
                        | ExpressionKind::Integer(_)
                        | ExpressionKind::String(_)
                        | ExpressionKind::Block(_)
                        | ExpressionKind::BinaryExpression(_, _, _)
                        | ExpressionKind::If { .. }
                        | ExpressionKind::FunctionCall { .. }
                        | ExpressionKind::List(_)
                        | ExpressionKind::PreUnaryExpression(_, _)
                        | ExpressionKind::Parenthesized(_) => {
                            Err(vec![CompilerError::invalid_lhs_in_assignment(&left.span)])
                        }
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
                    },
                }
            }
        }
        ExpressionKind::PreUnaryExpression(op, expr) => {
            let expr_type = visit_expression(expr, symbol_table, type_context)?;
            match op {
                PreUnaryOperator::Not => {
                    if expr_type.type_ != Type::Bool {
                        Err(vec![
                            CompilerError::unary_not_operator_not_supported_on_type(
                                &expr.span, &expr_type,
                            ),
                        ])
                    } else {
                        Ok(expr_type)
                    }
                }
                PreUnaryOperator::Increment | PreUnaryOperator::Decrement => match &expr.kind {
                    ExpressionKind::Boolean(_)
                    | ExpressionKind::Integer(_)
                    | ExpressionKind::Block(_)
                    | ExpressionKind::JsBlock(_, _)
                    | ExpressionKind::List(_)
                    | ExpressionKind::BinaryExpression(_, _, _)
                    | ExpressionKind::FunctionCall { .. }
                    | ExpressionKind::If { .. }
                    | ExpressionKind::String(_)
                    | ExpressionKind::Parenthesized(_) => Err(vec![
                        CompilerError::invalid_rhs_expression_in_prefix_operation(&expr.span),
                    ]),

                    ExpressionKind::Identifier(ident) => {
                        let symbol = symbol_table
                            .get_value(&ValueId(ident.name.clone()))
                            .unwrap();
                        match (symbol.is_mutable, &expr_type.type_) {
                            (true, Type::Int) => Ok(expr_type),
                            (false, Type::Int) => {
                                Err(vec![CompilerError::assignment_to_immutable_variable(
                                    &expr.span,
                                    &ident.original_name,
                                )])
                            }
                            (_, _) => Err(vec![
                                CompilerError::invalid_rhs_expression_in_prefix_operation(
                                    &expr.span,
                                ),
                            ]),
                        }
                    }

                    ExpressionKind::PreUnaryExpression(_, _) => todo!(),
                    ExpressionKind::PropertyAccess(_, _) => todo!(),
                    ExpressionKind::ArrayAccess(_, _) => todo!(),
                },
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

                // Access properties on built in types
                Type::Bool | Type::Int | Type::String => {
                    match type_context
                        .type_to_properties
                        .get(&left_type_symbol.type_.to_base_type())
                    {
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

                Type::List(element_type) => {
                    let left_type = build_list_type(element_type);
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
                Type::Unknown => {
                    // If the type is unknown at this point, we've already reported the issue elsewhere.
                    for arg in arguments {
                        visit_expression(arg, symbol_table, type_context)?;
                    }
                    return Ok(TypeSymbol::from(Type::Unknown));
                }
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
        ExpressionKind::List(expressions) => {
            let mut expr_types: Vec<TypeSymbol> = Vec::new();
            for expr in expressions {
                expr_types.push(visit_expression(expr, symbol_table, type_context)?);
            }

            let first_type = expr_types.first();
            if let Some(first_type) = first_type {
                for type_ in &expr_types {
                    if type_.type_ != first_type.type_ {
                        return Err(vec![CompilerError::mixed_types_in_list(
                            &expression.span,
                            &expr_types,
                        )]);
                    }
                }
                Ok(Type::List(Box::new(first_type.type_.clone())).into())
            } else {
                Ok(Type::List(Box::new(Type::Unknown)).into())
            }
        }
        ExpressionKind::ArrayAccess(left, index) => {
            let left_type_symbol = visit_expression(left, symbol_table, type_context);
            let index_type_symbol = visit_expression(index, symbol_table, type_context);
            match (left_type_symbol, index_type_symbol) {
                (Ok(left_type_symbol), Ok(index_type_symbol)) => match left_type_symbol.type_ {
                    Type::List(element_type) => {
                        if index_type_symbol.type_.to_base_type() != Type::Int {
                            todo!("Can't index arrays with non-int values")
                        }
                        Ok(TypeSymbol::from(*element_type))
                    }
                    _ => todo!("Can't index non-arrays"),
                },
                (Ok(_), Err(errors)) | (Err(errors), Ok(_)) => Err(errors),
                (Err(mut left_errors), Err(mut index_errors)) => {
                    left_errors.append(&mut index_errors);
                    Err(left_errors)
                }
            }
        }
    }
}
