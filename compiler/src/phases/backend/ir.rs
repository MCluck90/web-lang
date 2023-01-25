use crate::phases::{frontend::BinaryOperator, middle_end};

pub fn from_middle_end(program: middle_end::Program) -> Vec<Statement> {
    let mut statements: Vec<Statement> = Vec::new();
    for module in program.modules {
        visit_module(module, &mut statements);
    }
    statements
}

fn visit_module(module: middle_end::ir::Module, statements: &mut Vec<Statement>) {
    for statement in module.ast.statements {
        statements.push(visit_top_level_statement(statement));
    }
}

fn visit_top_level_statement(statement: middle_end::ir::TopLevelStatement) -> Statement {
    match statement.kind {
        middle_end::ir::TopLevelStatementKind::VariableDeclaration {
            is_public: _,
            is_mutable,
            identifier,
            initializer,
        } => Statement::VariableDeclaration {
            is_mutable,
            identifier: identifier.name,
            initializer: Some(Box::new(visit_expression(*initializer))),
        },
        middle_end::ir::TopLevelStatementKind::FunctionDefinition {
            is_public: _,
            name,
            parameters,
            return_type: _,
            body,
        } => Statement::FunctionDefinition {
            name: name.name,
            parameters: parameters.into_iter().map(|p| p.identifier.name).collect(),
            body: body.into_iter().map(visit_statement).collect(),
        },
        middle_end::ir::TopLevelStatementKind::Expression(expr) => {
            Statement::Expression(visit_expression(expr))
        }
    }
}

fn visit_statement(statement: middle_end::ir::Statement) -> Statement {
    match statement.kind {
        middle_end::ir::StatementKind::VariableDeclaration {
            is_mutable,
            identifier,
            initializer,
        } => Statement::VariableDeclaration {
            is_mutable,
            identifier: identifier.name,
            initializer: Some(Box::new(visit_expression(*initializer))),
        },
        middle_end::ir::StatementKind::FunctionDefinition {
            name,
            parameters,
            return_type: _,
            body,
        } => Statement::FunctionDefinition {
            name: name.name,
            parameters: parameters.into_iter().map(|p| p.identifier.name).collect(),
            body: body.into_iter().map(visit_statement).collect(),
        },
        middle_end::ir::StatementKind::Expression(_) => todo!(),
        middle_end::ir::StatementKind::Return(expression) => {
            Statement::Return(expression.map(visit_expression))
        }
    }
}

fn visit_expression(expression: middle_end::ir::Expression) -> Expression {
    match expression.kind {
        middle_end::ir::ExpressionKind::Boolean(value) => Expression::Boolean(value),
        middle_end::ir::ExpressionKind::Identifier(identifier) => {
            Expression::Identifier(identifier.name)
        }
        middle_end::ir::ExpressionKind::Integer(value) => Expression::Integer(value),
        middle_end::ir::ExpressionKind::String(value) => Expression::String(value),
        middle_end::ir::ExpressionKind::Block(_) => todo!(),
        middle_end::ir::ExpressionKind::JsBlock(_, expressions) => {
            Expression::JsBlock(expressions.into_iter().map(visit_expression).collect())
        }
        middle_end::ir::ExpressionKind::BinaryExpression(left, op, right) => {
            Expression::BinaryExpression(
                Box::new(visit_expression(*left)),
                op,
                Box::new(visit_expression(*right)),
            )
        }
        middle_end::ir::ExpressionKind::PropertyAccess(left, right) => {
            Expression::PropertyAccess(Box::new(visit_expression(*left)), right.name)
        }
        middle_end::ir::ExpressionKind::FunctionCall { callee, arguments } => {
            Expression::FunctionCall {
                callee: Box::new(visit_expression(*callee)),
                arguments: arguments.into_iter().map(visit_expression).collect(),
            }
        }
        middle_end::ir::ExpressionKind::If { .. } => todo!(),
    }
}

pub enum Statement {
    VariableDeclaration {
        is_mutable: bool,
        identifier: String,
        initializer: Option<Box<Expression>>,
    },
    FunctionDefinition {
        name: String,
        parameters: Vec<String>,
        body: Vec<Statement>,
    },
    Return(Option<Expression>),
    Expression(Expression),
}

pub enum Expression {
    Boolean(bool),
    Identifier(String),
    Integer(i64),
    String(String),
    JsBlock(Vec<Expression>),
    BinaryExpression(Box<Expression>, BinaryOperator, Box<Expression>),
    PropertyAccess(Box<Expression>, String),
    FunctionCall {
        callee: Box<Expression>,
        arguments: Vec<Expression>,
    },
}
