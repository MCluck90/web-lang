use crate::phases::frontend::ir::PreUnaryOperator;

use super::ir::{EnvironmentType, Expression, ExpressionKind, Statement};

pub struct CodeGenOutput {
    pub html: Option<String>,
    pub backend_js: Option<String>,
    pub frontend_js: Option<String>,
}

struct OutputBuilder {
    html: String,
    backend_js: String,
    frontend_js: String,
}

pub fn generate_code(statements: Vec<Statement>) -> CodeGenOutput {
    let mut builder = OutputBuilder {
        html: String::new(),
        backend_js: String::new(),
        frontend_js: String::new(),
    };

    for statement in statements {
        let output = visit_statement(&statement);
        match statement.environment() {
            EnvironmentType::Frontend => builder.frontend_js.push_str(output.as_str()),
            EnvironmentType::Backend => builder.backend_js.push_str(output.as_str()),
            EnvironmentType::Isomorphic => {
                builder.frontend_js.push_str(output.as_str());
                builder.backend_js.push_str(output.as_str());
            }
        }
    }

    CodeGenOutput {
        html: if builder.html.is_empty() {
            None
        } else {
            Some(builder.html)
        },
        backend_js: if builder.backend_js.is_empty() {
            None
        } else {
            let prelude = std::fs::read_to_string("./std/prelude.js").unwrap();
            Some(format!("{}{}", prelude, builder.backend_js))
        },
        frontend_js: if builder.frontend_js.is_empty() {
            None
        } else {
            let prelude = std::fs::read_to_string("./std/prelude.js").unwrap();
            Some(format!("{}{}", prelude, builder.frontend_js))
        },
    }
}

fn visit_statement(statement: &Statement) -> String {
    format!(
        "{};",
        match &statement {
            Statement::VariableDeclaration {
                environment: _,
                is_mutable,
                identifier,
                initializer,
            } => format!(
                "{} {}{}",
                if *is_mutable { "let" } else { "const" },
                identifier,
                match initializer {
                    None => "".to_string(),
                    Some(initializer) => format!("={}", visit_expression(initializer)),
                }
            ),
            Statement::FunctionDefinition {
                environment: _,
                name,
                parameters,
                body,
            } => format!(
                "const {}=({})=>{{{}}}",
                name,
                parameters.join(","),
                body.into_iter()
                    .map(visit_statement)
                    .collect::<Vec<_>>()
                    .join("")
            ),
            Statement::Return(_, expr) => match expr {
                Some(expr) => format!("return {}", visit_expression(expr)),
                None => "return".to_string(),
            },
            Statement::Expression(expr) => visit_expression(expr),
            Statement::If {
                environment: _,
                condition,
                body,
                else_,
            } => format!(
                "if({}){{{}}}{}",
                visit_expression(condition),
                body.into_iter()
                    .map(visit_statement)
                    .collect::<Vec<_>>()
                    .join(""),
                if !else_.is_empty() {
                    format!(
                        "else{{{}}}",
                        else_
                            .into_iter()
                            .map(visit_statement)
                            .collect::<Vec<_>>()
                            .join("")
                    )
                } else {
                    "".to_string()
                }
            ),
            Statement::WhileLoop(_, body) => format!(
                "while(true){{{}}}",
                body.into_iter()
                    .map(visit_statement)
                    .collect::<Vec<_>>()
                    .join("")
            ),
            Statement::Break(_) => "break".to_string(),
        }
    )
}

fn visit_expression(expression: &Expression) -> String {
    match &expression.kind {
        ExpressionKind::Parenthesized(expr) => format!("({})", visit_expression(expr)),
        ExpressionKind::BinaryExpression(left, op, right) => format!(
            "{}{}{}",
            visit_expression(&left),
            op,
            visit_expression(&right)
        )
        .to_string(),

        ExpressionKind::PreUnaryExpression(op, expr) => match op {
            PreUnaryOperator::Not => format!("{}({})", op, visit_expression(expr)),
            PreUnaryOperator::Increment | PreUnaryOperator::Decrement => {
                format!("({}{})", op, visit_expression(expr))
            }
        },

        ExpressionKind::PropertyAccess(left, right) => {
            format!("{}.{}", visit_expression(left), right.replace("-", "$"))
        }

        ExpressionKind::Boolean(b) => b.to_string(),

        ExpressionKind::JsBlock(expressions) => expressions
            .iter()
            .map(|expr| {
                if let ExpressionKind::String(contents) = &expr.kind {
                    contents.clone()
                } else {
                    visit_expression(expr)
                }
            })
            .collect::<Vec<_>>()
            .join(""),

        ExpressionKind::FunctionCall { callee, arguments } => format!(
            "{}({})",
            visit_expression(&callee),
            arguments
                .iter()
                .map(visit_expression)
                .collect::<Vec<String>>()
                .join(",")
        ),

        ExpressionKind::Identifier(ident) => ident.clone().replace("-", "$"),

        ExpressionKind::Integer(n) => n.to_string(),

        ExpressionKind::String(s) => format!(
            "`{}`",
            s.replace("`", "\\`")
                .replace("\r", "\\r")
                .replace("\n", "\\n")
                .replace("\t", "\\t")
        ),
        ExpressionKind::List(expressions) => format!(
            "[{}]",
            expressions
                .into_iter()
                .map(visit_expression)
                .collect::<Vec<_>>()
                .join(",")
        ),
        ExpressionKind::ArrayAccess(left, index) => {
            format!("{}[{}]", visit_expression(left), visit_expression(index))
        }
    }
}
