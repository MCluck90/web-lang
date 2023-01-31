use super::ir::{Expression, Statement};

pub struct CodeGenOutput {
    pub html: Option<String>,
    pub js: Option<String>,
}

struct OutputBuilder {
    html: String,
    js: String,
}

pub fn generate_code(statements: Vec<Statement>) -> CodeGenOutput {
    let mut builder = OutputBuilder {
        html: String::new(),
        js: String::new(),
    };

    for statement in statements {
        builder.js.push_str(visit_statement(&statement).as_str());
    }

    CodeGenOutput {
        html: if builder.html.is_empty() {
            None
        } else {
            Some(builder.html)
        },
        js: if builder.js.is_empty() {
            None
        } else {
            Some(builder.js)
        },
    }
}

fn visit_statement(statement: &Statement) -> String {
    format!(
        "{};",
        match &statement {
            Statement::VariableDeclaration {
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
            Statement::Return(expr) => match expr {
                Some(expr) => format!("return {}", visit_expression(expr)),
                None => "return".to_string(),
            },
            Statement::Expression(expr) => visit_expression(expr),
            Statement::If {
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
            Statement::WhileLoop(body) => format!(
                "while(true){{{}}}",
                body.into_iter()
                    .map(visit_statement)
                    .collect::<Vec<_>>()
                    .join("")
            ),
            Statement::Break => "break".to_string(),
        }
    )
}

fn visit_expression(expression: &Expression) -> String {
    match &expression {
        Expression::BinaryExpression(left, op, right) => format!(
            "{}{}{}",
            visit_expression(&left),
            op,
            visit_expression(&right)
        )
        .to_string(),

        Expression::PropertyAccess(left, right) => {
            format!("{}.{}", visit_expression(left), right)
        }

        Expression::Boolean(b) => b.to_string(),

        Expression::JsBlock(expressions) => expressions
            .iter()
            .map(|expr| {
                if let Expression::String(contents) = &expr {
                    contents.clone()
                } else {
                    visit_expression(expr)
                }
            })
            .collect::<Vec<_>>()
            .join(""),

        Expression::FunctionCall { callee, arguments } => format!(
            "{}({})",
            visit_expression(&callee),
            arguments
                .iter()
                .map(visit_expression)
                .collect::<Vec<String>>()
                .join(",")
        ),

        Expression::Identifier(ident) => ident.clone(),

        Expression::Integer(n) => n.to_string(),

        Expression::String(s) => format!(
            "`{}`",
            s.replace("`", "\\`")
                .replace("\r", "\\r")
                .replace("\n", "\\n")
                .replace("\t", "\\t")
        ),
        Expression::List(expressions) => format!(
            "[{}]",
            expressions
                .into_iter()
                .map(visit_expression)
                .collect::<Vec<_>>()
                .join(",")
        ),
        Expression::ArrayAccess(left, index) => {
            format!("{}[{}]", visit_expression(left), visit_expression(index))
        }
    }
}
