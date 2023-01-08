use crate::parser::{Expression, ExpressionKind, Program, Statement, StatementKind};

pub struct CodeGenOutput {
    pub html: Option<String>,
    pub js: Option<String>,
}

struct OutputBuilder {
    html: String,
    js: String,
}

pub fn generate_code(program: &Program) -> CodeGenOutput {
    let mut builder = OutputBuilder {
        html: String::new(),
        js: String::new(),
    };
    visit_program(program, &mut builder);
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

fn visit_program(program: &Program, output: &mut OutputBuilder) {
    for statement in &program.statements {
        output.js.push_str(&visit_statement(&statement).as_str());
    }
}

fn visit_statement(statement: &Statement) -> String {
    format!(
        "{};",
        match &statement.kind {
            StatementKind::Expression(expr) => visit_expression(expr),
            StatementKind::FunctionDefinition {
                name,
                parameters,
                body,
                ..
            } => format!(
                "const {}${}=({})=>{}",
                name,
                statement.id,
                parameters
                    .iter()
                    .map(|p| format!("{}${}", p.identifier.name.clone(), p.id))
                    .collect::<Vec<String>>()
                    .join(","),
                visit_expression(&body),
            ),
            StatementKind::JsBlock(expressions) => {
                expressions
                    .iter()
                    .map(|expr| {
                        if let ExpressionKind::String(contents) = &expr.kind {
                            contents.clone()
                        } else {
                            visit_expression(expr)
                        }
                    })
                    .collect::<Vec<_>>()
                    .join("")
            }
        }
    )
}

fn visit_expression(expression: &Expression) -> String {
    match &expression.kind {
        ExpressionKind::BinaryExpression(left, op, right) => format!(
            "{}{}{}",
            visit_expression(&left),
            op,
            visit_expression(&right)
        )
        .to_string(),

        ExpressionKind::Boolean(b) => b.to_string(),

        ExpressionKind::Block(block) => {
            format!(
                "(()=>{{{}{}}})()",
                block
                    .statements
                    .iter()
                    .map(|statement| visit_statement(statement))
                    .collect::<Vec<_>>()
                    .join(""),
                block
                    .return_expression
                    .clone()
                    .map(|expr| format!("return {}", visit_expression(&expr)))
                    .unwrap_or(String::new())
            )
        }

        ExpressionKind::FunctionCall { callee, arguments } => format!(
            "{}({})",
            visit_expression(&callee),
            arguments
                .iter()
                .map(visit_expression)
                .collect::<Vec<String>>()
                .join(",")
        ),

        ExpressionKind::Identifier(ident) => {
            format!("{}${}", ident.name.clone(), expression.id)
        }

        // TODO: How do early returns work when everything is an expression?
        ExpressionKind::If {
            condition,
            body,
            else_,
        } => format!(
            "({})?{}:{}",
            visit_expression(&condition),
            visit_expression(&body),
            else_
                .clone()
                .map_or("null".to_string(), |e| visit_expression(&e))
        ),

        ExpressionKind::Integer(n) => n.to_string(),

        // TODO: Generate strings with correct quotes and escape characters
        ExpressionKind::String(s) => format!("`{}`", s),

        ExpressionKind::VariableDeclaration {
            is_mutable,
            identifier,
            initializer,
        } => format!(
            "{} {}${}={}",
            if *is_mutable { "let" } else { "const" },
            identifier,
            identifier.id,
            visit_expression(&initializer)
        ),

        ExpressionKind::Error => unreachable!(),
    }
}
