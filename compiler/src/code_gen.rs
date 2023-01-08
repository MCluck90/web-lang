use crate::parser::{Expression, ExpressionKind, Program};

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
    for expression in &program.expressions {
        output.js.push_str(visit_expression(&expression).as_str());
        output.js.push_str(";\n");
    }
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

        ExpressionKind::Block(expressions) => {
            let num_of_expressions = expressions.len();
            format!(
                "(()=>{{\n{}\n}})()",
                expressions
                    .iter()
                    .enumerate()
                    .map(|(index, expr)| if index < num_of_expressions - 1 {
                        visit_expression(expr)
                    } else {
                        format!("return {}", visit_expression(expr))
                    })
                    .collect::<Vec<String>>()
                    .join(";\n")
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

        ExpressionKind::FunctionDefinition {
            name,
            parameters,
            body,
            ..
        } => format!(
            "const {}_{} = ({}) => {}",
            name,
            expression.id,
            parameters
                .iter()
                .map(|p| p.identifier.name.clone())
                .collect::<Vec<String>>()
                .join(","),
            visit_expression(&body),
        ),

        ExpressionKind::Identifier(ident) => {
            format!("{}_{}", ident.name.clone(), expression.id)
        }

        // TODO: How do early returns work when everything is an expression?
        ExpressionKind::If {
            condition,
            body,
            else_,
        } => format!(
            "({}) ? {} : {}",
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
            "{} {}_{}={}",
            if *is_mutable { "let" } else { "const" },
            identifier,
            identifier.id,
            visit_expression(&initializer)
        ),

        ExpressionKind::Error => unreachable!(),
    }
}
