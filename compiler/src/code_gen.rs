use crate::parser::{Expression, Program};

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
        output.js.push_str(visit_expression(&expression.0).as_str());
        output.js.push_str(";\n");
    }
}

fn visit_expression(expression: &Expression) -> String {
    match expression {
        Expression::BinaryExpression(left, op, right) => format!(
            "{}{}{}",
            visit_expression(left),
            op,
            visit_expression(right)
        )
        .to_string(),

        Expression::Boolean(b) => b.to_string(),

        Expression::Block(expressions) => {
            let num_of_expressions = expressions.len();
            format!(
                "(()=>{{{}}})()",
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

        Expression::FunctionCall { callee, arguments } => format!(
            "{}({})",
            visit_expression(callee),
            arguments
                .iter()
                .map(visit_expression)
                .collect::<Vec<String>>()
                .join(",")
        ),

        Expression::FunctionDefinition {
            name,
            parameters,
            body,
        } => format!(
            "const {} = ({}) => {}",
            name,
            parameters
                .iter()
                .map(|p| p.name.clone())
                .collect::<Vec<String>>()
                .join(","),
            visit_expression(body),
        ),

        Expression::Identifier(ident) => ident.clone(),

        // TODO: How do early returns work when everything is an expression?
        Expression::If {
            condition,
            body,
            else_,
        } => format!(
            "({}) ? {} : {}",
            visit_expression(condition),
            visit_expression(body),
            else_
                .clone()
                .map_or("null".to_string(), |e| visit_expression(&e))
        ),

        Expression::Integer(n) => n.to_string(),

        // TODO: Generate strings with correct quotes and escape characters
        Expression::String(s) => format!("`{}`", s),

        Expression::VariableDeclaration {
            is_mutable,
            identifier,
            initializer,
        } => format!(
            "{} {}={}",
            if *is_mutable { "let" } else { "const" },
            identifier,
            visit_expression(initializer)
        ),

        Expression::Error => unreachable!(),
    }
}
