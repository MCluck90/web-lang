use std::collections::HashMap;

use chumsky::prelude::Simple;

use crate::{
    asts::{name_resolved, source},
    errors::CompilerError,
};

struct IdCounter {
    next_id: u32,
}
impl IdCounter {
    fn new() -> Self {
        IdCounter { next_id: 0 }
    }
    fn next(&mut self) -> u32 {
        let id = self.next_id;
        self.next_id += 1;
        id
    }
}

struct Scope {
    rename_table: HashMap<String, Vec<String>>,
}
impl Scope {
    fn new() -> Self {
        Scope {
            rename_table: HashMap::new(),
        }
    }

    fn get(&self, identifier: &String) -> Option<&String> {
        self.rename_table.get(identifier).and_then(|l| l.last())
    }

    /// Returns the new identifier.
    fn set(&mut self, identifier: &String, id_counter: &mut IdCounter) -> String {
        let new_identifier = format!("${}", id_counter.next()).to_string();
        if let Some(identifiers) = self.rename_table.get_mut(identifier) {
            identifiers.push(new_identifier.clone());
        } else {
            self.rename_table
                .insert(identifier.clone(), vec![new_identifier.clone()]);
        }
        new_identifier
    }
}

// Original name -> Rename
type Exports = HashMap<String, String>;

struct Context {
    id_counter: IdCounter,
    scopes: Vec<Scope>,
    // Module path -> Original name -> New name
    exports: HashMap<String, Exports>,
}
impl Context {
    fn new() -> Context {
        Context {
            id_counter: IdCounter::new(),
            scopes: Vec::new(),
            exports: HashMap::new(),
        }
    }

    fn add_and_rename(&mut self, identifier: &source::Identifier) -> name_resolved::Identifier {
        let new_identifier = self
            .scopes
            .last_mut()
            .unwrap()
            .set(&identifier.name, &mut self.id_counter);
        name_resolved::Identifier::from_source(identifier, new_identifier)
    }

    fn start_scope(&mut self) {
        self.scopes.push(Scope::new());
    }

    fn end_scope(&mut self) {
        self.scopes
            .pop()
            .expect("Verify that a matching scope was started");
    }

    fn find_identifier(
        &self,
        identifier: &source::Identifier,
    ) -> Option<name_resolved::Identifier> {
        for scope in self.scopes.iter().rev() {
            if let Some(new_name) = scope.get(&identifier.name) {
                return Some(name_resolved::Identifier::from_source(
                    identifier,
                    new_name.to_string(),
                ));
            }
        }
        None
    }
}

pub fn resolve_names(source_modules: Vec<&source::Module>) -> Vec<name_resolved::Module> {
    let mut ctx = Context::new();
    let mut resolved_modules = Vec::<name_resolved::Module>::new();

    for module in &source_modules {
        let (mut module, exports) = resolve_module(&mut ctx, module);
        ctx.exports.insert(module.path.clone(), exports);
        resolved_modules.push(module);
    }

    resolved_modules
}

fn resolve_module(ctx: &mut Context, module: &source::Module) -> (name_resolved::Module, Exports) {
    ctx.start_scope();

    let mut exports = Exports::new();
    let mut imports: Vec<name_resolved::Import> = Vec::new();
    let mut statements: Vec<name_resolved::Statement> = Vec::new();
    let mut errors: Vec<CompilerError> = module.errors.clone();
    let ast = module.ast.as_ref().unwrap();

    for import in &ast.imports {
        let (import, mut errs) = resolve_import(ctx, import);
        imports.push(import);
        errors.append(&mut errs);
    }

    for statement in &ast.statements {
        let (statement, mut errs) = resolve_statement(ctx, statement);
        errors.append(&mut errs);

        match &statement.kind {
            name_resolved::StatementKind::FunctionDefinition { name, .. } => {
                // Export all function definitions
                exports.insert(name.original_name.clone(), name.name.clone());
            }
            name_resolved::StatementKind::Expression(_) => {}
            name_resolved::StatementKind::JsBlock(_) => {}
            name_resolved::StatementKind::Return(_) => {}
        }
        statements.push(statement);
    }

    ctx.end_scope();
    (
        name_resolved::Module {
            path: module.path.clone(),
            ast: name_resolved::ModuleAST {
                path: module.path.clone(),
                imports,
                statements,
            },
            errors,
        },
        exports,
    )
}

fn resolve_import(
    ctx: &mut Context,
    import: &source::Import,
) -> (name_resolved::Import, Vec<CompilerError>) {
    match &import.kind {
        source::ImportKind::Package {
            scope,
            package,
            path,
            selectors,
        } => todo!("TODO: Look at ctx.exports['scope:package/path'] for 'selectors' identifiers and insert in to scope")
    }
}

fn resolve_statement(
    ctx: &mut Context,
    statement: &source::Statement,
) -> (name_resolved::Statement, Vec<CompilerError>) {
    match &statement.kind {
        source::StatementKind::FunctionDefinition {
            name,
            parameters,
            return_type,
            body,
        } => {
            let new_name = ctx.add_and_rename(&name);
            ctx.start_scope();

            let parameters = parameters
                .iter()
                .map(|p| name_resolved::Parameter {
                    span: p.span.clone(),
                    identifier: ctx.add_and_rename(&p.identifier),
                    type_: p.type_.clone(),
                })
                .collect();

            let (body, errors) = resolve_expression(ctx, body);

            ctx.end_scope();
            (
                name_resolved::Statement {
                    span: statement.span.clone(),
                    kind: name_resolved::StatementKind::FunctionDefinition {
                        name: new_name,
                        parameters,
                        return_type: return_type.clone(),
                        body: Box::new(body),
                    },
                },
                errors,
            )
        }
        source::StatementKind::Expression(expression) => {
            let (expr, errors) = resolve_expression(ctx, expression);
            (
                name_resolved::Statement {
                    span: statement.span.clone(),
                    kind: name_resolved::StatementKind::Expression(expr),
                },
                errors,
            )
        }
        source::StatementKind::JsBlock(expressions) => {
            let mut errors: Vec<CompilerError> = Vec::new();
            let mut resolved_expressions: Vec<name_resolved::Expression> = Vec::new();
            for expr in expressions {
                let (expr, mut errs) = resolve_expression(ctx, expr);
                errors.append(&mut errs);
                resolved_expressions.push(expr);
            }
            (
                name_resolved::Statement {
                    span: statement.span.clone(),
                    kind: name_resolved::StatementKind::JsBlock(resolved_expressions),
                },
                errors,
            )
        }
        source::StatementKind::Return(_) => todo!(),
    }
}

fn resolve_expression(
    ctx: &mut Context,
    expression: &source::Expression,
) -> (name_resolved::Expression, Vec<CompilerError>) {
    let span = expression.span.clone();
    let to_expression = move |kind: name_resolved::ExpressionKind, errors: Vec<CompilerError>| {
        (name_resolved::Expression { span, kind }, errors)
    };
    match &expression.kind {
        source::ExpressionKind::Boolean(value) => {
            to_expression(name_resolved::ExpressionKind::Boolean(*value), Vec::new())
        }
        source::ExpressionKind::Integer(value) => {
            to_expression(name_resolved::ExpressionKind::Integer(*value), Vec::new())
        }
        source::ExpressionKind::String(value) => to_expression(
            name_resolved::ExpressionKind::String(value.clone()),
            Vec::new(),
        ),
        source::ExpressionKind::Identifier(identifier) => match ctx.find_identifier(identifier) {
            None => to_expression(
                name_resolved::ExpressionKind::Identifier(name_resolved::Identifier::from_source(
                    identifier,
                    identifier.name.clone(),
                )),
                vec![Simple::custom(
                    identifier.span.clone(),
                    format!("Unknown identifier: {}", identifier.name),
                )],
            ),
            Some(new_identifier) => to_expression(
                name_resolved::ExpressionKind::Identifier(new_identifier),
                Vec::new(),
            ),
        },
        source::ExpressionKind::Block(block) => {
            let (block, errs) = resolve_block(ctx, block);
            to_expression(name_resolved::ExpressionKind::Block(Box::new(block)), errs)
        }
        source::ExpressionKind::VariableDeclaration {
            identifier,
            initializer,
            is_mutable,
        } => {
            let new_identifier = ctx.add_and_rename(identifier);
            let (initializer, errs) = resolve_expression(ctx, initializer);
            to_expression(
                name_resolved::ExpressionKind::VariableDeclaration {
                    is_mutable: *is_mutable,
                    identifier: new_identifier,
                    initializer: Box::new(initializer),
                },
                errs,
            )
        }
        source::ExpressionKind::BinaryExpression(left, op, right) => {
            let (left, mut left_errors) = resolve_expression(ctx, left);
            let (right, right_errors) = resolve_expression(ctx, right);
            left_errors.extend(right_errors);
            to_expression(
                name_resolved::ExpressionKind::BinaryExpression(
                    Box::new(left),
                    op.clone(),
                    Box::new(right),
                ),
                left_errors,
            )
        }
        source::ExpressionKind::PropertyAccess(left, right) => {
            let (left, errors) = resolve_expression(ctx, left);
            to_expression(
                name_resolved::ExpressionKind::PropertyAccess(
                    Box::new(left),
                    name_resolved::Identifier::from_source(right, right.name.clone()),
                ),
                errors,
            )
        }
        source::ExpressionKind::FunctionCall { callee, arguments } => {
            let (callee, mut errors) = resolve_expression(ctx, callee);
            let mut args: Vec<name_resolved::Expression> = Vec::new();

            for arg in arguments {
                let (arg, mut errs) = resolve_expression(ctx, arg);
                errors.append(&mut errs);
                args.push(arg);
            }

            to_expression(
                name_resolved::ExpressionKind::FunctionCall {
                    callee: Box::new(callee),
                    arguments: args,
                },
                errors,
            )
        }
        source::ExpressionKind::If {
            condition,
            body,
            else_,
        } => {
            let (condition, mut errors) = resolve_expression(ctx, condition);
            let (body, mut errs) = resolve_expression(ctx, body);
            errors.append(&mut errs);
            let else_ = match else_ {
                None => None,
                Some(else_) => {
                    let (else_, mut errs) = resolve_expression(ctx, else_);
                    errors.append(&mut errs);
                    Some(Box::new(else_))
                }
            };

            to_expression(
                name_resolved::ExpressionKind::If {
                    condition: Box::new(condition),
                    body: Box::new(body),
                    else_,
                },
                errors,
            )
        }
        source::ExpressionKind::Error => unreachable!(),
    }
}

fn resolve_block(
    ctx: &mut Context,
    block: &source::Block,
) -> (name_resolved::Block, Vec<CompilerError>) {
    ctx.start_scope();

    let mut errors: Vec<CompilerError> = Vec::new();
    let mut statements: Vec<name_resolved::Statement> = Vec::new();

    for statement in &block.statements {
        let (statement, mut errs) = resolve_statement(ctx, statement);
        errors.append(&mut errs);
        statements.push(statement);
    }

    let return_expression = match &block.return_expression {
        Some(expression) => {
            let (expr, mut errs) = resolve_expression(ctx, expression);
            errors.append(&mut errs);
            Some(expr)
        }
        None => None,
    };

    ctx.end_scope();

    (
        name_resolved::Block {
            span: block.span.clone(),
            statements,
            return_expression,
        },
        errors,
    )
}
