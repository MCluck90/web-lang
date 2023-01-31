use std::collections::HashMap;

use crate::{
    errors::CompilerError,
    frontend, middle_end,
    types::symbol_table::{SymbolTable, ValueId, ValueSymbol},
};

use super::ir::ImportSelectorKind;

pub fn resolve_names(
    source_modules: Vec<&frontend::ir::Module>,
) -> (Vec<middle_end::ir::Module>, SymbolTable) {
    let mut ctx = Context::new();
    let mut resolved_modules = Vec::<middle_end::ir::Module>::new();

    for module in &source_modules {
        let (module, exports) = resolve_module(&mut ctx, module);
        ctx.exports.insert(module.path.clone(), exports);
        resolved_modules.push(module);
    }

    // Look for issues with imports
    for module in resolved_modules.iter_mut() {
        let imports = module
            .ast
            .imports
            .iter()
            .map(|import| (import.path.clone(), import.to_identifiers()));

        for (path, identifiers) in imports {
            if let Some(exports) = ctx.exports.get(&path) {
                for (ident, span) in &identifiers {
                    if !exports.contains_key(ident) {
                        module
                            .errors
                            .push(CompilerError::invalid_import(span, &path, ident));
                    }
                }
            }
        }
    }

    (resolved_modules, ctx.symbol_table)
}

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
        self.set_direct(identifier, &new_identifier);
        new_identifier
    }

    fn set_direct(&mut self, identifier: &String, new_identifier: &String) {
        if let Some(identifiers) = self.rename_table.get_mut(identifier) {
            identifiers.push(new_identifier.clone());
        } else {
            self.rename_table
                .insert(identifier.clone(), vec![new_identifier.clone()]);
        }
    }
}

// Original name -> Rename
type Exports = HashMap<String, String>;

struct Context {
    id_counter: IdCounter,
    scopes: Vec<Scope>,
    // Module path -> Original name -> New name
    exports: HashMap<String, Exports>,
    symbol_table: SymbolTable,
}
impl Context {
    fn new() -> Context {
        Context {
            id_counter: IdCounter::new(),
            scopes: Vec::new(),
            exports: HashMap::new(),
            symbol_table: SymbolTable::new(),
        }
    }

    fn add_and_rename(
        &mut self,
        identifier: &frontend::ir::Identifier,
        is_mutable: &bool,
    ) -> middle_end::ir::Identifier {
        let new_name = self
            .scopes
            .last_mut()
            .unwrap()
            .set(&identifier.name, &mut self.id_counter);
        let new_identifier = middle_end::ir::Identifier::from_source(identifier, new_name.clone());
        self.symbol_table.set_value(
            ValueId(new_name),
            ValueSymbol::new().with_mutability(*is_mutable),
        );
        new_identifier
    }

    fn add_from_module_to_scope(&mut self, module_path: &String, identifier: &String) {
        match self.exports.get(module_path) {
            Some(exports) => {
                if let Some(resolved_name) = exports.get(identifier) {
                    self.scopes
                        .last_mut()
                        .unwrap()
                        .set_direct(identifier, resolved_name);
                }
            }
            None => {}
        }
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
        identifier: &frontend::ir::Identifier,
    ) -> Option<middle_end::ir::Identifier> {
        for scope in self.scopes.iter().rev() {
            if let Some(new_name) = scope.get(&identifier.name) {
                return Some(middle_end::ir::Identifier::from_source(
                    identifier,
                    new_name.to_string(),
                ));
            }
        }
        None
    }
}

fn resolve_module(
    ctx: &mut Context,
    module: &frontend::ir::Module,
) -> (middle_end::ir::Module, Exports) {
    let mut exports = Exports::new();
    let mut imports: Vec<middle_end::ir::Import> = Vec::new();
    let mut statements: Vec<middle_end::ir::TopLevelStatement> = Vec::new();
    let mut errors: Vec<CompilerError> = module.errors.clone();
    if module.ast.is_none() {
        return (
            middle_end::ir::Module {
                path: module.path.clone(),
                ast: middle_end::ir::ModuleAST {
                    path: module.path.clone(),
                    imports,
                    statements,
                },
                errors,
            },
            exports,
        );
    }
    let ast = module.ast.as_ref().unwrap();

    ctx.start_scope();
    for import in &ast.imports {
        let (import, mut errs) = resolve_import(ctx, import);
        imports.push(import);
        errors.append(&mut errs);
    }

    for statement in &ast.statements {
        let (statement, mut errs) = resolve_top_level_statement(ctx, statement);
        errors.append(&mut errs);

        match &statement.kind {
            middle_end::ir::TopLevelStatementKind::VariableDeclaration {
                is_public,
                identifier,
                ..
            } => {
                if *is_public {
                    exports.insert(identifier.original_name.clone(), identifier.name.clone());
                }
            }
            middle_end::ir::TopLevelStatementKind::FunctionDefinition {
                is_public, name, ..
            } => {
                if *is_public {
                    exports.insert(name.original_name.clone(), name.name.clone());
                }
            }
            middle_end::ir::TopLevelStatementKind::Expression(_) => {}
            middle_end::ir::TopLevelStatementKind::Loop(_) => {}
            middle_end::ir::TopLevelStatementKind::ForLoop { .. } => {}
        }
        statements.push(statement);
    }

    ctx.end_scope();
    (
        middle_end::ir::Module {
            path: module.path.clone(),
            ast: middle_end::ir::ModuleAST {
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
    import: &frontend::ir::Import,
) -> (middle_end::ir::Import, Vec<CompilerError>) {
    let module_path = import.to_path();
    let new_import = middle_end::ir::Import::from_source(&import);
    for selector in &new_import.selectors {
        match &selector.kind {
            ImportSelectorKind::Name(name) => {
                ctx.add_from_module_to_scope(&module_path, name);
            }
        }
    }
    (new_import, Vec::new())
}

fn resolve_top_level_statement(
    ctx: &mut Context,
    statement: &frontend::ir::TopLevelStatement,
) -> (middle_end::ir::TopLevelStatement, Vec<CompilerError>) {
    match &statement.kind {
        frontend::ir::TopLevelStatementKind::VariableDeclaration {
            is_public,
            is_mutable,
            type_,
            identifier,
            initializer,
        } => {
            let new_identifier = ctx.add_and_rename(identifier, is_mutable);
            let (initializer, errs) = resolve_expression(ctx, initializer);
            (
                middle_end::ir::TopLevelStatement {
                    span: statement.span.clone(),
                    kind: middle_end::ir::TopLevelStatementKind::VariableDeclaration {
                        is_public: *is_public,
                        is_mutable: *is_mutable,
                        type_: type_.clone(),
                        identifier: new_identifier,
                        initializer: Box::new(initializer),
                    },
                },
                errs,
            )
        }
        frontend::ir::TopLevelStatementKind::FunctionDefinition {
            is_public,
            name,
            parameters,
            return_type,
            body,
        } => {
            let new_name = ctx.add_and_rename(&name, &false);
            ctx.start_scope();

            let parameters = parameters
                .iter()
                .map(|p| middle_end::ir::Parameter {
                    span: p.span.clone(),
                    identifier: ctx.add_and_rename(&p.identifier, &false),
                    type_: p.type_.clone(),
                })
                .collect();

            let (body, errors) = resolve_block(ctx, body);
            let mut body_statements = body.statements.clone();
            if let Some(return_expression) = body.return_expression {
                body_statements.push(middle_end::ir::Statement {
                    span: return_expression.span.clone(),
                    kind: middle_end::ir::StatementKind::Return(Some(return_expression)),
                });
            }

            ctx.end_scope();
            (
                middle_end::ir::TopLevelStatement {
                    span: statement.span.clone(),
                    kind: middle_end::ir::TopLevelStatementKind::FunctionDefinition {
                        is_public: *is_public,
                        name: new_name,
                        parameters,
                        return_type: return_type.clone(),
                        body: body_statements,
                    },
                },
                errors,
            )
        }
        frontend::ir::TopLevelStatementKind::Expression(expression) => {
            let (expr, errors) = resolve_expression(ctx, expression);
            (
                middle_end::ir::TopLevelStatement {
                    span: statement.span.clone(),
                    kind: middle_end::ir::TopLevelStatementKind::Expression(expr),
                },
                errors,
            )
        }
        frontend::ir::TopLevelStatementKind::Loop(body) => {
            let (body, errors) = resolve_block(ctx, body);
            let mut body_statements = body.statements.clone();
            if let Some(expr) = body.return_expression {
                body_statements.push(middle_end::ir::Statement {
                    span: expr.span.clone(),
                    kind: middle_end::ir::StatementKind::Expression(expr),
                });
            }
            (
                middle_end::ir::TopLevelStatement {
                    span: statement.span.clone(),
                    kind: middle_end::ir::TopLevelStatementKind::Loop(body_statements),
                },
                errors,
            )
        }
        frontend::ir::TopLevelStatementKind::ForLoop {
            initializer,
            condition,
            post_loop,
            body,
        } => {
            ctx.start_scope();
            let (initializer, mut errors) = initializer
                .clone()
                .map(|i| {
                    let (i, errors) = resolve_statement(ctx, &*i);
                    (Some(i), errors)
                })
                .unwrap_or((None, Vec::new()));
            let (condition, mut condition_errors) = condition
                .clone()
                .map(|c| {
                    let (c, errors) = resolve_expression(ctx, &c);
                    (Some(c), errors)
                })
                .unwrap_or((None, Vec::new()));
            let (post_loop, mut post_loop_errors) = post_loop
                .clone()
                .map(|i| {
                    let (i, errors) = resolve_expression(ctx, &i);
                    (Some(i), errors)
                })
                .unwrap_or((None, Vec::new()));
            let mut body_statements: Vec<middle_end::ir::Statement> = Vec::new();
            for stmt in body {
                let (stmt, mut errs) = resolve_statement(ctx, stmt);
                body_statements.push(stmt);
                errors.append(&mut errs);
            }

            errors.append(&mut condition_errors);
            errors.append(&mut post_loop_errors);
            ctx.end_scope();

            (
                middle_end::ir::TopLevelStatement {
                    span: statement.span.clone(),
                    kind: middle_end::ir::TopLevelStatementKind::ForLoop {
                        initializer,
                        condition,
                        post_loop,
                        body: body_statements,
                    },
                },
                errors,
            )
        }
    }
}

fn resolve_statement(
    ctx: &mut Context,
    statement: &frontend::ir::Statement,
) -> (middle_end::ir::Statement, Vec<CompilerError>) {
    match &statement.kind {
        frontend::ir::StatementKind::VariableDeclaration {
            is_mutable,
            identifier,
            initializer,
        } => {
            let new_identifier = ctx.add_and_rename(identifier, is_mutable);
            let (initializer, errs) = resolve_expression(ctx, initializer);
            (
                middle_end::ir::Statement {
                    span: statement.span.clone(),
                    kind: middle_end::ir::StatementKind::VariableDeclaration {
                        is_mutable: *is_mutable,
                        identifier: new_identifier,
                        initializer: Box::new(initializer),
                    },
                },
                errs,
            )
        }
        frontend::ir::StatementKind::FunctionDefinition {
            name,
            parameters,
            return_type,
            body,
        } => {
            let new_name = ctx.add_and_rename(&name, &false);
            ctx.start_scope();

            let parameters = parameters
                .iter()
                .map(|p| middle_end::ir::Parameter {
                    span: p.span.clone(),
                    identifier: ctx.add_and_rename(&p.identifier, &false),
                    type_: p.type_.clone(),
                })
                .collect();

            let (body, errors) = resolve_block(ctx, body);
            let mut body_statements = body.statements.clone();
            if let Some(return_expression) = body.return_expression {
                body_statements.push(middle_end::ir::Statement {
                    span: return_expression.span.clone(),
                    kind: middle_end::ir::StatementKind::Return(Some(return_expression)),
                });
            }

            ctx.end_scope();
            (
                middle_end::ir::Statement {
                    span: statement.span.clone(),
                    kind: middle_end::ir::StatementKind::FunctionDefinition {
                        name: new_name,
                        parameters,
                        return_type: return_type.clone(),
                        body: body_statements,
                    },
                },
                errors,
            )
        }
        frontend::ir::StatementKind::Expression(expression) => {
            let (expr, errors) = resolve_expression(ctx, expression);
            (
                middle_end::ir::Statement {
                    span: statement.span.clone(),
                    kind: middle_end::ir::StatementKind::Expression(expr),
                },
                errors,
            )
        }
        frontend::ir::StatementKind::Return(expression) => {
            let (expr, errs) = expression
                .as_ref()
                .map(|expr| {
                    let (expr, errs) = resolve_expression(ctx, expr);
                    (Some(expr), errs)
                })
                .unwrap_or((None, Vec::new()));
            (
                middle_end::ir::Statement {
                    span: statement.span.clone(),
                    kind: middle_end::ir::StatementKind::Return(expr),
                },
                errs,
            )
        }
        frontend::ir::StatementKind::Loop(body) => {
            let (body, errors) = resolve_block(ctx, body);
            (
                middle_end::ir::Statement {
                    span: statement.span.clone(),
                    kind: middle_end::ir::StatementKind::Loop(body.statements),
                },
                errors,
            )
        }
        frontend::ir::StatementKind::Break => (
            middle_end::ir::Statement {
                span: statement.span.clone(),
                kind: middle_end::ir::StatementKind::Break,
            },
            Vec::new(),
        ),
        frontend::ir::StatementKind::ForLoop {
            initializer,
            condition,
            post_loop,
            body,
        } => {
            ctx.start_scope();
            let (initializer, mut errors) = initializer
                .clone()
                .map(|i| {
                    let (i, errors) = resolve_statement(ctx, &*i);
                    (Some(i), errors)
                })
                .unwrap_or((None, Vec::new()));
            let (condition, mut condition_errors) = condition
                .clone()
                .map(|c| {
                    let (c, errors) = resolve_expression(ctx, &c);
                    (Some(c), errors)
                })
                .unwrap_or((None, Vec::new()));
            let (post_loop, mut post_loop_errors) = post_loop
                .clone()
                .map(|i| {
                    let (i, errors) = resolve_expression(ctx, &i);
                    (Some(i), errors)
                })
                .unwrap_or((None, Vec::new()));
            let mut body_statements: Vec<middle_end::ir::Statement> = Vec::new();
            for stmt in body {
                let (stmt, mut errs) = resolve_statement(ctx, stmt);
                body_statements.push(stmt);
                errors.append(&mut errs);
            }

            errors.append(&mut condition_errors);
            errors.append(&mut post_loop_errors);
            ctx.end_scope();

            (
                middle_end::ir::Statement {
                    span: statement.span.clone(),
                    kind: middle_end::ir::StatementKind::ForLoop {
                        initializer: initializer.map(Box::new),
                        condition: condition.map(Box::new),
                        post_loop: post_loop.map(Box::new),
                        body: body_statements,
                    },
                },
                errors,
            )
        }
    }
}

fn resolve_expression(
    ctx: &mut Context,
    expression: &frontend::ir::Expression,
) -> (middle_end::ir::Expression, Vec<CompilerError>) {
    let span = expression.span.clone();
    let to_expression = move |kind: middle_end::ir::ExpressionKind, errors: Vec<CompilerError>| {
        (middle_end::ir::Expression { span, kind }, errors)
    };
    match &expression.kind {
        frontend::ir::ExpressionKind::Boolean(value) => {
            to_expression(middle_end::ir::ExpressionKind::Boolean(*value), Vec::new())
        }
        frontend::ir::ExpressionKind::Integer(value) => {
            to_expression(middle_end::ir::ExpressionKind::Integer(*value), Vec::new())
        }
        frontend::ir::ExpressionKind::String(value) => to_expression(
            middle_end::ir::ExpressionKind::String(value.clone()),
            Vec::new(),
        ),
        frontend::ir::ExpressionKind::Identifier(identifier) => {
            match ctx.find_identifier(identifier) {
                None => to_expression(
                    middle_end::ir::ExpressionKind::Identifier(
                        middle_end::ir::Identifier::from_source(
                            identifier,
                            identifier.name.clone(),
                        ),
                    ),
                    vec![CompilerError::reference_error(
                        &identifier.span,
                        &identifier.name,
                    )],
                ),
                Some(new_identifier) => to_expression(
                    middle_end::ir::ExpressionKind::Identifier(new_identifier),
                    Vec::new(),
                ),
            }
        }
        frontend::ir::ExpressionKind::JsBlock(type_, expressions) => {
            let mut errors: Vec<CompilerError> = Vec::new();
            let mut resolved_expressions: Vec<middle_end::ir::Expression> = Vec::new();
            for expr in expressions {
                let (expr, mut errs) = resolve_expression(ctx, expr);
                errors.append(&mut errs);
                resolved_expressions.push(expr);
            }
            (
                middle_end::ir::Expression {
                    span: expression.span.clone(),
                    kind: middle_end::ir::ExpressionKind::JsBlock(
                        type_.clone(),
                        resolved_expressions,
                    ),
                },
                errors,
            )
        }
        frontend::ir::ExpressionKind::Block(block) => {
            let (block, errs) = resolve_block(ctx, block);
            to_expression(middle_end::ir::ExpressionKind::Block(Box::new(block)), errs)
        }
        frontend::ir::ExpressionKind::BinaryExpression(left, op, right) => {
            let (left, mut left_errors) = resolve_expression(ctx, left);
            let (right, right_errors) = resolve_expression(ctx, right);
            left_errors.extend(right_errors);
            to_expression(
                middle_end::ir::ExpressionKind::BinaryExpression(
                    Box::new(left),
                    op.clone(),
                    Box::new(right),
                ),
                left_errors,
            )
        }
        frontend::ir::ExpressionKind::PropertyAccess(left, right) => {
            let (left, errors) = resolve_expression(ctx, left);
            to_expression(
                middle_end::ir::ExpressionKind::PropertyAccess(
                    Box::new(left),
                    middle_end::ir::Identifier::from_source(right, right.name.clone()),
                ),
                errors,
            )
        }
        frontend::ir::ExpressionKind::FunctionCall { callee, arguments } => {
            let (callee, mut errors) = resolve_expression(ctx, callee);
            let mut args: Vec<middle_end::ir::Expression> = Vec::new();

            for arg in arguments {
                let (arg, mut errs) = resolve_expression(ctx, arg);
                errors.append(&mut errs);
                args.push(arg);
            }

            to_expression(
                middle_end::ir::ExpressionKind::FunctionCall {
                    callee: Box::new(callee),
                    arguments: args,
                },
                errors,
            )
        }
        frontend::ir::ExpressionKind::If {
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
                middle_end::ir::ExpressionKind::If {
                    condition: Box::new(condition),
                    body: Box::new(body),
                    else_,
                },
                errors,
            )
        }
        frontend::ir::ExpressionKind::Error => unreachable!(),
    }
}

fn resolve_block(
    ctx: &mut Context,
    block: &frontend::ir::Block,
) -> (middle_end::ir::Block, Vec<CompilerError>) {
    ctx.start_scope();

    let mut errors: Vec<CompilerError> = Vec::new();
    let mut statements: Vec<middle_end::ir::Statement> = Vec::new();

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
        middle_end::ir::Block {
            span: block.span.clone(),
            statements,
            return_expression,
        },
        errors,
    )
}
