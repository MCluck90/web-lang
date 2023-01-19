use std::collections::HashMap;

use crate::{
    errors::CompilerError,
    frontend, middle_end, module_paths,
    types::symbol_table::{SymbolTable, ValueId, ValueSymbol},
};

pub fn resolve_names(
    source_modules: Vec<&frontend::ast::Module>,
) -> (Vec<middle_end::ast::Module>, SymbolTable) {
    let mut ctx = Context::new();
    let mut resolved_modules = Vec::<middle_end::ast::Module>::new();

    for module in &source_modules {
        let (module, exports) = resolve_module(&mut ctx, module);
        ctx.exports.insert(module.path.clone(), exports);
        resolved_modules.push(module);
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
        identifier: &frontend::ast::Identifier,
        is_mutable: &bool,
    ) -> middle_end::ast::Identifier {
        let new_name = self
            .scopes
            .last_mut()
            .unwrap()
            .set(&identifier.name, &mut self.id_counter);
        let new_identifier = middle_end::ast::Identifier::from_source(identifier, new_name.clone());
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
        identifier: &frontend::ast::Identifier,
    ) -> Option<middle_end::ast::Identifier> {
        for scope in self.scopes.iter().rev() {
            if let Some(new_name) = scope.get(&identifier.name) {
                return Some(middle_end::ast::Identifier::from_source(
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
    module: &frontend::ast::Module,
) -> (middle_end::ast::Module, Exports) {
    let mut exports = Exports::new();
    let mut imports: Vec<middle_end::ast::Import> = Vec::new();
    let mut statements: Vec<middle_end::ast::Statement> = Vec::new();
    let mut errors: Vec<CompilerError> = module.errors.clone();
    if module.ast.is_none() {
        return (
            middle_end::ast::Module {
                path: module.path.clone(),
                ast: middle_end::ast::ModuleAST {
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
        let (statement, mut errs) = resolve_statement(ctx, statement);
        errors.append(&mut errs);

        // TODO: Change this to handle visibility modifiers
        match &statement.kind {
            middle_end::ast::StatementKind::VariableDeclaration { identifier, .. } => {
                // Export all variable definitions
                exports.insert(identifier.original_name.clone(), identifier.name.clone());
            }
            middle_end::ast::StatementKind::FunctionDefinition { name, .. } => {
                // Export all function definitions
                exports.insert(name.original_name.clone(), name.name.clone());
            }
            middle_end::ast::StatementKind::Expression(_) => {}
            middle_end::ast::StatementKind::Return(_) => {}
        }
        statements.push(statement);
    }

    ctx.end_scope();
    (
        middle_end::ast::Module {
            path: module.path.clone(),
            ast: middle_end::ast::ModuleAST {
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
    import: &frontend::ast::Import,
) -> (middle_end::ast::Import, Vec<CompilerError>) {
    match &import.kind {
        frontend::ast::ImportKind::Package {
            scope,
            package,
            path,
            selectors,
        } => {
            let module_path = module_paths::from_package_import(
                &scope.name,
                &package.name,
                &path.iter().map(|i| i.name.clone()).collect(),
            );
            let new_import = middle_end::ast::Import::from_source(&import);
            for selector in selectors {
                match &selector.kind {
                    frontend::ast::ImportSelectorKind::Name(name) => {
                        ctx.add_from_module_to_scope(&module_path, name);
                    }
                }
            }
            (new_import, Vec::new())
        }
    }
}

fn resolve_statement(
    ctx: &mut Context,
    statement: &frontend::ast::Statement,
) -> (middle_end::ast::Statement, Vec<CompilerError>) {
    match &statement.kind {
        frontend::ast::StatementKind::VariableDeclaration {
            is_mutable,
            identifier,
            initializer,
        } => {
            let new_identifier = ctx.add_and_rename(identifier, is_mutable);
            let (initializer, errs) = resolve_expression(ctx, initializer);
            (
                middle_end::ast::Statement {
                    span: statement.span.clone(),
                    kind: middle_end::ast::StatementKind::VariableDeclaration {
                        is_mutable: *is_mutable,
                        identifier: new_identifier,
                        initializer: Box::new(initializer),
                    },
                },
                errs,
            )
        }
        frontend::ast::StatementKind::FunctionDefinition {
            name,
            parameters,
            return_type,
            body,
        } => {
            let new_name = ctx.add_and_rename(&name, &false);
            ctx.start_scope();

            let parameters = parameters
                .iter()
                .map(|p| middle_end::ast::Parameter {
                    span: p.span.clone(),
                    identifier: ctx.add_and_rename(&p.identifier, &false),
                    type_: p.type_.clone(),
                })
                .collect();

            let (body, errors) = resolve_expression(ctx, body);

            ctx.end_scope();
            (
                middle_end::ast::Statement {
                    span: statement.span.clone(),
                    kind: middle_end::ast::StatementKind::FunctionDefinition {
                        name: new_name,
                        parameters,
                        return_type: return_type.clone(),
                        body: Box::new(body),
                    },
                },
                errors,
            )
        }
        frontend::ast::StatementKind::Expression(expression) => {
            let (expr, errors) = resolve_expression(ctx, expression);
            (
                middle_end::ast::Statement {
                    span: statement.span.clone(),
                    kind: middle_end::ast::StatementKind::Expression(expr),
                },
                errors,
            )
        }
        frontend::ast::StatementKind::Return(expression) => {
            let (expr, errs) = expression
                .as_ref()
                .map(|expr| {
                    let (expr, errs) = resolve_expression(ctx, expr);
                    (Some(expr), errs)
                })
                .unwrap_or((None, Vec::new()));
            (
                middle_end::ast::Statement {
                    span: statement.span.clone(),
                    kind: middle_end::ast::StatementKind::Return(expr),
                },
                errs,
            )
        }
    }
}

fn resolve_expression(
    ctx: &mut Context,
    expression: &frontend::ast::Expression,
) -> (middle_end::ast::Expression, Vec<CompilerError>) {
    let span = expression.span.clone();
    let to_expression = move |kind: middle_end::ast::ExpressionKind, errors: Vec<CompilerError>| {
        (middle_end::ast::Expression { span, kind }, errors)
    };
    match &expression.kind {
        frontend::ast::ExpressionKind::Boolean(value) => {
            to_expression(middle_end::ast::ExpressionKind::Boolean(*value), Vec::new())
        }
        frontend::ast::ExpressionKind::Integer(value) => {
            to_expression(middle_end::ast::ExpressionKind::Integer(*value), Vec::new())
        }
        frontend::ast::ExpressionKind::String(value) => to_expression(
            middle_end::ast::ExpressionKind::String(value.clone()),
            Vec::new(),
        ),
        frontend::ast::ExpressionKind::Identifier(identifier) => {
            match ctx.find_identifier(identifier) {
                None => to_expression(
                    middle_end::ast::ExpressionKind::Identifier(
                        middle_end::ast::Identifier::from_source(
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
                    middle_end::ast::ExpressionKind::Identifier(new_identifier),
                    Vec::new(),
                ),
            }
        }
        frontend::ast::ExpressionKind::JsBlock(type_, expressions) => {
            let mut errors: Vec<CompilerError> = Vec::new();
            let mut resolved_expressions: Vec<middle_end::ast::Expression> = Vec::new();
            for expr in expressions {
                let (expr, mut errs) = resolve_expression(ctx, expr);
                errors.append(&mut errs);
                resolved_expressions.push(expr);
            }
            (
                middle_end::ast::Expression {
                    span: expression.span.clone(),
                    kind: middle_end::ast::ExpressionKind::JsBlock(
                        type_.clone(),
                        resolved_expressions,
                    ),
                },
                errors,
            )
        }
        frontend::ast::ExpressionKind::Block(block) => {
            let (block, errs) = resolve_block(ctx, block);
            to_expression(
                middle_end::ast::ExpressionKind::Block(Box::new(block)),
                errs,
            )
        }
        frontend::ast::ExpressionKind::BinaryExpression(left, op, right) => {
            let (left, mut left_errors) = resolve_expression(ctx, left);
            let (right, right_errors) = resolve_expression(ctx, right);
            left_errors.extend(right_errors);
            to_expression(
                middle_end::ast::ExpressionKind::BinaryExpression(
                    Box::new(left),
                    op.clone(),
                    Box::new(right),
                ),
                left_errors,
            )
        }
        frontend::ast::ExpressionKind::PropertyAccess(left, right) => {
            let (left, errors) = resolve_expression(ctx, left);
            to_expression(
                middle_end::ast::ExpressionKind::PropertyAccess(
                    Box::new(left),
                    middle_end::ast::Identifier::from_source(right, right.name.clone()),
                ),
                errors,
            )
        }
        frontend::ast::ExpressionKind::FunctionCall { callee, arguments } => {
            let (callee, mut errors) = resolve_expression(ctx, callee);
            let mut args: Vec<middle_end::ast::Expression> = Vec::new();

            for arg in arguments {
                let (arg, mut errs) = resolve_expression(ctx, arg);
                errors.append(&mut errs);
                args.push(arg);
            }

            to_expression(
                middle_end::ast::ExpressionKind::FunctionCall {
                    callee: Box::new(callee),
                    arguments: args,
                },
                errors,
            )
        }
        frontend::ast::ExpressionKind::If {
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
                middle_end::ast::ExpressionKind::If {
                    condition: Box::new(condition),
                    body: Box::new(body),
                    else_,
                },
                errors,
            )
        }
        frontend::ast::ExpressionKind::Error => unreachable!(),
    }
}

fn resolve_block(
    ctx: &mut Context,
    block: &frontend::ast::Block,
) -> (middle_end::ast::Block, Vec<CompilerError>) {
    ctx.start_scope();

    let mut errors: Vec<CompilerError> = Vec::new();
    let mut statements: Vec<middle_end::ast::Statement> = Vec::new();

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
        middle_end::ast::Block {
            span: block.span.clone(),
            statements,
            return_expression,
        },
        errors,
    )
}
