use std::collections::HashMap;

use chumsky::prelude::Simple;

use crate::parser::{Expression, ExpressionKind, Identifier, Parameter, Program};

use super::shared::{NodeId, Symbol, SymbolTable};

pub struct Scope {
    pub symbols: HashMap<String, Vec<NodeId>>,
}
impl Scope {
    fn new() -> Self {
        Scope {
            symbols: HashMap::new(),
        }
    }

    fn set(&mut self, identifier: &String, id: NodeId) {
        if let Some(identifiers) = self.symbols.get_mut(identifier) {
            identifiers.push(id);
        } else {
            self.symbols.insert(identifier.clone(), vec![id]);
        }
    }

    fn get(&self, identifier: &String) -> Option<&NodeId> {
        self.symbols.get(identifier).map(|v| v.last()).flatten()
    }
}

pub struct Context {
    pub symbol_table: SymbolTable,
    pub scopes: Vec<Scope>,
    pub errors: Vec<Simple<Expression>>,
    pub owner_id: NodeId,
}
impl Context {
    pub fn new() -> Self {
        Context {
            symbol_table: SymbolTable::new(),
            scopes: vec![Scope::new()],
            errors: Vec::new(),
            owner_id: NodeId::from_u32(0),
        }
    }

    fn insert_symbol(&mut self, id: Option<NodeId>, symbol: Symbol) -> NodeId {
        let id = match id {
            Some(id) => id,
            None => self.symbol_table.generate_id(),
        };
        self.symbol_table.insert(id.clone(), symbol);
        id
    }

    fn get_from_scope(&self, identifier: &String) -> Option<&NodeId> {
        for scope in self.scopes.iter().rev() {
            if let Some(id) = scope.get(identifier) {
                return Some(id);
            }
        }
        None
    }

    fn add_to_scope(&mut self, identifier: &String, id: &NodeId) {
        self.scopes.last_mut().unwrap().set(identifier, id.clone());
    }

    fn push_scope(&mut self, scope: Scope) {
        self.scopes.push(scope)
    }

    fn pop_scope(&mut self) -> Option<Scope> {
        self.scopes.pop()
    }
}

/// Generate the initial symbols.
pub fn generate_symbols(program: Program) -> Result<(Program, SymbolTable), Vec<Simple<String>>> {
    let mut ctx = Context::new();

    let program = visit_program(program, &mut ctx);
    if !ctx.errors.is_empty() {
        Err(ctx
            .errors
            .into_iter()
            .map(|e| e.map(|f| f.id.to_string()))
            .collect::<Vec<Simple<String>>>())
    } else {
        Ok((program, ctx.symbol_table))
    }
}

fn visit_program(program: Program, ctx: &mut Context) -> Program {
    Program {
        expressions: program
            .expressions
            .iter()
            .map(|expr| visit_expression(expr, ctx, None))
            .collect(),
    }
}

fn visit_expression(
    expression: &Expression,
    ctx: &mut Context,
    node_id: Option<NodeId>,
) -> Expression {
    match &expression.kind {
        ExpressionKind::Boolean(_) | ExpressionKind::Integer(_) | ExpressionKind::String(_) => {
            let id = ctx.insert_symbol(node_id, Symbol::new(ctx.owner_id.clone()));
            Expression {
                id,
                ..expression.clone()
            }
        }

        ExpressionKind::Identifier(identifier) => {
            if let Some(id) = ctx.get_from_scope(&identifier.name) {
                Expression {
                    id: id.clone(),
                    ..expression.clone()
                }
            } else {
                let id = ctx.insert_symbol(node_id, Symbol::new(ctx.owner_id.clone()));
                ctx.add_to_scope(&identifier.name, &id);
                Expression {
                    id,
                    ..expression.clone()
                }
            }
        }

        ExpressionKind::Block(expressions) => {
            let old_owner_id = ctx.owner_id.clone();
            ctx.owner_id = ctx.insert_symbol(node_id, Symbol::new(old_owner_id.clone()));

            ctx.push_scope(Scope::new());
            let new_block = Expression {
                id: ctx.owner_id.clone(),
                kind: ExpressionKind::Block(
                    expressions
                        .into_iter()
                        .map(|expr| visit_expression(expr, ctx, None))
                        .collect(),
                ),
                ..expression.clone()
            };
            ctx.pop_scope();

            ctx.owner_id = old_owner_id;
            new_block
        }

        ExpressionKind::VariableDeclaration {
            is_mutable,
            identifier,
            initializer,
        } => {
            let initializer = visit_expression(initializer, ctx, None);
            let identifier_id = ctx.insert_symbol(None, Symbol::new(ctx.owner_id.clone()));
            let declaration_id = ctx.insert_symbol(node_id, Symbol::new(ctx.owner_id.clone()));
            ctx.add_to_scope(&identifier.name, &identifier_id);
            Expression {
                id: declaration_id,
                kind: ExpressionKind::VariableDeclaration {
                    is_mutable: *is_mutable,
                    identifier: Identifier {
                        id: identifier_id,
                        name: identifier.name.clone(),
                        span: identifier.span.clone(),
                    },
                    initializer: Box::new(initializer.clone()),
                },
                ..expression.clone()
            }
        }

        ExpressionKind::FunctionDefinition {
            name,
            parameters,
            body,
        } => {
            let name = Identifier {
                id: ctx.insert_symbol(None, Symbol::new(ctx.owner_id.clone())),
                name: name.name.clone(),
                span: name.span.clone(),
            };
            let func_def_id = ctx.insert_symbol(node_id, Symbol::new(ctx.owner_id.clone()));

            let block_id = ctx.symbol_table.generate_id();
            let parameters = parameters
                .clone()
                .iter()
                .map(|param| {
                    let id = ctx.insert_symbol(None, Symbol::new(block_id.clone()));
                    Parameter {
                        id,
                        ..param.clone()
                    }
                })
                .collect::<Vec<Parameter>>();

            let body = visit_expression(body, ctx, Some(block_id));

            Expression {
                id: func_def_id,
                kind: ExpressionKind::FunctionDefinition {
                    name,
                    parameters,
                    body: Box::new(body),
                },
                ..expression.clone()
            }
        }

        // TODO: Create a scope when doing a property access
        // TODO: Allow for getting from scope but only one layer deep
        ExpressionKind::BinaryExpression(left, op, right) => {
            let expression_id = ctx.insert_symbol(None, Symbol::new(ctx.owner_id.clone()));

            let left = visit_expression(left, ctx, None);
            let right = visit_expression(right, ctx, None);

            Expression {
                id: expression_id,
                kind: ExpressionKind::BinaryExpression(Box::new(left), op.clone(), Box::new(right)),
                ..expression.clone()
            }
        }

        ExpressionKind::FunctionCall { callee, arguments } => {
            let id = ctx.insert_symbol(None, Symbol::new(ctx.owner_id.clone()));
            let callee = Box::new(visit_expression(callee, ctx, None));
            let arguments = arguments
                .clone()
                .iter()
                .map(|arg| visit_expression(arg, ctx, None))
                .collect::<Vec<Expression>>();

            Expression {
                id,
                kind: ExpressionKind::FunctionCall { callee, arguments },
                ..expression.clone()
            }
        }

        ExpressionKind::If {
            condition,
            body,
            else_,
        } => {
            let id = ctx.insert_symbol(None, Symbol::new(ctx.owner_id.clone()));
            let condition = Box::new(visit_expression(condition, ctx, None));
            let body = Box::new(visit_expression(body, ctx, None));
            let else_ = else_
                .clone()
                .map(|e| Box::new(visit_expression(&e, ctx, None)));

            Expression {
                id,
                kind: ExpressionKind::If {
                    condition,
                    body,
                    else_,
                },
                ..expression.clone()
            }
        }

        ExpressionKind::Error => todo!(),
    }
}

#[cfg(test)]
mod tests {
    use chumsky::{Parser, Stream};

    use crate::{lexer::lexer, parser::main_parser};

    use super::generate_symbols;

    #[test]
    fn should_use_same_id_for_identifiers_in_same_scope() {
        let src = "a;a;";
        let len = src.chars().count();
        let (tokens, _) = lexer().parse_recovery(src);
        let (program, _) = main_parser()
            .parse_recovery(Stream::from_iter(len..len + 1, tokens.unwrap().into_iter()));
        let (program, _) = generate_symbols(program.unwrap()).unwrap();
        assert_eq!(program.expressions.len(), 2);

        let first = program.expressions.first().unwrap();
        let last = program.expressions.last().unwrap();
        assert_eq!(first.id, last.id);
    }
}