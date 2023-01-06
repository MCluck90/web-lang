use std::collections::HashMap;

use chumsky::prelude::Simple;

use crate::parser::{Expression, ExpressionKind, Identifier, NodeId, Parameter, Program};

pub struct Context {
    pub symbol_table: SymbolTable,
    pub errors: Vec<Simple<Expression>>,
    pub owner_id: NodeId,
}
impl Context {
    pub fn new() -> Self {
        Context {
            symbol_table: SymbolTable::new(),
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
}

#[derive(Clone, Debug)]
pub struct Symbol {
    pub owner_id: NodeId,
}

#[derive(Debug)]
pub struct SymbolTable {
    next_id: u32,
    table: HashMap<NodeId, Symbol>,
}

impl SymbolTable {
    pub fn new() -> Self {
        SymbolTable {
            next_id: 0,
            table: HashMap::new(),
        }
    }

    pub fn generate_id(&mut self) -> NodeId {
        let node_id = NodeId::from_u32(self.next_id);
        self.next_id += 1;
        node_id
    }

    pub fn insert(&mut self, id: NodeId, symbol: Symbol) -> Option<Symbol> {
        self.table.insert(id, symbol)
    }
}

/// Generate the initial symbols.
pub fn generate_symbols(program: Program) -> Result<Program, Vec<Simple<String>>> {
    let mut ctx = Context::new();

    let program = visit_program(program, &mut ctx);
    if !ctx.errors.is_empty() {
        Err(ctx
            .errors
            .into_iter()
            .map(|e| e.map(|f| f.id.to_string()))
            .collect::<Vec<Simple<String>>>())
    } else {
        Ok(program)
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
            let id = ctx.insert_symbol(
                node_id,
                Symbol {
                    owner_id: ctx.owner_id.clone(),
                },
            );
            Expression {
                id,
                ..expression.clone()
            }
        }

        ExpressionKind::Identifier(_) => Expression {
            id: ctx.insert_symbol(
                node_id,
                Symbol {
                    owner_id: ctx.owner_id.clone(),
                },
            ),
            ..expression.clone()
        },

        ExpressionKind::Block(expressions) => {
            let old_owner_id = ctx.owner_id.clone();
            ctx.owner_id = ctx.insert_symbol(
                node_id,
                Symbol {
                    owner_id: old_owner_id.clone(),
                },
            );

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

            ctx.owner_id = old_owner_id;
            new_block
        }

        ExpressionKind::VariableDeclaration {
            is_mutable,
            identifier,
            initializer,
        } => {
            let initializer = visit_expression(initializer, ctx, None);
            let identifier_id = ctx.insert_symbol(
                None,
                Symbol {
                    owner_id: ctx.owner_id.clone(),
                },
            );
            let declaration_id = ctx.insert_symbol(
                node_id,
                Symbol {
                    owner_id: ctx.owner_id.clone(),
                },
            );
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
                id: ctx.insert_symbol(
                    None,
                    Symbol {
                        owner_id: ctx.owner_id.clone(),
                    },
                ),
                name: name.name.clone(),
                span: name.span.clone(),
            };
            let func_def_id = ctx.insert_symbol(
                node_id,
                Symbol {
                    owner_id: ctx.owner_id.clone(),
                },
            );

            let block_id = ctx.symbol_table.generate_id();
            let parameters = parameters
                .clone()
                .iter()
                .map(|param| {
                    let id = ctx.insert_symbol(
                        None,
                        Symbol {
                            owner_id: block_id.clone(),
                        },
                    );
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

        ExpressionKind::BinaryExpression(left, op, right) => {
            let expression_id = ctx.insert_symbol(
                None,
                Symbol {
                    owner_id: ctx.owner_id.clone(),
                },
            );

            let left = visit_expression(left, ctx, None);
            let right = visit_expression(right, ctx, None);

            Expression {
                id: expression_id,
                kind: ExpressionKind::BinaryExpression(Box::new(left), op.clone(), Box::new(right)),
                ..expression.clone()
            }
        }

        ExpressionKind::FunctionCall { callee, arguments } => {
            let id = ctx.insert_symbol(
                None,
                Symbol {
                    owner_id: ctx.owner_id.clone(),
                },
            );
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
            let id = ctx.insert_symbol(
                None,
                Symbol {
                    owner_id: ctx.owner_id.clone(),
                },
            );
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
