use crate::phases::{frontend::BinaryOperator, middle_end};

pub fn from_middle_end(program: middle_end::Program) -> Vec<Statement> {
    let mut ctx = Context::new();
    let mut basic_blocks: Vec<BasicBlock> = Vec::new();
    for module in program.modules {
        basic_blocks.push(module_to_basic_block(&mut ctx, module));
    }
    basic_blocks
        .into_iter()
        .map(block_to_statements)
        .flatten()
        .collect()
}

fn block_to_statements(block: BasicBlock) -> Vec<Statement> {
    block
        .block_or_statements
        .into_iter()
        .map(|block_or_statement| match block_or_statement {
            BlockOrStatement::Block(block) => block_to_statements(block),
            BlockOrStatement::Statement(statement) => vec![statement],
        })
        .flatten()
        .collect()
}

fn module_to_basic_block(ctx: &mut Context, module: middle_end::ir::Module) -> BasicBlock {
    let mut basic_block = BasicBlock::new();
    for statement in module.ast.statements {
        basic_block
            .block_or_statements
            .push(top_level_statement_to_block_or_statement(ctx, statement));
    }
    basic_block
}

fn top_level_statement_to_block_or_statement(
    ctx: &mut Context,
    statement: middle_end::ir::TopLevelStatement,
) -> BlockOrStatement {
    match statement.kind {
        middle_end::ir::TopLevelStatementKind::VariableDeclaration {
            is_public: _,
            is_mutable,
            identifier,
            initializer,
        } => match expression_to_block(ctx, *initializer) {
            (expr, None) => BlockOrStatement::Statement(Statement::VariableDeclaration {
                is_mutable,
                identifier: identifier.name,
                initializer: Some(Box::new(expr)),
            }),
            (expr, Some(block)) => BlockOrStatement::Block(BasicBlock {
                block_or_statements: vec![
                    BlockOrStatement::Block(block),
                    BlockOrStatement::Statement(Statement::VariableDeclaration {
                        is_mutable,
                        identifier: identifier.name,
                        initializer: Some(Box::new(expr)),
                    }),
                ],
            }),
        },
        middle_end::ir::TopLevelStatementKind::FunctionDefinition {
            is_public: _,
            name,
            parameters,
            return_type: _,
            body,
        } => BlockOrStatement::Statement(Statement::FunctionDefinition {
            name: name.name,
            parameters: parameters.into_iter().map(|p| p.identifier.name).collect(),
            body: body
                .into_iter()
                .map(|stmt| {
                    middle_statement_to_block_or_statement(ctx, stmt)
                        .into_iter()
                        .map(|b| b.to_statements())
                        .flatten()
                })
                .flatten()
                .collect(),
        }),
        middle_end::ir::TopLevelStatementKind::Expression(expression) => {
            let (expr, maybe_block) = expression_to_block(ctx, expression);
            if let Some(block) = maybe_block {
                let mut basic_block = BasicBlock::new();
                basic_block
                    .block_or_statements
                    .push(BlockOrStatement::Block(block));
                basic_block
                    .block_or_statements
                    .push(BlockOrStatement::Statement(Statement::Expression(expr)));
                BlockOrStatement::Block(basic_block)
            } else {
                BlockOrStatement::Statement(Statement::Expression(expr))
            }
        }
    }
}

fn middle_statement_to_block_or_statement(
    ctx: &mut Context,
    statement: middle_end::ir::Statement,
) -> Vec<BlockOrStatement> {
    match statement.kind {
        middle_end::ir::StatementKind::VariableDeclaration {
            is_mutable,
            identifier,
            initializer,
        } => vec![match expression_to_block(ctx, *initializer) {
            (expr, None) => BlockOrStatement::Statement(Statement::VariableDeclaration {
                is_mutable,
                identifier: identifier.name,
                initializer: Some(Box::new(expr)),
            }),
            (expr, Some(block)) => BlockOrStatement::Block(BasicBlock {
                block_or_statements: vec![
                    BlockOrStatement::Block(block),
                    BlockOrStatement::Statement(Statement::VariableDeclaration {
                        is_mutable,
                        identifier: identifier.name,
                        initializer: Some(Box::new(expr)),
                    }),
                ],
            }),
        }],
        middle_end::ir::StatementKind::FunctionDefinition {
            name,
            parameters,
            return_type: _,
            body,
        } => vec![BlockOrStatement::Statement(Statement::FunctionDefinition {
            name: name.name,
            parameters: parameters.into_iter().map(|p| p.identifier.name).collect(),
            body: body
                .into_iter()
                .map(|stmt| {
                    middle_statement_to_block_or_statement(ctx, stmt)
                        .into_iter()
                        .map(|b| b.to_statements())
                        .flatten()
                })
                .flatten()
                .collect(),
        })],
        middle_end::ir::StatementKind::Expression(expression) => {
            let (expr, maybe_block) = expression_to_block(ctx, expression);
            if let Some(block) = maybe_block {
                vec![
                    BlockOrStatement::Block(block),
                    BlockOrStatement::Statement(Statement::Expression(expr)),
                ]
            } else {
                vec![BlockOrStatement::Statement(Statement::Expression(expr))]
            }
        }
        middle_end::ir::StatementKind::Return(maybe_expr) => match maybe_expr {
            Some(expression) => {
                let (expr, maybe_block) = expression_to_block(ctx, expression);
                if let Some(block) = maybe_block {
                    vec![
                        BlockOrStatement::Block(block),
                        BlockOrStatement::Statement(Statement::Return(Some(expr))),
                    ]
                } else {
                    vec![BlockOrStatement::Statement(Statement::Return(Some(expr)))]
                }
            }
            None => vec![BlockOrStatement::Statement(Statement::Return(None))],
        },
    }
}

fn expression_to_block(
    ctx: &mut Context,
    expression: middle_end::ir::Expression,
) -> (Expression, Option<BasicBlock>) {
    match expression.kind {
        middle_end::ir::ExpressionKind::Boolean(value) => (Expression::Boolean(value), None),
        middle_end::ir::ExpressionKind::Integer(value) => (Expression::Integer(value), None),
        middle_end::ir::ExpressionKind::String(value) => (Expression::String(value), None),
        middle_end::ir::ExpressionKind::Identifier(identifier) => {
            (Expression::Identifier(identifier.name), None)
        }
        middle_end::ir::ExpressionKind::Block(block) => {
            ctx.begin_scope();
            let temp_variable = ctx.new_temp_identifier();
            let mut basic_block = BasicBlock::new();
            basic_block
                .block_or_statements
                .push(BlockOrStatement::Statement(
                    Statement::VariableDeclaration {
                        is_mutable: true,
                        identifier: temp_variable.clone(),
                        initializer: None,
                    },
                ));
            for stmt in block.statements {
                basic_block
                    .block_or_statements
                    .append(&mut middle_statement_to_block_or_statement(ctx, stmt));
            }
            if let Some(return_expr) = block.return_expression {
                let return_expr = expression_to_block(ctx, return_expr);
                if let Some(nested_block) = return_expr.1 {
                    basic_block
                        .block_or_statements
                        .push(BlockOrStatement::Block(nested_block));
                }
                basic_block
                    .block_or_statements
                    .push(BlockOrStatement::Statement(Statement::Expression(
                        Expression::BinaryExpression(
                            Box::new(Expression::Identifier(temp_variable.clone())),
                            BinaryOperator::Assignment,
                            Box::new(return_expr.0),
                        ),
                    )));
            }
            ctx.end_scope();
            (Expression::Identifier(temp_variable), Some(basic_block))
        }
        middle_end::ir::ExpressionKind::JsBlock(_, expressions) => {
            let mut converted_expressions: Vec<Expression> = Vec::new();
            let mut blocks: Vec<BlockOrStatement> = Vec::new();
            for (expr, maybe_block) in expressions
                .into_iter()
                .map(|expr| expression_to_block(ctx, expr))
            {
                converted_expressions.push(expr);
                if let Some(block) = maybe_block {
                    blocks.push(BlockOrStatement::Block(block));
                }
            }

            if blocks.is_empty() {
                (Expression::JsBlock(converted_expressions), None)
            } else {
                let mut basic_block = BasicBlock::new();
                basic_block.block_or_statements.append(&mut blocks);
                (
                    Expression::JsBlock(converted_expressions),
                    Some(basic_block),
                )
            }
        }
        middle_end::ir::ExpressionKind::BinaryExpression(left, op, right) => {
            let (left, maybe_block_lhs) = expression_to_block(ctx, *left);
            let (right, maybe_block_rhs) = expression_to_block(ctx, *right);
            (
                Expression::BinaryExpression(Box::new(left), op, Box::new(right)),
                match (maybe_block_lhs, maybe_block_rhs) {
                    (None, None) => None,
                    (Some(left_block), Some(right_block)) => {
                        let mut basic_block = BasicBlock::new();
                        basic_block
                            .block_or_statements
                            .push(BlockOrStatement::Block(right_block));
                        basic_block
                            .block_or_statements
                            .push(BlockOrStatement::Block(left_block));
                        Some(basic_block)
                    }
                    (Some(block), _) | (_, Some(block)) => Some(block),
                },
            )
        }
        middle_end::ir::ExpressionKind::PropertyAccess(left, right) => {
            let (left, maybe_block) = expression_to_block(ctx, *left);
            (
                Expression::PropertyAccess(Box::new(left), right.name),
                maybe_block,
            )
        }
        middle_end::ir::ExpressionKind::FunctionCall { callee, arguments } => {
            let mut args: Vec<Expression> = Vec::new();
            let mut blocks: Vec<BlockOrStatement> = Vec::new();
            for (expr, maybe_block) in arguments
                .into_iter()
                .map(|expr| expression_to_block(ctx, expr))
            {
                args.push(expr);
                if let Some(block) = maybe_block {
                    blocks.push(BlockOrStatement::Block(block));
                }
            }

            let (callee, maybe_block) = expression_to_block(ctx, *callee);
            if let Some(block) = maybe_block {
                blocks.push(BlockOrStatement::Block(block));
            }

            if blocks.is_empty() {
                (
                    Expression::FunctionCall {
                        callee: Box::new(callee),
                        arguments: args,
                    },
                    None,
                )
            } else {
                let mut basic_block = BasicBlock::new();
                basic_block.block_or_statements.append(&mut blocks);
                (
                    Expression::FunctionCall {
                        callee: Box::new(callee),
                        arguments: args,
                    },
                    Some(basic_block),
                )
            }
        }
        middle_end::ir::ExpressionKind::If {
            condition,
            body,
            else_,
        } => {
            let condition = expression_to_block(ctx, *condition);
            let body = expression_to_block(ctx, *body);
            let else_ = else_.map(|e| expression_to_block(ctx, *e));
            ctx.begin_scope();
            let temp_variable = ctx.new_temp_identifier();
            let mut basic_block = BasicBlock::new();
            basic_block
                .block_or_statements
                .push(BlockOrStatement::Statement(
                    Statement::VariableDeclaration {
                        is_mutable: true,
                        identifier: temp_variable.clone(),
                        initializer: None,
                    },
                ));
            if let Some(block) = condition.1 {
                basic_block
                    .block_or_statements
                    .push(BlockOrStatement::Block(block));
            }

            let mut body_statements = body.1.map(block_to_statements).unwrap_or(Vec::new());
            body_statements.push(Statement::Expression(Expression::BinaryExpression(
                Box::new(Expression::Identifier(temp_variable.clone())),
                BinaryOperator::Assignment,
                Box::new(body.0),
            )));

            let else_statements = else_
                .map(|(expr, maybe_block)| {
                    let mut else_statements =
                        maybe_block.map(block_to_statements).unwrap_or(Vec::new());
                    else_statements.push(Statement::Expression(Expression::BinaryExpression(
                        Box::new(Expression::Identifier(temp_variable.clone())),
                        BinaryOperator::Assignment,
                        Box::new(expr),
                    )));
                    else_statements
                })
                .unwrap_or(Vec::new());
            basic_block
                .block_or_statements
                .push(BlockOrStatement::Statement(Statement::If {
                    condition: condition.0,
                    body: body_statements,
                    else_: else_statements,
                }));
            ctx.end_scope();
            (Expression::Identifier(temp_variable), Some(basic_block))
        }
    }
}

pub struct BasicBlock {
    block_or_statements: Vec<BlockOrStatement>,
}
impl BasicBlock {
    fn new() -> Self {
        BasicBlock {
            block_or_statements: Vec::new(),
        }
    }
}

enum BlockOrStatement {
    Block(BasicBlock),
    Statement(Statement),
}
impl BlockOrStatement {
    fn to_statements(self) -> Vec<Statement> {
        match self {
            BlockOrStatement::Block(block) => block_to_statements(block),
            BlockOrStatement::Statement(statement) => vec![statement],
        }
    }
}

pub enum Statement {
    VariableDeclaration {
        is_mutable: bool,
        identifier: String,
        initializer: Option<Box<Expression>>,
    },
    FunctionDefinition {
        name: String,
        parameters: Vec<String>,
        body: Vec<Statement>,
    },
    Return(Option<Expression>),
    Expression(Expression),
    If {
        condition: Expression,
        body: Vec<Statement>,
        else_: Vec<Statement>,
    },
}

pub enum Expression {
    Boolean(bool),
    Identifier(String),
    Integer(i64),
    String(String),
    JsBlock(Vec<Expression>),
    BinaryExpression(Box<Expression>, BinaryOperator, Box<Expression>),
    PropertyAccess(Box<Expression>, String),
    FunctionCall {
        callee: Box<Expression>,
        arguments: Vec<Expression>,
    },
}

struct Context {
    id_counters: Vec<IdCounter>,
}
impl Context {
    fn new() -> Self {
        Self {
            id_counters: vec![IdCounter::new()],
        }
    }

    fn begin_scope(&mut self) {
        let start_id = self.id_counters.last_mut().unwrap().next();
        self.id_counters
            .push(IdCounter::new_with_start_id(start_id));
    }

    fn end_scope(&mut self) {
        self.id_counters.pop().unwrap();
    }

    fn new_temp_identifier(&mut self) -> String {
        format!("$tmp{}", self.id_counters.last_mut().unwrap().next())
    }
}

struct IdCounter {
    next_id: u32,
}
impl IdCounter {
    fn new() -> Self {
        IdCounter { next_id: 0 }
    }
    fn new_with_start_id(id: u32) -> Self {
        IdCounter { next_id: id }
    }
    fn next(&mut self) -> u32 {
        let id = self.next_id;
        self.next_id += 1;
        id
    }
}