use crate::{
    phases::{
        frontend, middle_end,
        shared::{BinOp, PrefixUnaryOp},
    },
    types::environment::EnvironmentType,
};

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
            type_: _,
            identifier,
            initializer,
        } => match expression_to_block(ctx, *initializer) {
            (expr, None) => BlockOrStatement::Statement(Statement::VariableDeclaration {
                environment: ctx.environment(),
                is_mutable,
                identifier: identifier.name,
                initializer: Some(Box::new(expr)),
            }),
            (expr, Some(block)) => BlockOrStatement::Block(BasicBlock {
                block_or_statements: vec![
                    BlockOrStatement::Block(block),
                    BlockOrStatement::Statement(Statement::VariableDeclaration {
                        environment: ctx.environment(),
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
            environment: ctx.environment(),
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
        middle_end::ir::TopLevelStatementKind::Loop(statements) => {
            BlockOrStatement::Statement(Statement::WhileLoop(
                ctx.environment(),
                statements
                    .into_iter()
                    .map(|stmt| {
                        middle_statement_to_block_or_statement(ctx, stmt)
                            .into_iter()
                            .map(|b| b.to_statements())
                            .flatten()
                    })
                    .flatten()
                    .collect(),
            ))
        }
        middle_end::ir::TopLevelStatementKind::ForLoop {
            initializer,
            condition,
            post_loop,
            body,
        } => {
            let mut block = BasicBlock::new();
            if let Some(initializer) = initializer {
                block
                    .block_or_statements
                    .append(&mut middle_statement_to_block_or_statement(
                        ctx,
                        initializer,
                    ));
            }

            let mut loop_statements: Vec<Statement> = Vec::new();
            // Generate the break condition
            if let Some(condition) = condition {
                let (condition, maybe_block) = expression_to_block(ctx, condition);
                if let Some(block) = maybe_block {
                    loop_statements.append(&mut block_to_statements(block));
                }
                loop_statements.push(Statement::If {
                    environment: ctx.environment(),
                    condition: Expression::new_binary_expression(
                        ctx,
                        Box::new(condition),
                        BinOp::Eq,
                        Box::new(Expression::new_boolean(ctx, false)),
                    ),
                    body: vec![Statement::Break(ctx.environment())],
                    else_: Vec::new(),
                });
            }

            // Add in the body of the loop
            loop_statements.append(
                &mut body
                    .into_iter()
                    .map(|stmt| {
                        middle_statement_to_block_or_statement(ctx, stmt)
                            .into_iter()
                            .map(|b| b.to_statements())
                            .flatten()
                    })
                    .flatten()
                    .collect(),
            );

            // Add in the post-loop
            if let Some(post_loop) = post_loop {
                let (post_loop, maybe_block) = expression_to_block(ctx, post_loop);
                if let Some(block) = maybe_block {
                    loop_statements.append(&mut block_to_statements(block));
                }
                loop_statements.push(Statement::Expression(post_loop));
            }

            let loop_ = Statement::WhileLoop(ctx.environment(), loop_statements);
            block
                .block_or_statements
                .push(BlockOrStatement::Statement(loop_));

            BlockOrStatement::Block(block)
        }
        middle_end::ir::TopLevelStatementKind::EnvironmentBlock(environment, statements) => {
            ctx.start_environment(match environment {
                frontend::ir::EnvironmentType::Frontend => EnvironmentType::Frontend,
                frontend::ir::EnvironmentType::Backend => EnvironmentType::Backend,
            });

            let mut block_or_statements: Vec<BlockOrStatement> = Vec::new();
            for stmt in statements {
                block_or_statements.append(&mut middle_statement_to_block_or_statement(ctx, stmt));
            }
            ctx.end_environment();
            BlockOrStatement::Block(BasicBlock {
                block_or_statements,
            })
        }
    }
}

fn middle_statement_to_block_or_statement(
    ctx: &mut Context,
    statement: middle_end::ir::Statement,
) -> Vec<BlockOrStatement> {
    match statement.kind {
        middle_end::ir::StatementKind::VariableDeclaration {
            type_: _,
            is_mutable,
            identifier,
            initializer,
        } => vec![match expression_to_block(ctx, *initializer) {
            (expr, None) => BlockOrStatement::Statement(Statement::VariableDeclaration {
                environment: ctx.environment(),
                is_mutable,
                identifier: identifier.name,
                initializer: Some(Box::new(expr)),
            }),
            (expr, Some(block)) => BlockOrStatement::Block(BasicBlock {
                block_or_statements: vec![
                    BlockOrStatement::Block(block),
                    BlockOrStatement::Statement(Statement::VariableDeclaration {
                        environment: ctx.environment(),
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
            environment: ctx.environment(),
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
                        BlockOrStatement::Statement(Statement::Return(
                            ctx.environment(),
                            Some(expr),
                        )),
                    ]
                } else {
                    vec![BlockOrStatement::Statement(Statement::Return(
                        ctx.environment(),
                        Some(expr),
                    ))]
                }
            }
            None => vec![BlockOrStatement::Statement(Statement::Return(
                ctx.environment(),
                None,
            ))],
        },
        middle_end::ir::StatementKind::Loop(statements) => {
            vec![BlockOrStatement::Statement(Statement::WhileLoop(
                ctx.environment(),
                statements
                    .into_iter()
                    .map(|stmt| {
                        middle_statement_to_block_or_statement(ctx, stmt)
                            .into_iter()
                            .map(|b| b.to_statements())
                            .flatten()
                    })
                    .flatten()
                    .collect(),
            ))]
        }
        middle_end::ir::StatementKind::Break => vec![BlockOrStatement::Statement(
            Statement::Break(ctx.environment()),
        )],
        middle_end::ir::StatementKind::ForLoop {
            initializer,
            condition,
            post_loop,
            body,
        } => {
            let mut block = BasicBlock::new();
            if let Some(initializer) = initializer {
                block
                    .block_or_statements
                    .append(&mut middle_statement_to_block_or_statement(
                        ctx,
                        *initializer,
                    ));
            }

            let mut loop_statements: Vec<Statement> = Vec::new();
            // Generate the break condition
            if let Some(condition) = condition {
                let (condition, maybe_block) = expression_to_block(ctx, *condition);
                if let Some(block) = maybe_block {
                    loop_statements.append(&mut block_to_statements(block));
                }
                loop_statements.push(Statement::If {
                    environment: ctx.environment(),
                    condition: Expression::new_binary_expression(
                        ctx,
                        Box::new(condition),
                        BinOp::Eq,
                        Box::new(Expression::new_boolean(ctx, false)),
                    ),
                    body: vec![Statement::Break(ctx.environment())],
                    else_: Vec::new(),
                });
            }

            // Add in the body of the loop
            loop_statements.append(
                &mut body
                    .into_iter()
                    .map(|stmt| {
                        middle_statement_to_block_or_statement(ctx, stmt)
                            .into_iter()
                            .map(|b| b.to_statements())
                            .flatten()
                    })
                    .flatten()
                    .collect(),
            );

            // Add in the post-loop
            if let Some(post_loop) = post_loop {
                let (post_loop, maybe_block) = expression_to_block(ctx, *post_loop);
                if let Some(block) = maybe_block {
                    loop_statements.append(&mut block_to_statements(block));
                }
                loop_statements.push(Statement::Expression(post_loop));
            }

            let loop_ = Statement::WhileLoop(ctx.environment(), loop_statements);
            block
                .block_or_statements
                .push(BlockOrStatement::Statement(loop_));

            vec![BlockOrStatement::Block(block)]
        }
    }
}

fn expression_to_block(
    ctx: &mut Context,
    expression: middle_end::ir::Expression,
) -> (Expression, Option<BasicBlock>) {
    match expression.kind {
        middle_end::ir::ExpressionKind::Parenthesized(expr) => {
            let (expr, maybe_block) = expression_to_block(ctx, *expr);
            (
                Expression::new_parenthesized(ctx, Box::new(expr)),
                maybe_block,
            )
        }
        middle_end::ir::ExpressionKind::Boolean(value) => {
            (Expression::new_boolean(ctx, value), None)
        }
        middle_end::ir::ExpressionKind::Integer(value) => {
            (Expression::new_integer(ctx, value), None)
        }
        middle_end::ir::ExpressionKind::String(value) => (Expression::new_string(ctx, value), None),
        middle_end::ir::ExpressionKind::Identifier(identifier) => {
            (Expression::new_identifier(ctx, identifier.name), None)
        }
        middle_end::ir::ExpressionKind::Block(block) => {
            ctx.begin_scope();
            let temp_variable = ctx.new_temp_identifier();
            let mut basic_block = BasicBlock::new();
            basic_block
                .block_or_statements
                .push(BlockOrStatement::Statement(
                    Statement::VariableDeclaration {
                        environment: ctx.environment(),
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
                        Expression::new_binary_expression(
                            ctx,
                            Box::new(Expression::new_identifier(ctx, temp_variable.clone())),
                            BinOp::Assign,
                            Box::new(return_expr.0),
                        ),
                    )));
            }
            ctx.end_scope();
            (
                Expression::new_identifier(ctx, temp_variable),
                Some(basic_block),
            )
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
                (Expression::new_js_block(ctx, converted_expressions), None)
            } else {
                let mut basic_block = BasicBlock::new();
                basic_block.block_or_statements.append(&mut blocks);
                (
                    Expression::new_js_block(ctx, converted_expressions),
                    Some(basic_block),
                )
            }
        }
        middle_end::ir::ExpressionKind::BinaryExpression(left, op, right) => {
            let (left, maybe_block_lhs) = expression_to_block(ctx, *left);
            let (right, maybe_block_rhs) = expression_to_block(ctx, *right);
            (
                Expression::new_binary_expression(ctx, Box::new(left), op, Box::new(right)),
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
        middle_end::ir::ExpressionKind::PreUnaryExpression(op, expr) => {
            let (expr, maybe_block) = expression_to_block(ctx, *expr);
            (
                Expression::new_pre_unary_expression(ctx, op, Box::new(expr)),
                maybe_block,
            )
        }
        middle_end::ir::ExpressionKind::PropertyAccess(left, right) => {
            let (left, maybe_block) = expression_to_block(ctx, *left);
            (
                Expression::new_property_access(ctx, Box::new(left), right.name),
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
                    Expression::new_function_call(ctx, Box::new(callee), args),
                    None,
                )
            } else {
                let mut basic_block = BasicBlock::new();
                basic_block.block_or_statements.append(&mut blocks);
                (
                    Expression::new_function_call(ctx, Box::new(callee), args),
                    Some(basic_block),
                )
            }
        }
        middle_end::ir::ExpressionKind::If {
            condition,
            body,
            else_,
        } => {
            let temp_variable = ctx.new_temp_identifier();
            let condition = expression_to_block(ctx, *condition);
            let body = expression_to_block(ctx, *body);
            let else_ = else_.map(|e| expression_to_block(ctx, *e));
            let mut basic_block = BasicBlock::new();
            basic_block
                .block_or_statements
                .push(BlockOrStatement::Statement(
                    Statement::VariableDeclaration {
                        environment: ctx.environment(),
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
            body_statements.push(Statement::Expression(Expression::new_binary_expression(
                ctx,
                Box::new(Expression::new_identifier(ctx, temp_variable.clone())),
                BinOp::Assign,
                Box::new(body.0),
            )));

            let else_statements = else_
                .map(|(expr, maybe_block)| {
                    let mut else_statements =
                        maybe_block.map(block_to_statements).unwrap_or(Vec::new());
                    else_statements.push(Statement::Expression(Expression::new_binary_expression(
                        ctx,
                        Box::new(Expression::new_identifier(ctx, temp_variable.clone())),
                        BinOp::Assign,
                        Box::new(expr),
                    )));
                    else_statements
                })
                .unwrap_or(Vec::new());
            basic_block
                .block_or_statements
                .push(BlockOrStatement::Statement(Statement::If {
                    environment: ctx.environment(),
                    condition: condition.0,
                    body: body_statements,
                    else_: else_statements,
                }));
            (
                Expression::new_identifier(ctx, temp_variable),
                Some(basic_block),
            )
        }
        middle_end::ir::ExpressionKind::List(expressions) => {
            let mut inner_blocks: Vec<BasicBlock> = Vec::new();
            let mut initial_values: Vec<Expression> = Vec::new();
            for expr in expressions {
                let (expr, maybe_block) = expression_to_block(ctx, expr);
                initial_values.push(expr);
                if let Some(block) = maybe_block {
                    inner_blocks.push(block);
                }
            }

            if inner_blocks.is_empty() {
                (Expression::new_list(ctx, initial_values), None)
            } else {
                (
                    Expression::new_list(ctx, initial_values),
                    Some(BasicBlock {
                        block_or_statements: inner_blocks
                            .into_iter()
                            .map(BlockOrStatement::Block)
                            .collect(),
                    }),
                )
            }
        }
        middle_end::ir::ExpressionKind::ArrayAccess(left, index) => {
            let (left, maybe_left_block) = expression_to_block(ctx, *left);
            let (index, maybe_index_block) = expression_to_block(ctx, *index);

            let maybe_block = match (maybe_left_block, maybe_index_block) {
                (None, None) => None,
                (None, Some(block)) | (Some(block), None) => Some(block),
                (Some(left_block), Some(index_block)) => Some(BasicBlock {
                    block_or_statements: vec![
                        BlockOrStatement::Block(left_block),
                        BlockOrStatement::Block(index_block),
                    ],
                }),
            };
            (
                Expression::new_array_access(ctx, Box::new(left), Box::new(index)),
                maybe_block,
            )
        }
    }
}

#[derive(Debug)]
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

#[derive(Debug)]
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

#[derive(Debug)]
pub enum Statement {
    VariableDeclaration {
        environment: EnvironmentType,
        is_mutable: bool,
        identifier: String,
        initializer: Option<Box<Expression>>,
    },
    FunctionDefinition {
        environment: EnvironmentType,
        name: String,
        parameters: Vec<String>,
        body: Vec<Statement>,
    },
    Return(EnvironmentType, Option<Expression>),
    Expression(Expression),
    If {
        environment: EnvironmentType,
        condition: Expression,
        body: Vec<Statement>,
        else_: Vec<Statement>,
    },
    WhileLoop(EnvironmentType, Vec<Statement>),
    Break(EnvironmentType),
}
impl Statement {
    pub fn environment(&self) -> EnvironmentType {
        match self {
            Statement::VariableDeclaration { environment, .. } => *environment,
            Statement::FunctionDefinition { environment, .. } => *environment,
            Statement::Return(environment, _) => *environment,
            Statement::Expression(expr) => expr.environment,
            Statement::If { environment, .. } => *environment,
            Statement::WhileLoop(environment, _) => *environment,
            Statement::Break(environment) => *environment,
        }
    }
}

#[derive(Debug)]
pub struct Expression {
    pub environment: EnvironmentType,
    pub kind: ExpressionKind,
}
impl Expression {
    fn new_boolean(ctx: &Context, value: bool) -> Self {
        Self {
            environment: ctx.environment(),
            kind: ExpressionKind::Boolean(value),
        }
    }

    fn new_identifier(ctx: &Context, ident: String) -> Self {
        Self {
            environment: ctx.environment(),
            kind: ExpressionKind::Identifier(ident),
        }
    }

    fn new_integer(ctx: &Context, value: i32) -> Self {
        Self {
            environment: ctx.environment(),
            kind: ExpressionKind::Integer(value),
        }
    }

    fn new_string(ctx: &Context, value: String) -> Self {
        Self {
            environment: ctx.environment(),
            kind: ExpressionKind::String(value),
        }
    }
    fn new_parenthesized(ctx: &Context, expression: Box<Expression>) -> Self {
        Self {
            environment: ctx.environment(),
            kind: ExpressionKind::Parenthesized(expression),
        }
    }
    fn new_list(ctx: &Context, elements: Vec<Expression>) -> Self {
        Self {
            environment: ctx.environment(),
            kind: ExpressionKind::List(elements),
        }
    }
    fn new_js_block(ctx: &Context, expressions: Vec<Expression>) -> Self {
        Self {
            environment: ctx.environment(),
            kind: ExpressionKind::JsBlock(expressions),
        }
    }
    fn new_binary_expression(
        ctx: &Context,
        left: Box<Expression>,
        operator: BinOp,
        right: Box<Expression>,
    ) -> Self {
        Self {
            environment: ctx.environment(),
            kind: ExpressionKind::BinaryExpression(left, operator, right),
        }
    }
    fn new_pre_unary_expression(
        ctx: &Context,
        operator: PrefixUnaryOp,
        expr: Box<Expression>,
    ) -> Self {
        Self {
            environment: ctx.environment(),
            kind: ExpressionKind::PreUnaryExpression(operator, expr),
        }
    }
    fn new_property_access(ctx: &Context, left: Box<Expression>, right: String) -> Self {
        Self {
            environment: ctx.environment(),
            kind: ExpressionKind::PropertyAccess(left, right),
        }
    }
    fn new_array_access(ctx: &Context, array: Box<Expression>, index: Box<Expression>) -> Self {
        Self {
            environment: ctx.environment(),
            kind: ExpressionKind::ArrayAccess(array, index),
        }
    }
    fn new_function_call(
        ctx: &Context,
        callee: Box<Expression>,
        arguments: Vec<Expression>,
    ) -> Self {
        Self {
            environment: ctx.environment(),
            kind: ExpressionKind::FunctionCall { callee, arguments },
        }
    }
}

#[derive(Debug)]
pub enum ExpressionKind {
    Boolean(bool),
    Identifier(String),
    Integer(i32),
    String(String),
    Parenthesized(Box<Expression>),
    List(Vec<Expression>),
    JsBlock(Vec<Expression>),
    BinaryExpression(Box<Expression>, BinOp, Box<Expression>),
    PreUnaryExpression(PrefixUnaryOp, Box<Expression>),
    PropertyAccess(Box<Expression>, String),
    ArrayAccess(Box<Expression>, Box<Expression>),
    FunctionCall {
        callee: Box<Expression>,
        arguments: Vec<Expression>,
    },
}

struct Context {
    id_counters: Vec<IdCounter>,
    environments: Vec<EnvironmentType>,
}
impl Context {
    fn new() -> Self {
        Self {
            id_counters: vec![IdCounter::new()],
            environments: vec![EnvironmentType::Isomorphic],
        }
    }

    fn begin_scope(&mut self) {
        let start_id = self.id_counters.last_mut().unwrap().next_id;
        self.id_counters
            .push(IdCounter::new_with_start_id(start_id));
    }

    fn end_scope(&mut self) {
        self.id_counters
            .pop()
            .expect("Attempted to end scope but was not in a scope");
    }

    fn new_temp_identifier(&mut self) -> String {
        format!("$tmp{}", self.id_counters.last_mut().unwrap().next())
    }

    fn start_environment(&mut self, environment: EnvironmentType) {
        self.environments.push(environment);
    }

    fn end_environment(&mut self) {
        self.environments
            .pop()
            .expect("Attempted to end an environment but was not in an environment");
    }

    fn environment(&self) -> EnvironmentType {
        *self
            .environments
            .last()
            .expect("Attempted to access current environment but no environment available")
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
