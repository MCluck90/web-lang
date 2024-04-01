use std::collections::HashMap;

use crate::{
    data_structures::index_vec::{Idx, IndexVec},
    errors::CompilerError,
    phases::{
        frontend::{self, ir::Block},
        shared::{BinOp, PrefixUnaryOp, Span, Type},
    },
    types::environment::EnvironmentType,
};

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub struct Local(usize);
impl Idx for Local {
    fn new(idx: usize) -> Self {
        Local(idx)
    }

    fn index(self) -> usize {
        self.0
    }

    fn next(&self) -> Self {
        Local::new(self.0 + 1)
    }
}

impl Into<Place> for Local {
    fn into(self) -> Place {
        Place {
            local: self,
            projection: Vec::new(),
        }
    }
}

#[derive(Debug)]
pub struct LocalDecl {
    pub span: Span,
    pub mutability: Mutability,
    pub type_: Option<Type>,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Mutability {
    Immutable,
    Mutable,
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub struct BasicBlockId(usize);
impl Idx for BasicBlockId {
    fn new(idx: usize) -> Self {
        BasicBlockId(idx)
    }

    fn index(self) -> usize {
        self.0
    }

    fn next(&self) -> Self {
        BasicBlockId(self.0 + 1)
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Place {
    pub local: Local,
    pub projection: Vec<PlaceElem>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum PlaceElem {
    Field(String),
    Index(Local),
    ConstantIndex(i32),
}

#[derive(Clone, Debug)]
pub enum Operand {
    Reference(Place),
    Constant(ConstOperand),
    List(Vec<Operand>),
}
impl Operand {
    fn to_type(&self) -> Option<Type> {
        match self {
            Operand::Constant(const_op) => Some(const_op.to_type()),
            Operand::Reference(_) => None,
            Operand::List(ops) => {
                let mut ops_type = None;

                for op in ops {
                    match op.to_type() {
                        None => return None,
                        Some(t) => {
                            if ops_type.is_none() {
                                ops_type = Some(t);
                            } else if Some(t) != ops_type {
                                return None;
                            }
                        }
                    }
                }

                ops_type.map(|t| Type::List(Box::new(t)))
            }
        }
    }
}

#[derive(Clone, Debug)]
pub enum ConstOperand {
    Void,
    Int(i32),
    String(String),
    Bool(bool),
}
impl ConstOperand {
    fn to_type(&self) -> Type {
        match self {
            ConstOperand::Void => Type::Void,
            ConstOperand::Bool(_) => Type::Bool,
            ConstOperand::Int(_) => Type::Int,
            ConstOperand::String(_) => Type::String,
        }
    }
}

#[derive(Clone, Debug)]
pub enum RValue {
    Use(Operand),
    BinaryOp(Operand, BinOp, Operand),
    UnaryOp(PrefixUnaryOp, Operand),
}

impl RValue {
    fn to_type(&self) -> Option<Type> {
        match self {
            RValue::Use(op) => op.to_type(),
            RValue::BinaryOp(lhs, op, rhs) => match op {
                BinOp::Add
                | BinOp::Div
                | BinOp::Ge
                | BinOp::Gt
                | BinOp::Le
                | BinOp::Lt
                | BinOp::Mod
                | BinOp::Mul
                | BinOp::Sub => Some(Type::Int),
                BinOp::And | BinOp::Or => Some(Type::Bool),
                BinOp::Assign | BinOp::Ne | BinOp::Eq => match (lhs.to_type(), rhs.to_type()) {
                    (Some(lhs), Some(rhs)) => {
                        if lhs == rhs {
                            Some(lhs)
                        } else {
                            None
                        }
                    }
                    _ => None,
                },
            },
            RValue::UnaryOp(op, operand) => {
                let operand_type = operand.to_type();
                if let Some(operand_type) = operand.to_type() {
                    match (op, operand_type) {
                        (PrefixUnaryOp::Not, Type::Bool) => Some(Type::Bool),
                        (PrefixUnaryOp::Inc | PrefixUnaryOp::Dec, Type::Int) => Some(Type::Int),
                        _ => None,
                    }
                } else {
                    Some(match op {
                        PrefixUnaryOp::Not => Type::Bool,
                        PrefixUnaryOp::Inc | PrefixUnaryOp::Dec => Type::Int,
                    })
                }
            }
        }
    }

    fn to_place(&self) -> Option<Place> {
        match self {
            RValue::Use(Operand::Reference(place)) => Some(place.clone()),
            _ => None,
        }
    }
}

#[derive(Debug)]
pub struct Statement {
    pub span: Span,
    pub kind: StatementKind,
}

#[derive(Debug)]
pub enum StatementKind {
    Assign(Place, RValue),
    Eval(RValue), // Evaluates an expression without saving the result anywhere
}

#[derive(Clone, Debug)]
pub enum Terminator {
    Goto(BasicBlockId),
    Conditional {
        condition: RValue,
        true_branch: BasicBlockId,
        else_branch: Option<BasicBlockId>,
    },
    EndOfModule,
}

#[derive(Debug)]
pub struct BasicBlock {
    env: EnvironmentType,
    stmts: Vec<Statement>,
    terminator: Terminator,
}

impl Default for BasicBlock {
    fn default() -> Self {
        Self {
            env: EnvironmentType::Isomorphic,
            stmts: Vec::new(),
            terminator: Terminator::EndOfModule,
        }
    }
}

pub struct Module {
    pub path: String,
    pub errors: Vec<CompilerError>,
    pub exports: HashMap<String, Local>,
    pub locals: IndexVec<Local, LocalDecl>,
    pub basic_blocks: IndexVec<BasicBlockId, BasicBlock>,
}

impl Module {
    pub fn from_frontend_module(fe_module: frontend::ir::Module) -> Module {
        ModuleBuilder::from_frontend_module(fe_module)
    }
}

#[derive(Default)]
struct Scope {
    values: HashMap<String, Local>,
}

impl Scope {
    fn insert(&mut self, k: String, v: Local) {
        self.values.insert(k, v);
    }
}

struct ModuleBuilder {
    module: Module,
    active_block: BasicBlockId,
    scopes: Vec<Scope>,
}

impl ModuleBuilder {
    fn from_frontend_module(fe_module: frontend::ir::Module) -> Module {
        let mut builder = ModuleBuilder {
            module: Module {
                path: fe_module.path,
                errors: fe_module.errors,
                exports: HashMap::new(),
                locals: IndexVec::new(),
                basic_blocks: IndexVec::from_raw(vec![BasicBlock::default()]),
            },
            active_block: BasicBlockId(0),
            scopes: vec![Scope::default()],
        };

        for item in fe_module.ast.items {
            match item.kind {
                frontend::ir::ModuleItemKind::EnvironmentBlock(env_type, stmts) => {
                    builder.goto_new_block(Some(env_type.into()));
                    for stmt in stmts {
                        builder.convert_statement(stmt, None);
                    }
                    builder.goto_new_block(Some(EnvironmentType::Isomorphic));
                }
                frontend::ir::ModuleItemKind::Statement(stmt) => {
                    builder.convert_statement(stmt, None)
                }
            }
        }

        builder.module
    }

    fn convert_statement(
        &mut self,
        stmt: frontend::ir::Statement,
        next_block_id: Option<BasicBlockId>,
    ) {
        match stmt.kind {
            frontend::ir::StatementKind::VariableDeclaration {
                is_mutable,
                type_,
                identifier,
                initializer,
            } => {
                let rhs = self
                    .convert_expression(initializer, next_block_id)
                    .unwrap_or(RValue::Use(Operand::Constant(ConstOperand::Void)));

                let type_ = type_.or(self.get_r_value_type(&rhs));
                let local = self.add_to_scope(
                    stmt.span.clone(),
                    identifier.name,
                    if is_mutable {
                        Mutability::Mutable
                    } else {
                        Mutability::Immutable
                    },
                    type_,
                );
                self.append_statement(Statement {
                    span: stmt.span,
                    kind: StatementKind::Assign(local.into(), rhs),
                })
            }
            frontend::ir::StatementKind::FunctionDefinition {
                name,
                parameters,
                return_type,
                body,
            } => todo!(),
            frontend::ir::StatementKind::Expression(expr) => {
                let span = expr.span.clone();
                if let Some(r_value) = self.convert_expression(expr, None) {
                    self.append_statement(Statement {
                        span,
                        kind: StatementKind::Eval(r_value),
                    });
                }
            }
            frontend::ir::StatementKind::Return(_) => todo!(),
            frontend::ir::StatementKind::Loop(stmts) => {
                let loop_block_id = self.goto_new_block(None);
                self.set_block_terminator(Terminator::Goto(loop_block_id));
                let next_block_id = self.create_new_block(None);
                for stmt in stmts {
                    self.convert_statement(stmt, Some(next_block_id));
                }
                self.active_block = next_block_id;
            }
            frontend::ir::StatementKind::ForLoop {
                initializer,
                condition,
                post_loop,
                body,
            } => todo!(),
            frontend::ir::StatementKind::Break => match next_block_id {
                Some(next_block_id) => {
                    self.set_block_terminator(Terminator::Goto(next_block_id));
                }
                None => {
                    self.report_error(CompilerError::break_must_occur_within_loop(&stmt.span));
                }
            },
        }
    }

    fn convert_expression(
        &mut self,
        expr: frontend::ir::Expression,
        next_block_id: Option<BasicBlockId>,
    ) -> Option<RValue> {
        match expr.kind {
            frontend::ir::ExpressionKind::Boolean(b) => {
                Some(RValue::Use(Operand::Constant(ConstOperand::Bool(b))))
            }
            frontend::ir::ExpressionKind::Identifier(ident) => match self.find_local(&ident.name) {
                Some(local) => Some(RValue::Use(Operand::Reference(local.into()))),
                None => {
                    self.report_error(CompilerError::reference_error(&expr.span, &ident.name));
                    None
                }
            },
            frontend::ir::ExpressionKind::Integer(n) => {
                Some(RValue::Use(Operand::Constant(ConstOperand::Int(n))))
            }
            frontend::ir::ExpressionKind::String(s) => {
                Some(RValue::Use(Operand::Constant(ConstOperand::String(s))))
            }
            frontend::ir::ExpressionKind::Block(block) => {
                let local = if block.return_expression.is_some() {
                    Some(self.create_local(expr.span.clone(), Mutability::Immutable, None))
                } else {
                    None
                };
                self.goto_new_block(None);
                self.scopes.push(Scope::default());

                for stmt in block.statements {
                    self.convert_statement(stmt, None);
                }

                if let Some(return_expr) = block
                    .return_expression
                    .and_then(|return_expr| self.convert_expression(return_expr, next_block_id))
                {
                    let local = local.unwrap();
                    self.append_statement(Statement {
                        span: expr.span.clone(),
                        kind: StatementKind::Assign(local.into(), return_expr),
                    });
                    self.scopes.pop();
                    match next_block_id {
                        None => {
                            self.goto_new_block(None);
                        }
                        Some(next_block_id) => {
                            self.active_block = next_block_id;
                        }
                    }
                    Some(RValue::Use(Operand::Reference(local.into())))
                } else {
                    self.scopes.pop();
                    match next_block_id {
                        None => {
                            self.goto_new_block(None);
                        }
                        Some(next_block_id) => {
                            self.active_block = next_block_id;
                        }
                    }
                    None
                }
            }
            frontend::ir::ExpressionKind::List(items) => {
                let mut list_items = Vec::new();
                for item in items {
                    let span = item.span.clone();
                    let r_value = self.convert_expression(item, next_block_id)?;
                    list_items.push(self.r_value_to_operand(span, r_value));
                }
                Some(RValue::Use(Operand::List(list_items)))
            }
            frontend::ir::ExpressionKind::JsBlock(_, _) => todo!(),
            frontend::ir::ExpressionKind::If {
                condition,
                body,
                else_,
            } => todo!(),
            frontend::ir::ExpressionKind::Parenthesized(expr) => {
                self.convert_expression(*expr, next_block_id)
            }
            frontend::ir::ExpressionKind::BinaryOp(lhs, op, rhs) => {
                let lhs = self.convert_expression(*lhs, next_block_id)?;
                let rhs = self.convert_expression(*rhs, next_block_id)?;

                match op {
                    BinOp::Assign => match lhs.to_place() {
                        None => {
                            self.report_error(CompilerError::invalid_lhs_in_assignment(&expr.span));
                            None
                        }
                        Some(lhs_place) => {
                            let decl = self.get_local_decl(lhs_place.local);
                            match decl.mutability {
                                Mutability::Immutable => {
                                    self.report_error(
                                        CompilerError::assignment_to_immutable_variable(
                                            &expr.span,
                                            "TODO: Change this error message",
                                        ),
                                    );
                                    None
                                }
                                Mutability::Mutable => {
                                    self.append_statement(Statement {
                                        span: expr.span,
                                        kind: StatementKind::Assign(lhs_place, rhs),
                                    });
                                    Some(lhs)
                                }
                            }
                        }
                    },
                    BinOp::Add
                    | BinOp::Div
                    | BinOp::Ge
                    | BinOp::Gt
                    | BinOp::Le
                    | BinOp::Lt
                    | BinOp::Mod
                    | BinOp::Mul
                    | BinOp::Sub => {
                        match (self.get_r_value_type(&lhs), self.get_r_value_type(&rhs)) {
                            (Some(Type::Int), Some(Type::Int))
                            | (None, None)
                            | (Some(Type::Int), None)
                            | (None, Some(Type::Int)) => {
                                let lhs = self.r_value_to_operand(expr.span.clone(), lhs);
                                let rhs = self.r_value_to_operand(expr.span, rhs);
                                Some(RValue::BinaryOp(lhs, op, rhs))
                            }
                            _ => todo!(),
                        }
                    }
                    _ => todo!(),
                }
            }
            frontend::ir::ExpressionKind::PrefixUnaryOp(operator, operand) => {
                let operand = self.convert_expression(*operand, next_block_id)?;
                Some(RValue::UnaryOp(
                    operator,
                    self.r_value_to_operand(expr.span, operand),
                ))
            }
            frontend::ir::ExpressionKind::PropertyAccess(_, _) => todo!(),
            frontend::ir::ExpressionKind::ArrayAccess(_, _) => todo!(),
            frontend::ir::ExpressionKind::FunctionCall { callee, arguments } => todo!(),
        }
    }

    fn create_new_block(&mut self, env: Option<EnvironmentType>) -> BasicBlockId {
        self.module.basic_blocks.push(BasicBlock {
            env: env.unwrap_or(self.module.basic_blocks.get(self.active_block).unwrap().env),
            stmts: Vec::new(),
            terminator: Terminator::EndOfModule,
        })
    }

    fn goto_new_block(&mut self, env: Option<EnvironmentType>) -> BasicBlockId {
        let new_block_id = self.create_new_block(env);
        self.module
            .basic_blocks
            .get_mut(self.active_block)
            .unwrap()
            .terminator = Terminator::Goto(new_block_id);
        self.active_block = new_block_id;
        new_block_id
    }

    fn set_block_terminator(&mut self, term: Terminator) {
        self.module
            .basic_blocks
            .get_mut(self.active_block)
            .unwrap()
            .terminator = term;
    }

    fn create_local(&mut self, span: Span, mutability: Mutability, type_: Option<Type>) -> Local {
        self.module.locals.push(LocalDecl {
            span,
            mutability,
            type_,
        })
    }

    fn get_local_decl(&self, local: Local) -> &LocalDecl {
        self.module.locals.get(local).unwrap()
    }

    fn get_r_value_type(&self, r_value: &RValue) -> Option<Type> {
        match r_value {
            RValue::Use(Operand::Reference(place)) => {
                self.get_local_decl(place.local).type_.clone()
            }
            _ => r_value.to_type(),
        }
    }

    fn r_value_to_operand(&mut self, span: Span, r_value: RValue) -> Operand {
        let type_ = r_value.to_type();
        match r_value {
            RValue::Use(op) => op,
            RValue::BinaryOp(lhs, op, rhs) => {
                let local = self.create_local(span.clone(), Mutability::Immutable, type_);
                self.append_statement(Statement {
                    span,
                    kind: StatementKind::Assign(local.into(), RValue::BinaryOp(lhs, op, rhs)),
                });
                Operand::Reference(local.into())
            }
            _ => todo!(),
        }
    }

    fn add_to_scope(
        &mut self,
        span: Span,
        name: String,
        mutability: Mutability,
        type_: Option<Type>,
    ) -> Local {
        let local = self.create_local(span, mutability, type_);
        self.scopes.last_mut().unwrap().insert(name, local);
        local
    }

    fn append_statement(&mut self, stmt: Statement) {
        self.module
            .basic_blocks
            .get_mut(self.active_block)
            .unwrap()
            .stmts
            .push(stmt);
    }

    fn find_local(&self, name: &str) -> Option<Local> {
        for scope in self.scopes.iter().rev() {
            if let Some(id) = scope.values.get(name) {
                return Some(*id);
            }
        }
        None
    }

    fn report_error(&mut self, error: CompilerError) {
        self.module.errors.push(error);
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        data_structures::index_vec::Idx,
        errors::{CompilerError, CompilerErrorReason},
        phases::{
            frontend,
            shared::{BinOp, PrefixUnaryOp, Type},
        },
        types::environment::EnvironmentType,
    };

    use super::{BasicBlock, BasicBlockId, Local, LocalDecl, Module, Statement, Terminator};

    struct TestContext {
        module: Module,
        active_block: BasicBlockId,
        active_local: Local,
    }

    impl TestContext {
        fn assert_no_errors(&self) {
            if !self.module.errors.is_empty() {
                panic!("Expected no errors, found {:?}", self.module.errors);
            }
        }

        fn assert_num_of_errors(&self, count: usize) {
            assert_eq!(self.module.errors.len(), count);
        }

        fn assert_num_of_basic_blocks(&self, count: usize) {
            assert_eq!(self.module.basic_blocks.len(), count);
        }

        fn assert_num_of_locals(&self, count: usize) {
            assert_eq!(self.module.locals.len(), count);
        }

        fn get_active_block_mut(&mut self) -> &mut BasicBlock {
            match self.module.basic_blocks.get_mut(self.active_block) {
                None => panic!(
                    "Expected a basic block with ID {} but block does not exist",
                    self.active_block.0
                ),
                Some(block) => block,
            }
        }

        fn consume_statement(&mut self) -> Statement {
            let block = self.get_active_block_mut();
            if block.stmts.is_empty() {
                panic!(
                    "Expected to find a statement but basic block {} is empty",
                    self.active_block.0
                );
            }
            block.stmts.remove(0)
        }

        fn consume_error(&mut self) -> CompilerError {
            self.module.errors.remove(0)
        }

        fn consume_local(&mut self) -> (Local, LocalDecl) {
            let id = self.active_local;
            let decl = self.module.locals.remove(Local(0));
            self.active_local = id.next();
            (id, decl)
        }

        pub fn basic_block(&self) -> &BasicBlock {
            self.module.basic_blocks.get(self.active_block).unwrap()
        }

        pub fn follow_goto(&mut self) -> BasicBlockId {
            let terminator = self.basic_block().terminator.clone();
            match terminator {
                Terminator::Goto(next_block_id) => {
                    self.active_block = next_block_id;
                    next_block_id
                }
                term => panic!("Expected a goto, found {:?}", term),
            }
        }

        pub fn set_active_block(&mut self, id: BasicBlockId) {
            self.active_block = id;
        }

        pub fn current_env(&self) -> EnvironmentType {
            self.basic_block().env
        }
    }

    mod assert_statement {
        use crate::phases::middle_end::ir_v3::{Place, RValue, Statement, StatementKind};

        pub fn is_assign(stmt: Statement) -> (Place, RValue) {
            match stmt.kind {
                StatementKind::Assign(place, r_value) => (place, r_value),
                _ => panic!("Expected an assign, found {:?}", stmt.kind),
            }
        }

        pub fn is_eval(stmt: Statement) -> RValue {
            match stmt.kind {
                StatementKind::Eval(value) => value,
                _ => panic!("Expected an evaluate, found {:?}", stmt.kind),
            }
        }
    }

    mod assert_place {
        use crate::phases::{
            middle_end::ir_v3::{Mutability, Place},
            shared::Type,
        };

        use super::TestContext;

        pub fn is_immutable(ctx: &TestContext, place: &Place) {
            match ctx.module.locals.get(place.local) {
                Some(local) => {
                    assert_eq!(local.mutability, Mutability::Immutable);
                }
                None => panic!(
                    "Expected to find an immutable local via {:?} but no local was found",
                    place
                ),
            }
        }

        pub fn is_mutable(ctx: &TestContext, place: &Place) {
            match ctx.module.locals.get(place.local) {
                Some(local) => {
                    assert_eq!(local.mutability, Mutability::Mutable);
                }
                None => panic!(
                    "Expected to find an mutable local via {:?} but no local was found",
                    place
                ),
            }
        }

        pub fn has_type(ctx: &TestContext, place: &Place, type_: Type) {
            match ctx.module.locals.get(place.local) {
                Some(local) => {
                    assert_eq!(local.type_, Some(type_));
                }
                None => panic!(
                    "Expected to find an immutable local via {:?} but no local was found",
                    place
                ),
            }
        }
    }

    mod assert_r_value {
        use crate::phases::{
            middle_end::ir_v3::{ConstOperand, Operand, Place, RValue},
            shared::{BinOp, PrefixUnaryOp, Type},
        };

        pub fn is_use(r_value: RValue) -> Operand {
            match r_value {
                RValue::Use(op) => op,
                _ => panic!("Expected to find a use, found {:?}", r_value),
            }
        }

        pub fn is_binary_op(r_value: RValue) -> (Operand, BinOp, Operand) {
            match r_value {
                RValue::BinaryOp(lhs, op, rhs) => (lhs, op, rhs),
                _ => panic!("Expected a binary operation, found {:?}", r_value),
            }
        }

        pub fn is_unary_op(r_value: RValue) -> (PrefixUnaryOp, Operand) {
            match r_value {
                RValue::UnaryOp(op, operand) => (op, operand),
                _ => panic!("Expected a unary operation, found {:?}", r_value),
            }
        }

        pub fn is_reference(r_value: RValue) -> Place {
            match r_value {
                RValue::Use(Operand::Reference(place)) => place,
                _ => panic!("Expected a reference, found {:?}", r_value),
            }
        }

        pub fn is_constant(r_value: RValue) -> ConstOperand {
            match r_value {
                RValue::Use(Operand::Constant(c)) => c,
                _ => panic!("Expected a constant, found {:?}", r_value),
            }
        }

        pub fn has_type(r_value: &RValue, type_: Type) {
            assert_eq!(r_value.to_type(), Some(type_));
        }
    }

    mod assert_operand {
        use crate::phases::middle_end::ir_v3::{ConstOperand, Operand, Place};

        pub fn is_int_with_value(op: &Operand, n: i32) {
            match op {
                Operand::Constant(ConstOperand::Int(actual_n)) => assert_eq!(n, *actual_n),
                _ => panic!("Expected an integer, found {:?}", op),
            }
        }

        pub fn is_bool_with_value(op: &Operand, b: bool) {
            match op {
                Operand::Constant(ConstOperand::Bool(actual_b)) => assert_eq!(b, *actual_b),
                _ => panic!("Expected a boolean, found {:?}", op),
            }
        }

        pub fn is_string_with_value(op: &Operand, s: &str) {
            match op {
                Operand::Constant(ConstOperand::String(actual_s)) => assert_eq!(s, *actual_s),
                _ => panic!("Expected a string, found {:?}", op),
            }
        }

        pub fn refers_to_place(op: &Operand, place: &Place) {
            match op {
                Operand::Reference(actual_place) => assert_eq!(actual_place, place),
                _ => panic!("Expected a reference, found {:?}", op),
            }
        }
    }

    mod assert_error {
        use crate::errors::{CompilerError, CompilerErrorReason};

        pub fn is_reference_error(error: CompilerError) -> String {
            match error.reason {
                CompilerErrorReason::ReferenceError { identifier } => identifier,
                _ => panic!("Expected a reference error, found {:?}", error.reason),
            }
        }

        pub fn is_assign_to_immutable_error(error: CompilerError) -> String {
            match error.reason {
                CompilerErrorReason::AssignmentToImmutableVariable { identifier } => identifier,
                _ => panic!(
                    "Expected an assignment to immutable variable error, found {:?}",
                    error.reason
                ),
            }
        }
    }

    mod assert_env {
        use crate::types::environment::EnvironmentType;

        use super::TestContext;

        pub fn is_isomorphic(ctx: &TestContext) {
            match ctx.current_env() {
                EnvironmentType::Isomorphic => {}
                env => panic!("Expected an isomorphic environment, found {:?}", env),
            }
        }

        pub fn is_backend(ctx: &TestContext) {
            match ctx.current_env() {
                EnvironmentType::Backend => {}
                env => panic!("Expected a backend environment, found {:?}", env),
            }
        }

        pub fn is_frontend(ctx: &TestContext) {
            match ctx.current_env() {
                EnvironmentType::Frontend => {}
                env => panic!("Expected a frontend environment, found {:?}", env),
            }
        }
    }

    mod assert_basic_block {
        use crate::phases::middle_end::ir_v3::{BasicBlockId, RValue, Terminator};

        use super::TestContext;

        pub fn has_num_of_statements(ctx: &TestContext, count: usize) {
            assert_eq!(ctx.basic_block().stmts.len(), count);
        }

        pub fn is_end_of_module(ctx: &TestContext) {
            match &ctx.basic_block().terminator {
                Terminator::EndOfModule => {}
                term => panic!("Expected the end of a module, found {:?}", term),
            }
        }

        pub fn terminates_with_goto(ctx: &TestContext) -> BasicBlockId {
            match &ctx.basic_block().terminator {
                Terminator::Goto(next_block_id) => *next_block_id,
                term => panic!("Expected a goto, found {:?}", term),
            }
        }

        pub fn will_goto(ctx: &TestContext, next_id: BasicBlockId) {
            match &ctx.basic_block().terminator {
                Terminator::Goto(actual_next_id) => assert_eq!(*actual_next_id, next_id),
                term => panic!("Expected a goto, found {:?}", term),
            }
        }

        pub fn terminates_with_conditional(
            ctx: &TestContext,
        ) -> (RValue, BasicBlockId, Option<BasicBlockId>) {
            let terminator = ctx.basic_block().terminator.clone();
            match terminator {
                Terminator::Conditional {
                    condition,
                    true_branch,
                    else_branch,
                } => (condition, true_branch, else_branch),
                term => panic!("Expected a conditional terminator, found {:?}", term),
            }
        }
    }

    fn start_test(source: &str) -> TestContext {
        let module = Module::from_frontend_module(frontend::ir::Module {
            path: String::new(),
            errors: Vec::new(),
            ast: crate::phases::frontend::parser::ModuleParser::new()
                .parse("<test>.nux", source)
                .unwrap()
                .into(),
        });

        TestContext {
            module,
            active_block: BasicBlockId(0),
            active_local: Local(0),
        }
    }

    #[test]
    fn can_build_an_empty_module() {
        let ctx = start_test("");
        ctx.assert_no_errors();
        ctx.assert_num_of_locals(0);
    }

    #[test]
    fn can_convert_constants() {
        let mut ctx = start_test("1;");
        ctx.assert_no_errors();
        ctx.assert_num_of_basic_blocks(1);
        ctx.assert_num_of_locals(0);
        let rhs = assert_statement::is_eval(ctx.consume_statement());
        let operand = assert_r_value::is_use(rhs);
        assert_operand::is_int_with_value(&operand, 1);

        let mut ctx = start_test("true;");
        ctx.assert_no_errors();
        ctx.assert_num_of_basic_blocks(1);
        ctx.assert_num_of_locals(0);
        let rhs = assert_statement::is_eval(ctx.consume_statement());
        let operand = assert_r_value::is_use(rhs);
        assert_operand::is_bool_with_value(&operand, true);

        let mut ctx = start_test("'hello';");
        ctx.assert_no_errors();
        ctx.assert_num_of_basic_blocks(1);
        ctx.assert_num_of_locals(0);
        let rhs = assert_statement::is_eval(ctx.consume_statement());
        let operand = assert_r_value::is_use(rhs);
        assert_operand::is_string_with_value(&operand, "hello");
    }

    #[test]
    fn reports_error_when_undeclared_identifier_is_found() {
        let mut ctx = start_test("foo;");
        ctx.assert_num_of_errors(1);
        let reference = assert_error::is_reference_error(ctx.consume_error());
        assert_eq!(reference, "foo");
    }

    #[test]
    fn can_convert_variable_declarations() {
        let mut ctx = start_test("let x = 0;");
        ctx.assert_no_errors();
        ctx.assert_num_of_basic_blocks(1);
        ctx.assert_num_of_locals(1);
        let (lhs, rhs) = assert_statement::is_assign(ctx.consume_statement());
        assert_place::is_immutable(&ctx, &lhs);
        let operand = assert_r_value::is_use(rhs);
        assert_operand::is_int_with_value(&operand, 0);

        let mut ctx = start_test("mut x = 0;");
        ctx.assert_no_errors();
        ctx.assert_num_of_basic_blocks(1);
        ctx.assert_num_of_locals(1);
        let (lhs, rhs) = assert_statement::is_assign(ctx.consume_statement());
        assert_place::is_mutable(&ctx, &lhs);
        let operand = assert_r_value::is_use(rhs);
        assert_operand::is_int_with_value(&operand, 0);
    }

    #[test]
    fn can_refer_to_local_variables() {
        let mut ctx = start_test("let x = 0; x;");
        ctx.assert_no_errors();
        ctx.assert_num_of_basic_blocks(1);
        ctx.assert_num_of_locals(1);
        let (x_place, _) = assert_statement::is_assign(ctx.consume_statement());
        let x_r_value = assert_statement::is_eval(ctx.consume_statement());
        let operand = assert_r_value::is_use(x_r_value);
        assert_operand::refers_to_place(&operand, &x_place);
    }

    #[test]
    fn assigning_to_immutable_variables_causes_an_error() {
        let mut ctx = start_test("let x = 0; x = 1;");
        ctx.assert_num_of_errors(1);
        assert_error::is_assign_to_immutable_error(ctx.consume_error());
    }

    #[test]
    fn assigning_to_mutable_variables_works() {
        let mut ctx = start_test("mut x = 0; x = 1;");
        ctx.assert_no_errors();
        ctx.assert_num_of_basic_blocks(1);
        ctx.assert_num_of_locals(1);

        let (first_x_place, _) = assert_statement::is_assign(ctx.consume_statement());
        let (second_x_place, rhs) = assert_statement::is_assign(ctx.consume_statement());
        assert_eq!(first_x_place, second_x_place);
        assert_operand::is_int_with_value(&assert_r_value::is_use(rhs), 1);
    }

    #[test]
    fn prefers_type_annotation_over_rhs_for_variable_types() {
        let mut ctx = start_test("let x: int = true;");
        ctx.assert_no_errors();
        ctx.assert_num_of_basic_blocks(1);
        ctx.assert_num_of_locals(1);

        let (x_place, rhs) = assert_statement::is_assign(ctx.consume_statement());
        assert_place::has_type(&ctx, &x_place, Type::Int);
        assert_r_value::has_type(&rhs, Type::Bool);
    }

    #[test]
    fn infers_type_from_constant_initializer() {
        let mut ctx = start_test("let x = true;");
        ctx.assert_no_errors();
        ctx.assert_num_of_basic_blocks(1);
        ctx.assert_num_of_locals(1);

        let (x_place, rhs) = assert_statement::is_assign(ctx.consume_statement());
        assert_place::has_type(&ctx, &x_place, Type::Bool);
        assert_r_value::has_type(&rhs, Type::Bool);
    }

    #[test]
    fn infers_type_from_known_reference() {
        let mut ctx = start_test("let x = true; let y = x;");
        ctx.assert_no_errors();
        ctx.assert_num_of_basic_blocks(1);
        ctx.assert_num_of_locals(2);

        let (x_place, _) = assert_statement::is_assign(ctx.consume_statement());
        let (y_place, rhs) = assert_statement::is_assign(ctx.consume_statement());
        let rhs = assert_r_value::is_reference(rhs);
        assert_eq!(rhs, x_place);
        assert_place::has_type(&ctx, &x_place, Type::Bool);
        assert_place::has_type(&ctx, &y_place, Type::Bool);
    }

    #[test]
    fn can_add_two_integers_together() {
        let mut ctx = start_test("1 + 2;");
        ctx.assert_no_errors();
        ctx.assert_num_of_basic_blocks(1);
        ctx.assert_num_of_locals(0);

        let expr = assert_statement::is_eval(ctx.consume_statement());
        let (lhs, op, rhs) = assert_r_value::is_binary_op(expr);
        assert_operand::is_int_with_value(&lhs, 1);
        assert_eq!(op, BinOp::Add);
        assert_operand::is_int_with_value(&rhs, 2);
    }

    #[test]
    fn can_add_three_integers_together() {
        let mut ctx = start_test("1 + 2 + 3;");
        ctx.assert_no_errors();
        ctx.assert_num_of_basic_blocks(1);
        ctx.assert_num_of_locals(1);

        let (tmp_place, rhs) = assert_statement::is_assign(ctx.consume_statement());
        let (lhs, op, rhs) = assert_r_value::is_binary_op(rhs);
        assert_operand::is_int_with_value(&lhs, 1);
        assert_eq!(op, BinOp::Add);
        assert_operand::is_int_with_value(&rhs, 2);

        let r_value = assert_statement::is_eval(ctx.consume_statement());
        let (lhs, op, rhs) = assert_r_value::is_binary_op(r_value);
        assert_operand::refers_to_place(&lhs, &tmp_place);
        assert_eq!(op, BinOp::Add);
        assert_operand::is_int_with_value(&rhs, 3);
    }

    #[test]
    fn order_of_operations_is_respected_for_arithmetic() {
        let mut ctx = start_test("1 + 2 * 3;");
        ctx.assert_no_errors();
        ctx.assert_num_of_basic_blocks(1);
        ctx.assert_num_of_locals(1);

        let (_, rhs) = assert_statement::is_assign(ctx.consume_statement());
        let (_, op, _) = assert_r_value::is_binary_op(rhs);
        assert_eq!(op, BinOp::Mul);

        let r_value = assert_statement::is_eval(ctx.consume_statement());
        let (_, op, _) = assert_r_value::is_binary_op(r_value);
        assert_eq!(op, BinOp::Add);
    }

    #[test]
    fn can_produce_list_literals() {
        let mut ctx = start_test("let x = ['a', 'b', 'c'];");
        ctx.assert_no_errors();
        ctx.assert_num_of_basic_blocks(1);
        ctx.assert_num_of_locals(1);

        let (x_place, rhs) = assert_statement::is_assign(ctx.consume_statement());
        assert_r_value::has_type(&rhs, Type::List(Box::new(Type::String)));
        assert_place::has_type(&ctx, &x_place, Type::List(Box::new(Type::String)));
    }

    #[test]
    fn can_negate_bools() {
        let mut ctx = start_test("!true;");
        ctx.assert_no_errors();
        ctx.assert_num_of_basic_blocks(1);

        let r_value = assert_statement::is_eval(ctx.consume_statement());
        let (operator, operand) = assert_r_value::is_unary_op(r_value);
        assert_eq!(operator, PrefixUnaryOp::Not);
        assert_operand::is_bool_with_value(&operand, true);

        let mut ctx = start_test("let x = !true;");
        ctx.assert_no_errors();
        ctx.assert_num_of_basic_blocks(1);

        let (lhs, rhs) = assert_statement::is_assign(ctx.consume_statement());
        assert_r_value::has_type(&rhs, Type::Bool);
        assert_place::has_type(&ctx, &lhs, Type::Bool);
    }

    #[test]
    fn environment_blocks_set_the_environment_type() {
        let mut ctx = start_test("back {} front {}");
        ctx.assert_no_errors();
        ctx.assert_num_of_basic_blocks(5);

        assert_env::is_isomorphic(&ctx);
        ctx.follow_goto();
        assert_env::is_backend(&ctx);
        ctx.follow_goto();
        assert_env::is_isomorphic(&ctx);
        ctx.follow_goto();
        assert_env::is_frontend(&ctx);
        ctx.follow_goto();
        assert_env::is_isomorphic(&ctx);
    }

    #[test]
    fn statements_in_environment_blocks_are_executed() {
        let mut ctx = start_test(
            "
        let a = 'start';

        back {
            let b = 1;
            b + 2;
        }

        let c = 'middle';

        front {
            let d = true;
            !d;
        }

        let e = 'end';
        ",
        );

        assert_env::is_isomorphic(&ctx);
        assert_basic_block::has_num_of_statements(&ctx, 1);
        ctx.follow_goto();

        assert_env::is_backend(&ctx);
        assert_basic_block::has_num_of_statements(&ctx, 2);
        ctx.follow_goto();

        assert_env::is_isomorphic(&ctx);
        assert_basic_block::has_num_of_statements(&ctx, 1);
        ctx.follow_goto();

        assert_env::is_frontend(&ctx);
        assert_basic_block::has_num_of_statements(&ctx, 2);
        ctx.follow_goto();

        assert_env::is_isomorphic(&ctx);
        assert_basic_block::has_num_of_statements(&ctx, 1);
        assert_basic_block::is_end_of_module(&ctx);
    }

    #[test]
    fn infinite_loops_loop_on_themselves() {
        let mut ctx = start_test("loop {}");
        ctx.assert_no_errors();
        ctx.assert_num_of_basic_blocks(3);

        let loop_block_id = ctx.follow_goto();
        assert_basic_block::will_goto(&ctx, loop_block_id);
    }

    #[test]
    fn breaks_will_exit_loops() {
        let mut ctx = start_test("loop { break; } 1;");
        ctx.assert_no_errors();
        ctx.assert_num_of_basic_blocks(3);

        let loop_block_id = ctx.follow_goto();
        let after_loop_block_id = assert_basic_block::terminates_with_goto(&ctx);
        assert_ne!(loop_block_id, after_loop_block_id);

        ctx.follow_goto();
        assert_statement::is_eval(ctx.consume_statement());
    }

    #[test]
    fn cannot_break_outside_of_a_loop() {
        let mut ctx = start_test("break;");
        ctx.assert_num_of_errors(1);

        assert_eq!(
            ctx.consume_error().reason,
            CompilerErrorReason::BreakMustOccurWithinLoop
        );
    }

    #[test]
    fn blocks_create_new_basic_blocks() {
        let mut ctx = start_test("{};");
        ctx.assert_no_errors();
        ctx.assert_num_of_basic_blocks(3);

        let first_bb = ctx.active_block;
        let second_bb = ctx.follow_goto();
        let third_bb = ctx.follow_goto();
        assert_ne!(first_bb, second_bb);
        assert_ne!(first_bb, third_bb);
        assert_ne!(second_bb, third_bb);
        assert_basic_block::is_end_of_module(&ctx);
    }

    #[test]
    fn blocks_execute_statements() {
        let mut ctx = start_test(
            "
        'before';
        {
            let first = 1;
            let second = 2;
            3;
        };
        'after';
        ",
        );

        ctx.assert_no_errors();
        ctx.assert_num_of_basic_blocks(3);

        assert_basic_block::has_num_of_statements(&ctx, 1);
        ctx.follow_goto();
        assert_basic_block::has_num_of_statements(&ctx, 3);
        ctx.follow_goto();
        assert_basic_block::has_num_of_statements(&ctx, 1);
    }

    #[test]
    fn can_assign_result_of_block_to_variable() {
        let source = "
        let a = {
            let x = 1;
            let y = 2;
            x + y
        };
        a;
        ";
        let mut ctx = start_test(source);
        ctx.assert_no_errors();
        ctx.assert_num_of_basic_blocks(3);
        ctx.assert_num_of_locals(4);

        assert_basic_block::has_num_of_statements(&ctx, 0);
        ctx.follow_goto();

        assert_basic_block::has_num_of_statements(&ctx, 3);
        let (x_place, _) = assert_statement::is_assign(ctx.consume_statement());
        let (y_place, _) = assert_statement::is_assign(ctx.consume_statement());
        let (block_result_place, rhs) = assert_statement::is_assign(ctx.consume_statement());
        let (lhs, op, rhs) = assert_r_value::is_binary_op(rhs);
        assert_operand::refers_to_place(&lhs, &x_place);
        assert_eq!(op, BinOp::Add);
        assert_operand::refers_to_place(&rhs, &y_place);
        ctx.follow_goto();

        assert_basic_block::has_num_of_statements(&ctx, 2);
        let (a_place, rhs) = assert_statement::is_assign(ctx.consume_statement());
        let rhs = assert_r_value::is_reference(rhs);
        assert_eq!(rhs, block_result_place);

        let val = assert_statement::is_eval(ctx.consume_statement());
        let val_place = assert_r_value::is_reference(val);
        assert_eq!(val_place, a_place);
    }
}