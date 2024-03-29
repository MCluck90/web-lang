use std::collections::HashMap;

use crate::{
    errors::CompilerError,
    phases::{
        frontend::ir::{
            EnvironmentType, Expression, ExpressionKind, Module, ModuleItemKind, Statement,
            StatementKind,
        },
        shared::{BinOp, PrefixUnaryOp, Span, Type},
    },
};

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub struct ValueId(usize);

#[derive(Default)]
struct ValueIdBuilder {
    next_id: usize,
}

impl ValueIdBuilder {
    fn get_next(&mut self) -> ValueId {
        let next = ValueId(self.next_id);
        self.next_id += 1;
        next
    }
}

#[derive(Debug)]
pub struct MirModule {
    pub path: String,
    pub insts: Vec<MirInstruction>,
    pub values: HashMap<ValueId, Symbol>,
    pub types: HashMap<ValueId, Type>,
    pub errors: Vec<CompilerError>,
}

#[derive(Default)]
struct Scope {
    values: HashMap<String, ValueId>,
}

impl Scope {
    fn insert(&mut self, k: String, v: ValueId) {
        self.values.insert(k, v);
    }
}

fn find_value(scopes: &[Scope], name: &str) -> Option<ValueId> {
    for scope in scopes.iter().rev() {
        if let Some(id) = scope.values.get(name) {
            return Some(*id);
        }
    }
    None
}

struct MirModuleContext {
    errors: Vec<CompilerError>,
    insts: Vec<MirInstruction>,
    values: HashMap<ValueId, Symbol>,
    scopes: Vec<Scope>,
    value_ids: ValueIdBuilder,
}

impl MirModuleContext {
    fn create_value(&mut self, span: Span, declared_type: Option<Type>) -> ValueId {
        let id = self.value_ids.get_next();

        self.values.insert(
            id,
            Symbol {
                id,
                span,
                declared_type,
            },
        );

        id
    }

    fn create_value_with_name(
        &mut self,
        span: Span,
        declared_type: Option<Type>,
        name: String,
    ) -> ValueId {
        let value_id = self.create_value(span, declared_type);
        self.scopes.last_mut().unwrap().insert(name, value_id);
        value_id
    }

    /// Returns the declared type for the value, if one exists
    fn get_declared_type(&self, id: ValueId) -> Option<Type> {
        self.values
            .get(&id)
            .and_then(|sym| sym.declared_type.clone())
    }

    /// If the given ID points to a function, return its return type
    fn get_return_type(&self, id: ValueId) -> Option<Type> {
        self.get_declared_type(id).and_then(|type_| match type_ {
            Type::Function {
                parameters: _,
                return_type,
            } => Some(*return_type.clone()),
            _ => None,
        })
    }

    /// If the given ID points to a list, return its element type
    fn get_list_element_type(&self, id: ValueId) -> Option<Type> {
        self.get_declared_type(id).and_then(|type_| match type_ {
            Type::List(t) => Some(*t.clone()),
            _ => None,
        })
    }
}

fn convert_statement(ctx: &mut MirModuleContext, stmt: Statement) {
    match stmt.kind {
        StatementKind::VariableDeclaration {
            is_mutable: _,
            type_,
            identifier,
            initializer,
        } => {
            let value_id =
                ctx.create_value_with_name(identifier.span, type_, identifier.name.clone());

            if let Some(rhs) = convert_expression(ctx, initializer) {
                ctx.insts
                    .push(MirInstruction::VariableDeclaration(value_id));
                ctx.insts
                    .push(MirInstruction::Assign(LValue::NamedValue(value_id), rhs));
            } else {
                ctx.insts
                    .push(MirInstruction::VariableDeclaration(value_id));
            }
        }
        StatementKind::Expression(expr) => {
            if let Some(r_value) = convert_expression(ctx, expr) {
                ctx.insts.push(MirInstruction::Statement(r_value));
            }
        }
        StatementKind::FunctionDefinition {
            name,
            parameters,
            return_type,
            body,
        } => {
            // Pre-push this scope to capture the parameters
            let function_scope = Scope::default();
            ctx.scopes.push(function_scope);

            let mut parameter_ids: Vec<ValueId> = Vec::new();
            let mut parameter_types: Vec<Type> = Vec::new();
            for param in parameters {
                let param_id = ctx.create_value_with_name(
                    param.span,
                    Some(param.type_.clone()),
                    param.identifier.name,
                );
                parameter_ids.push(param_id);
                parameter_types.push(param.type_);
            }

            // Pop the scope so we don't bind the function to it's own scope
            let function_scope = ctx.scopes.pop().unwrap();

            let id = ctx.create_value_with_name(
                stmt.span,
                Some(Type::Function {
                    parameters: parameter_types,
                    return_type: Box::new(return_type),
                }),
                name.name,
            );

            ctx.insts
                .push(MirInstruction::StartFunction(id, parameter_ids));

            ctx.scopes.push(function_scope);
            for stmt in body.statements {
                convert_statement(ctx, stmt);
            }
            if let Some(expr) = body.return_expression {
                if let Some(value) = convert_expression(ctx, expr) {
                    ctx.insts.push(MirInstruction::Return(Some(value)));
                };
            }
            ctx.scopes.pop();

            ctx.insts.push(MirInstruction::EndFunction);
        }
        StatementKind::Return(expr) => {
            let return_value = expr.and_then(|expr| convert_expression(ctx, expr));
            ctx.insts.push(MirInstruction::Return(return_value));
        }
        StatementKind::Loop(body) => {
            ctx.insts.push(MirInstruction::StartLoop);
            for stmt in body {
                convert_statement(ctx, stmt);
            }
            ctx.insts.push(MirInstruction::EndLoop);
        }
        StatementKind::Break => {
            ctx.insts.push(MirInstruction::Break);
        }
        StatementKind::ForLoop {
            initializer,
            condition,
            post_loop,
            body,
        } => {
            ctx.scopes.push(Scope::default());
            if let Some(stmt) = initializer {
                convert_statement(ctx, *stmt);
            }

            ctx.insts.push(MirInstruction::StartLoop);
            if let Some(condition) = condition.and_then(|expr| convert_expression(ctx, expr)) {
                // TODO: Need to invert the condition
                ctx.insts.push(MirInstruction::StartIf(condition));
                ctx.insts.push(MirInstruction::Break);
                ctx.insts.push(MirInstruction::EndIf);
            }

            for stmt in body {
                convert_statement(ctx, stmt);
            }

            if let Some(expr) = post_loop {
                convert_expression(ctx, expr);
            }

            ctx.insts.push(MirInstruction::EndLoop);
        }
    };
}

fn convert_expression(ctx: &mut MirModuleContext, expr: Expression) -> Option<RValue> {
    match expr.kind {
        ExpressionKind::Boolean(b) => Some(RValue::bool(expr.span, b)),
        ExpressionKind::Integer(n) => Some(RValue::integer(expr.span, n)),
        ExpressionKind::String(s) => Some(RValue::string(expr.span, s)),
        ExpressionKind::List(items) => {
            let mut r_value_items = Vec::new();
            for item in items {
                if let Some(item) = convert_expression(ctx, item).to_terminal() {
                    r_value_items.push(item);
                } else {
                    return None;
                }
            }

            Some(RValue::list(expr.span, r_value_items))
        }
        ExpressionKind::Identifier(id) => find_value(&ctx.scopes, &id.name)
            .map(|value_id| RValue::named_value(id.span, value_id))
            .or_else(|| {
                ctx.errors
                    .push(CompilerError::reference_error(&expr.span, &id.name));
                None
            }),
        ExpressionKind::BinaryOp(lhs, op, rhs) => {
            if op == BinOp::Assign {
                // Handle assigning to things other than variables
                if let ExpressionKind::PropertyAccess(lhs, prop) = lhs.kind {
                    let rhs = convert_expression(ctx, *rhs)?;
                    let lhs = convert_expression(ctx, *lhs).to_terminal()?;
                    let lhs = match lhs {
                        RValueTerminal::NamedValue(lhs) => lhs,
                        _ => return None,
                    };

                    ctx.insts.push(MirInstruction::Assign(
                        LValue::Property(lhs, prop.name.clone()),
                        rhs,
                    ));

                    return Some(RValue::property_access(expr.span, lhs, prop.name));
                } else if let ExpressionKind::ArrayAccess(lhs, index) = lhs.kind {
                    let rhs = convert_expression(ctx, *rhs)?;
                    let index = convert_expression(ctx, *index).to_terminal()?;
                    let lhs = convert_expression(ctx, *lhs).to_terminal()?;
                    let lhs = match lhs {
                        RValueTerminal::NamedValue(lhs) => lhs,
                        _ => return None,
                    };

                    ctx.insts.push(MirInstruction::Assign(
                        LValue::ListAccess(lhs, index.clone()),
                        rhs,
                    ));

                    return Some(RValue::list_access(expr.span, lhs, index));
                } else if let ExpressionKind::Identifier(lhs) = lhs.kind {
                    let rhs = convert_expression(ctx, *rhs)?;
                    let lhs = find_value(&ctx.scopes, &lhs.name)?;

                    ctx.insts
                        .push(MirInstruction::Assign(LValue::NamedValue(lhs), rhs));

                    return Some(RValue::named_value(expr.span, lhs));
                }
            }

            match (
                convert_expression(ctx, *rhs).to_terminal(),
                convert_expression(ctx, *lhs).to_terminal(),
            ) {
                (Some(rhs), Some(lhs)) => {
                    let value_id = ctx.create_value(expr.span.clone(), None);

                    ctx.insts
                        .push(MirInstruction::VariableDeclaration(value_id));

                    ctx.insts.push(MirInstruction::Assign(
                        LValue::NamedValue(value_id),
                        RValue::bin_op(expr.span.clone(), lhs, op, rhs),
                    ));

                    Some(RValue::named_value(expr.span, value_id))
                }
                _ => None,
            }
        }
        ExpressionKind::PrefixUnaryOp(op, expr) => {
            if op == PrefixUnaryOp::Not {
                let span = expr.span.clone();
                let expr = convert_expression(ctx, *expr)?;
                // If we've reached a terminal, apply the negation.
                // Otherwise, create a temporary then apply the negation
                return match expr.clone().to_terminal() {
                    Some(term) => Some(RValue::not(span, term)),
                    None => {
                        let value_id = ctx.create_value(span.clone(), Some(Type::Bool));
                        ctx.insts
                            .push(MirInstruction::VariableDeclaration(value_id));
                        ctx.insts
                            .push(MirInstruction::Assign(LValue::NamedValue(value_id), expr));

                        Some(RValue::not(span, RValueTerminal::NamedValue(value_id)))
                    }
                };
            }

            // Only allow named values to be incremented or decremented
            match expr.kind {
                ExpressionKind::ArrayAccess(_, _)
                | ExpressionKind::Identifier(_)
                | ExpressionKind::PropertyAccess(_, _) => {}
                _ => {
                    ctx.errors
                        .push(CompilerError::invalid_rhs_expression_in_prefix_operation(
                            &expr.span,
                        ));
                    return None;
                }
            }

            if let Some(expr) = convert_expression(ctx, *expr) {
                let op = match op {
                    crate::phases::shared::PrefixUnaryOp::Not => unreachable!(),
                    crate::phases::shared::PrefixUnaryOp::Inc => BinOp::Add,
                    crate::phases::shared::PrefixUnaryOp::Dec => BinOp::Sub,
                };
                match expr.kind {
                    RValueKind::NamedValue(id) => {
                        let r_value = RValue {
                            span: expr.span.clone(),
                            kind: RValueKind::BinOp(
                                expr.to_terminal().unwrap(),
                                op,
                                RValueTerminal::Integer(1),
                            ),
                        };
                        ctx.insts.push(MirInstruction::Assign(
                            LValue::NamedValue(id),
                            r_value.clone(),
                        ));
                        Some(r_value)
                    }
                    _ => unreachable!(),
                }
            } else {
                None
            }
        }
        ExpressionKind::PropertyAccess(lhs, prop) => {
            let lhs = match convert_expression(ctx, *lhs).to_terminal() {
                Some(RValueTerminal::NamedValue(lhs)) => lhs,
                Some(_) => unreachable!(),
                None => return None,
            };

            let value_id = ctx.create_value(expr.span.clone(), None);
            ctx.insts
                .push(MirInstruction::VariableDeclaration(value_id));

            ctx.insts.push(MirInstruction::Assign(
                LValue::NamedValue(value_id),
                RValue::property_access(expr.span.clone(), lhs, prop.name),
            ));

            Some(RValue::named_value(expr.span, value_id))
        }
        ExpressionKind::FunctionCall { callee, arguments } => {
            if let ExpressionKind::PropertyAccess(lhs, prop) = callee.kind {
                let lhs = match convert_expression(ctx, *lhs).to_terminal() {
                    Some(RValueTerminal::NamedValue(lhs)) => lhs,
                    Some(_) => unreachable!(),
                    None => return None,
                };

                let mut args = Vec::new();
                for arg in arguments {
                    let arg = match convert_expression(ctx, arg).to_terminal() {
                        Some(arg) => arg,
                        None => return None,
                    };

                    args.push(arg);
                }

                let result_value_id = ctx.create_value(expr.span.clone(), ctx.get_return_type(lhs));
                ctx.insts
                    .push(MirInstruction::VariableDeclaration(result_value_id));

                ctx.insts.push(MirInstruction::Assign(
                    LValue::NamedValue(result_value_id),
                    RValue::method_call(expr.span.clone(), lhs, prop.name, args),
                ));

                Some(RValue::named_value(expr.span, result_value_id))
            } else {
                let lhs = match convert_expression(ctx, *callee).to_terminal() {
                    Some(RValueTerminal::NamedValue(lhs)) => lhs,
                    Some(_) => unreachable!(),
                    None => return None,
                };

                let mut args = Vec::new();
                for arg in arguments {
                    let arg = match convert_expression(ctx, arg).to_terminal() {
                        Some(arg) => arg,
                        None => return None,
                    };

                    args.push(arg);
                }

                let value_id = ctx.create_value(expr.span.clone(), ctx.get_return_type(lhs));
                ctx.insts
                    .push(MirInstruction::VariableDeclaration(value_id));

                ctx.insts.push(MirInstruction::Assign(
                    LValue::NamedValue(value_id),
                    RValue::fn_call(expr.span.clone(), lhs, args),
                ));

                Some(RValue::named_value(expr.span, value_id))
            }
        }
        ExpressionKind::ArrayAccess(lhs, index) => {
            match (
                convert_expression(ctx, *index).to_terminal(),
                convert_expression(ctx, *lhs).to_terminal(),
            ) {
                (Some(index), Some(RValueTerminal::NamedValue(lhs))) => {
                    let value_id =
                        ctx.create_value(expr.span.clone(), ctx.get_list_element_type(lhs));
                    ctx.insts
                        .push(MirInstruction::VariableDeclaration(value_id));

                    ctx.insts.push(MirInstruction::Assign(
                        LValue::NamedValue(value_id),
                        RValue::list_access(expr.span.clone(), lhs, index),
                    ));

                    Some(RValue::named_value(expr.span, value_id))
                }
                _ => None,
            }
        }
        ExpressionKind::Block(block) => {
            ctx.scopes.push(Scope::default());
            for stmt in block.statements {
                convert_statement(ctx, stmt);
            }

            let result = match block.return_expression {
                Some(expr) => convert_expression(ctx, expr),
                None => Some(RValue::void(expr.span)),
            };

            ctx.scopes.pop();

            result
        }
        ExpressionKind::Parenthesized(expr) => convert_expression(ctx, *expr),
        ExpressionKind::JsBlock(type_, expressions_) => {
            let value_id = ctx.create_value(expr.span.clone(), Some(type_));

            ctx.insts.push(MirInstruction::StartJsBlock(value_id));

            for expr in expressions_ {
                match convert_expression(ctx, expr).to_terminal() {
                    None => break,
                    Some(term) => ctx.insts.push(MirInstruction::JsBlockInstr(term)),
                }
            }

            ctx.insts.push(MirInstruction::EndJsBlock);

            Some(RValue::named_value(expr.span, value_id))
        }
        ExpressionKind::If {
            condition,
            body,
            else_,
        } => {
            let result_id = ctx.create_value(expr.span.clone(), None);
            ctx.insts
                .push(MirInstruction::VariableDeclaration(result_id));

            let condition = convert_expression(ctx, *condition)?;
            ctx.insts.push(MirInstruction::StartIf(condition));
            let body = convert_expression(ctx, *body)?;
            ctx.insts
                .push(MirInstruction::Assign(LValue::NamedValue(result_id), body));

            if let Some(else_) = else_ {
                ctx.insts.push(MirInstruction::Else);
                let else_ = convert_expression(ctx, *else_)?;
                ctx.insts
                    .push(MirInstruction::Assign(LValue::NamedValue(result_id), else_));
            }

            ctx.insts.push(MirInstruction::EndIf);

            Some(RValue::named_value(expr.span, result_id))
        }
    }
}

impl From<Module> for MirModule {
    fn from(module: Module) -> Self {
        let mut ctx = MirModuleContext {
            errors: module.errors,
            insts: Vec::new(),
            values: HashMap::new(),
            scopes: vec![Scope::default()],
            value_ids: ValueIdBuilder::default(),
        };

        for item in module.ast.items {
            match item.kind {
                ModuleItemKind::EnvironmentBlock(env_type, statements) => {
                    ctx.insts.push(MirInstruction::StartDeclaredEnvironment(
                        item.span, env_type,
                    ));

                    for stmt in statements {
                        convert_statement(&mut ctx, stmt);
                    }

                    ctx.insts.push(MirInstruction::EndDeclaredEnvironment);
                }
                ModuleItemKind::Statement(stmt) => convert_statement(&mut ctx, stmt),
            }
        }

        Self {
            path: module.path,
            insts: ctx.insts,
            values: ctx.values,
            types: HashMap::new(),
            errors: ctx.errors,
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Symbol {
    id: ValueId,
    span: Span,
    declared_type: Option<Type>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum LValue {
    NamedValue(ValueId),
    Property(ValueId, String),
    ListAccess(ValueId, RValueTerminal),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum RValueTerminal {
    NamedValue(ValueId),
    Void,
    Integer(i32),
    String(String),
    Bool(bool),
    List(Vec<RValueTerminal>),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct RValue {
    span: Span,
    kind: RValueKind,
}

impl RValue {
    fn named_value(span: Span, id: ValueId) -> Self {
        RValue {
            span,
            kind: RValueKind::NamedValue(id),
        }
    }

    fn integer(span: Span, value: i32) -> Self {
        RValue {
            span,
            kind: RValueKind::Integer(value),
        }
    }

    fn string(span: Span, value: String) -> Self {
        RValue {
            span,
            kind: RValueKind::String(value),
        }
    }

    fn bool(span: Span, value: bool) -> Self {
        RValue {
            span,
            kind: RValueKind::Bool(value),
        }
    }

    fn list(span: Span, items: Vec<RValueTerminal>) -> Self {
        RValue {
            span,
            kind: RValueKind::List(items),
        }
    }

    fn void(span: Span) -> Self {
        RValue {
            span,
            kind: RValueKind::Void,
        }
    }

    fn method_call(span: Span, lhs: ValueId, name: String, args: Vec<RValueTerminal>) -> Self {
        RValue {
            span,
            kind: RValueKind::MethodCall(lhs, name, args),
        }
    }

    fn fn_call(span: Span, lhs: ValueId, args: Vec<RValueTerminal>) -> Self {
        RValue {
            span,
            kind: RValueKind::FnCall(lhs, args),
        }
    }

    fn bin_op(span: Span, lhs: RValueTerminal, op: BinOp, rhs: RValueTerminal) -> Self {
        RValue {
            span,
            kind: RValueKind::BinOp(lhs, op, rhs),
        }
    }

    fn property_access(span: Span, lhs: ValueId, prop: String) -> Self {
        RValue {
            span,
            kind: RValueKind::PropertyAccess(lhs, prop),
        }
    }

    fn list_access(span: Span, lhs: ValueId, index: RValueTerminal) -> Self {
        RValue {
            span,
            kind: RValueKind::ListAccess(lhs, index),
        }
    }

    fn not(span: Span, term: RValueTerminal) -> Self {
        RValue {
            span,
            kind: RValueKind::Not(term),
        }
    }

    fn to_terminal(self) -> Option<RValueTerminal> {
        match self.kind {
            RValueKind::NamedValue(id) => Some(RValueTerminal::NamedValue(id)),
            RValueKind::Void => Some(RValueTerminal::Void),
            RValueKind::Integer(n) => Some(RValueTerminal::Integer(n)),
            RValueKind::String(s) => Some(RValueTerminal::String(s)),
            RValueKind::Bool(b) => Some(RValueTerminal::Bool(b)),
            RValueKind::List(items) => Some(RValueTerminal::List(items)),
            RValueKind::ListAccess(_, _)
            | RValueKind::PropertyAccess(_, _)
            | RValueKind::MethodCall(_, _, _)
            | RValueKind::FnCall(_, _)
            | RValueKind::BinOp(_, _, _)
            | RValueKind::Not(_) => None,
        }
    }
}

// Just a little bit of sugar to smooth over the mapping
trait OptionRValueToTerminal {
    fn to_terminal(self) -> Option<RValueTerminal>;
}

impl OptionRValueToTerminal for Option<RValue> {
    fn to_terminal(self) -> Option<RValueTerminal> {
        self.and_then(|r| r.to_terminal())
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum RValueKind {
    Void,
    NamedValue(ValueId),
    Integer(i32),
    String(String),
    Bool(bool),
    List(Vec<RValueTerminal>),
    PropertyAccess(ValueId, String),
    ListAccess(ValueId, RValueTerminal),
    MethodCall(ValueId, String, Vec<RValueTerminal>),
    FnCall(ValueId, Vec<RValueTerminal>),
    BinOp(RValueTerminal, BinOp, RValueTerminal),
    Not(RValueTerminal),
}

#[derive(Debug, PartialEq, Eq)]
pub enum MirInstruction {
    VariableDeclaration(ValueId),
    Assign(LValue, RValue),
    Statement(RValue),
    Return(Option<RValue>),
    Break,
    StartFunction(ValueId, Vec<ValueId>),
    EndFunction,
    StartDeclaredEnvironment(Span, EnvironmentType),
    EndDeclaredEnvironment,
    StartLoop,
    EndLoop,
    StartIf(RValue),
    Else,
    EndIf,
    StartJsBlock(ValueId),
    JsBlockInstr(RValueTerminal),
    EndJsBlock,
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::phases::frontend::ir::Module;

    // Conveniently pull out the instructions from a module
    trait InstructionStream {
        fn dequeue_instr(&mut self) -> MirInstruction;
    }

    impl InstructionStream for MirModule {
        fn dequeue_instr(&mut self) -> MirInstruction {
            self.insts.remove(0)
        }
    }

    fn create_module(source: &str) -> MirModule {
        MirModule::from(Module {
            path: String::new(),
            errors: Vec::new(),
            ast: crate::phases::frontend::parser::ModuleParser::new()
                .parse("<test>.nux", source)
                .unwrap()
                .into(),
        })
    }

    mod assert_inst {
        use super::{EnvironmentType, LValue, MirInstruction, RValue, RValueTerminal, ValueId};

        pub fn is_statement(node: MirInstruction) -> RValue {
            match node {
                MirInstruction::Statement(r) => r,
                _ => panic!("Expected a statement, found {:?}", node),
            }
        }

        pub fn is_variable_declaration(node: MirInstruction) -> ValueId {
            match node {
                MirInstruction::VariableDeclaration(id) => id,
                _ => panic!("Expected a variable declaration, found {:?}", node),
            }
        }

        pub fn is_assignment(node: MirInstruction) -> (LValue, RValue) {
            match node {
                MirInstruction::Assign(lhs, rhs) => (lhs, rhs),
                _ => panic!("Expected an assignment, found {:?}", node),
            }
        }

        pub fn is_start_declared_environment(node: MirInstruction) -> EnvironmentType {
            match node {
                MirInstruction::StartDeclaredEnvironment(_, env) => env,
                _ => panic!("Expected the start of an environment, found {:?}", node),
            }
        }

        pub fn is_end_declared_environment(node: MirInstruction) {
            match node {
                MirInstruction::EndDeclaredEnvironment => {}
                _ => panic!(
                    "Expected the end of an environment declaration, found {:?}",
                    node
                ),
            }
        }

        pub fn is_start_function(node: MirInstruction) -> (ValueId, Vec<ValueId>) {
            match node {
                MirInstruction::StartFunction(id, params) => (id, params),
                _ => panic!("Expected the start of a function, found {:?}", node),
            }
        }

        pub fn is_end_function(node: MirInstruction) {
            match node {
                MirInstruction::EndFunction => {}
                _ => panic!("Expected the end of a function, found {:?}", node),
            }
        }

        pub fn is_return_with_value(node: MirInstruction) -> RValue {
            match node {
                MirInstruction::Return(Some(value)) => value,
                _ => panic!("Expected a return with a value, found {:?}", node),
            }
        }

        pub fn is_empty_return(node: MirInstruction) {
            match node {
                MirInstruction::Return(None) => {}
                _ => panic!("Expected a return with a value, found {:?}", node),
            }
        }

        pub fn is_start_loop(node: MirInstruction) {
            match node {
                MirInstruction::StartLoop => {}
                _ => panic!("Expected the start of a loop, found {:?}", node),
            }
        }

        pub fn is_end_loop(node: MirInstruction) {
            match node {
                MirInstruction::EndLoop => {}
                _ => panic!("Expected the end of a loop, found {:?}", node),
            }
        }

        pub fn is_break(node: MirInstruction) {
            match node {
                MirInstruction::Break => {}
                _ => panic!("Expected a break, found {:?}", node),
            }
        }

        pub fn is_start_if(node: MirInstruction) -> RValue {
            match node {
                MirInstruction::StartIf(condition) => condition,
                _ => panic!("Expected the start of an `if`, found {:?}", node),
            }
        }

        pub fn is_else(instr: MirInstruction) {
            match instr {
                MirInstruction::Else => {}
                _ => panic!("Expected an else, found {:?}", instr),
            }
        }

        pub fn is_end_if(node: MirInstruction) {
            match node {
                MirInstruction::EndIf => {}
                _ => panic!("Expected the end of an `if`, found {:?}", node),
            }
        }

        pub fn is_start_js_block(instr: MirInstruction) -> ValueId {
            match instr {
                MirInstruction::StartJsBlock(id) => id,
                _ => panic!("Expected the start of a JS block, found {:?}", instr),
            }
        }

        pub fn is_js_block_instr(instr: MirInstruction) -> RValueTerminal {
            match instr {
                MirInstruction::JsBlockInstr(instr) => instr,
                _ => panic!("Expected a JS block instruction, found {:?}", instr),
            }
        }

        pub fn is_end_js_block(instr: MirInstruction) {
            match instr {
                MirInstruction::EndJsBlock => {}
                _ => panic!("Expected the end of a JS block, found {:?}", instr),
            }
        }
    }

    mod assert_l_value {
        use super::{LValue, RValueTerminal, ValueId};

        pub fn is_named_value(l_value: LValue) -> ValueId {
            match l_value {
                LValue::NamedValue(id) => id,
                _ => panic!("Expected a named value, found {:?}", l_value),
            }
        }

        pub fn is_named_value_with_id(l_value: LValue, id: ValueId) {
            match l_value {
                LValue::NamedValue(actual_id) => assert_eq!(actual_id, id),
                _ => panic!("Expected a named value, found {:?}", l_value),
            }
        }

        pub fn is_property_access(l_value: LValue) -> (ValueId, String) {
            match l_value {
                LValue::Property(id, prop) => (id, prop),
                _ => panic!("Expected a property access, found {:?}", l_value),
            }
        }

        pub fn is_list_access(l_value: LValue) -> (ValueId, RValueTerminal) {
            match l_value {
                LValue::ListAccess(id, index) => (id, index),
                _ => panic!("Expected a list access, found {:?}", l_value),
            }
        }
    }

    mod assert_r_value {
        use super::{BinOp, RValue, RValueKind, RValueTerminal, ValueId};

        pub fn is_integer_with_value(r_value: RValue, n: i32) {
            match r_value.kind {
                RValueKind::Integer(actual_n) => assert_eq!(n, actual_n),
                _ => panic!("Expected an integer, found {:?}", r_value.kind),
            }
        }

        pub fn is_string_with_value(r_value: RValue, s: &str) {
            match r_value.kind {
                RValueKind::String(actual_s) => assert_eq!(s, actual_s),
                _ => panic!("Expected a string, found {:?}", r_value.kind),
            }
        }

        pub fn is_bool_with_value(r_value: RValue, b: bool) {
            match r_value.kind {
                RValueKind::Bool(actual_b) => assert_eq!(b, actual_b),
                _ => panic!("Expected a bool, found {:?}", r_value.kind),
            }
        }

        pub fn is_bin_op(r_value: RValue) -> (RValueTerminal, BinOp, RValueTerminal) {
            match r_value.kind {
                RValueKind::BinOp(lhs, op, rhs) => (lhs, op, rhs),
                _ => panic!("Expected a binary operation, found {:?}", r_value.kind),
            }
        }

        pub fn is_named_value(r_value: RValue) -> ValueId {
            match r_value.kind {
                RValueKind::NamedValue(id) => id,
                _ => panic!("Expected a named value, found {:?}", r_value.kind),
            }
        }

        pub fn is_named_value_with_id(r_value: RValue, id: ValueId) {
            match r_value.kind {
                RValueKind::NamedValue(actual_id) => assert_eq!(actual_id, id),
                _ => panic!("Expected a named value, found {:?}", r_value.kind),
            }
        }

        pub fn is_list(r_value: RValue) -> Vec<RValueTerminal> {
            match r_value.kind {
                RValueKind::List(items) => items,
                _ => panic!("Expected a list, found {:?}", r_value),
            }
        }

        pub fn is_property_access(r_value: RValue) -> (ValueId, String) {
            match r_value.kind {
                RValueKind::PropertyAccess(lhs, rhs) => (lhs, rhs),
                _ => panic!("Expected a property access, found {:?}", r_value.kind),
            }
        }

        pub fn is_list_access(r_value: RValue) -> (ValueId, RValueTerminal) {
            match r_value.kind {
                RValueKind::ListAccess(id, index) => (id, index),
                _ => panic!("Expected a list access, found {:?}", r_value.kind),
            }
        }

        pub fn is_fn_call(r_value: RValue) -> (ValueId, Vec<RValueTerminal>) {
            match r_value.kind {
                RValueKind::FnCall(id, args) => (id, args),
                _ => panic!("Expected a function call, found {:?}", r_value.kind),
            }
        }

        pub fn is_method_call(r_value: RValue) -> (ValueId, String, Vec<RValueTerminal>) {
            match r_value.kind {
                RValueKind::MethodCall(id, name, args) => (id, name, args),
                _ => panic!("Expected a method call, found {:?}", r_value.kind),
            }
        }

        pub fn is_not(r_value: RValue) -> RValueTerminal {
            match r_value.kind {
                RValueKind::Not(operand) => operand,
                _ => panic!("Expected a not operation, found {:?}", r_value.kind),
            }
        }
    }

    mod assert_r_value_terminal {
        use super::{RValueTerminal, ValueId};

        pub fn is_named_value(term: RValueTerminal) -> ValueId {
            match term {
                RValueTerminal::NamedValue(id) => id,
                _ => panic!("Expected a named value, found {:?}", term),
            }
        }

        pub fn is_named_value_with_id(term: RValueTerminal, id: ValueId) {
            match term {
                RValueTerminal::NamedValue(actual_id) => assert_eq!(id, actual_id),
                _ => panic!("Expected a named value, found {:?}", term),
            }
        }

        pub fn is_integer_with_value(term: RValueTerminal, value: i32) {
            match term {
                RValueTerminal::Integer(n) => assert_eq!(n, value),
                _ => panic!("Expected an integer, found {:?}", term),
            }
        }

        pub fn is_boolean_with_value(term: RValueTerminal, value: bool) {
            match term {
                RValueTerminal::Bool(actual_value) => assert_eq!(actual_value, value),
                _ => panic!("Expected a boolean, found {:?}", term),
            }
        }

        pub fn is_string_with_value(term: RValueTerminal, value: &str) {
            match term {
                RValueTerminal::String(actual_value) => assert_eq!(actual_value, value),
                _ => panic!("Expected a string, found {:?}", term),
            }
        }
    }

    mod assert_error {
        use crate::errors::CompilerErrorReason;

        use super::CompilerError;

        pub fn is_reference_error(error: CompilerError) -> String {
            match error.reason {
                CompilerErrorReason::ReferenceError { identifier } => identifier,
                _ => panic!("Expected a reference error, found {:?}", error.reason),
            }
        }

        pub fn is_invalid_prefix_operation(error: CompilerError) {
            match error.reason {
                CompilerErrorReason::InvalidRhsExpressionInPrefixOperation => {}
                _ => panic!(
                    "Expected an invalid prefix operation error, found {:?}",
                    error.reason
                ),
            }
        }
    }

    #[test]
    fn can_lower_empty_ast() {
        let module = create_module("");
        let expected = Vec::<MirInstruction>::new();
        assert_eq!(expected, MirModule::from(module).insts);
    }

    #[test]
    fn lowers_single_statements() {
        let mut module = create_module("1;");
        let statement_value = assert_inst::is_statement(module.dequeue_instr());
        assert_r_value::is_integer_with_value(statement_value, 1);

        let mut module = create_module("'a';");
        let statement_value = assert_inst::is_statement(module.dequeue_instr());
        assert_r_value::is_string_with_value(statement_value, "a");

        let mut module = create_module("true;");
        let statement_value = assert_inst::is_statement(module.dequeue_instr());
        assert_r_value::is_bool_with_value(statement_value, true);
    }

    #[test]
    fn reports_an_error_when_an_unrecognized_identifier_is_used() {
        let mut module = create_module("id;");
        assert_eq!(module.errors.len(), 1);

        let id = assert_error::is_reference_error(module.errors.remove(0));
        assert_eq!(id, "id");
    }

    #[test]
    fn does_not_report_an_error_when_a_declared_variable_is_used() {
        let module = create_module("let id = 0; id;");
        assert_eq!(module.errors.len(), 0);
        assert_eq!(module.values.len(), 1);

        assert!(module.values.contains_key(&ValueId(0)));
    }

    #[test]
    fn turns_var_declarations_in_to_assignments() {
        let mut module = create_module("let id = 32;");
        assert_eq!(module.errors.len(), 0);
        assert_eq!(module.values.len(), 1);

        let value_id = assert_inst::is_variable_declaration(module.dequeue_instr());
        let (lhs, rhs) = assert_inst::is_assignment(module.dequeue_instr());
        let lhs = assert_l_value::is_named_value(lhs);
        assert_eq!(value_id, lhs);
        assert_eq!(rhs.kind, RValueKind::Integer(32));
    }

    #[test]
    fn turns_complex_expressions_in_to_sequences_of_assignments() {
        let mut module = create_module("1 + 2 * 3;");
        assert_eq!(module.errors.len(), 0);
        assert_eq!(module.values.len(), 2);

        // let $tmp1 = 2 * 3;
        // let $tmp2 = 1 + $tmp1;
        let tmp1_id = assert_inst::is_variable_declaration(module.dequeue_instr());
        let (lhs, rhs) = assert_inst::is_assignment(module.dequeue_instr());
        assert_l_value::is_named_value_with_id(lhs, tmp1_id);
        let rhs = assert_r_value::is_bin_op(rhs);
        assert_eq!(
            rhs,
            (
                RValueTerminal::Integer(2),
                BinOp::Mul,
                RValueTerminal::Integer(3)
            )
        );

        let tmp2_id = assert_inst::is_variable_declaration(module.dequeue_instr());
        let (lhs, rhs) = assert_inst::is_assignment(module.dequeue_instr());
        assert_l_value::is_named_value_with_id(lhs, tmp2_id);
        let (lhs, op, rhs) = assert_r_value::is_bin_op(rhs);
        assert_eq!(lhs, RValueTerminal::Integer(1));
        assert_eq!(op, BinOp::Add);
        assert_eq!(rhs, RValueTerminal::NamedValue(ValueId(0)));
    }

    #[test]
    fn handles_variable_declarations_with_complex_expressions() {
        let mut module = create_module("let n = 1 + 2 * 3;");
        assert_eq!(module.errors.len(), 0);
        assert_eq!(module.values.len(), 3);

        // let $tmp1 = 2 * 3;
        // let $tmp2 = 1 + $tmp1;
        // let n = $tmp2;

        let tmp1_id = assert_inst::is_variable_declaration(module.dequeue_instr());

        let (lhs, rhs) = assert_inst::is_assignment(module.dequeue_instr());
        assert_l_value::is_named_value_with_id(lhs, tmp1_id);
        let rhs = assert_r_value::is_bin_op(rhs);
        assert_eq!(
            rhs,
            (
                RValueTerminal::Integer(2),
                BinOp::Mul,
                RValueTerminal::Integer(3)
            )
        );

        let tmp2_id = assert_inst::is_variable_declaration(module.dequeue_instr());

        let (lhs, rhs) = assert_inst::is_assignment(module.dequeue_instr());
        assert_l_value::is_named_value_with_id(lhs, tmp2_id);
        let (lhs, op, rhs) = assert_r_value::is_bin_op(rhs);
        assert_eq!(lhs, RValueTerminal::Integer(1));
        assert_eq!(op, BinOp::Add);
        assert_r_value_terminal::is_named_value(rhs);

        let n_id = assert_inst::is_variable_declaration(module.dequeue_instr());

        let (lhs, rhs) = assert_inst::is_assignment(module.dequeue_instr());
        assert_l_value::is_named_value_with_id(lhs, n_id);
        assert_r_value::is_named_value(rhs);
    }

    #[test]
    fn recognizes_environment_blocks() {
        let mut module = create_module("back {} front {}");
        assert_eq!(module.errors.len(), 0);

        let env = assert_inst::is_start_declared_environment(module.dequeue_instr());
        assert_eq!(env, EnvironmentType::Backend);

        assert_inst::is_end_declared_environment(module.dequeue_instr());

        let env = assert_inst::is_start_declared_environment(module.dequeue_instr());
        assert_eq!(env, EnvironmentType::Frontend);

        assert_inst::is_end_declared_environment(module.dequeue_instr());
    }

    #[test]
    fn assigns_declared_type_for_variable_declarations() {
        let module = create_module("let i: int = 1;");
        assert_eq!(module.errors.len(), 0);

        let symbol = module.values.get(&ValueId(0)).unwrap();
        assert_eq!(symbol.declared_type, Some(Type::Int));
    }

    #[test]
    fn flattens_empty_functions() {
        let mut module = create_module("fn noop() {}");
        assert_eq!(module.errors.len(), 0);
        assert_eq!(module.values.len(), 1);

        assert_inst::is_start_function(module.dequeue_instr());
        assert_inst::is_end_function(module.dequeue_instr());

        let symbol = module.values.get(&ValueId(0)).unwrap();
        assert_eq!(
            Some(Type::Function {
                parameters: Vec::new(),
                return_type: Box::new(Type::Void),
            }),
            symbol.declared_type
        );
    }

    #[test]
    fn flattens_identity_function() {
        let mut module = create_module("fn id(n: int): int { n }");
        assert_eq!(module.errors.len(), 0);
        assert_eq!(module.values.len(), 2);

        let (fn_id, _) = assert_inst::is_start_function(module.dequeue_instr());
        let return_value = assert_inst::is_return_with_value(module.dequeue_instr());
        assert_r_value::is_named_value(return_value);
        assert_inst::is_end_function(module.dequeue_instr());

        let symbol = module.values.get(&fn_id).unwrap();
        assert_eq!(
            Some(Type::Function {
                parameters: vec![Type::Int],
                return_type: Box::new(Type::Int),
            }),
            symbol.declared_type
        );
    }

    #[test]
    fn handles_return_statements() {
        let mut module = create_module("fn noop() { return; }");
        assert_eq!(module.errors.len(), 0);
        assert_eq!(module.values.len(), 1);

        assert_inst::is_start_function(module.dequeue_instr());
        assert_inst::is_empty_return(module.dequeue_instr());
        assert_inst::is_end_function(module.dequeue_instr());

        let mut module = create_module("fn get_5() { return 5; }");
        assert_eq!(module.errors.len(), 0);

        assert_inst::is_start_function(module.dequeue_instr());
        let return_value = assert_inst::is_return_with_value(module.dequeue_instr());
        assert_r_value::is_integer_with_value(return_value, 5);
        assert_inst::is_end_function(module.dequeue_instr());
    }

    #[test]
    fn handles_loops() {
        let mut module = create_module("loop { 1; break; }");
        assert_eq!(module.errors.len(), 0);

        assert_inst::is_start_loop(module.dequeue_instr());
        let value = assert_inst::is_statement(module.dequeue_instr());
        assert_r_value::is_integer_with_value(value, 1);
        assert_inst::is_break(module.dequeue_instr());
        assert_inst::is_end_loop(module.dequeue_instr());
    }

    #[test]
    fn handles_for_loops() {
        let mut module = create_module("for (let i = 0; i < 10; ++i) {}");
        assert_eq!(module.errors.len(), 0);
        assert_eq!(module.values.len(), 2);

        // initializer
        let i_id = assert_inst::is_variable_declaration(module.dequeue_instr());
        let (lhs, rhs) = assert_inst::is_assignment(module.dequeue_instr());
        assert_l_value::is_named_value_with_id(lhs, i_id);
        assert_r_value::is_integer_with_value(rhs, 0);

        // begin loop
        assert_inst::is_start_loop(module.dequeue_instr());

        // condition
        let condition_id = assert_inst::is_variable_declaration(module.dequeue_instr());
        let (lhs, rhs) = assert_inst::is_assignment(module.dequeue_instr());
        assert_l_value::is_named_value_with_id(lhs, condition_id);

        let (lhs, op, rhs) = assert_r_value::is_bin_op(rhs);
        assert_r_value_terminal::is_named_value_with_id(lhs, i_id);
        assert_eq!(op, BinOp::Lt);
        assert_r_value_terminal::is_integer_with_value(rhs, 10);

        // check the condition
        let condition = assert_inst::is_start_if(module.dequeue_instr());
        assert_r_value::is_named_value_with_id(condition, condition_id);
        assert_inst::is_break(module.dequeue_instr());
        assert_inst::is_end_if(module.dequeue_instr());

        // post body
        let (lhs, rhs) = assert_inst::is_assignment(module.dequeue_instr());
        assert_l_value::is_named_value_with_id(lhs, i_id);
        let (lhs, op, rhs) = assert_r_value::is_bin_op(rhs);
        assert_r_value_terminal::is_named_value_with_id(lhs, i_id);
        assert_eq!(op, BinOp::Add);
        assert_r_value_terminal::is_integer_with_value(rhs, 1);
        assert_inst::is_end_loop(module.dequeue_instr());
    }

    #[test]
    fn reports_errors_when_prefix_incrementing_invalid_values() {
        let sources = vec!["++true;", "++1;", "++(1 + 2);", "++'hello';"];
        for source in sources {
            let mut module = create_module(source);
            assert_eq!(module.errors.len(), 1);

            assert_error::is_invalid_prefix_operation(module.errors.remove(0));
        }
    }

    #[test]
    fn handles_basic_list_expressions() {
        let mut module = create_module("[1, 2, 3];");
        assert_eq!(module.errors.len(), 0);

        let value = assert_inst::is_statement(module.dequeue_instr());
        let items = assert_r_value::is_list(value);
        assert_eq!(
            items,
            vec![
                RValueTerminal::Integer(1),
                RValueTerminal::Integer(2),
                RValueTerminal::Integer(3)
            ]
        );
    }

    #[test]
    fn handles_property_access() {
        let mut module = create_module("let a = 0; a.b;");
        assert_eq!(module.errors.len(), 0);
        assert_eq!(module.values.len(), 2);

        // let a
        let a_id = assert_inst::is_variable_declaration(module.dequeue_instr());

        // a = 0
        let (lhs, rhs) = assert_inst::is_assignment(module.dequeue_instr());
        assert_l_value::is_named_value_with_id(lhs, a_id);
        assert_r_value::is_integer_with_value(rhs, 0);

        // let $tmp0
        let tmp_id = assert_inst::is_variable_declaration(module.dequeue_instr());

        // $tmp0 = a.b;
        let (lhs, rhs) = assert_inst::is_assignment(module.dequeue_instr());
        assert_l_value::is_named_value_with_id(lhs, tmp_id);
        let (lhs, rhs) = assert_r_value::is_property_access(rhs);
        assert_eq!(lhs, ValueId(0));
        assert_eq!(rhs, "b");

        // $tmp0;
        let value = assert_inst::is_statement(module.dequeue_instr());
        assert_r_value::is_named_value_with_id(value, tmp_id);
    }

    #[test]
    fn elides_property_access_when_lhs_does_not_exist() {
        let mut module = create_module("a.b;");
        assert_eq!(module.errors.len(), 1);
        assert_eq!(module.insts.len(), 0);

        let id = assert_error::is_reference_error(module.errors.remove(0));
        assert_eq!(id, "a");
    }

    #[test]
    fn handles_function_calls() {
        let mut module = create_module("fn noop() {} noop(10);");
        assert_eq!(module.errors.len(), 0);
        assert_eq!(module.values.len(), 2);

        let (id, params) = assert_inst::is_start_function(module.dequeue_instr());
        assert_eq!(id, ValueId(0));
        assert_eq!(params.len(), 0);
        assert_inst::is_end_function(module.dequeue_instr());

        // let $tmp;
        // $tmp = noop(10);
        // $tmp;
        let tmp_id = assert_inst::is_variable_declaration(module.dequeue_instr());
        let (lhs, rhs) = assert_inst::is_assignment(module.dequeue_instr());
        assert_l_value::is_named_value_with_id(lhs, tmp_id);
        let (id, args) = assert_r_value::is_fn_call(rhs);
        assert_eq!(id, ValueId(0));
        assert_eq!(args, vec![RValueTerminal::Integer(10)]);
    }

    #[test]
    fn handles_method_calls() {
        let mut module = create_module("let a = 0; a.to-vector(2);");
        assert_eq!(module.errors.len(), 0);
        assert_eq!(module.values.len(), 2);

        // let a
        let a_id = assert_inst::is_variable_declaration(module.dequeue_instr());

        // a = 0
        let (lhs, rhs) = assert_inst::is_assignment(module.dequeue_instr());
        assert_l_value::is_named_value_with_id(lhs, a_id);
        assert_r_value::is_integer_with_value(rhs, 0);

        // let $tmp0
        let tmp_id = assert_inst::is_variable_declaration(module.dequeue_instr());

        // $tmp0 = a.to-vector(2)
        let (lhs, rhs) = assert_inst::is_assignment(module.dequeue_instr());
        assert_l_value::is_named_value_with_id(lhs, tmp_id);
        let (lhs, name, args) = assert_r_value::is_method_call(rhs);
        assert_eq!(lhs, ValueId(0));
        assert_eq!(name, "to-vector");
        assert_eq!(args, vec![RValueTerminal::Integer(2)]);

        // $tmp0;
        let stmt = assert_inst::is_statement(module.dequeue_instr());
        assert_r_value::is_named_value_with_id(stmt, tmp_id);
    }

    #[test]
    fn allows_assigning_to_properties() {
        let mut module = create_module("let a = 0; a.b = 1;");
        assert_eq!(module.errors.len(), 0);
        assert_eq!(module.values.len(), 1);

        // let a
        let a_id = assert_inst::is_variable_declaration(module.dequeue_instr());

        // a = 0
        let (lhs, rhs) = assert_inst::is_assignment(module.dequeue_instr());
        assert_l_value::is_named_value_with_id(lhs, a_id);
        assert_r_value::is_integer_with_value(rhs, 0);

        // a.b = 1
        let (lhs, rhs) = assert_inst::is_assignment(module.dequeue_instr());
        let (id, prop) = assert_l_value::is_property_access(lhs);
        assert_eq!(id, ValueId(0));
        assert_eq!(prop, "b");
        assert_r_value::is_integer_with_value(rhs, 1);
    }

    #[test]
    fn can_access_list_items() {
        let mut module = create_module("let a = [1, 2, 3]; a[2];");
        assert_eq!(module.errors.len(), 0);
        assert_eq!(module.values.len(), 2);

        // let a
        let a_id = assert_inst::is_variable_declaration(module.dequeue_instr());

        // a = [1]
        let (lhs, rhs) = assert_inst::is_assignment(module.dequeue_instr());
        assert_l_value::is_named_value_with_id(lhs, a_id);
        let list = assert_r_value::is_list(rhs);
        assert_eq!(
            list,
            vec![
                RValueTerminal::Integer(1),
                RValueTerminal::Integer(2),
                RValueTerminal::Integer(3)
            ]
        );

        // let $tmp
        let tmp_id = assert_inst::is_variable_declaration(module.dequeue_instr());

        // $tmp = a[0]
        let (lhs, rhs) = assert_inst::is_assignment(module.dequeue_instr());
        assert_l_value::is_named_value_with_id(lhs, tmp_id);
        let (lhs, index) = assert_r_value::is_list_access(rhs);
        assert_eq!(lhs, ValueId(0));
        assert_r_value_terminal::is_integer_with_value(index, 2);
    }

    #[test]
    fn can_assign_to_list_indices() {
        let mut module = create_module("let a = [1]; a[0] = 2;");
        assert_eq!(module.errors.len(), 0);
        assert_eq!(module.values.len(), 1);

        // let a
        let a_id = assert_inst::is_variable_declaration(module.dequeue_instr());

        // a = [1]
        let (lhs, rhs) = assert_inst::is_assignment(module.dequeue_instr());
        assert_l_value::is_named_value_with_id(lhs, a_id);
        let list = assert_r_value::is_list(rhs);
        assert_eq!(list, vec![RValueTerminal::Integer(1)]);

        // a[0] = 2
        let (lhs, rhs) = assert_inst::is_assignment(module.dequeue_instr());
        let (id, index) = assert_l_value::is_list_access(lhs);
        assert_eq!(id, ValueId(0));
        assert_r_value_terminal::is_integer_with_value(index, 0);
        assert_r_value::is_integer_with_value(rhs, 2);
    }

    #[test]
    fn handles_blocks() {
        /* Block with only a simple return expression */
        let mut module = create_module("{ true };");
        assert_eq!(module.errors.len(), 0);
        assert_eq!(module.values.len(), 0);

        let r_value = assert_inst::is_statement(module.dequeue_instr());
        assert_r_value::is_bool_with_value(r_value, true);

        /* Returns the value of a variable */
        let mut module = create_module("{ let a = true; a };");
        assert_eq!(module.errors.len(), 0);
        assert_eq!(module.values.len(), 1);

        // let a
        let a_id = assert_inst::is_variable_declaration(module.dequeue_instr());

        // a = true
        let (lhs, rhs) = assert_inst::is_assignment(module.dequeue_instr());
        assert_l_value::is_named_value_with_id(lhs, a_id);
        assert_r_value::is_bool_with_value(rhs, true);

        // a
        let block_result = assert_inst::is_statement(module.dequeue_instr());
        assert_r_value::is_named_value_with_id(block_result, a_id);

        /* Assigns simple result to variable */
        let mut module = create_module("let a = { true };");
        assert_eq!(module.errors.len(), 0);
        assert_eq!(module.values.len(), 1);

        // let a
        let a_id = assert_inst::is_variable_declaration(module.dequeue_instr());

        // a = true
        let (lhs, rhs) = assert_inst::is_assignment(module.dequeue_instr());
        assert_l_value::is_named_value_with_id(lhs, a_id);
        assert_r_value::is_bool_with_value(rhs, true);

        /* Assigns a named value to the result of a block */
        let mut module = create_module("let a = { let b = true; b };");
        assert_eq!(module.errors.len(), 0);
        assert_eq!(module.values.len(), 2);

        // let b
        let b_id = assert_inst::is_variable_declaration(module.dequeue_instr());

        // b = true
        let (lhs, rhs) = assert_inst::is_assignment(module.dequeue_instr());
        assert_l_value::is_named_value_with_id(lhs, b_id);
        assert_r_value::is_bool_with_value(rhs, true);

        // let a
        let a_id = assert_inst::is_variable_declaration(module.dequeue_instr());

        // a = b
        let (lhs, rhs) = assert_inst::is_assignment(module.dequeue_instr());
        assert_l_value::is_named_value_with_id(lhs, a_id);
        assert_r_value::is_named_value_with_id(rhs, b_id);
    }

    #[test]
    fn can_negate_bools() {
        let mut module = create_module("!true;");
        assert_eq!(module.errors.len(), 0);
        assert_eq!(module.values.len(), 0);

        let r_value = assert_inst::is_statement(module.dequeue_instr());
        let term = assert_r_value::is_not(r_value);
        assert_r_value_terminal::is_boolean_with_value(term, true);
    }

    #[test]
    fn handles_parenthesized_expressions() {
        let mut module = create_module("(true);");
        assert_eq!(module.errors.len(), 0);
        assert_eq!(module.values.len(), 0);

        let r_value = assert_inst::is_statement(module.dequeue_instr());
        assert_r_value::is_bool_with_value(r_value, true);
    }

    #[test]
    fn allows_for_js_blocks() {
        /* Simple block */
        let mut module = create_module(r#"#js { "console.log('Hello');" };"#);
        assert_eq!(module.errors.len(), 0);
        assert_eq!(module.values.len(), 1);

        assert_inst::is_start_js_block(module.dequeue_instr());
        let term = assert_inst::is_js_block_instr(module.dequeue_instr());
        assert_r_value_terminal::is_string_with_value(term, "console.log('Hello');");
        assert_inst::is_end_js_block(module.dequeue_instr());

        /* Block that produces a value */
        let mut module = create_module("let a = #js : int { '1' };");
        assert_eq!(module.errors.len(), 0);
        assert_eq!(module.values.len(), 2);

        let js_block_id = assert_inst::is_start_js_block(module.dequeue_instr());
        assert_r_value_terminal::is_string_with_value(
            assert_inst::is_js_block_instr(module.dequeue_instr()),
            "1",
        );
        assert_inst::is_end_js_block(module.dequeue_instr());

        let a_id = assert_inst::is_variable_declaration(module.dequeue_instr());
        let (lhs, rhs) = assert_inst::is_assignment(module.dequeue_instr());
        assert_l_value::is_named_value_with_id(lhs, a_id);
        assert_r_value::is_named_value_with_id(rhs, js_block_id);
    }

    #[test]
    fn handles_if_expressions() {
        let mut module = create_module("if true { 1 } else { 2 };");
        assert_eq!(module.errors.len(), 0);
        assert_eq!(module.values.len(), 1);

        let result_id = assert_inst::is_variable_declaration(module.dequeue_instr());
        let condition = assert_inst::is_start_if(module.dequeue_instr());
        assert_r_value::is_bool_with_value(condition, true);

        let (lhs, rhs) = assert_inst::is_assignment(module.dequeue_instr());
        assert_l_value::is_named_value_with_id(lhs, result_id);
        assert_r_value::is_integer_with_value(rhs, 1);

        assert_inst::is_else(module.dequeue_instr());
        let (lhs, rhs) = assert_inst::is_assignment(module.dequeue_instr());
        assert_l_value::is_named_value_with_id(lhs, result_id);
        assert_r_value::is_integer_with_value(rhs, 2);
        assert_inst::is_end_if(module.dequeue_instr());

        let mut module = create_module("let a = if true { 1 } else { 2 };");
        assert_eq!(module.errors.len(), 0);
        assert_eq!(module.values.len(), 2);

        let result_id = assert_inst::is_variable_declaration(module.dequeue_instr());
        let condition = assert_inst::is_start_if(module.dequeue_instr());
        assert_r_value::is_bool_with_value(condition, true);

        let (lhs, rhs) = assert_inst::is_assignment(module.dequeue_instr());
        assert_l_value::is_named_value_with_id(lhs, result_id);
        assert_r_value::is_integer_with_value(rhs, 1);

        assert_inst::is_else(module.dequeue_instr());
        let (lhs, rhs) = assert_inst::is_assignment(module.dequeue_instr());
        assert_l_value::is_named_value_with_id(lhs, result_id);
        assert_r_value::is_integer_with_value(rhs, 2);
        assert_inst::is_end_if(module.dequeue_instr());

        let a_id = assert_inst::is_variable_declaration(module.dequeue_instr());
        let (lhs, rhs) = assert_inst::is_assignment(module.dequeue_instr());
        assert_l_value::is_named_value_with_id(lhs, a_id);
        assert_r_value::is_named_value_with_id(rhs, result_id);
    }
}
