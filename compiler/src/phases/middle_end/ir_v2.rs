use std::collections::HashMap;

use crate::{
    errors::CompilerError,
    phases::{
        frontend::{
            self,
            ir::{
                EnvironmentType, Expression, ExpressionKind, ModuleItemKind, Statement,
                StatementKind,
            },
        },
        shared::{BinOp, Span, Type},
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
pub struct LoweredModule {
    pub path: String,
    pub nodes: Vec<LoweredModuleAstNode>,
    pub name_bindings: HashMap<ValueId, Symbol>,
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

struct LoweredModuleContext {
    errors: Vec<CompilerError>,
    nodes: Vec<LoweredModuleAstNode>,
    name_bindings: HashMap<ValueId, Symbol>,
    scopes: Vec<Scope>,
    value_ids: ValueIdBuilder,
}

fn convert_statement(ctx: &mut LoweredModuleContext, stmt: Statement) {
    match stmt.kind {
        StatementKind::VariableDeclaration {
            is_mutable: _,
            type_,
            identifier,
            initializer,
        } => {
            let value_id = ctx.value_ids.get_next();
            ctx.name_bindings.insert(
                value_id,
                Symbol {
                    id: value_id,
                    span: identifier.span,
                    declared_type: type_,
                },
            );
            ctx.scopes
                .last_mut()
                .unwrap()
                .insert(identifier.name.clone(), value_id);

            if let Some(rhs) = convert_expression(ctx, initializer) {
                ctx.nodes
                    .push(LoweredModuleAstNode::VariableDeclaration(value_id));
                ctx.nodes.push(LoweredModuleAstNode::Assign(
                    LValue::NamedValue(value_id),
                    rhs,
                ));
            } else {
                ctx.nodes
                    .push(LoweredModuleAstNode::VariableDeclaration(value_id));
            }
        }
        StatementKind::Expression(expr) => {
            if let Some(r_value) = convert_expression(ctx, expr) {
                ctx.nodes.push(LoweredModuleAstNode::Statement(r_value));
            }
        }
        StatementKind::FunctionDefinition {
            name,
            parameters,
            return_type,
            body,
        } => {
            let id = ctx.value_ids.get_next();
            ctx.scopes.last_mut().unwrap().insert(name.name, id);

            let mut function_scope = Scope::default();

            let mut parameter_ids: Vec<ValueId> = Vec::new();
            let mut parameter_types: Vec<Type> = Vec::new();
            for param in parameters {
                let param_id = ctx.value_ids.get_next();
                function_scope.insert(param.identifier.name, param_id);
                ctx.name_bindings.insert(
                    param_id,
                    Symbol {
                        id: param_id,
                        span: param.span,
                        declared_type: Some(param.type_.clone()),
                    },
                );
                parameter_ids.push(param_id);
                parameter_types.push(param.type_);
            }

            ctx.name_bindings.insert(
                id,
                Symbol {
                    id,
                    span: stmt.span,
                    declared_type: Some(Type::Function {
                        parameters: parameter_types,
                        return_type: Box::new(return_type),
                    }),
                },
            );

            ctx.nodes
                .push(LoweredModuleAstNode::StartFunction(id, parameter_ids));

            ctx.scopes.push(function_scope);
            for stmt in body.statements {
                convert_statement(ctx, stmt);
            }
            if let Some(expr) = body.return_expression {
                if let Some(value) = convert_expression(ctx, expr) {
                    ctx.nodes.push(LoweredModuleAstNode::Return(Some(value)));
                };
            }
            ctx.scopes.pop();

            ctx.nodes.push(LoweredModuleAstNode::EndFunction);
        }
        StatementKind::Return(expr) => {
            let return_value = expr.and_then(|expr| convert_expression(ctx, expr));
            ctx.nodes.push(LoweredModuleAstNode::Return(return_value));
        }
        StatementKind::Loop(body) => {
            ctx.nodes.push(LoweredModuleAstNode::StartLoop);
            for stmt in body {
                convert_statement(ctx, stmt);
            }
            ctx.nodes.push(LoweredModuleAstNode::EndLoop);
        }
        StatementKind::Break => {
            ctx.nodes.push(LoweredModuleAstNode::Break);
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

            ctx.nodes.push(LoweredModuleAstNode::StartLoop);
            if let Some(condition) = condition.and_then(|expr| convert_expression(ctx, expr)) {
                ctx.nodes.push(LoweredModuleAstNode::StartIf(condition));
                ctx.nodes.push(LoweredModuleAstNode::Break);
                ctx.nodes.push(LoweredModuleAstNode::EndIf);
            }

            for stmt in body {
                convert_statement(ctx, stmt);
            }

            if let Some(expr) = post_loop {
                convert_expression(ctx, expr);
            }

            ctx.nodes.push(LoweredModuleAstNode::EndLoop);
        }
    };
}

fn convert_expression(ctx: &mut LoweredModuleContext, expr: Expression) -> Option<RValue> {
    match expr.kind {
        frontend::ir::ExpressionKind::Boolean(b) => Some(RValue::bool(expr.span, b)),
        frontend::ir::ExpressionKind::Integer(n) => Some(RValue::integer(expr.span, n)),
        frontend::ir::ExpressionKind::String(s) => Some(RValue::string(expr.span, s)),
        frontend::ir::ExpressionKind::List(items) => {
            let mut r_value_items = Vec::new();
            for item in items {
                if let Some(item) = convert_expression(ctx, item).and_then(|i| i.to_terminal()) {
                    r_value_items.push(item);
                } else {
                    return None;
                }
            }

            Some(RValue::list(expr.span, r_value_items))
        }
        frontend::ir::ExpressionKind::Identifier(id) => find_value(&ctx.scopes, &id.name)
            .map(|value_id| RValue::named_value(id.span, value_id))
            .or_else(|| {
                ctx.errors
                    .push(CompilerError::reference_error(&expr.span, &id.name));
                None
            }),
        frontend::ir::ExpressionKind::BinaryOp(lhs, op, rhs) => {
            match (
                convert_expression(ctx, *lhs).and_then(|lhs| lhs.to_terminal()),
                convert_expression(ctx, *rhs).and_then(|rhs| rhs.to_terminal()),
            ) {
                (Some(lhs), Some(rhs)) => {
                    let value_id = ctx.value_ids.get_next();
                    ctx.name_bindings.insert(
                        value_id,
                        Symbol {
                            id: value_id,
                            span: expr.span.clone(),
                            declared_type: None,
                        },
                    );

                    ctx.nodes
                        .push(LoweredModuleAstNode::VariableDeclaration(value_id));

                    ctx.nodes.push(LoweredModuleAstNode::Assign(
                        LValue::NamedValue(value_id),
                        RValue::bin_op(expr.span.clone(), lhs, op, rhs),
                    ));

                    Some(RValue::named_value(expr.span, value_id))
                }
                _ => None,
            }
        }
        frontend::ir::ExpressionKind::PrefixUnaryOp(op, expr) => {
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
                    crate::phases::shared::PrefixUnaryOp::Not => todo!(),
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
                        ctx.nodes.push(LoweredModuleAstNode::Assign(
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
        frontend::ir::ExpressionKind::PropertyAccess(lhs, prop) => {
            let lhs = match convert_expression(ctx, *lhs).and_then(|lhs| lhs.to_terminal()) {
                Some(RValueTerminal::NamedValue(lhs)) => lhs,
                Some(_) => unreachable!(),
                None => return None,
            };

            let value_id = ctx.value_ids.get_next();
            ctx.nodes
                .push(LoweredModuleAstNode::VariableDeclaration(value_id));

            ctx.nodes.push(LoweredModuleAstNode::Assign(
                LValue::NamedValue(value_id),
                RValue::property_access(expr.span.clone(), lhs, prop.name),
            ));

            Some(RValue::named_value(expr.span, value_id))
        }
        frontend::ir::ExpressionKind::FunctionCall { callee, arguments } => {
            let lhs = match convert_expression(ctx, *callee).and_then(|lhs| lhs.to_terminal()) {
                Some(RValueTerminal::NamedValue(lhs)) => lhs,
                Some(_) => unreachable!(),
                None => return None,
            };

            let mut args = Vec::new();
            for arg in arguments {
                let arg = match convert_expression(ctx, arg).and_then(|arg| arg.to_terminal()) {
                    Some(arg) => arg,
                    None => return None,
                };

                args.push(arg);
            }

            let value_id = ctx.value_ids.get_next();
            ctx.nodes
                .push(LoweredModuleAstNode::VariableDeclaration(value_id));

            ctx.nodes.push(LoweredModuleAstNode::Assign(
                LValue::NamedValue(value_id),
                RValue::fn_call(expr.span.clone(), lhs, args),
            ));

            Some(RValue::named_value(expr.span, value_id))
        }
        _ => None,
    }
}

impl From<frontend::ir::Module> for LoweredModule {
    fn from(module: frontend::ir::Module) -> Self {
        let mut ctx = LoweredModuleContext {
            errors: module.errors,
            nodes: Vec::new(),
            name_bindings: HashMap::new(),
            scopes: vec![Scope::default()],
            value_ids: ValueIdBuilder::default(),
        };

        for item in module.ast.items {
            match item.kind {
                ModuleItemKind::EnvironmentBlock(env_type, statements) => {
                    ctx.nodes
                        .push(LoweredModuleAstNode::StartDeclaredEnvironment(
                            item.span, env_type,
                        ));

                    for stmt in statements {
                        convert_statement(&mut ctx, stmt);
                    }

                    ctx.nodes.push(LoweredModuleAstNode::EndDeclaredEnvironment);
                }
                ModuleItemKind::Statement(stmt) => convert_statement(&mut ctx, stmt),
            }
        }

        Self {
            path: module.path,
            nodes: ctx.nodes,
            name_bindings: ctx.name_bindings,
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
    ArrayAccess(ValueId, ValueId),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum RValueTerminal {
    NamedValue(ValueId),
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

    fn to_terminal(self) -> Option<RValueTerminal> {
        match self.kind {
            RValueKind::NamedValue(id) => Some(RValueTerminal::NamedValue(id)),
            RValueKind::Integer(n) => Some(RValueTerminal::Integer(n)),
            RValueKind::String(s) => Some(RValueTerminal::String(s)),
            RValueKind::Bool(b) => Some(RValueTerminal::Bool(b)),
            RValueKind::List(items) => Some(RValueTerminal::List(items)),
            RValueKind::PropertyAccess(_, _)
            | RValueKind::MethodCall(_, _, _)
            | RValueKind::FnCall(_, _)
            | RValueKind::BinOp(_, _, _) => None,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum RValueKind {
    NamedValue(ValueId),
    Integer(i32),
    String(String),
    Bool(bool),
    List(Vec<RValueTerminal>),
    PropertyAccess(ValueId, String),
    MethodCall(ValueId, String, Vec<RValueTerminal>),
    FnCall(ValueId, Vec<RValueTerminal>),
    BinOp(RValueTerminal, BinOp, RValueTerminal),
}

#[derive(Debug, PartialEq, Eq)]
pub enum LoweredModuleAstNode {
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
    EndIf,
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::phases::frontend::ir::Module;

    fn create_module(source: &str) -> LoweredModule {
        LoweredModule::from(Module {
            path: String::new(),
            errors: Vec::new(),
            ast: crate::phases::frontend::parser::ModuleParser::new()
                .parse("<test>.nux", source)
                .unwrap()
                .into(),
        })
    }

    mod assert_node {
        use super::{frontend::ir::EnvironmentType, LValue, LoweredModuleAstNode, RValue, ValueId};

        pub fn is_statement(node: LoweredModuleAstNode) -> RValue {
            match node {
                LoweredModuleAstNode::Statement(r) => r,
                _ => panic!("Expected a statement, found {:?}", node),
            }
        }

        pub fn is_variable_declaration(node: LoweredModuleAstNode) -> ValueId {
            match node {
                LoweredModuleAstNode::VariableDeclaration(id) => id,
                _ => panic!("Expected a variable declaration, found {:?}", node),
            }
        }

        pub fn is_variable_declaration_with_id(node: LoweredModuleAstNode, id: usize) {
            match node {
                LoweredModuleAstNode::VariableDeclaration(ValueId(actual_id)) => {
                    assert_eq!(actual_id, id)
                }
                _ => panic!("Expected a variable declaration, found {:?}", node),
            }
        }

        pub fn is_assignment(node: LoweredModuleAstNode) -> (LValue, RValue) {
            match node {
                LoweredModuleAstNode::Assign(lhs, rhs) => (lhs, rhs),
                _ => panic!("Expected an assignment, found {:?}", node),
            }
        }

        pub fn is_start_declared_environment(node: LoweredModuleAstNode) -> EnvironmentType {
            match node {
                LoweredModuleAstNode::StartDeclaredEnvironment(_, env) => env,
                _ => panic!("Expected the start of an environment, found {:?}", node),
            }
        }

        pub fn is_end_declared_environment(node: LoweredModuleAstNode) {
            match node {
                LoweredModuleAstNode::EndDeclaredEnvironment => {}
                _ => panic!(
                    "Expected the end of an environment declaration, found {:?}",
                    node
                ),
            }
        }

        pub fn is_start_function(node: LoweredModuleAstNode) -> (ValueId, Vec<ValueId>) {
            match node {
                LoweredModuleAstNode::StartFunction(id, params) => (id, params),
                _ => panic!("Expected the start of a function, found {:?}", node),
            }
        }

        pub fn is_end_function(node: LoweredModuleAstNode) {
            match node {
                LoweredModuleAstNode::EndFunction => {}
                _ => panic!("Expected the end of a function, found {:?}", node),
            }
        }

        pub fn is_return_with_value(node: LoweredModuleAstNode) -> RValue {
            match node {
                LoweredModuleAstNode::Return(Some(value)) => value,
                _ => panic!("Expected a return with a value, found {:?}", node),
            }
        }

        pub fn is_empty_return(node: LoweredModuleAstNode) {
            match node {
                LoweredModuleAstNode::Return(None) => {}
                _ => panic!("Expected a return with a value, found {:?}", node),
            }
        }

        pub fn is_start_loop(node: LoweredModuleAstNode) {
            match node {
                LoweredModuleAstNode::StartLoop => {}
                _ => panic!("Expected the start of a loop, found {:?}", node),
            }
        }

        pub fn is_end_loop(node: LoweredModuleAstNode) {
            match node {
                LoweredModuleAstNode::EndLoop => {}
                _ => panic!("Expected the end of a loop, found {:?}", node),
            }
        }

        pub fn is_break(node: LoweredModuleAstNode) {
            match node {
                LoweredModuleAstNode::Break => {}
                _ => panic!("Expected a break, found {:?}", node),
            }
        }

        pub fn is_start_if(node: LoweredModuleAstNode) -> RValue {
            match node {
                LoweredModuleAstNode::StartIf(condition) => condition,
                _ => panic!("Expected the start of an `if`, found {:?}", node),
            }
        }

        pub fn is_end_if(node: LoweredModuleAstNode) {
            match node {
                LoweredModuleAstNode::EndIf => {}
                _ => panic!("Expected the end of an `if`, found {:?}", node),
            }
        }
    }

    mod assert_l_value {
        use super::{LValue, ValueId};

        pub fn is_named_value(l_value: LValue) -> ValueId {
            match l_value {
                LValue::NamedValue(id) => id,
                _ => panic!("Expected a named value, found {:?}", l_value),
            }
        }

        pub fn is_named_value_with_id(l_value: LValue, id: usize) {
            match l_value {
                LValue::NamedValue(actual_id) => assert_eq!(actual_id, ValueId(id)),
                _ => panic!("Expected a named value, found {:?}", l_value),
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

        pub fn is_named_value_with_id(r_value: RValue, id: usize) {
            match r_value.kind {
                RValueKind::NamedValue(ValueId(actual_id)) => assert_eq!(actual_id, id),
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

        pub fn is_fn_call(r_value: RValue) -> (ValueId, Vec<RValueTerminal>) {
            match r_value.kind {
                RValueKind::FnCall(id, args) => (id, args),
                _ => panic!("Expected a function call, found {:?}", r_value.kind),
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

        pub fn is_named_value_with_id(term: RValueTerminal, id: usize) {
            match term {
                RValueTerminal::NamedValue(ValueId(actual_id)) => assert_eq!(id, actual_id),
                _ => panic!("Expected a named value, found {:?}", term),
            }
        }

        pub fn is_integer_with_value(term: RValueTerminal, value: i32) {
            match term {
                RValueTerminal::Integer(n) => assert_eq!(n, value),
                _ => panic!("Expected an integer, found {:?}", term),
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
        let expected = Vec::<LoweredModuleAstNode>::new();
        assert_eq!(expected, LoweredModule::from(module).nodes);
    }

    #[test]
    fn lowers_single_statements() {
        let mut module = create_module("1;");
        let statement_value = assert_node::is_statement(module.nodes.remove(0));
        assert_r_value::is_integer_with_value(statement_value, 1);

        let mut module = create_module("'a';");
        let statement_value = assert_node::is_statement(module.nodes.remove(0));
        assert_r_value::is_string_with_value(statement_value, "a");

        let mut module = create_module("true;");
        let statement_value = assert_node::is_statement(module.nodes.remove(0));
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

        assert!(module.name_bindings.contains_key(&ValueId(0)));
    }

    #[test]
    fn turns_var_declarations_in_to_assignments() {
        let mut module = create_module("let id = 32;");
        assert_eq!(module.errors.len(), 0);

        let value_id = assert_node::is_variable_declaration(module.nodes.remove(0));
        let (lhs, rhs) = assert_node::is_assignment(module.nodes.remove(0));
        let lhs = assert_l_value::is_named_value(lhs);
        assert_eq!(value_id, lhs);
        assert_eq!(rhs.kind, RValueKind::Integer(32));
    }

    #[test]
    fn turns_complex_expressions_in_to_sequences_of_assignments() {
        let mut module = create_module("1 + 2 * 3;");
        assert_eq!(module.errors.len(), 0);

        // let $tmp1 = 2 * 3;
        // let $tmp2 = 1 + $tmp1;
        assert_node::is_variable_declaration(module.nodes.remove(0));
        let (_, rhs) = assert_node::is_assignment(module.nodes.remove(0));
        let rhs = assert_r_value::is_bin_op(rhs);
        assert_eq!(
            rhs,
            (
                RValueTerminal::Integer(2),
                BinOp::Mul,
                RValueTerminal::Integer(3)
            )
        );

        assert_node::is_variable_declaration(module.nodes.remove(0));
        let (_, rhs) = assert_node::is_assignment(module.nodes.remove(0));
        let (lhs, op, rhs) = assert_r_value::is_bin_op(rhs);
        assert_eq!(lhs, RValueTerminal::Integer(1));
        assert_eq!(op, BinOp::Add);
        assert_eq!(rhs, RValueTerminal::NamedValue(ValueId(0)));
    }

    #[test]
    fn handles_variable_declarations_with_complex_expressions() {
        let mut module = create_module("let n = 1 + 2 * 3;");
        assert_eq!(module.errors.len(), 0);

        // let $tmp1 = 2 * 3;
        // let $tmp2 = 1 + $tmp1;
        // let n = $tmp2;

        assert_node::is_variable_declaration(module.nodes.remove(0));

        let (_, rhs) = assert_node::is_assignment(module.nodes.remove(0));
        let rhs = assert_r_value::is_bin_op(rhs);
        assert_eq!(
            rhs,
            (
                RValueTerminal::Integer(2),
                BinOp::Mul,
                RValueTerminal::Integer(3)
            )
        );

        assert_node::is_variable_declaration(module.nodes.remove(0));

        let (_, rhs) = assert_node::is_assignment(module.nodes.remove(0));
        let (lhs, op, rhs) = assert_r_value::is_bin_op(rhs);
        assert_eq!(lhs, RValueTerminal::Integer(1));
        assert_eq!(op, BinOp::Add);
        assert_r_value_terminal::is_named_value(rhs);

        assert_node::is_variable_declaration(module.nodes.remove(0));

        let (_, rhs) = assert_node::is_assignment(module.nodes.remove(0));
        assert_r_value::is_named_value(rhs);
    }

    #[test]
    fn recognizes_environment_blocks() {
        let mut module = create_module("back {} front {}");
        assert_eq!(module.errors.len(), 0);

        let env = assert_node::is_start_declared_environment(module.nodes.remove(0));
        assert_eq!(env, EnvironmentType::Backend);

        assert_node::is_end_declared_environment(module.nodes.remove(0));

        let env = assert_node::is_start_declared_environment(module.nodes.remove(0));
        assert_eq!(env, EnvironmentType::Frontend);

        assert_node::is_end_declared_environment(module.nodes.remove(0));
    }

    #[test]
    fn assigns_declared_type_for_variable_declarations() {
        let module = create_module("let i: int = 1;");
        assert_eq!(module.errors.len(), 0);

        let symbol = module.name_bindings.get(&ValueId(0)).unwrap();
        assert_eq!(symbol.declared_type, Some(Type::Int));
    }

    #[test]
    fn flattens_empty_functions() {
        let mut module = create_module("fn noop() {}");
        assert_eq!(module.errors.len(), 0);

        assert_node::is_start_function(module.nodes.remove(0));
        assert_node::is_end_function(module.nodes.remove(0));

        let symbol = module.name_bindings.get(&ValueId(0)).unwrap();
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

        assert_node::is_start_function(module.nodes.remove(0));
        let return_value = assert_node::is_return_with_value(module.nodes.remove(0));
        let return_value = assert_r_value::is_named_value(return_value);
        assert_eq!(return_value, ValueId(1));
        assert_node::is_end_function(module.nodes.remove(0));

        let symbol = module.name_bindings.get(&ValueId(0)).unwrap();
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

        assert_node::is_start_function(module.nodes.remove(0));
        assert_node::is_empty_return(module.nodes.remove(0));
        assert_node::is_end_function(module.nodes.remove(0));

        let mut module = create_module("fn get_5() { return 5; }");
        assert_eq!(module.errors.len(), 0);

        assert_node::is_start_function(module.nodes.remove(0));
        let return_value = assert_node::is_return_with_value(module.nodes.remove(0));
        assert_r_value::is_integer_with_value(return_value, 5);
        assert_node::is_end_function(module.nodes.remove(0));
    }

    #[test]
    fn handles_loops() {
        let mut module = create_module("loop { 1; break; }");
        assert_eq!(module.errors.len(), 0);

        assert_node::is_start_loop(module.nodes.remove(0));
        let value = assert_node::is_statement(module.nodes.remove(0));
        assert_r_value::is_integer_with_value(value, 1);
        assert_node::is_break(module.nodes.remove(0));
        assert_node::is_end_loop(module.nodes.remove(0));
    }

    #[test]
    fn handles_for_loops() {
        let mut module = create_module("for (let i = 0; i < 10; ++i) {}");
        assert_eq!(module.errors.len(), 0);

        // initializer
        assert_node::is_variable_declaration_with_id(module.nodes.remove(0), 0);
        let (lhs, rhs) = assert_node::is_assignment(module.nodes.remove(0));
        assert_l_value::is_named_value_with_id(lhs, 0);
        assert_r_value::is_integer_with_value(rhs, 0);

        // begin loop
        assert_node::is_start_loop(module.nodes.remove(0));

        // condition
        assert_node::is_variable_declaration_with_id(module.nodes.remove(0), 1);
        let (lhs, rhs) = assert_node::is_assignment(module.nodes.remove(0));
        assert_l_value::is_named_value_with_id(lhs, 1);

        let (lhs, op, rhs) = assert_r_value::is_bin_op(rhs);
        assert_r_value_terminal::is_named_value_with_id(lhs, 0);
        assert_eq!(op, BinOp::Lt);
        assert_r_value_terminal::is_integer_with_value(rhs, 10);

        // check the condition
        let condition = assert_node::is_start_if(module.nodes.remove(0));
        assert_r_value::is_named_value_with_id(condition, 1);
        assert_node::is_break(module.nodes.remove(0));
        assert_node::is_end_if(module.nodes.remove(0));

        // post body
        let (lhs, rhs) = assert_node::is_assignment(module.nodes.remove(0));
        assert_l_value::is_named_value_with_id(lhs, 0);
        let (lhs, op, rhs) = assert_r_value::is_bin_op(rhs);
        assert_r_value_terminal::is_named_value_with_id(lhs, 0);
        assert_eq!(op, BinOp::Add);
        assert_r_value_terminal::is_integer_with_value(rhs, 1);
        assert_node::is_end_loop(module.nodes.remove(0));
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

        let value = assert_node::is_statement(module.nodes.remove(0));
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

        // let a
        assert_node::is_variable_declaration_with_id(module.nodes.remove(0), 0);

        // a = 0
        let (lhs, rhs) = assert_node::is_assignment(module.nodes.remove(0));
        assert_l_value::is_named_value_with_id(lhs, 0);
        assert_r_value::is_integer_with_value(rhs, 0);

        // let $tmp0
        assert_node::is_variable_declaration_with_id(module.nodes.remove(0), 1);

        // $tmp0 = a.b;
        let (lhs, rhs) = assert_node::is_assignment(module.nodes.remove(0));
        assert_l_value::is_named_value_with_id(lhs, 1);
        let (lhs, rhs) = assert_r_value::is_property_access(rhs);
        assert_eq!(lhs, ValueId(0));
        assert_eq!(rhs, "b");

        // $tmp0;
        let value = assert_node::is_statement(module.nodes.remove(0));
        assert_r_value::is_named_value_with_id(value, 1);
    }

    #[test]
    fn elides_property_access_when_lhs_does_not_exist() {
        let mut module = create_module("a.b;");
        assert_eq!(module.errors.len(), 1);
        assert_eq!(module.nodes.len(), 0);

        let id = assert_error::is_reference_error(module.errors.remove(0));
        assert_eq!(id, "a");
    }

    #[test]
    fn handles_function_calls() {
        let mut module = create_module("fn noop() {} noop(10);");
        assert_eq!(module.errors.len(), 0);

        let (id, params) = assert_node::is_start_function(module.nodes.remove(0));
        assert_eq!(id, ValueId(0));
        assert_eq!(params.len(), 0);
        assert_node::is_end_function(module.nodes.remove(0));

        // let $tmp;
        // $tmp = noop(10);
        // $tmp;
        assert_node::is_variable_declaration_with_id(module.nodes.remove(0), 1);
        let (lhs, rhs) = assert_node::is_assignment(module.nodes.remove(0));
        assert_l_value::is_named_value_with_id(lhs, 1);
        let (id, args) = assert_r_value::is_fn_call(rhs);
        assert_eq!(id, ValueId(0));
        assert_eq!(args, vec![RValueTerminal::Integer(10)]);
    }

    /*
    #[test]
    fn handles_method_calls() {
        let mut module = create_module("let a = 0; a.to-vector(2);");
        assert_eq!(module.errors.len(), 0);

        // let a
        assert!(matches!(
            module.nodes.remove(0),
            LoweredModuleASTNode::VariableDeclaration(ValueId(0))
        ));

        // a = 0
        assert!(matches!(
            module.nodes.remove(0),
            LoweredModuleASTNode::Assign(
                LValue::NamedValue(ValueId(0)),
                RValue {
                    span: _,
                    kind: RValueKind::Integer(0)
                }
            )
        ));

        // let $tmp0
        assert!(matches!(
            module.nodes.remove(0),
            LoweredModuleASTNode::VariableDeclaration(ValueId(1))
        ));

        // $tmp0 = a.to-vector(2)
        let node = module.nodes.remove(0);
        assert!(matches!(
            node,
            LoweredModuleASTNode::Assign(
                LValue::NamedValue(ValueId(1)),
                RValue {
                    span: _,
                    kind: RValueKind::MethodCall(ValueId(0), _, _)
                }
            )
        ));

        if let LoweredModuleASTNode::Assign(
            _,
            RValue {
                span: _,
                kind: RValueKind::MethodCall(_, name, args),
            },
        ) = node
        {
            assert_eq!(name, "to-vector");
            assert_eq!(args.len(), 1);
            assert_eq!(args.get(0).unwrap(), &RValueTerminal::Integer(2));
        } else {
            unreachable!();
        }

        // $tmp0;
        assert!(matches!(
            module.nodes.remove(0),
            LoweredModuleASTNode::Statement(RValue {
                span: _,
                kind: RValueKind::NamedValue(ValueId(1))
            })
        ));
    }
    */
}
