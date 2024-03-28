use std::collections::HashMap;

use crate::{
    errors::CompilerError,
    phases::{
        frontend::{
            self,
            ir::{EnvironmentType, Expression, ModuleItemKind, Statement, StatementKind},
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
    pub nodes: Vec<LoweredModuleASTNode>,
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
    nodes: Vec<LoweredModuleASTNode>,
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
                    type_depends_on: Vec::new(),
                },
            );
            ctx.scopes
                .last_mut()
                .unwrap()
                .insert(identifier.name.clone(), value_id);

            if let Some(rhs) = convert_expression(ctx, initializer) {
                ctx.nodes
                    .push(LoweredModuleASTNode::VariableDeclaration(value_id));
                ctx.nodes.push(LoweredModuleASTNode::Assign(
                    LValue::NamedValue(value_id),
                    rhs,
                ));
            } else {
                ctx.nodes
                    .push(LoweredModuleASTNode::VariableDeclaration(value_id));
            }
        }
        StatementKind::Expression(expr) => {
            if let Some(r_value) = convert_expression(ctx, expr) {
                ctx.nodes.push(LoweredModuleASTNode::Statement(r_value));
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
                        type_depends_on: Vec::new(),
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
                    type_depends_on: Vec::new(),
                },
            );

            ctx.nodes
                .push(LoweredModuleASTNode::StartFunction(id, parameter_ids));

            ctx.scopes.push(function_scope);
            for stmt in body.statements {
                convert_statement(ctx, stmt);
            }
            if let Some(expr) = body.return_expression {
                if let Some(value) = convert_expression(ctx, expr) {
                    ctx.nodes.push(LoweredModuleASTNode::Return(Some(value)));
                };
            }
            ctx.scopes.pop();

            ctx.nodes.push(LoweredModuleASTNode::EndFunction);
        }
        StatementKind::Return(expr) => {
            let return_value = expr.and_then(|expr| convert_expression(ctx, expr));
            ctx.nodes.push(LoweredModuleASTNode::Return(return_value));
        }
        StatementKind::Loop(body) => {
            ctx.nodes.push(LoweredModuleASTNode::StartLoop);
            for stmt in body {
                convert_statement(ctx, stmt);
            }
            ctx.nodes.push(LoweredModuleASTNode::EndLoop);
        }
        _ => {}
    };
}

fn convert_expression(ctx: &mut LoweredModuleContext, expr: Expression) -> Option<RValue> {
    match expr.kind {
        frontend::ir::ExpressionKind::Boolean(b) => Some(RValue {
            span: expr.span,
            kind: RValueKind::Bool(b),
        }),
        frontend::ir::ExpressionKind::Integer(n) => Some(RValue {
            span: expr.span,
            kind: RValueKind::Integer(n),
        }),
        frontend::ir::ExpressionKind::String(s) => Some(RValue {
            span: expr.span,
            kind: RValueKind::String(s),
        }),
        frontend::ir::ExpressionKind::Identifier(id) => find_value(&ctx.scopes, &id.name)
            .map(|value_id| RValue {
                span: id.span,
                kind: RValueKind::NamedValue(value_id),
            })
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
                            type_depends_on: Vec::new(),
                        },
                    );

                    ctx.nodes
                        .push(LoweredModuleASTNode::VariableDeclaration(value_id));

                    ctx.nodes.push(LoweredModuleASTNode::Assign(
                        LValue::NamedValue(value_id),
                        RValue {
                            span: expr.span.clone(),
                            kind: RValueKind::BinOp(lhs, op, rhs),
                        },
                    ));

                    Some(RValue {
                        span: expr.span,
                        kind: RValueKind::NamedValue(value_id),
                    })
                }
                _ => None,
            }
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
                        .push(LoweredModuleASTNode::StartDeclaredEnvironment(
                            item.span, env_type,
                        ));

                    for stmt in statements {
                        convert_statement(&mut ctx, stmt);
                    }

                    ctx.nodes.push(LoweredModuleASTNode::EndDeclaredEnvironment);
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
    type_depends_on: Vec<ValueId>,
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
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct RValue {
    span: Span,
    kind: RValueKind,
}

impl RValue {
    fn new(span: Span, kind: RValueKind) -> Self {
        Self { span, kind }
    }

    fn to_terminal(self) -> Option<RValueTerminal> {
        match self.kind {
            RValueKind::NamedValue(id) => Some(RValueTerminal::NamedValue(id)),
            RValueKind::Integer(n) => Some(RValueTerminal::Integer(n)),
            RValueKind::String(s) => Some(RValueTerminal::String(s)),
            RValueKind::Bool(b) => Some(RValueTerminal::Bool(b)),
            RValueKind::MethodCall(_, _) => None,
            RValueKind::FnCall(_, _) => None,
            RValueKind::BinOp(_, _, _) => None,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum RValueKind {
    NamedValue(ValueId),
    Integer(i32),
    String(String),
    Bool(bool),
    MethodCall(LValue, Vec<RValue>),
    FnCall(ValueId, Vec<RValue>),
    BinOp(RValueTerminal, BinOp, RValueTerminal),
}

#[derive(Debug, PartialEq, Eq)]
pub enum LoweredModuleASTNode {
    VariableDeclaration(ValueId),
    Assign(LValue, RValue),
    Statement(RValue),
    Return(Option<RValue>),
    StartFunction(ValueId, Vec<ValueId>),
    EndFunction,
    StartDeclaredEnvironment(Span, EnvironmentType),
    EndDeclaredEnvironment,
    StartLoop,
    EndLoop,
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{errors::CompilerErrorReason, phases::frontend::ir::Module};

    fn create_module(source: &str) -> Module {
        Module {
            path: String::new(),
            errors: Vec::new(),
            ast: crate::phases::frontend::parser::ModuleParser::new()
                .parse("<test>.nux", source)
                .unwrap()
                .into(),
        }
    }

    #[test]
    fn can_lower_empty_ast() {
        let module = create_module("");
        let expected = Vec::<LoweredModuleASTNode>::new();
        assert_eq!(expected, LoweredModule::from(module).nodes);
    }

    #[test]
    fn lowers_single_statements() {
        let module = create_module("1;");
        let expected = vec![LoweredModuleASTNode::Statement(RValue {
            span: Span { start: 0, end: 1 },
            kind: RValueKind::Integer(1),
        })];
        assert_eq!(expected, LoweredModule::from(module).nodes);

        let module = create_module("'a';");
        let expected = vec![LoweredModuleASTNode::Statement(RValue {
            span: Span { start: 0, end: 3 },
            kind: RValueKind::String("a".to_string()),
        })];
        assert_eq!(expected, LoweredModule::from(module).nodes);

        let module = create_module("true;");
        let expected = vec![LoweredModuleASTNode::Statement(RValue {
            span: Span { start: 0, end: 4 },
            kind: RValueKind::Bool(true),
        })];
        assert_eq!(expected, LoweredModule::from(module).nodes);
    }

    #[test]
    fn reports_an_error_when_an_unrecognized_identifier_is_used() {
        let module = create_module("id;");
        let errors = LoweredModule::from(module).errors;
        assert_eq!(errors.len(), 1);

        let error = errors.get(0).unwrap();
        assert_eq!(
            error.to_error_code(),
            CompilerErrorReason::ReferenceError {
                identifier: String::new()
            }
            .to_error_code()
        );
    }

    #[test]
    fn does_not_report_an_error_when_a_declared_variable_is_used() {
        let module = create_module("let id = 0; id;");
        let lowered_module = LoweredModule::from(module);
        assert_eq!(lowered_module.errors.len(), 0);

        assert!(lowered_module.name_bindings.contains_key(&ValueId(0)));
    }

    #[test]
    fn turns_var_declarations_in_to_assignments() {
        let module = create_module("let id = 32;");
        let lowered_module = LoweredModule::from(module);
        assert_eq!(lowered_module.errors.len(), 0);

        let value_id = match lowered_module.nodes.get(0).unwrap() {
            LoweredModuleASTNode::VariableDeclaration(value_id) => value_id,
            _ => {
                panic!(
                    "Expected to find a variable declaration, found {:?}",
                    lowered_module.nodes.get(0).unwrap()
                );
            }
        };

        match lowered_module.nodes.get(1).unwrap() {
            LoweredModuleASTNode::Assign(LValue::NamedValue(lhs), rhs) => {
                assert_eq!(value_id, lhs);
                assert_eq!(rhs.kind, RValueKind::Integer(32));
            }
            _ => {
                panic!(
                    "Expected an assignment, found {:?}",
                    lowered_module.nodes.get(1).unwrap()
                );
            }
        }
    }

    #[test]
    fn turns_complex_expressions_in_to_sequences_of_assignments() {
        let module = create_module("1 + 2 * 3;");
        let lowered_module = LoweredModule::from(module);
        assert_eq!(lowered_module.errors.len(), 0);

        // let $tmp1 = 2 * 3;
        // let $tmp2 = 1 + $tmp1;
        assert!(matches!(
            lowered_module.nodes.get(0),
            Some(LoweredModuleASTNode::VariableDeclaration(_,))
        ));
        assert!(matches!(
            lowered_module.nodes.get(1),
            Some(LoweredModuleASTNode::Assign(
                _,
                RValue {
                    span: _,
                    kind: RValueKind::BinOp(
                        RValueTerminal::Integer(2),
                        BinOp::Mul,
                        RValueTerminal::Integer(3)
                    )
                }
            ))
        ));
        assert!(matches!(
            lowered_module.nodes.get(2),
            Some(LoweredModuleASTNode::VariableDeclaration(_,))
        ));
        assert!(matches!(
            lowered_module.nodes.get(3),
            Some(LoweredModuleASTNode::Assign(
                _,
                RValue {
                    span: _,
                    kind: RValueKind::BinOp(
                        RValueTerminal::Integer(1),
                        BinOp::Add,
                        RValueTerminal::NamedValue(_)
                    )
                }
            ))
        ));
    }

    #[test]
    fn handles_variable_declarations_with_complex_expressions() {
        let module = create_module("let n = 1 + 2 * 3;");
        let lowered_module = LoweredModule::from(module);
        assert_eq!(lowered_module.errors.len(), 0);

        // let $tmp1 = 2 * 3;
        // let $tmp2 = 1 + $tmp1;
        // let n = $tmp2;
        assert!(matches!(
            lowered_module.nodes.get(0),
            Some(LoweredModuleASTNode::VariableDeclaration(_,))
        ));
        assert!(matches!(
            lowered_module.nodes.get(1),
            Some(LoweredModuleASTNode::Assign(
                _,
                RValue {
                    span: _,
                    kind: RValueKind::BinOp(
                        RValueTerminal::Integer(2),
                        BinOp::Mul,
                        RValueTerminal::Integer(3)
                    )
                }
            ))
        ));
        assert!(matches!(
            lowered_module.nodes.get(2),
            Some(LoweredModuleASTNode::VariableDeclaration(_,))
        ));
        assert!(matches!(
            lowered_module.nodes.get(3),
            Some(LoweredModuleASTNode::Assign(
                _,
                RValue {
                    span: _,
                    kind: RValueKind::BinOp(
                        RValueTerminal::Integer(1),
                        BinOp::Add,
                        RValueTerminal::NamedValue(_)
                    )
                }
            ))
        ));
        assert!(matches!(
            lowered_module.nodes.get(4),
            Some(LoweredModuleASTNode::VariableDeclaration(_,))
        ));
        assert!(matches!(
            lowered_module.nodes.get(5),
            Some(LoweredModuleASTNode::Assign(
                _,
                RValue {
                    span: _,
                    kind: RValueKind::NamedValue(_),
                }
            ))
        ));
    }

    #[test]
    fn recognizes_environment_blocks() {
        let module = create_module("back {} front {}");
        let lowered_module = LoweredModule::from(module);
        assert_eq!(lowered_module.errors.len(), 0);

        assert!(matches!(
            lowered_module.nodes.get(0),
            Some(LoweredModuleASTNode::StartDeclaredEnvironment(
                _,
                EnvironmentType::Backend
            ))
        ));
        assert!(matches!(
            lowered_module.nodes.get(1),
            Some(LoweredModuleASTNode::EndDeclaredEnvironment)
        ));
        assert!(matches!(
            lowered_module.nodes.get(2),
            Some(LoweredModuleASTNode::StartDeclaredEnvironment(
                _,
                EnvironmentType::Frontend
            ))
        ));
        assert!(matches!(
            lowered_module.nodes.get(1),
            Some(LoweredModuleASTNode::EndDeclaredEnvironment)
        ));
    }

    #[test]
    fn assigns_declared_type_for_variable_declarations() {
        let module = create_module("let i: int = 1;");
        let lowered_module = LoweredModule::from(module);
        assert_eq!(lowered_module.errors.len(), 0);

        let symbol = lowered_module.name_bindings.get(&ValueId(0)).unwrap();
        assert_eq!(symbol.declared_type, Some(Type::Int));
    }

    #[test]
    fn flattens_empty_functions() {
        let module = create_module("fn noop() {}");
        let lowered_module = LoweredModule::from(module);
        assert_eq!(lowered_module.errors.len(), 0);

        assert!(matches!(
            lowered_module.nodes.get(0),
            Some(LoweredModuleASTNode::StartFunction(_, _))
        ));
        assert!(matches!(
            lowered_module.nodes.get(1),
            Some(LoweredModuleASTNode::EndFunction)
        ));

        let symbol = lowered_module.name_bindings.get(&ValueId(0)).unwrap();
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
        let module = create_module("fn id(n: int): int { n }");
        let lowered_module = LoweredModule::from(module);
        assert_eq!(lowered_module.errors.len(), 0);

        assert!(matches!(
            lowered_module.nodes.get(0),
            Some(LoweredModuleASTNode::StartFunction(_, _))
        ));
        assert!(matches!(
            lowered_module.nodes.get(1),
            Some(LoweredModuleASTNode::Return(Some(RValue {
                span: _,
                kind: RValueKind::NamedValue(ValueId(1))
            })))
        ));
        assert!(matches!(
            lowered_module.nodes.get(2),
            Some(LoweredModuleASTNode::EndFunction)
        ));

        let symbol = lowered_module.name_bindings.get(&ValueId(0)).unwrap();
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
        let module = create_module("fn noop() { return; }");
        let lowered_module = LoweredModule::from(module);
        assert_eq!(lowered_module.errors.len(), 0);

        assert!(matches!(
            lowered_module.nodes.get(0),
            Some(LoweredModuleASTNode::StartFunction(_, _))
        ));
        assert!(matches!(
            lowered_module.nodes.get(1),
            Some(LoweredModuleASTNode::Return(None))
        ));
        assert!(matches!(
            lowered_module.nodes.get(2),
            Some(LoweredModuleASTNode::EndFunction)
        ));

        let module = create_module("fn get_5() { return 5; }");
        let lowered_module = LoweredModule::from(module);
        assert_eq!(lowered_module.errors.len(), 0);

        assert!(matches!(
            lowered_module.nodes.get(0),
            Some(LoweredModuleASTNode::StartFunction(_, _))
        ));
        assert!(matches!(
            lowered_module.nodes.get(1),
            Some(LoweredModuleASTNode::Return(Some(RValue {
                span: _,
                kind: RValueKind::Integer(5)
            })))
        ));
        assert!(matches!(
            lowered_module.nodes.get(2),
            Some(LoweredModuleASTNode::EndFunction)
        ));
    }

    #[test]
    fn handles_loops() {
        let module = create_module("loop { 1; }");
        let lowered_module = LoweredModule::from(module);
        assert_eq!(lowered_module.errors.len(), 0);

        assert!(matches!(
            lowered_module.nodes.get(0),
            Some(LoweredModuleASTNode::StartLoop)
        ));
        assert!(matches!(
            lowered_module.nodes.get(1),
            Some(LoweredModuleASTNode::Statement(RValue {
                span: _,
                kind: RValueKind::Integer(1)
            }))
        ));
        assert!(matches!(
            lowered_module.nodes.get(2),
            Some(LoweredModuleASTNode::EndLoop)
        ));
    }
}
