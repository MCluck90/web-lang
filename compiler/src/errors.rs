use ariadne::{Color, Label, Report, ReportKind, Source};

use crate::{
    phases::frontend::{ir::BinaryOperator, lexer, Span, Token},
    types::symbol_table::TypeSymbol,
};

#[derive(Clone, Debug, Hash)]
pub struct CompilerError {
    span: Span,
    reason: CompilerErrorReason,
}
impl chumsky::Error<char> for CompilerError {
    type Span = lexer::Span;

    type Label = String;

    fn expected_input_found<Iter: IntoIterator<Item = Option<char>>>(
        span: Self::Span,
        expected: Iter,
        found: Option<char>,
    ) -> Self {
        Self::unexpected_character(&span, expected.into_iter().collect(), found)
    }

    fn with_label(self, label: Self::Label) -> Self {
        eprintln!(
            "Attempted to add label `{}` but was ignored. Here's where you need to change:\n{}:{}",
            label,
            file!(),
            line!()
        );
        self
    }

    fn merge(self, other: Self) -> Self {
        merge_errors(self, other)
    }
}
impl chumsky::Error<Token> for CompilerError {
    type Span = lexer::Span;

    type Label = String;

    fn expected_input_found<Iter: IntoIterator<Item = Option<Token>>>(
        span: Self::Span,
        expected: Iter,
        found: Option<Token>,
    ) -> Self {
        Self::unexpected_token(&span, expected.into_iter().collect(), found)
    }

    fn with_label(self, label: Self::Label) -> Self {
        eprintln!(
            "Attempted to add label `{}` but was ignored. Here's where you need to change:\n{}:{}",
            label,
            file!(),
            line!()
        );
        self
    }

    fn merge(self, other: Self) -> Self {
        merge_errors(self, other)
    }
}

impl CompilerError {
    pub fn unexpected_character(
        span: &Span,
        expected: Vec<Option<char>>,
        found: Option<char>,
    ) -> CompilerError {
        CompilerError {
            span: span.clone(),
            reason: CompilerErrorReason::UnexpectedCharacter { expected, found },
        }
    }

    pub fn unexpected_token(
        span: &Span,
        expected: Vec<Option<Token>>,
        found: Option<Token>,
    ) -> CompilerError {
        CompilerError {
            span: span.clone(),
            reason: CompilerErrorReason::UnexpectedToken {
                found: found,
                expected: expected.clone(),
            },
        }
    }

    pub fn reference_error(span: &Span, identifier: &String) -> CompilerError {
        CompilerError {
            span: span.clone(),
            reason: CompilerErrorReason::ReferenceError {
                identifier: identifier.clone(),
            },
        }
    }

    pub fn invalid_return_value(
        span: &Span,
        expected: &TypeSymbol,
        found: &TypeSymbol,
    ) -> CompilerError {
        CompilerError {
            span: span.clone(),
            reason: CompilerErrorReason::InvalidReturnValue {
                expected: expected.clone(),
                found: found.clone(),
            },
        }
    }

    pub fn binary_operator_not_supported_on_type(
        span: &Span,
        operator: &BinaryOperator,
        found: &TypeSymbol,
    ) -> CompilerError {
        CompilerError {
            span: span.clone(),
            reason: CompilerErrorReason::BinaryOperatorNotSupportedOnTypeSymbol {
                operator: operator.clone(),
                found: found.clone(),
            },
        }
    }

    pub fn unary_not_operator_not_supported_on_type(
        span: &Span,
        found: &TypeSymbol,
    ) -> CompilerError {
        CompilerError {
            span: span.clone(),
            reason: CompilerErrorReason::UnaryNotOperatorNotSupportedOnTypeSymbol {
                found: found.clone(),
            },
        }
    }

    pub fn invalid_rhs_expression_in_prefix_operation(span: &Span) -> CompilerError {
        CompilerError {
            span: span.clone(),
            reason: CompilerErrorReason::InvalidRhsExpressionInPrefixOperation,
        }
    }

    pub fn mismatched_types(
        span: &Span,
        expected: &TypeSymbol,
        found: &TypeSymbol,
    ) -> CompilerError {
        CompilerError {
            span: span.clone(),
            reason: CompilerErrorReason::MismatchedTypeSymbols {
                expected: expected.clone(),
                found: found.clone(),
            },
        }
    }

    pub fn assignment_to_immutable_variable(span: &Span, identifier: &String) -> CompilerError {
        CompilerError {
            span: span.clone(),
            reason: CompilerErrorReason::AssignmentToImmutableVariable {
                identifier: identifier.clone(),
            },
        }
    }

    pub fn type_cannot_be_called_as_a_function(span: &Span, type_: &TypeSymbol) -> CompilerError {
        CompilerError {
            span: span.clone(),
            reason: CompilerErrorReason::TypeCannotBeCalledAsAFunction(type_.clone()),
        }
    }

    pub fn invalid_arguments(
        span: &Span,
        expected: Vec<TypeSymbol>,
        found: Vec<TypeSymbol>,
    ) -> CompilerError {
        CompilerError {
            span: span.clone(),
            reason: CompilerErrorReason::InvalidArguments { expected, found },
        }
    }

    pub fn if_branch_incompatiable_types(
        span: &Span,
        expected: &TypeSymbol,
        found: &TypeSymbol,
    ) -> CompilerError {
        CompilerError {
            span: span.clone(),
            reason: CompilerErrorReason::IfBranchIncompatiableTypes {
                expected: expected.clone(),
                found: found.clone(),
            },
        }
    }

    pub fn no_field_on_type(span: &Span, field: &String, type_: &TypeSymbol) -> CompilerError {
        CompilerError {
            span: span.clone(),
            reason: CompilerErrorReason::NoFieldOnType {
                field: field.clone(),
                type_: type_.clone(),
            },
        }
    }

    pub fn invalid_lhs_in_assignment(span: &Span) -> CompilerError {
        CompilerError {
            span: span.clone(),
            reason: CompilerErrorReason::InvalidLhsInAssignment,
        }
    }

    pub fn could_not_find_module(span: &Span, module_path: &String) -> CompilerError {
        CompilerError {
            span: span.clone(),
            reason: CompilerErrorReason::CouldNotFindModule {
                path: module_path.clone(),
            },
        }
    }

    pub fn invalid_import(span: &Span, import_path: &String, identifier: &String) -> CompilerError {
        CompilerError {
            span: span.clone(),
            reason: CompilerErrorReason::InvalidImport {
                import_path: import_path.clone(),
                identifier: identifier.clone(),
            },
        }
    }

    pub fn mixed_types_in_list(span: &Span, types: &Vec<TypeSymbol>) -> CompilerError {
        CompilerError {
            span: span.clone(),
            reason: CompilerErrorReason::MixedTypesInList {
                types: types.clone(),
            },
        }
    }

    pub fn to_error_code(&self) -> i32 {
        self.reason.to_error_code()
    }

    pub fn to_message(&self) -> String {
        self.reason.to_message()
    }

    pub fn to_label(&self, path: &str) -> Label<(String, Span)> {
        self.reason.to_label(path, &self.span)
    }
}

#[derive(Clone, Debug, Hash)]
pub enum CompilerErrorReason {
    // Ex: Unexpected character: `(char)`
    UnexpectedCharacter {
        expected: Vec<Option<char>>,
        found: Option<char>,
    },

    // Ex: Unexpected (found), expected (list of possible tokens)
    UnexpectedToken {
        found: Option<Token>,
        expected: Vec<Option<Token>>,
    },

    // Ex: Unknown identifier `(identifier)`
    ReferenceError {
        identifier: String,
    },

    // Ex: Function expected to return (expected) but found (found)
    InvalidReturnValue {
        expected: TypeSymbol,
        found: TypeSymbol,
    },

    // Ex: (operator) is not supported on type (found)
    BinaryOperatorNotSupportedOnTypeSymbol {
        operator: BinaryOperator,
        found: TypeSymbol,
    },

    // Ex: Mismatched types. Expected `(expected)`, found `(found)`
    MismatchedTypeSymbols {
        expected: TypeSymbol,
        found: TypeSymbol,
    },

    // Ex: Cannot reassign value of immutable variable `(identifier)`
    AssignmentToImmutableVariable {
        identifier: String,
    },

    // Ex: type `{}` cannot be called as a function
    TypeCannotBeCalledAsAFunction(TypeSymbol),

    // Ex: Expected arguments to be: `(found_arg_types)`, received: `(found_arg_types)`",
    InvalidArguments {
        expected: Vec<TypeSymbol>,
        found: Vec<TypeSymbol>,
    },

    // Ex: `if` and `else` have incompatible types expected `(expected)`, found `(found)`
    IfBranchIncompatiableTypes {
        expected: TypeSymbol,
        found: TypeSymbol,
    },

    // Ex: no field `(field)` on type `(type_)`
    NoFieldOnType {
        field: String,
        type_: TypeSymbol,
    },

    // Ex: Invalid left-hand side in assignment
    InvalidLhsInAssignment,

    CouldNotFindModule {
        path: String,
    },

    // Ex: Could not find `(identifier)` in module at `(import_path)`
    InvalidImport {
        import_path: String,
        identifier: String,
    },

    // Ex: Lists can only contain one type. Found `(types)`
    MixedTypesInList {
        types: Vec<TypeSymbol>,
    },

    // Ex: Unary not (`!`) is not supported on type `{}`. Can only be used on boolean values
    UnaryNotOperatorNotSupportedOnTypeSymbol {
        found: TypeSymbol,
    },

    // Ex: Invalid right-hand side expression in prefix operation
    InvalidRhsExpressionInPrefixOperation,
}
impl CompilerErrorReason {
    pub fn to_error_code(&self) -> i32 {
        match self {
            CompilerErrorReason::UnexpectedToken { .. } => 0,
            CompilerErrorReason::ReferenceError { .. } => 1,
            CompilerErrorReason::InvalidReturnValue { .. } => 2,
            CompilerErrorReason::BinaryOperatorNotSupportedOnTypeSymbol { .. } => 3,
            CompilerErrorReason::MismatchedTypeSymbols { .. } => 4,
            CompilerErrorReason::UnexpectedCharacter { .. } => 5,
            CompilerErrorReason::AssignmentToImmutableVariable { .. } => 6,
            CompilerErrorReason::TypeCannotBeCalledAsAFunction(_) => 7,
            CompilerErrorReason::InvalidArguments { .. } => 8,
            CompilerErrorReason::IfBranchIncompatiableTypes { .. } => 9,
            CompilerErrorReason::NoFieldOnType { .. } => 10,
            CompilerErrorReason::InvalidLhsInAssignment => 11,
            CompilerErrorReason::CouldNotFindModule { .. } => 12,
            CompilerErrorReason::InvalidImport { .. } => 13,
            CompilerErrorReason::MixedTypesInList { .. } => 14,
            CompilerErrorReason::UnaryNotOperatorNotSupportedOnTypeSymbol { .. } => 15,
            CompilerErrorReason::InvalidRhsExpressionInPrefixOperation => 16,
        }
    }

    pub fn to_message(&self) -> String {
        match self {
            CompilerErrorReason::UnexpectedCharacter { expected, found } => {
                format!(
                    "Unexpected {}, expected {}",
                    match found {
                        Some(found) => format!("{}", found),
                        None => "end of input".into(),
                    },
                    if expected.is_empty() {
                        "end of input".into()
                    } else {
                        expected
                            .iter()
                            .map(|c| {
                                c.map(|c| c.to_string())
                                    .unwrap_or("end of input".to_string())
                            })
                            .collect::<Vec<_>>()
                            .join(", ")
                    }
                )
            }
            CompilerErrorReason::UnexpectedToken { found, expected } => {
                let expected = expected
                    .iter()
                    .filter_map(|e| e.as_ref())
                    .collect::<Vec<_>>();
                if found.is_none() && expected.is_empty() {
                    "Unexpected end of input".to_string()
                } else {
                    format!(
                        "Unexpected {}, expected {}",
                        match found {
                            Some(found) => found.to_unexpected_error_message(),
                            None => "end of input".into(),
                        },
                        if expected.is_empty() {
                            "something else".into()
                        } else {
                            expected
                                .iter()
                                .map(|token| token.to_string())
                                .collect::<Vec<_>>()
                                .join(", ")
                        }
                    )
                }
            }
            CompilerErrorReason::ReferenceError { identifier } => {
                format!("Cannot find value `{}` in this scope", identifier)
            }
            CompilerErrorReason::InvalidReturnValue { expected, found } => format!(
                "Invalid return value. Expected `{}`, found `{}`",
                expected, found
            ),
            CompilerErrorReason::BinaryOperatorNotSupportedOnTypeSymbol { operator, found } => {
                format!(
                    "{} is not supported for type `{}`",
                    match operator {
                        BinaryOperator::Add => "addition",
                        BinaryOperator::Sub => "subtraction",
                        BinaryOperator::Mul => "multiplication",
                        BinaryOperator::Div => "division",
                        BinaryOperator::Modulus => "modulus",
                        BinaryOperator::NotEqual => "equality checking",
                        BinaryOperator::Equal => "equality checking",
                        BinaryOperator::LessThan => "order comparison",
                        BinaryOperator::LessThanOrEqual => "order comparison",
                        BinaryOperator::GreaterThan => "order comparison",
                        BinaryOperator::GreaterThanOrEqual => "order comparison",
                        BinaryOperator::And => "boolean AND",
                        BinaryOperator::Or => "boolean OR",
                        BinaryOperator::Assignment => "assignment",
                    },
                    found
                )
            }
            CompilerErrorReason::MismatchedTypeSymbols { expected, found } => format!(
                "Mismatched types: expected `{}`, found `{}`",
                expected, found
            ),
            CompilerErrorReason::AssignmentToImmutableVariable { identifier } => format!(
                "Cannot reassign value of immutable variable `{}`",
                identifier
            ),
            CompilerErrorReason::TypeCannotBeCalledAsAFunction(type_) => {
                format!("type `{}` cannot be called as a function", type_)
            }
            CompilerErrorReason::InvalidArguments { expected, found } => format!(
                "Expected arguments to be: ({}), received: ({})",
                expected
                    .iter()
                    .map(|typ| format!("{}", typ))
                    .collect::<Vec<_>>()
                    .join(", "),
                found
                    .iter()
                    .map(|typ| format!("{}", typ))
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            CompilerErrorReason::IfBranchIncompatiableTypes { expected, found } => format!(
                "`if` and `else` have incompatible types expected `{}`, found `{}`",
                expected.type_, found.type_
            ),
            CompilerErrorReason::NoFieldOnType { field, type_ } => {
                format!("no field `{}` on type `{}`", field, type_)
            }
            CompilerErrorReason::InvalidLhsInAssignment => {
                "Invalid left-hand side in assignment".into()
            }
            CompilerErrorReason::CouldNotFindModule { path } => {
                format!("Could not find module at path: {}", path)
            }
            CompilerErrorReason::InvalidImport {
                import_path,
                identifier,
            } => format!(
                "Could not find `{}` in module at `{}`",
                identifier, import_path
            ),
            CompilerErrorReason::MixedTypesInList { types } => format!(
                "Lists can only contain one type. Found {}",
                types
                    .iter()
                    .map(|t| format!("`{}`", t))
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            CompilerErrorReason::UnaryNotOperatorNotSupportedOnTypeSymbol { found } => format!(
                "Unary not (`!`) is not supported on type `{}`. Can only be used on boolean values",
                found
            ),
            CompilerErrorReason::InvalidRhsExpressionInPrefixOperation => {
                "Invalid right-hand side expression in prefix operation".to_owned()
            }
        }
    }

    pub fn to_label(&self, path: &str, span: &Span) -> Label<(String, Span)> {
        let label = Label::new((path.to_string(), span.clone()));
        match self {
            CompilerErrorReason::UnexpectedCharacter { expected, .. } => {
                let expected = expected
                    .iter()
                    .filter_map(|c| c.map(|c| c.to_string()))
                    .collect::<Vec<_>>();
                label
                    .with_message(if expected.is_empty() {
                        "Unexpected character".to_string()
                    } else {
                        expected.join(", ")
                    })
                    .with_color(Color::Red)
            }
            CompilerErrorReason::UnexpectedToken { expected, .. } => {
                let expected = expected
                    .iter()
                    .filter_map(|c| c.as_ref().map(|c| c.to_string()))
                    .collect::<Vec<_>>();
                label
                    .with_message(if expected.is_empty() {
                        "expected something else".to_string()
                    } else {
                        expected.join(", ")
                    })
                    .with_color(Color::Red)
            }
            CompilerErrorReason::ReferenceError { .. } => label
                .with_message("not found in this scope")
                .with_color(Color::Red),
            CompilerErrorReason::InvalidReturnValue { expected, .. } => label
                .with_message(format!("expected {}", expected))
                .with_color(Color::Red),
            CompilerErrorReason::BinaryOperatorNotSupportedOnTypeSymbol { .. } => label
                .with_message("invalid operation")
                .with_color(Color::Red),
            CompilerErrorReason::MismatchedTypeSymbols { expected, .. } => label
                .with_message(format!("expected `{}`", expected))
                .with_color(Color::Red),
            CompilerErrorReason::AssignmentToImmutableVariable { .. } => label
                .with_message("immutable variable")
                .with_color(Color::Red),
            CompilerErrorReason::TypeCannotBeCalledAsAFunction(type_) => label
                .with_message(format!("type `{}` cannot be called as a function", type_))
                .with_color(Color::Red),
            CompilerErrorReason::InvalidArguments { .. } => label
                .with_message("invalid argument(s)")
                .with_color(Color::Red),
            CompilerErrorReason::IfBranchIncompatiableTypes { expected, .. } => label
                .with_message(format!("expected `{}`", expected))
                .with_color(Color::Red),
            CompilerErrorReason::NoFieldOnType { type_, .. } => label
                .with_message(format!("does not exist on `{}`", type_))
                .with_color(Color::Red),
            CompilerErrorReason::InvalidLhsInAssignment => label.with_color(Color::Red),
            CompilerErrorReason::CouldNotFindModule { .. } => label.with_color(Color::Red),
            CompilerErrorReason::InvalidImport { import_path, .. } => label
                .with_message(format!("{} does not export this identifier", import_path))
                .with_color(Color::Red),
            CompilerErrorReason::MixedTypesInList { .. } => label.with_color(Color::Red),
            CompilerErrorReason::UnaryNotOperatorNotSupportedOnTypeSymbol { .. } => label
                .with_message("this value is not a boolean")
                .with_color(Color::Red),
            CompilerErrorReason::InvalidRhsExpressionInPrefixOperation => {
                label.with_color(Color::Red)
            }
        }
    }
}

pub fn print_error_report<'a>(module_path: &String, errors: &Vec<CompilerError>) {
    let src = std::fs::read_to_string(&module_path).unwrap_or(String::new());
    for error in errors {
        let report = Report::build(ReportKind::Error, module_path, error.span.start)
            .with_code(error.to_error_code())
            .with_message(error.to_message())
            .with_label(error.to_label(&module_path));

        report
            .finish()
            .eprint((module_path.clone(), Source::from(&src)))
            .unwrap();
    }
}

fn merge_errors(left: CompilerError, right: CompilerError) -> CompilerError {
    use CompilerErrorReason::*;
    match (left.reason.clone(), right.reason.clone()) {
        (
            UnexpectedCharacter {
                expected: expected_left,
                found: found_left,
            },
            UnexpectedCharacter {
                expected: expected_right,
                ..
            },
        ) => {
            let mut expected = if expected_left.is_empty() {
                vec![None]
            } else {
                expected_left
            };

            for expected_char in expected_right {
                if !expected.contains(&expected_char) {
                    expected.push(expected_char);
                }
            }

            CompilerError {
                span: left.span,
                reason: UnexpectedCharacter {
                    expected,
                    found: found_left,
                },
            }
        }
        (
            UnexpectedToken {
                expected: expected_left,
                found: found_left,
            },
            UnexpectedToken {
                expected: expected_right,
                ..
            },
        ) => {
            let mut expected = if expected_left.is_empty() {
                vec![None]
            } else {
                expected_left
            };

            for expected_char in expected_right {
                if !expected.contains(&expected_char) {
                    expected.push(expected_char);
                }
            }

            CompilerError {
                span: left.span,
                reason: UnexpectedToken {
                    expected,
                    found: found_left,
                },
            }
        }
        _ => unimplemented!(
            "Unhandled compiler merge scenario: {:?} vs {:?}",
            left,
            right
        ),
    }
}
