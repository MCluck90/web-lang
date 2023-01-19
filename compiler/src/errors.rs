use ariadne::{Color, Label, Report, ReportKind, Source};

use crate::phases::{
    frontend::{lexer, BinaryOperator, Span, Token},
    shared::Type,
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
        // TODO
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
        // TODO
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

    pub fn invalid_return_value(span: &Span, expected: &Type, found: &Type) -> CompilerError {
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
        found: &Type,
    ) -> CompilerError {
        CompilerError {
            span: span.clone(),
            reason: CompilerErrorReason::BinaryOperatorNotSupportedOnType {
                operator: operator.clone(),
                found: found.clone(),
            },
        }
    }
    pub fn mismatched_types(span: &Span, expected: &Type, found: &Type) -> CompilerError {
        CompilerError {
            span: span.clone(),
            reason: CompilerErrorReason::MismatchedTypes {
                expected: expected.clone(),
                found: found.clone(),
            },
        }
    }
    pub fn custom_warning(span: &Span, message: String) -> CompilerError {
        CompilerError {
            span: span.clone(),
            reason: CompilerErrorReason::CustomWarning(message),
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

    pub fn is_error(&self) -> bool {
        self.reason.is_error()
    }

    pub fn is_warning(&self) -> bool {
        self.reason.is_warning()
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
        expected: Type,
        found: Type,
    },

    // Ex: (operator) is not supported on type (found)
    BinaryOperatorNotSupportedOnType {
        operator: BinaryOperator,
        found: Type,
    },

    // Ex: Mismatched types. Expected `(expected)`, found `(found)`
    MismatchedTypes {
        expected: Type,
        found: Type,
    },

    // Ex: Warning: (message)
    CustomWarning(String),
}
impl CompilerErrorReason {
    pub fn to_error_code(&self) -> i32 {
        match self {
            CompilerErrorReason::UnexpectedToken { .. } => 0,
            CompilerErrorReason::ReferenceError { .. } => 1,
            CompilerErrorReason::InvalidReturnValue { .. } => 2,
            CompilerErrorReason::BinaryOperatorNotSupportedOnType { .. } => 3,
            CompilerErrorReason::MismatchedTypes { .. } => 4,
            CompilerErrorReason::CustomWarning(_) => 5,
            CompilerErrorReason::UnexpectedCharacter { .. } => 6,
        }
    }

    pub fn is_warning(&self) -> bool {
        match self {
            CompilerErrorReason::UnexpectedToken { .. } => false,
            CompilerErrorReason::ReferenceError { .. } => false,
            CompilerErrorReason::InvalidReturnValue { .. } => false,
            CompilerErrorReason::BinaryOperatorNotSupportedOnType { .. } => false,
            CompilerErrorReason::MismatchedTypes { .. } => false,
            CompilerErrorReason::CustomWarning(_) => true,
            CompilerErrorReason::UnexpectedCharacter { .. } => false,
        }
    }

    pub fn is_error(&self) -> bool {
        !self.is_warning()
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
            CompilerErrorReason::InvalidReturnValue { expected, found } => todo!(),
            CompilerErrorReason::BinaryOperatorNotSupportedOnType { operator, found } => todo!(),
            CompilerErrorReason::MismatchedTypes { expected, found } => todo!(),
            CompilerErrorReason::CustomWarning(_) => todo!(),
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

            // label
            //     .with_message(format!(
            //         "Unexpected {}",
            //         match found {
            //             Some(token) => format!("{}", token),
            //             None => "end of input".to_string(),
            //         }
            //     ))
            //     .with_color(Color::Red),
            CompilerErrorReason::ReferenceError { .. } => label
                .with_message("not found in this scope")
                .with_color(Color::Red),
            CompilerErrorReason::InvalidReturnValue { expected, found } => todo!(),
            CompilerErrorReason::BinaryOperatorNotSupportedOnType { operator, found } => todo!(),
            CompilerErrorReason::MismatchedTypes { expected, found } => todo!(),
            CompilerErrorReason::CustomWarning(_) => todo!(),
        }
    }
}

pub fn print_error_report<'a>(module_path: &String, errors: &Vec<CompilerError>) {
    let src = std::fs::read_to_string(&module_path).unwrap();
    for error in errors {
        let report = Report::build(ReportKind::Error, module_path, error.span.start)
            .with_code(error.to_error_code())
            .with_message(error.to_message())
            .with_label(error.to_label(&module_path));

        // chumsky::error::SimpleReason::Unexpected => report
        //     .with_message(format!(
        //         "{}, expected {}",
        //         if error.found().is_some() {
        //             "Unexpected token in input"
        //         } else {
        //             "Unexpected end of input"
        //         },
        //         if error.expected().len() == 0 {
        //             "something else".to_string()
        //         } else {
        //             error
        //                 .expected()
        //                 .map(|expected| match expected {
        //                     Some(expected) => expected.to_string(),
        //                     None => "end of input".to_string(),
        //                 })
        //                 .collect::<Vec<_>>()
        //                 .join(", ")
        //         }
        //     ))
        //     .with_label(
        //         Label::new((&module_path, error.span()))
        //             .with_message(format!(
        //                 "Unexpected token {}",
        //                 error
        //                     .found()
        //                     .unwrap_or(&"end of file".to_string())
        //                     .fg(Color::Red)
        //             ))
        //             .with_color(Color::Red),
        //     ),
        // chumsky::error::SimpleReason::Unclosed { span, delimiter } => report
        //     .with_message(format!(
        //         "Unclosed delimiter {}",
        //         delimiter.fg(Color::Yellow)
        //     ))
        //     .with_label(
        //         Label::new((&module_path, span.clone()))
        //             .with_message(format!(
        //                 "Unclosed delimiter {}",
        //                 delimiter.fg(Color::Yellow)
        //             ))
        //             .with_color(Color::Yellow),
        //     )
        //     .with_label(
        //         Label::new((&module_path, error.span()))
        //             .with_message(
        //                 format!(
        //                     "Must be closed before this {}",
        //                     error
        //                         .found()
        //                         .unwrap_or(&"end of file".to_string())
        //                         .fg(Color::Red)
        //                 )
        //                 .to_string(),
        //             )
        //             .with_color(Color::Red),
        //     ),
        // chumsky::error::SimpleReason::Unexpected => report
        //     .with_message(format!(
        //         "{}, expected {}",
        //         if error.found().is_some() {
        //             "Unexpected token in input"
        //         } else {
        //             "Unexpected end of input"
        //         },
        //         if error.expected().len() == 0 {
        //             "something else".to_string()
        //         } else {
        //             error
        //                 .expected()
        //                 .map(|expected| match expected {
        //                     Some(expected) => expected.to_string(),
        //                     None => "end of input".to_string(),
        //                 })
        //                 .collect::<Vec<_>>()
        //                 .join(", ")
        //         }
        //     ))
        //     .with_label(
        //         Label::new((&module_path, error.span()))
        //             .with_message(format!(
        //                 "Unexpected token {}",
        //                 error
        //                     .found()
        //                     .unwrap_or(&"end of file".to_string())
        //                     .fg(Color::Red)
        //             ))
        //             .with_color(Color::Red),
        //     ),
        // // chumsky::error::SimpleReason::Custom(msg) => report.with_message(msg).with_label(
        // //     Label::new((&file_path, e.span()))
        // //         .with_message(format!("{}", msg.fg(Color::Red)))
        // //         .with_color(Color::Red),
        // // ),
        // chumsky::error::SimpleReason::Custom(msg) => {
        //     let expected = error
        //         .expected()
        //         .map(|expected| {
        //             expected
        //                 .clone()
        //                 .map_or(String::new(), |expectation| expectation.to_string())
        //         })
        //         .collect::<Vec<_>>()
        //         .join(", ");
        //     let expected = if !expected.is_empty() {
        //         Some(expected)
        //     } else {
        //         None
        //     };
        //     let found = error.found().map(|e| e.to_string());

        //     report.with_message(msg).with_label(
        //         Label::new((&module_path, error.span()))
        //             .with_message(match (expected, found) {
        //                 (Some(expected), Some(found)) => {
        //                     format!("expected: {}, found: {}", expected, found)
        //                 }
        //                 (Some(expected), None) => format!("expected {}", expected),
        //                 (None, Some(found)) => format!("found {}", found),
        //                 (None, None) => "".to_string(),
        //             })
        //             .with_color(Color::Red),
        //     )
        // }

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
                expected: mut expected_right,
                ..
            },
        ) => {
            let mut expected = if expected_left.is_empty() {
                vec![None]
            } else {
                expected_left
            };

            // TODO: Filter out duplicates
            expected.append(&mut expected_right);
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
                expected: mut expected_right,
                ..
            },
        ) => {
            let mut expected = if expected_left.is_empty() {
                vec![None]
            } else {
                expected_left
            };

            // TODO: Filter out duplicates
            expected.append(&mut expected_right);
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
