use ariadne::{Color, Fmt, Label, Report, ReportKind, Source};
use chumsky::prelude::Simple;

pub type CompilerError = Simple<String>;

pub fn print_error_report<'a>(module_path: &String, errors: &Vec<CompilerError>) {
    errors.iter().for_each(|e| {
        let report = Report::build(ReportKind::Error, &module_path, e.span().start);

        let report = match e.reason() {
            chumsky::error::SimpleReason::Unclosed { span, delimiter } => report
                .with_message(format!(
                    "Unclosed delimiter {}",
                    delimiter.fg(Color::Yellow)
                ))
                .with_label(
                    Label::new((&module_path, span.clone()))
                        .with_message(format!(
                            "Unclosed delimiter {}",
                            delimiter.fg(Color::Yellow)
                        ))
                        .with_color(Color::Yellow),
                )
                .with_label(
                    Label::new((&module_path, e.span()))
                        .with_message(
                            format!(
                                "Must be closed before this {}",
                                e.found()
                                    .unwrap_or(&"end of file".to_string())
                                    .fg(Color::Red)
                            )
                            .to_string(),
                        )
                        .with_color(Color::Red),
                ),
            chumsky::error::SimpleReason::Unexpected => report
                .with_message(format!(
                    "{}, expected {}",
                    if e.found().is_some() {
                        "Unexpected token in input"
                    } else {
                        "Unexpected end of input"
                    },
                    if e.expected().len() == 0 {
                        "something else".to_string()
                    } else {
                        e.expected()
                            .map(|expected| match expected {
                                Some(expected) => expected.to_string(),
                                None => "end of input".to_string(),
                            })
                            .collect::<Vec<_>>()
                            .join(", ")
                    }
                ))
                .with_label(
                    Label::new((&module_path, e.span()))
                        .with_message(format!(
                            "Unexpected token {}",
                            e.found()
                                .unwrap_or(&"end of file".to_string())
                                .fg(Color::Red)
                        ))
                        .with_color(Color::Red),
                ),
            // chumsky::error::SimpleReason::Custom(msg) => report.with_message(msg).with_label(
            //     Label::new((&file_path, e.span()))
            //         .with_message(format!("{}", msg.fg(Color::Red)))
            //         .with_color(Color::Red),
            // ),
            chumsky::error::SimpleReason::Custom(msg) => {
                let expected = e
                    .expected()
                    .map(|expected| {
                        expected
                            .clone()
                            .map_or(String::new(), |expectation| expectation.to_string())
                    })
                    .collect::<Vec<_>>()
                    .join(", ");
                let expected = if !expected.is_empty() {
                    Some(expected)
                } else {
                    None
                };
                let found = e.found().map(|e| e.to_string());

                report.with_message(msg).with_label(
                    Label::new((&module_path, e.span()))
                        .with_message(match (expected, found) {
                            (Some(expected), Some(found)) => {
                                format!("expected: {}, found: {}", expected, found)
                            }
                            (Some(expected), None) => format!("expected {}", expected),
                            (None, Some(found)) => format!("found {}", found),
                            (None, None) => "".to_string(),
                        })
                        .with_color(Color::Red),
                )
            }
        };

        let src = std::fs::read_to_string(&module_path).unwrap();
        report
            .finish()
            .eprint((&module_path, Source::from(&src)))
            .unwrap();
    });
}
