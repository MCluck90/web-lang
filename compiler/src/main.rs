mod lexer;
mod parser;

use ariadne::{Color, Fmt, Label, Report, ReportKind, Source};
use chumsky::{prelude::*, Stream};
use lexer::lexer;
use parser::main_parser;

fn main() {
    let file_path = std::env::args().nth(1).unwrap();
    let src = std::fs::read_to_string(&file_path).unwrap();
    let (tokens, errors) = lexer().parse_recovery(src.as_str());
    let parse_errors = if let Some(tokens) = tokens {
        let len = src.chars().count();
        let (ast, parse_errs) =
            main_parser().parse_recovery(Stream::from_iter(len..len + 1, tokens.into_iter()));

        if let Some(ast) = ast {
            println!("{:?}", ast);
        }

        parse_errs
    } else {
        Vec::new()
    };

    errors
        .into_iter()
        .map(|e| e.map(|c| c.to_string()))
        .chain(
            parse_errors
                .into_iter()
                .map(|e| e.map(|token| token.to_string())),
        )
        .for_each(|e| {
            let report = Report::build(ReportKind::Error, &file_path, e.span().start);

            let report = match e.reason() {
                chumsky::error::SimpleReason::Unclosed { span, delimiter } => report
                    .with_message(format!(
                        "Unclosed delimiter {}",
                        delimiter.fg(Color::Yellow)
                    ))
                    .with_label(
                        Label::new((&file_path, span.clone()))
                            .with_message(format!(
                                "Unclosed delimiter {}",
                                delimiter.fg(Color::Yellow)
                            ))
                            .with_color(Color::Yellow),
                    )
                    .with_label(
                        Label::new((&file_path, e.span()))
                            .with_message(format!(
                                "Must be closed before this {}",
                                e.found()
                                    .unwrap_or(&"end of file".to_string())
                                    .fg(Color::Red)
                            ))
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
                        Label::new((&file_path, e.span()))
                            .with_message(format!(
                                "Unexpected token {}",
                                e.found()
                                    .unwrap_or(&"end of file".to_string())
                                    .fg(Color::Red)
                            ))
                            .with_color(Color::Red),
                    ),
                chumsky::error::SimpleReason::Custom(msg) => report.with_message(msg).with_label(
                    Label::new((&file_path, e.span()))
                        .with_message(format!("{}", msg.fg(Color::Red)))
                        .with_color(Color::Red),
                ),
            };

            report
                .finish()
                .print((&file_path, Source::from(&src)))
                .unwrap();
        });
}
