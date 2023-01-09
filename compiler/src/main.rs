mod code_gen;
mod lexer;
mod parser;
mod passes;

use std::process;

use ariadne::{Color, Fmt, Label, Report, ReportKind, Source};
use chumsky::{prelude::*, Stream};
use lexer::lexer;
use parser::main_parser;
use passes::transform_ast;

fn main() {
    let file_path = std::env::args().nth(1).unwrap();
    let src = std::fs::read_to_string(&file_path).unwrap();
    let (tokens, lex_errs) = lexer().parse_recovery(src.as_str());
    let mut errors = lex_errs
        .clone()
        .into_iter()
        .map(|e| e.map(|c| c.to_string()))
        .collect::<Vec<Simple<String>>>();
    let parse_errors = if let Some(tokens) = tokens {
        let len = src.chars().count();
        let (program, parse_errs) =
            main_parser().parse_recovery(Stream::from_iter(len..len + 1, tokens.into_iter()));

        let mut parse_errs = parse_errs
            .into_iter()
            .map(|e| e.map(|token| token.to_string()))
            .collect::<Vec<Simple<String>>>();

        errors.append(&mut parse_errs);

        if let Some(program) = program {
            if lex_errs.is_empty() && parse_errs.is_empty() {
                match transform_ast(program) {
                    Ok(program) => {
                        let output = code_gen::generate_code(&program);
                        if let Some(be_js) = output.js {
                            println!("{}", be_js);
                        }
                    }
                    Err(mut compile_errors) => {
                        errors.append(&mut compile_errors);
                    }
                }
            } else {
                println!("{:?}", program);
            }
        }

        errors
    } else {
        Vec::new()
    };

    let all_errors = lex_errs
        .into_iter()
        .map(|e| e.map(|c| c.to_string()))
        .chain(
            parse_errors
                .into_iter()
                .map(|e| e.map(|token| token.to_string())),
        )
        .collect::<Vec<_>>();

    let has_errors = !all_errors.is_empty();
    all_errors.iter().for_each(|e| {
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
                    Label::new((&file_path, e.span()))
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

        report
            .finish()
            .eprint((&file_path, Source::from(&src)))
            .unwrap();
    });

    if has_errors {
        process::exit(1);
    }
}
