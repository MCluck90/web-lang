use lalrpop_util::lalrpop_mod;

lalrpop_mod!(pub parser);

#[test]
fn example_files() {
    use parser::ModuleParser;

    let hello_world_source = include_str!("../../../examples/hello-world.nux");
    let _ = ModuleParser::new()
        .parse("hello-world.nux", hello_world_source)
        .unwrap();

    let block_expressions_source = include_str!("../../../examples/block-expressions.nux");
    let _ = ModuleParser::new()
        .parse("block-expressions.nux", block_expressions_source)
        .unwrap();

    let if_expressions_source = include_str!("../../../examples/if-expressions.nux");
    let _ = ModuleParser::new()
        .parse("if-expressions.nux", if_expressions_source)
        .unwrap();

    let strings_source = include_str!("../../../examples/strings.nux");
    let _ = ModuleParser::new()
        .parse("strings.nux", strings_source)
        .unwrap();

    let variable_declarations_source = include_str!("../../../examples/variable-declarations.nux");
    let _ = ModuleParser::new()
        .parse("variable-declarations.nux", variable_declarations_source)
        .unwrap();
}
