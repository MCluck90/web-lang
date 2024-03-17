use lalrpop_util::lalrpop_mod;

lalrpop_mod!(pub parser);

#[test]
fn example_files() {
    use parser::ModuleParser;

    let basic_type_checking_source = include_str!("../../../examples/basic-type-checking.nux");
    let _ = ModuleParser::new()
        .parse("basic-type-checking.nux", basic_type_checking_source)
        .unwrap();

    let block_expressions_source = include_str!("../../../examples/block-expressions.nux");
    let _ = ModuleParser::new()
        .parse("block-expressions.nux", block_expressions_source)
        .unwrap();

    let fibonacci_source = include_str!("../../../examples/fibonacci.nux");
    let _ = ModuleParser::new()
        .parse("fibonacci.nux", fibonacci_source)
        .unwrap();

    let hello_world_source = include_str!("../../../examples/hello-world.nux");
    let _ = ModuleParser::new()
        .parse("hello-world.nux", hello_world_source)
        .unwrap();

    let if_expressions_source = include_str!("../../../examples/if-expressions.nux");
    let _ = ModuleParser::new()
        .parse("if-expressions.nux", if_expressions_source)
        .unwrap();

    let lists_source = include_str!("../../../examples/lists.nux");
    let _ = ModuleParser::new()
        .parse("lists.nux", lists_source)
        .unwrap();

    let property_access_source = include_str!("../../../examples/property-access.nux");
    let _ = ModuleParser::new()
        .parse("property-access.nux", property_access_source)
        .unwrap();

    let rule_90_source = include_str!("../../../examples/rule-90.nux");
    let _ = ModuleParser::new()
        .parse("rule-90.nux", rule_90_source)
        .unwrap();

    let rule_110_source = include_str!("../../../examples/rule-110.nux");
    let _ = ModuleParser::new()
        .parse("rule-110.nux", rule_110_source)
        .unwrap();

    let strings_source = include_str!("../../../examples/strings.nux");
    let _ = ModuleParser::new()
        .parse("strings.nux", strings_source)
        .unwrap();

    let unary_operators_source = include_str!("../../../examples/unary-operators.nux");
    let _ = ModuleParser::new()
        .parse("unary-operators.nux", unary_operators_source)
        .unwrap();

    let variable_declarations_source = include_str!("../../../examples/variable-declarations.nux");
    let _ = ModuleParser::new()
        .parse("variable-declarations.nux", variable_declarations_source)
        .unwrap();
}
