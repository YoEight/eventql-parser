use crate::lexer::parse_tokens;

#[test]
fn test_lexer_all_kind() {
    let tokens = parse_tokens("foo != 123(]{.:").unwrap();
    insta::assert_yaml_snapshot!(tokens);
}
