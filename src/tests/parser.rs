use crate::lexer::tokenize;
use crate::parser::parse;

#[test]
fn test_parse_from_events_nested_data() {
    let tokens = tokenize(include_str!("./resources/from_events_nested_data.eql")).unwrap();
    insta::assert_yaml_snapshot!(parse(tokens.as_slice()).unwrap());
}

#[test]
fn test_parse_from_events_using_subquery() {
    let tokens = tokenize(include_str!("./resources/from_events_using_subquery.eql")).unwrap();
    insta::assert_yaml_snapshot!(parse(tokens.as_slice()).unwrap());
}

#[test]
fn test_parse_from_events_where_subject_project_record_with_count() {
    let tokens = tokenize(include_str!(
        "./resources/from_events_where_subject_project_record_with_count.eql"
    ))
    .unwrap();
    insta::assert_yaml_snapshot!(parse(tokens.as_slice()).unwrap());
}

#[test]
fn test_parse_from_events_with_top_identity_projection() {
    let tokens = tokenize(include_str!(
        "./resources/from_events_with_top_identity_projection.eql"
    ))
    .unwrap();
    insta::assert_yaml_snapshot!(parse(tokens.as_slice()).unwrap());
}

#[test]
fn test_parse_from_events_with_type_to_project_record() {
    let tokens = tokenize(include_str!(
        "./resources/from_events_with_type_to_project_record.eql"
    ))
    .unwrap();
    insta::assert_yaml_snapshot!(parse(tokens.as_slice()).unwrap());
}

#[test]
fn test_parse_binary_op() {
    let tokens = tokenize(include_str!("./resources/parser_binary_op.eql")).unwrap();
    insta::assert_yaml_snapshot!(parse(tokens.as_slice()).unwrap());
}

#[test]
fn test_parser_unhinged_unary_op() {
    let tokens = tokenize(include_str!("./resources/parser_unhinged_unary_op.eql")).unwrap();
    insta::assert_yaml_snapshot!(parse(tokens.as_slice()).unwrap());
}

#[test]
fn test_parser_from_events_with_group_by_and_having() {
    let tokens = tokenize(include_str!(
        "./resources/from_events_with_group_by_and_having.eql"
    ))
    .unwrap();
    insta::assert_yaml_snapshot!(parse(tokens.as_slice()).unwrap());
}

#[test]
fn test_parser_from_events_with_distinct() {
    let tokens = tokenize(include_str!("./resources/from_events_with_distinct.eql")).unwrap();
    insta::assert_yaml_snapshot!(parse(tokens.as_slice()).unwrap());
}
