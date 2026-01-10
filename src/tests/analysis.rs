use crate::{parse_query, prelude::AnalysisOptions};

#[test]
fn test_infer_wrong_where_clause_1() {
    let query = parse_query(include_str!("./resources/infer_wrong_where_clause_1.eql")).unwrap();
    insta::assert_yaml_snapshot!(query.run_static_analysis(&Default::default()));
}

#[test]
fn test_infer_wrong_where_clause_2() {
    let query = parse_query(include_str!("./resources/infer_wrong_where_clause_2.eql")).unwrap();
    insta::assert_yaml_snapshot!(query.run_static_analysis(&Default::default()));
}

#[test]
fn test_rename_duplicate_variable_names() {
    let query = parse_query(include_str!(
        "./resources/rename_duplicate_variable_names.eql"
    ))
    .unwrap();
    insta::assert_yaml_snapshot!(query.run_static_analysis(&Default::default()));
}

#[test]
fn test_rename_non_existing_variable() {
    let query = parse_query(include_str!("./resources/rename_non_existing_variable.eql")).unwrap();
    insta::assert_yaml_snapshot!(query.run_static_analysis(&Default::default()));
}

#[test]
fn test_rename_subquery() {
    let query = parse_query(include_str!("./resources/rename_subquery.eql")).unwrap();
    insta::assert_yaml_snapshot!(query.run_static_analysis(&Default::default()));
}

#[test]
fn test_analyze_valid_contains() {
    let query = parse_query(include_str!("./resources/valid_contains.eql")).unwrap();
    insta::assert_yaml_snapshot!(query.run_static_analysis(&Default::default()));
}

#[test]
fn test_analyze_invalid_type_contains() {
    let query = parse_query(include_str!("./resources/invalid_type_contains.eql")).unwrap();
    insta::assert_yaml_snapshot!(query.run_static_analysis(&Default::default()));
}

#[test]
fn test_analyze_valid_type_conversion() {
    let query = parse_query(include_str!("./resources/valid_type_conversion.eql")).unwrap();
    insta::assert_yaml_snapshot!(query.run_static_analysis(&Default::default()));
}

#[test]
fn test_analyze_invalid_type_conversion_custom_type() {
    let query = parse_query(include_str!("./resources/type_conversion_custom_type.eql")).unwrap();
    insta::assert_yaml_snapshot!(query.run_static_analysis(&Default::default()));
}

#[test]
fn test_analyze_valid_type_conversion_custom_type() {
    let query = parse_query(include_str!("./resources/type_conversion_custom_type.eql")).unwrap();
    insta::assert_yaml_snapshot!(
        query.run_static_analysis(&AnalysisOptions::default().add_custom_type("Foobar"))
    );
}

#[test]
fn test_analyze_valid_type_conversion_weird_case() {
    let query = parse_query(include_str!(
        "./resources/valid_type_conversion-weird-case.eql"
    ))
    .unwrap();
    insta::assert_yaml_snapshot!(query.run_static_analysis(&Default::default()));
}

#[test]
fn test_analyze_prevent_using_aggregate_with_source_based_props() {
    let query = parse_query(include_str!(
        "./resources/aggregate_with_sourced_bases_props.eql"
    ))
    .unwrap();
    insta::assert_yaml_snapshot!(query.run_static_analysis(&Default::default()));
}

#[test]
fn test_analyze_valid_agg_usage() {
    let query = parse_query(include_str!("./resources/valid_agg_usage.eql")).unwrap();
    insta::assert_yaml_snapshot!(query.run_static_analysis(&Default::default()));
}

#[test]
fn test_analyze_reject_agg_in_predicate() {
    let query = parse_query(include_str!("./resources/reject_agg_in_predicate.eql")).unwrap();
    insta::assert_yaml_snapshot!(query.run_static_analysis(&Default::default()));
}

#[test]
fn test_analyze_agg_must_use_source_bound() {
    let query = parse_query(include_str!("./resources/agg_must_use_source_bound.eql")).unwrap();
    insta::assert_yaml_snapshot!(query.run_static_analysis(&Default::default()));
}

#[test]
fn test_analyze_optional_param_func() {
    let query = parse_query(include_str!("./resources/optional_param_func.eql")).unwrap();
    insta::assert_yaml_snapshot!(query.run_static_analysis(&Default::default()));
}
