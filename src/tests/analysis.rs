use crate::parse_query;

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
