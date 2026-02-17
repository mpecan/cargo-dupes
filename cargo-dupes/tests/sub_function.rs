mod common;

use common::{cargo_dupes, fixture_path};
use predicates::prelude::*;

#[test]
fn sub_function_detects_duplicate_branches() {
    cargo_dupes()
        .args([
            "--path",
            fixture_path("sub_function_dupes").to_str().unwrap(),
            "--sub-function",
            "report",
        ])
        .assert()
        .success()
        .stdout(predicate::str::contains("Sub-function Exact Duplicates"))
        .stdout(predicate::str::contains("if-then branch"))
        .stdout(predicate::str::contains("match arm"))
        .stdout(predicate::str::contains("for body"));
}

#[test]
fn sub_function_shows_parent_names() {
    cargo_dupes()
        .args([
            "--path",
            fixture_path("sub_function_dupes").to_str().unwrap(),
            "--sub-function",
            "report",
        ])
        .assert()
        .success()
        .stdout(predicate::str::contains("in handle_positive"))
        .stdout(predicate::str::contains("in process_value"))
        .stdout(predicate::str::contains("in classify_number"))
        .stdout(predicate::str::contains("in describe_value"));
}

#[test]
fn sub_function_stats_shown() {
    cargo_dupes()
        .args([
            "--path",
            fixture_path("sub_function_dupes").to_str().unwrap(),
            "--sub-function",
            "stats",
        ])
        .assert()
        .success()
        .stdout(predicate::str::contains("Sub-function exact: 3 groups"));
}

#[test]
fn sub_function_json_stats() {
    let output = cargo_dupes()
        .args([
            "--path",
            fixture_path("sub_function_dupes").to_str().unwrap(),
            "--sub-function",
            "--format",
            "json",
            "stats",
        ])
        .assert()
        .success()
        .get_output()
        .stdout
        .clone();
    let text = String::from_utf8(output).unwrap();
    let parsed: serde_json::Value = serde_json::from_str(&text).unwrap();
    assert_eq!(parsed["sub_exact_groups"].as_u64().unwrap(), 3);
    assert_eq!(parsed["sub_exact_units"].as_u64().unwrap(), 6);
}

#[test]
fn without_sub_function_flag_no_sub_sections() {
    cargo_dupes()
        .args([
            "--path",
            fixture_path("sub_function_dupes").to_str().unwrap(),
            "report",
        ])
        .assert()
        .success()
        .stdout(predicate::str::contains("Exact Duplicates"))
        .stdout(predicate::str::contains("Sub-function").not());
}

#[test]
fn without_sub_function_json_no_sub_fields() {
    let output = cargo_dupes()
        .args([
            "--path",
            fixture_path("sub_function_dupes").to_str().unwrap(),
            "--format",
            "json",
            "stats",
        ])
        .assert()
        .success()
        .get_output()
        .stdout
        .clone();
    let text = String::from_utf8(output).unwrap();
    let parsed: serde_json::Value = serde_json::from_str(&text).unwrap();
    // Without --sub-function, sub fields should be absent (skip_serializing_if = "is_zero")
    assert!(parsed.get("sub_exact_groups").is_none());
    assert!(parsed.get("sub_near_groups").is_none());
}

#[test]
fn sub_function_min_sub_nodes_filters() {
    // With very high min-sub-nodes, no sub-function units should be found,
    // so the sub-function stats line is not printed at all
    cargo_dupes()
        .args([
            "--path",
            fixture_path("sub_function_dupes").to_str().unwrap(),
            "--sub-function",
            "--min-sub-nodes",
            "1000",
            "stats",
        ])
        .assert()
        .success()
        .stdout(predicate::str::contains("Sub-function").not());
}
