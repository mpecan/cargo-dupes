mod common;

use common::{cargo_dupes, fixture_path};
use predicates::prelude::*;

#[test]
fn min_nodes_option() {
    // With very high min_nodes, nothing should be analyzed
    cargo_dupes()
        .args([
            "--path",
            fixture_path("exact_dupes").to_str().unwrap(),
            "--min-nodes",
            "1000",
            "stats",
        ])
        .assert()
        .success()
        .stdout(predicate::str::contains("Exact duplicates: 0 groups"));
}

#[test]
fn min_lines_option() {
    // With very high min_lines, short functions should be excluded
    cargo_dupes()
        .args([
            "--path",
            fixture_path("exact_dupes").to_str().unwrap(),
            "--min-lines",
            "1000",
            "stats",
        ])
        .assert()
        .success()
        .stdout(predicate::str::contains("Exact duplicates: 0 groups"));
}

#[test]
fn exclude_option() {
    // When all files are excluded, the tool reports no source files found
    cargo_dupes()
        .args([
            "--path",
            fixture_path("exact_dupes").to_str().unwrap(),
            "--exclude",
            "lib.rs",
            "stats",
        ])
        .assert()
        .code(2)
        .stderr(predicate::str::contains("No source files"));
}

#[test]
fn exclude_tests_flag_reduces_duplicates() {
    // Without --exclude-tests: 3 units in 1 group (2 production + 1 in #[cfg(test)] mod)
    let output_all = cargo_dupes()
        .args([
            "--path",
            fixture_path("test_code").to_str().unwrap(),
            "--format",
            "json",
            "stats",
        ])
        .assert()
        .success()
        .get_output()
        .stdout
        .clone();
    let all: serde_json::Value =
        serde_json::from_str(&String::from_utf8(output_all).unwrap()).unwrap();
    assert_eq!(all["exact_duplicate_units"].as_u64().unwrap(), 3);

    // With --exclude-tests: only 2 production units remain
    let output_excl = cargo_dupes()
        .args([
            "--path",
            fixture_path("test_code").to_str().unwrap(),
            "--exclude-tests",
            "--format",
            "json",
            "stats",
        ])
        .assert()
        .success()
        .get_output()
        .stdout
        .clone();
    let excl: serde_json::Value =
        serde_json::from_str(&String::from_utf8(output_excl).unwrap()).unwrap();
    assert_eq!(excl["exact_duplicate_units"].as_u64().unwrap(), 2);
    assert_eq!(excl["total_code_units"].as_u64().unwrap(), 2);
}

#[test]
fn exclude_tests_text_report() {
    cargo_dupes()
        .args([
            "--path",
            fixture_path("test_code").to_str().unwrap(),
            "--exclude-tests",
            "report",
        ])
        .assert()
        .success()
        .stdout(predicate::str::contains("Exact Duplicates"))
        .stdout(predicate::str::contains("Group 1"));
}

#[test]
fn error_on_nonexistent_path() {
    cargo_dupes()
        .args(["--path", "/nonexistent/path/that/does/not/exist", "stats"])
        .assert()
        .code(2)
        .stderr(predicate::str::contains("No source files"));
}

#[test]
fn help_works() {
    cargo_dupes()
        .arg("--help")
        .assert()
        .success()
        .stdout(predicate::str::contains("Detect duplicate code"));
}
