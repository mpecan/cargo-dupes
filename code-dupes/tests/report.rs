mod common;

use common::{code_dupes, fixture_path};
use predicates::prelude::*;

#[test]
fn report_exact_dupes_fixture() {
    code_dupes()
        .args([
            "--path",
            fixture_path("exact_dupes").to_str().unwrap(),
            "report",
        ])
        .assert()
        .success()
        .stdout(predicate::str::contains("Exact Duplicates"))
        .stdout(predicate::str::contains("Group 1"));
}

#[test]
fn report_no_dupes_fixture() {
    code_dupes()
        .args([
            "--path",
            fixture_path("no_dupes").to_str().unwrap(),
            "report",
        ])
        .assert()
        .success()
        .stdout(predicate::str::contains("No exact duplicates"));
}

#[test]
fn report_mixed_fixture() {
    code_dupes()
        .args(["--path", fixture_path("mixed").to_str().unwrap(), "report"])
        .assert()
        .success()
        .stdout(predicate::str::contains("Exact Duplicates"))
        .stdout(predicate::str::contains("Group 1"));
}

#[test]
fn stats_shows_summary() {
    code_dupes()
        .args([
            "--path",
            fixture_path("exact_dupes").to_str().unwrap(),
            "stats",
        ])
        .assert()
        .success()
        .stdout(predicate::str::contains("Total code units analyzed"))
        .stdout(predicate::str::contains("Exact duplicates"));
}

#[test]
fn stats_shows_duplicate_lines() {
    code_dupes()
        .args([
            "--path",
            fixture_path("exact_dupes").to_str().unwrap(),
            "stats",
        ])
        .assert()
        .success()
        .stdout(predicate::str::contains("Duplicated lines (exact):"))
        .stdout(predicate::str::contains("Duplicated lines (near):"));
}

#[test]
fn default_command_is_report() {
    code_dupes()
        .args(["--path", fixture_path("exact_dupes").to_str().unwrap()])
        .assert()
        .success()
        .stdout(predicate::str::contains("Duplication Statistics"))
        .stdout(predicate::str::contains("Exact Duplicates"));
}

#[test]
fn near_dupes_detected() {
    code_dupes()
        .args([
            "--path",
            fixture_path("near_dupes").to_str().unwrap(),
            "--threshold",
            "0.7",
            "report",
        ])
        .assert()
        .success()
        .stdout(predicate::str::contains("Near Duplicates"))
        .stdout(predicate::str::contains("Group 1"))
        .stdout(predicate::str::contains("similarity:"));
}

#[test]
fn json_format_stats() {
    let output = code_dupes()
        .args([
            "--path",
            fixture_path("exact_dupes").to_str().unwrap(),
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
    assert!(parsed["total_code_units"].as_u64().unwrap() > 0);
}

#[test]
fn json_format_report() {
    let output = code_dupes()
        .args([
            "--path",
            fixture_path("exact_dupes").to_str().unwrap(),
            "--format",
            "json",
            "report",
        ])
        .assert()
        .success()
        .get_output()
        .stdout
        .clone();
    let text = String::from_utf8(output).unwrap();
    // JSON report outputs stats object then groups array, separated by newlines
    let parts: Vec<&str> = text.splitn(2, "\n\n").collect();
    assert!(parts.len() >= 2, "expected stats + groups sections");
    let stats: serde_json::Value = serde_json::from_str(parts[0]).unwrap();
    assert!(stats["total_code_units"].as_u64().unwrap() > 0);
    assert!(stats["exact_duplicate_groups"].as_u64().unwrap() > 0);
    let groups: serde_json::Value = serde_json::from_str(parts[1]).unwrap();
    assert!(groups.as_array().unwrap().len() > 0);
    assert!(groups[0]["fingerprint"].is_string());
    assert!(groups[0]["members"].is_array());
}

#[test]
fn json_stats_includes_line_counts() {
    let output = code_dupes()
        .args([
            "--path",
            fixture_path("exact_dupes").to_str().unwrap(),
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
    assert!(parsed["exact_duplicate_lines"].is_u64());
    assert!(parsed["near_duplicate_lines"].is_u64());
}
