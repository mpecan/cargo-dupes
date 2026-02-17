mod common;

use common::{code_dupes, fixture_path};
use predicates::prelude::*;

#[test]
fn check_no_thresholds_passes_with_duplicates() {
    code_dupes()
        .args([
            "--path",
            fixture_path("exact_dupes").to_str().unwrap(),
            "check",
        ])
        .assert()
        .success()
        .stdout(predicate::str::contains("Check passed"));
}

#[test]
fn check_fails_with_duplicates() {
    code_dupes()
        .args([
            "--path",
            fixture_path("exact_dupes").to_str().unwrap(),
            "check",
            "--max-exact",
            "0",
        ])
        .assert()
        .code(1)
        .stdout(predicate::str::contains("Check FAILED"));
}

#[test]
fn check_passes_with_high_threshold() {
    code_dupes()
        .args([
            "--path",
            fixture_path("exact_dupes").to_str().unwrap(),
            "check",
            "--max-exact",
            "100",
        ])
        .assert()
        .success()
        .stdout(predicate::str::contains("Check passed"));
}

#[test]
fn check_no_dupes_passes() {
    code_dupes()
        .args([
            "--path",
            fixture_path("no_dupes").to_str().unwrap(),
            "check",
            "--max-exact",
            "0",
        ])
        .assert()
        .success()
        .stdout(predicate::str::contains("Check passed"));
}

#[test]
fn check_fails_with_percentage_threshold_exceeded() {
    code_dupes()
        .args([
            "--path",
            fixture_path("exact_dupes").to_str().unwrap(),
            "check",
            "--max-exact",
            "100",
            "--max-exact-percent",
            "0.0",
        ])
        .assert()
        .code(1)
        .stdout(predicate::str::contains("Check FAILED"))
        .stdout(predicate::str::contains("exact duplicate lines"));
}

#[test]
fn check_passes_with_generous_percentage_threshold() {
    code_dupes()
        .args([
            "--path",
            fixture_path("exact_dupes").to_str().unwrap(),
            "check",
            "--max-exact",
            "100",
            "--max-exact-percent",
            "100.0",
        ])
        .assert()
        .success()
        .stdout(predicate::str::contains("Check passed"));
}

#[test]
fn check_absolute_passes_percentage_fails() {
    code_dupes()
        .args([
            "--path",
            fixture_path("exact_dupes").to_str().unwrap(),
            "check",
            "--max-exact",
            "100",
            "--max-exact-percent",
            "0.0",
        ])
        .assert()
        .code(1)
        .stdout(predicate::str::contains("Check FAILED"));
}
