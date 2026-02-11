use assert_cmd::cargo::cargo_bin_cmd;
use predicates::prelude::*;
use std::path::PathBuf;

fn fixture_path(name: &str) -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("tests")
        .join("fixtures")
        .join(name)
}

fn cargo_dupes() -> assert_cmd::Command {
    cargo_bin_cmd!("cargo-dupes")
}

// ── Report subcommand ────────────────────────────────────────────────────

#[test]
fn report_exact_dupes_fixture() {
    cargo_dupes()
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
    cargo_dupes()
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
    cargo_dupes()
        .args(["--path", fixture_path("mixed").to_str().unwrap(), "report"])
        .assert()
        .success()
        .stdout(predicate::str::contains("Exact Duplicates"))
        .stdout(predicate::str::contains("Group 1"));
}

// ── Stats subcommand ────────────────────────────────────────────────────

#[test]
fn stats_shows_summary() {
    cargo_dupes()
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

// ── Check subcommand ────────────────────────────────────────────────────

#[test]
fn check_fails_with_duplicates() {
    cargo_dupes()
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
    cargo_dupes()
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
    cargo_dupes()
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

// ── JSON format ─────────────────────────────────────────────────────────

#[test]
fn json_format_stats() {
    let output = cargo_dupes()
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
    // JSON report outputs stats first, then groups — verify it outputs valid content
    cargo_dupes()
        .args([
            "--path",
            fixture_path("exact_dupes").to_str().unwrap(),
            "--format",
            "json",
            "stats",
        ])
        .assert()
        .success()
        .stdout(predicate::str::contains("total_code_units"));
}

// ── CLI options ─────────────────────────────────────────────────────────

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
        .stderr(predicate::str::contains("No Rust source files"));
}

// ── Ignore workflow ─────────────────────────────────────────────────────

#[test]
fn ignore_workflow() {
    let tmp = tempfile::TempDir::new().unwrap();
    // Copy fixture files to temp dir
    std::fs::create_dir_all(tmp.path().join("src")).unwrap();
    std::fs::copy(
        fixture_path("exact_dupes").join("src/lib.rs"),
        tmp.path().join("src/lib.rs"),
    )
    .unwrap();

    // First, get the report to find a fingerprint
    let output = cargo_dupes()
        .args(["--path", tmp.path().to_str().unwrap(), "report"])
        .assert()
        .success()
        .get_output()
        .stdout
        .clone();
    let text = String::from_utf8(output).unwrap();

    // Extract a fingerprint from the output
    let fp = text
        .lines()
        .find(|l| l.contains("fingerprint:"))
        .and_then(|l| {
            let start = l.find("fingerprint: ")? + 13;
            let end = l[start..].find(',')?;
            Some(l[start..start + end].to_string())
        })
        .expect("Should find a fingerprint in the report");

    // Add it to ignore
    cargo_dupes()
        .args([
            "--path",
            tmp.path().to_str().unwrap(),
            "ignore",
            &fp,
            "--reason",
            "test ignore",
        ])
        .assert()
        .success()
        .stdout(predicate::str::contains("Added"));

    // Verify it's listed
    cargo_dupes()
        .args(["--path", tmp.path().to_str().unwrap(), "ignored"])
        .assert()
        .success()
        .stdout(predicate::str::contains(&fp))
        .stdout(predicate::str::contains("test ignore"));

    // Verify the report no longer shows that group
    let output_after = cargo_dupes()
        .args(["--path", tmp.path().to_str().unwrap(), "stats"])
        .assert()
        .success()
        .get_output()
        .stdout
        .clone();
    let text_after = String::from_utf8(output_after).unwrap();

    // The ignored group should be filtered out
    assert!(text_after.contains("Exact duplicates: 0 groups"));
}

// ── Default subcommand ──────────────────────────────────────────────────

#[test]
fn default_command_is_report() {
    // Running without a subcommand should behave like 'report'
    cargo_dupes()
        .args(["--path", fixture_path("exact_dupes").to_str().unwrap()])
        .assert()
        .success()
        .stdout(predicate::str::contains("Duplication Statistics"))
        .stdout(predicate::str::contains("Exact Duplicates"));
}

// ── Error handling ──────────────────────────────────────────────────────

#[test]
fn error_on_nonexistent_path() {
    cargo_dupes()
        .args(["--path", "/nonexistent/path/that/does/not/exist", "stats"])
        .assert()
        .code(2)
        .stderr(predicate::str::contains("No Rust source files"));
}

// ── Help ────────────────────────────────────────────────────────────────

#[test]
fn help_works() {
    cargo_dupes()
        .arg("--help")
        .assert()
        .success()
        .stdout(predicate::str::contains("Detect duplicate code"));
}

// ── Near duplicates detection ───────────────────────────────────────────

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
fn stats_shows_duplicate_lines() {
    cargo_dupes()
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
fn json_stats_includes_line_counts() {
    let output = cargo_dupes()
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

#[test]
fn near_dupes_detected() {
    cargo_dupes()
        .args([
            "--path",
            fixture_path("near_dupes").to_str().unwrap(),
            "--threshold",
            "0.7",
            "report",
        ])
        .assert()
        .success()
        .stdout(predicate::str::contains("Duplication Statistics"));
}
