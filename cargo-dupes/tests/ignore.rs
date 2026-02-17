mod common;

use common::{cargo_dupes, fixture_path};
use predicates::prelude::*;

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

#[test]
fn ignore_near_duplicate_workflow() {
    let tmp = tempfile::TempDir::new().unwrap();
    // Copy fixture files to temp dir
    std::fs::create_dir_all(tmp.path().join("src")).unwrap();
    std::fs::copy(
        fixture_path("near_dupes").join("src/lib.rs"),
        tmp.path().join("src/lib.rs"),
    )
    .unwrap();

    // Get report to find a near-duplicate fingerprint
    let output = cargo_dupes()
        .args([
            "--path",
            tmp.path().to_str().unwrap(),
            "--threshold",
            "0.7",
            "report",
        ])
        .assert()
        .success()
        .get_output()
        .stdout
        .clone();
    let text = String::from_utf8(output).unwrap();

    // Extract fingerprint from near-duplicate group
    let fp = text
        .lines()
        .find(|l| l.contains("fingerprint:") && l.contains("similarity:"))
        .and_then(|l| {
            let start = l.find("fingerprint: ")? + 13;
            let end = l[start..].find(',')?;
            Some(l[start..start + end].to_string())
        })
        .expect("Should find a fingerprint in near-duplicate group");

    // Add it to ignore
    cargo_dupes()
        .args([
            "--path",
            tmp.path().to_str().unwrap(),
            "ignore",
            &fp,
            "--reason",
            "near dupe ignore test",
        ])
        .assert()
        .success()
        .stdout(predicate::str::contains("Added"));

    // Verify the near-duplicate group is now filtered out
    let output_after = cargo_dupes()
        .args([
            "--path",
            tmp.path().to_str().unwrap(),
            "--threshold",
            "0.7",
            "stats",
        ])
        .assert()
        .success()
        .get_output()
        .stdout
        .clone();
    let text_after = String::from_utf8(output_after).unwrap();
    assert!(text_after.contains("Near duplicates:  0 groups"));
}

#[test]
fn cleanup_removes_stale_entries() {
    let tmp = tempfile::TempDir::new().unwrap();
    std::fs::create_dir_all(tmp.path().join("src")).unwrap();
    std::fs::copy(
        fixture_path("exact_dupes").join("src/lib.rs"),
        tmp.path().join("src/lib.rs"),
    )
    .unwrap();

    // Get a real fingerprint from the report
    let output = cargo_dupes()
        .args(["--path", tmp.path().to_str().unwrap(), "report"])
        .assert()
        .success()
        .get_output()
        .stdout
        .clone();
    let text = String::from_utf8(output).unwrap();

    let real_fp = text
        .lines()
        .find(|l| l.contains("fingerprint:"))
        .and_then(|l| {
            let start = l.find("fingerprint: ")? + 13;
            let end = l[start..].find(',')?;
            Some(l[start..start + end].to_string())
        })
        .expect("Should find a fingerprint");

    // Ignore the real fingerprint
    cargo_dupes()
        .args(["--path", tmp.path().to_str().unwrap(), "ignore", &real_fp])
        .assert()
        .success();

    // Add a fake/stale fingerprint manually
    let ignore_path = tmp.path().join(".dupes-ignore.toml");
    let content = std::fs::read_to_string(&ignore_path).unwrap();
    let new_content = format!(
        "{content}\n[[ignore]]\nfingerprint = \"deadbeefdeadbeef\"\nreason = \"stale entry\"\n"
    );
    std::fs::write(&ignore_path, new_content).unwrap();

    // Run cleanup
    cargo_dupes()
        .args(["--path", tmp.path().to_str().unwrap(), "cleanup"])
        .assert()
        .success()
        .stdout(predicate::str::contains("Removed stale entries"))
        .stdout(predicate::str::contains("deadbeefdeadbeef"))
        .stdout(predicate::str::contains("Removed 1 stale entries"));

    // Verify the real fingerprint is still in the ignore file
    cargo_dupes()
        .args(["--path", tmp.path().to_str().unwrap(), "ignored"])
        .assert()
        .success()
        .stdout(predicate::str::contains(&real_fp));

    // Verify the stale entry is gone
    let final_content = std::fs::read_to_string(&ignore_path).unwrap();
    assert!(!final_content.contains("deadbeefdeadbeef"));
}

#[test]
fn cleanup_dry_run() {
    let tmp = tempfile::TempDir::new().unwrap();
    std::fs::create_dir_all(tmp.path().join("src")).unwrap();
    std::fs::copy(
        fixture_path("exact_dupes").join("src/lib.rs"),
        tmp.path().join("src/lib.rs"),
    )
    .unwrap();

    // Create an ignore file with a stale fingerprint only
    let ignore_path = tmp.path().join(".dupes-ignore.toml");
    std::fs::write(
        &ignore_path,
        "[[ignore]]\nfingerprint = \"deadbeefdeadbeef\"\nreason = \"stale\"\n",
    )
    .unwrap();

    // Run cleanup with --dry-run
    cargo_dupes()
        .args([
            "--path",
            tmp.path().to_str().unwrap(),
            "cleanup",
            "--dry-run",
        ])
        .assert()
        .success()
        .stdout(predicate::str::contains("Stale entries (dry run)"))
        .stdout(predicate::str::contains("deadbeefdeadbeef"))
        .stdout(predicate::str::contains("would be removed"));

    // Verify the file is unchanged
    let content = std::fs::read_to_string(&ignore_path).unwrap();
    assert!(content.contains("deadbeefdeadbeef"));
}
