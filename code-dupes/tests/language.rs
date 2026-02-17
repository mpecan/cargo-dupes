mod common;

use common::{code_dupes, fixture_path};
use predicates::prelude::*;

#[test]
fn explicit_language_rust() {
    code_dupes()
        .args([
            "--path",
            fixture_path("exact_dupes").to_str().unwrap(),
            "--language",
            "rust",
            "stats",
        ])
        .assert()
        .success()
        .stdout(predicate::str::contains("Total code units analyzed"));
}

#[test]
fn auto_detect_rust_from_rs_files() {
    // Fixture directories contain .rs files, so Rust should be auto-detected
    code_dupes()
        .args([
            "--path",
            fixture_path("exact_dupes").to_str().unwrap(),
            "stats",
        ])
        .assert()
        .success()
        .stdout(predicate::str::contains("Total code units analyzed"));
}

#[test]
fn error_on_empty_directory() {
    let tmp = tempfile::TempDir::new().unwrap();
    code_dupes()
        .args(["--path", tmp.path().to_str().unwrap(), "stats"])
        .assert()
        .code(2)
        .stderr(predicate::str::contains("No recognized source files"));
}

#[test]
fn error_on_directory_with_unknown_files_only() {
    let tmp = tempfile::TempDir::new().unwrap();
    std::fs::write(tmp.path().join("readme.txt"), "hello").unwrap();
    std::fs::write(tmp.path().join("data.csv"), "a,b,c").unwrap();
    code_dupes()
        .args(["--path", tmp.path().to_str().unwrap(), "stats"])
        .assert()
        .code(2)
        .stderr(predicate::str::contains("No recognized source files"));
}

#[test]
fn invalid_language_shows_error() {
    code_dupes()
        .args([
            "--path",
            fixture_path("exact_dupes").to_str().unwrap(),
            "--language",
            "unknown",
            "stats",
        ])
        .assert()
        .code(2)
        .stderr(predicate::str::contains("invalid value"));
}

#[test]
fn no_cargo_subcommand_arg_needed() {
    // Unlike cargo-dupes, code-dupes should NOT require a hidden first arg
    code_dupes()
        .args([
            "--path",
            fixture_path("exact_dupes").to_str().unwrap(),
            "stats",
        ])
        .assert()
        .success();
}

#[test]
fn explicit_language_on_empty_dir_reports_no_source_files() {
    let tmp = tempfile::TempDir::new().unwrap();
    code_dupes()
        .args([
            "--path",
            tmp.path().to_str().unwrap(),
            "--language",
            "rust",
            "stats",
        ])
        .assert()
        .code(2)
        .stderr(predicate::str::contains("No source files"));
}

#[test]
fn auto_detect_ignores_non_rust_files() {
    // Directory with .rs + other files should still auto-detect Rust
    let tmp = tempfile::TempDir::new().unwrap();
    let src = tmp.path().join("src");
    std::fs::create_dir(&src).unwrap();
    std::fs::write(
        src.join("lib.rs"),
        "pub fn hello() { println!(\"hello\"); }\npub fn world() { println!(\"world\"); }\n",
    )
    .unwrap();
    std::fs::write(tmp.path().join("readme.txt"), "some text").unwrap();
    std::fs::write(tmp.path().join("data.csv"), "a,b,c").unwrap();
    code_dupes()
        .args(["--path", tmp.path().to_str().unwrap(), "stats"])
        .assert()
        .success()
        .stdout(predicate::str::contains("Total code units analyzed"));
}

#[test]
fn auto_detect_finds_deeply_nested_rs_files() {
    let tmp = tempfile::TempDir::new().unwrap();
    let deep = tmp.path().join("a").join("b").join("c");
    std::fs::create_dir_all(&deep).unwrap();
    std::fs::write(
        deep.join("lib.rs"),
        "pub fn deep() { println!(\"deep\"); }\n",
    )
    .unwrap();
    code_dupes()
        .args(["--path", tmp.path().to_str().unwrap(), "stats"])
        .assert()
        .success()
        .stdout(predicate::str::contains("Total code units analyzed"));
}
