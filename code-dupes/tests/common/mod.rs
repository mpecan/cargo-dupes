use assert_cmd::cargo::cargo_bin_cmd;
use std::path::PathBuf;

pub fn fixture_path(name: &str) -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("../cargo-dupes/tests/fixtures")
        .join(name)
}

pub fn code_dupes() -> assert_cmd::Command {
    cargo_bin_cmd!("code-dupes")
}
