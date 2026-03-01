use std::path::PathBuf;

use dupes_core::analyzer::LanguageAnalyzer;
use dupes_core::config::AnalysisConfig;
use dupes_core::grouper::{compute_stats, find_near_duplicates, group_exact_duplicates};
use dupes_core::similarity::similarity_score;
use dupes_python::PythonAnalyzer;

fn default_config() -> AnalysisConfig {
    AnalysisConfig {
        min_nodes: 1,
        min_lines: 1,
    }
}

fn parse(source: &str) -> Vec<dupes_core::code_unit::CodeUnit> {
    let analyzer = PythonAnalyzer::new();
    analyzer
        .parse_file(&PathBuf::from("test.py"), source, &default_config())
        .expect("parse should succeed")
}

#[test]
fn exact_duplicates_grouped() {
    let units = parse(
        r#"
def add(a, b):
    result = a + b
    return result

def add2(x, y):
    result = x + y
    return result

def mul(a, b):
    result = a * b
    return result
"#,
    );
    assert_eq!(units.len(), 3);
    let groups = group_exact_duplicates(&units);
    // add and add2 are exact duplicates
    assert_eq!(groups.len(), 1);
    assert_eq!(groups[0].members.len(), 2);
}

#[test]
fn no_exact_duplicates_when_all_different() {
    let units = parse(
        r#"
def add(a, b):
    return a + b

def mul(a, b):
    return a * b

def div(a, b):
    return a / b
"#,
    );
    assert_eq!(units.len(), 3);
    let groups = group_exact_duplicates(&units);
    assert!(groups.is_empty());
}

#[test]
fn near_duplicates_found() {
    let units = parse(
        r#"
def process_add(a, b):
    result = a + b
    x = result * 2
    return x

def process_mul(a, b):
    result = a * b
    x = result * 2
    return x
"#,
    );
    assert_eq!(units.len(), 2);
    let exact = group_exact_duplicates(&units);
    assert!(exact.is_empty(), "these should not be exact duplicates");

    let exact_fps: Vec<_> = exact.iter().map(|g| g.fingerprint).collect();
    let near = find_near_duplicates(&units, 0.5, &exact_fps);
    assert_eq!(near.len(), 1, "should find one near-duplicate group");
}

#[test]
fn similarity_score_identical_bodies() {
    let units = parse(
        r#"
def foo(a, b):
    return a + b

def bar(x, y):
    return x + y
"#,
    );
    assert_eq!(units.len(), 2);
    let score = similarity_score(&units[0].body, &units[1].body);
    assert!(
        (score - 1.0).abs() < f64::EPSILON,
        "identical bodies should have score 1.0, got {score}"
    );
}

#[test]
fn similarity_score_different_bodies() {
    let units = parse(
        r#"
def simple(a):
    return a

def complex(a, b, c):
    x = a + b
    y = x * c
    z = y - a
    return z
"#,
    );
    assert_eq!(units.len(), 2);
    let score = similarity_score(&units[0].body, &units[1].body);
    assert!(
        score < 0.5,
        "very different bodies should have low score, got {score}"
    );
}

#[test]
fn compute_stats_with_exact_duplicates() {
    let units = parse(
        r#"
def add(a, b):
    result = a + b
    return result

def add2(x, y):
    result = x + y
    return result
"#,
    );
    let exact = group_exact_duplicates(&units);
    let exact_fps: Vec<_> = exact.iter().map(|g| g.fingerprint).collect();
    let near = find_near_duplicates(&units, 0.8, &exact_fps);
    let stats = compute_stats(&units, &exact, &near);
    assert!(stats.exact_duplicate_groups > 0);
    assert!(stats.exact_duplicate_units > 0);
    assert!(stats.exact_duplicate_lines > 0);
}

#[test]
fn compute_stats_no_duplicates() {
    let units = parse(
        r#"
def add(a, b):
    return a + b

def mul(a, b):
    return a * b
"#,
    );
    let exact = group_exact_duplicates(&units);
    let exact_fps: Vec<_> = exact.iter().map(|g| g.fingerprint).collect();
    let near = find_near_duplicates(&units, 0.8, &exact_fps);
    let stats = compute_stats(&units, &exact, &near);
    assert_eq!(stats.exact_duplicate_groups, 0);
    assert_eq!(stats.exact_duplicate_units, 0);
    assert_eq!(stats.exact_duplicate_lines, 0);
}

#[test]
fn is_test_code_through_trait() {
    let analyzer = PythonAnalyzer::new();
    let units = parse(
        r#"
def test_something():
    assert 1 == 1

def regular():
    return 42
"#,
    );
    assert!(analyzer.is_test_code(&units[0]));
    assert!(!analyzer.is_test_code(&units[1]));
}

#[test]
fn analyze_end_to_end_with_exclude_tests() {
    let analyzer = PythonAnalyzer::new();
    let tmp = tempfile::TempDir::new().unwrap();
    let py_file = tmp.path().join("example.py");
    std::fs::write(
        &py_file,
        r#"
def add(a, b):
    result = a + b
    return result

def add2(x, y):
    result = x + y
    return result

def test_add():
    assert add(1, 2) == 3

def test_add2():
    assert add2(1, 2) == 3
"#,
    )
    .unwrap();

    let files = vec![py_file];

    // Without excluding tests — use low thresholds so test functions are included
    let mut config_with_tests = dupes_core::config::Config::default();
    config_with_tests.exclude_tests = false;
    config_with_tests.min_nodes = 1;
    config_with_tests.min_lines = 1;
    let result_with =
        dupes_core::analyze(&analyzer, &files, &config_with_tests).expect("analyze should succeed");
    let total_with = result_with.stats.total_code_units;

    // With excluding tests
    let mut config_no_tests = dupes_core::config::Config::default();
    config_no_tests.exclude_tests = true;
    config_no_tests.min_nodes = 1;
    config_no_tests.min_lines = 1;
    let result_without =
        dupes_core::analyze(&analyzer, &files, &config_no_tests).expect("analyze should succeed");
    let total_without = result_without.stats.total_code_units;

    assert!(
        total_with > total_without,
        "excluding tests should reduce unit count: {total_with} vs {total_without}"
    );
}
