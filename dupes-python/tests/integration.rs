use std::path::PathBuf;

use dupes_core::analyzer::LanguageAnalyzer;
use dupes_core::config::AnalysisConfig;
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

// -- Basic parsing --

#[test]
fn parses_top_level_functions() {
    let units = parse(
        r#"
def add(a, b):
    return a + b

def subtract(a, b):
    return a - b
"#,
    );
    assert_eq!(units.len(), 2);
    assert_eq!(units[0].name, "add");
    assert_eq!(units[1].name, "subtract");
}

#[test]
fn parses_class_methods() {
    let units = parse(
        r#"
class Calculator:
    def add(self, a, b):
        return a + b

    def subtract(self, a, b):
        return a - b
"#,
    );
    assert_eq!(units.len(), 2);
    assert_eq!(units[0].name, "add");
    assert_eq!(units[1].name, "subtract");
}

#[test]
fn parses_nested_functions() {
    let units = parse(
        r#"
def outer(a, b):
    def inner(x):
        return x * 2
    return inner(a) + inner(b)
"#,
    );
    assert_eq!(units.len(), 2);
    assert_eq!(units[0].name, "outer");
    assert_eq!(units[1].name, "inner");
}

#[test]
fn parses_async_functions() {
    let units = parse(
        r#"
async def fetch(url):
    result = await get(url)
    return result

def sync_fetch(url):
    result = get(url)
    return result
"#,
    );
    assert_eq!(units.len(), 2);
    assert_eq!(units[0].name, "fetch");
    assert_eq!(units[1].name, "sync_fetch");
}

// -- Test detection --

#[test]
fn test_detection_by_name() {
    let units = parse(
        r#"
def test_addition():
    assert 1 + 1 == 2

def test_subtraction():
    assert 2 - 1 == 1
"#,
    );
    assert_eq!(units.len(), 2);
    assert!(units[0].is_test, "test_addition should be tagged as test");
    assert!(
        units[1].is_test,
        "test_subtraction should be tagged as test"
    );
}

#[test]
fn non_test_functions_not_tagged() {
    let units = parse(
        r#"
def add(a, b):
    return a + b

def helper():
    return 42
"#,
    );
    assert_eq!(units.len(), 2);
    assert!(!units[0].is_test, "add should not be tagged as test");
    assert!(!units[1].is_test, "helper should not be tagged as test");
}

// -- Fingerprinting --

#[test]
fn duplicate_functions_same_fingerprint() {
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
    assert_eq!(units.len(), 2);
    assert_eq!(
        units[0].fingerprint, units[1].fingerprint,
        "Structurally identical functions should have the same fingerprint"
    );
}

#[test]
fn different_functions_different_fingerprint() {
    let units = parse(
        r#"
def add(a, b):
    return a + b

def mul(a, b):
    return a * b
"#,
    );
    assert_eq!(units.len(), 2);
    assert_ne!(
        units[0].fingerprint, units[1].fingerprint,
        "Structurally different functions should have different fingerprints"
    );
}

#[test]
fn renamed_variables_same_fingerprint() {
    let units = parse(
        r#"
def foo(a, b):
    return a + b

def bar(x, y):
    return x + y
"#,
    );
    assert_eq!(units.len(), 2);
    assert_eq!(
        units[0].fingerprint, units[1].fingerprint,
        "Functions with renamed variables should have the same fingerprint"
    );
}

#[test]
fn break_and_continue_have_different_fingerprints() {
    let units = parse(
        r#"
def with_break(items):
    for x in items:
        if x > 10:
            break
    return x

def with_continue(items):
    for x in items:
        if x > 10:
            continue
    return x
"#,
    );
    assert_eq!(units.len(), 2);
    assert_ne!(
        units[0].fingerprint, units[1].fingerprint,
        "break and continue should produce different fingerprints"
    );
}

#[test]
fn none_literal_distinguished_from_bool() {
    let units = parse(
        r#"
def returns_none(x):
    y = None
    return y

def returns_true(x):
    y = True
    return y
"#,
    );
    assert_eq!(units.len(), 2);
    assert_ne!(
        units[0].fingerprint, units[1].fingerprint,
        "None and True should produce different fingerprints"
    );
}

#[test]
fn augmented_assignment_operators_distinguished() {
    let units = parse(
        r#"
def add_assign(a, b):
    a += b
    return a

def sub_assign(a, b):
    a -= b
    return a
"#,
    );
    assert_eq!(units.len(), 2);
    assert_ne!(
        units[0].fingerprint, units[1].fingerprint,
        "+= and -= should produce different fingerprints"
    );
}

#[test]
fn comparison_operators_preserve_operands() {
    let units = parse(
        r#"
def check_eq(a, b):
    if a == b:
        return a
    return b

def check_lt(a, b):
    if a < b:
        return a
    return b
"#,
    );
    assert_eq!(units.len(), 2);
    assert_ne!(
        units[0].fingerprint, units[1].fingerprint,
        "== and < should produce different fingerprints"
    );
}

#[test]
fn tuple_and_list_distinguished() {
    let units = parse(
        r#"
def make_tuple(a, b):
    x = (a, b)
    return x

def make_list(a, b):
    x = [a, b]
    return x
"#,
    );
    assert_eq!(units.len(), 2);
    assert_ne!(
        units[0].fingerprint, units[1].fingerprint,
        "Tuple and list should produce different fingerprints"
    );
}

// -- Filtering --

#[test]
fn respects_min_nodes() {
    let analyzer = PythonAnalyzer::new();
    let config = AnalysisConfig {
        min_nodes: 100,
        min_lines: 1,
    };
    let units = analyzer
        .parse_file(
            &PathBuf::from("test.py"),
            "def tiny():\n    pass\n",
            &config,
        )
        .expect("parse should succeed");
    assert!(
        units.is_empty(),
        "Small function should be filtered by min_nodes"
    );
}

#[test]
fn respects_min_lines() {
    let analyzer = PythonAnalyzer::new();
    let config = AnalysisConfig {
        min_nodes: 1,
        min_lines: 10,
    };
    let units = analyzer
        .parse_file(
            &PathBuf::from("test.py"),
            "def short():\n    return 1\n",
            &config,
        )
        .expect("parse should succeed");
    assert!(
        units.is_empty(),
        "Short function should be filtered by min_lines"
    );
}

// -- Edge cases --

#[test]
fn empty_file_returns_no_units() {
    let units = parse("");
    assert!(units.is_empty());
}

#[test]
fn comments_only_file_returns_no_units() {
    let units = parse("# This is a comment\n# Another comment\n");
    assert!(units.is_empty());
}

#[test]
fn syntax_error_does_not_panic() {
    let analyzer = PythonAnalyzer::new();
    let result = analyzer.parse_file(
        &PathBuf::from("bad.py"),
        "def broken(\n    pass\n)))\n",
        &default_config(),
    );
    // Should succeed (tree-sitter is error-tolerant) or return an error, but not panic
    assert!(result.is_ok());
}

#[test]
fn decorated_functions_are_parsed() {
    let units = parse(
        r#"
@some_decorator
def decorated(x):
    return x * 2

def plain(x):
    return x * 2
"#,
    );
    assert_eq!(units.len(), 2);
    // Decorators are skipped in normalization, so these should have the same fingerprint
    assert_eq!(units[0].fingerprint, units[1].fingerprint);
}

#[test]
fn pyi_stub_file_parses() {
    let analyzer = PythonAnalyzer::new();
    let result = analyzer.parse_file(
        &PathBuf::from("stubs.pyi"),
        "def foo(x: int) -> int: ...\ndef bar(x: str) -> str: ...\n",
        &default_config(),
    );
    assert!(result.is_ok());
}
