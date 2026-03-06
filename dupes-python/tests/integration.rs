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
    // 1 class + 2 methods
    assert_eq!(units.len(), 3);
    let methods: Vec<_> = units
        .iter()
        .filter(|u| u.kind == dupes_core::code_unit::CodeUnitKind::Function)
        .collect();
    assert_eq!(methods.len(), 2);
    assert_eq!(methods[0].name, "add");
    assert_eq!(methods[1].name, "subtract");
    let classes: Vec<_> = units
        .iter()
        .filter(|u| u.kind == dupes_core::code_unit::CodeUnitKind::Class)
        .collect();
    assert_eq!(classes.len(), 1);
    assert_eq!(classes[0].name, "Calculator");
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
    use std::panic::{AssertUnwindSafe, catch_unwind};
    let result = catch_unwind(AssertUnwindSafe(|| {
        let analyzer = PythonAnalyzer::new();
        let _ = analyzer.parse_file(
            &PathBuf::from("bad.py"),
            "def broken(\n    pass\n)))\n",
            &default_config(),
        );
    }));
    assert!(
        result.is_ok(),
        "parse_file should not panic on syntax errors"
    );
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

#[test]
fn chained_comparison_preserves_all_operands() {
    let units = parse(
        r#"
def chained(a, b, c):
    if a < b < c:
        return a
    return c

def simple(a, b, c):
    if a < c:
        return a
    return c
"#,
    );
    assert_eq!(units.len(), 2);
    assert_ne!(
        units[0].fingerprint, units[1].fingerprint,
        "a < b < c and a < c should produce different fingerprints"
    );
}

#[test]
fn set_and_list_distinguished() {
    let units = parse(
        r#"
def make_set(a, b):
    x = {a, b}
    return x

def make_list(a, b):
    x = [a, b]
    return x
"#,
    );
    assert_eq!(units.len(), 2);
    assert_ne!(
        units[0].fingerprint, units[1].fingerprint,
        "Set and list should produce different fingerprints"
    );
}

#[test]
fn floor_div_and_regular_div_different_fingerprint() {
    let units = parse(
        r#"
def floor_div(a, b):
    return a // b

def regular_div(a, b):
    return a / b
"#,
    );
    assert_eq!(units.len(), 2);
    assert_ne!(
        units[0].fingerprint, units[1].fingerprint,
        "// and / should produce different fingerprints"
    );
}

#[test]
fn pow_and_mul_different_fingerprint() {
    let units = parse(
        r#"
def power(a, b):
    return a ** b

def multiply(a, b):
    return a * b
"#,
    );
    assert_eq!(units.len(), 2);
    assert_ne!(
        units[0].fingerprint, units[1].fingerprint,
        "** and * should produce different fingerprints"
    );
}

// -- Lambda extraction --

#[test]
fn extracts_lambdas() {
    let units = parse(
        r#"
f = lambda x: x + 1
g = lambda y: y * 2
"#,
    );
    let lambdas: Vec<_> = units
        .iter()
        .filter(|u| u.kind == dupes_core::code_unit::CodeUnitKind::Closure)
        .collect();
    assert_eq!(lambdas.len(), 2, "Should extract two lambda expressions");
    assert!(
        lambdas[0].name.contains("anonymous"),
        "Lambda should have anonymous name, got: {}",
        lambdas[0].name
    );
}

#[test]
fn duplicate_lambdas_same_fingerprint() {
    let units = parse(
        r#"
f = lambda x: x + 1
g = lambda y: y + 1
"#,
    );
    let lambdas: Vec<_> = units
        .iter()
        .filter(|u| u.kind == dupes_core::code_unit::CodeUnitKind::Closure)
        .collect();
    assert_eq!(lambdas.len(), 2);
    assert_eq!(
        lambdas[0].fingerprint, lambdas[1].fingerprint,
        "Structurally identical lambdas should have the same fingerprint"
    );
}

#[test]
fn different_lambdas_different_fingerprint() {
    let units = parse(
        r#"
f = lambda x: x + 1
g = lambda x: x * 2
"#,
    );
    let lambdas: Vec<_> = units
        .iter()
        .filter(|u| u.kind == dupes_core::code_unit::CodeUnitKind::Closure)
        .collect();
    assert_eq!(lambdas.len(), 2);
    assert_ne!(
        lambdas[0].fingerprint, lambdas[1].fingerprint,
        "Structurally different lambdas should have different fingerprints"
    );
}

// -- Class extraction --

#[test]
fn extracts_class_bodies() {
    let units = parse(
        r#"
class Foo:
    def method(self):
        return 1
"#,
    );
    let classes: Vec<_> = units
        .iter()
        .filter(|u| u.kind == dupes_core::code_unit::CodeUnitKind::Class)
        .collect();
    assert_eq!(classes.len(), 1, "Should extract one class definition");
    assert_eq!(classes[0].name, "Foo");
}

#[test]
fn duplicate_classes_same_fingerprint() {
    let units = parse(
        r#"
class Foo:
    def method(self):
        return self + 1

class Bar:
    def method(self):
        return self + 1
"#,
    );
    let classes: Vec<_> = units
        .iter()
        .filter(|u| u.kind == dupes_core::code_unit::CodeUnitKind::Class)
        .collect();
    assert_eq!(classes.len(), 2);
    assert_eq!(
        classes[0].fingerprint, classes[1].fingerprint,
        "Structurally identical classes should have the same fingerprint"
    );
}

#[test]
fn class_methods_still_extracted_separately() {
    let units = parse(
        r#"
class MyClass:
    def method_a(self, x):
        return x + 1

    def method_b(self, x):
        return x * 2
"#,
    );
    let classes: Vec<_> = units
        .iter()
        .filter(|u| u.kind == dupes_core::code_unit::CodeUnitKind::Class)
        .collect();
    let functions: Vec<_> = units
        .iter()
        .filter(|u| u.kind == dupes_core::code_unit::CodeUnitKind::Function)
        .collect();
    assert_eq!(classes.len(), 1, "Should extract the class itself");
    assert_eq!(
        functions.len(),
        2,
        "Should extract both methods as Function units"
    );
}

#[test]
fn test_class_detected_as_test() {
    let units = parse(
        r#"
class TestCalculator:
    def test_add(self):
        assert 1 + 1 == 2
"#,
    );
    let classes: Vec<_> = units
        .iter()
        .filter(|u| u.kind == dupes_core::code_unit::CodeUnitKind::Class)
        .collect();
    assert_eq!(classes.len(), 1);
    assert!(
        classes[0].is_test,
        "Class with Test prefix should be tagged as test"
    );
}

// -- Lambda edge cases --

#[test]
fn lambda_with_no_parameters() {
    let units = parse(
        r#"
f = lambda: 42
g = lambda: 99
"#,
    );
    let lambdas: Vec<_> = units
        .iter()
        .filter(|u| u.kind == dupes_core::code_unit::CodeUnitKind::Closure)
        .collect();
    assert_eq!(
        lambdas.len(),
        2,
        "Lambdas with no parameters should be extracted"
    );
    // Both return a literal (Int), so they have the same fingerprint
    assert_eq!(
        lambdas[0].fingerprint, lambdas[1].fingerprint,
        "Parameterless lambdas with same structure should match"
    );
}

#[test]
fn lambda_with_multiple_parameters() {
    let units = parse(
        r#"
f = lambda x, y: x + y
g = lambda a, b: a + b
"#,
    );
    let lambdas: Vec<_> = units
        .iter()
        .filter(|u| u.kind == dupes_core::code_unit::CodeUnitKind::Closure)
        .collect();
    assert_eq!(lambdas.len(), 2);
    assert_eq!(
        lambdas[0].fingerprint, lambdas[1].fingerprint,
        "Lambdas with renamed multi-params and same body should match"
    );
}

#[test]
fn lambda_inside_function_extracted() {
    let units = parse(
        r#"
def outer(items):
    result = list(map(lambda x: x + 1, items))
    return result
"#,
    );
    let lambdas: Vec<_> = units
        .iter()
        .filter(|u| u.kind == dupes_core::code_unit::CodeUnitKind::Closure)
        .collect();
    assert!(
        !lambdas.is_empty(),
        "Lambda inside a function should be extracted"
    );
}

// -- Class edge cases --

#[test]
fn empty_class_body_respects_min_nodes() {
    let analyzer = PythonAnalyzer::new();
    let config = AnalysisConfig {
        min_nodes: 100,
        min_lines: 1,
    };
    let units = analyzer
        .parse_file(
            &PathBuf::from("test.py"),
            "class Empty:\n    pass\n",
            &config,
        )
        .expect("parse should succeed");
    let classes: Vec<_> = units
        .iter()
        .filter(|u| u.kind == dupes_core::code_unit::CodeUnitKind::Class)
        .collect();
    assert!(
        classes.is_empty(),
        "Empty class should be filtered by high min_nodes"
    );
}

#[test]
fn decorated_class_same_fingerprint_as_plain() {
    let units = parse(
        r#"
@some_decorator
class Foo:
    def method(self):
        return self + 1

class Bar:
    def method(self):
        return self + 1
"#,
    );
    let classes: Vec<_> = units
        .iter()
        .filter(|u| u.kind == dupes_core::code_unit::CodeUnitKind::Class)
        .collect();
    assert_eq!(classes.len(), 2);
    assert_eq!(
        classes[0].fingerprint, classes[1].fingerprint,
        "Decorated and plain classes with same body should have same fingerprint"
    );
}

#[test]
fn nested_class_extraction() {
    let units = parse(
        r#"
class Outer:
    class Inner:
        def method(self):
            return 1
    def outer_method(self):
        return 2
"#,
    );
    let classes: Vec<_> = units
        .iter()
        .filter(|u| u.kind == dupes_core::code_unit::CodeUnitKind::Class)
        .collect();
    assert!(
        classes.len() >= 2,
        "Both outer and inner classes should be extracted, got {}",
        classes.len()
    );
}

#[test]
fn class_with_inheritance_extracted() {
    let units = parse(
        r#"
class Child(Parent):
    def method(self):
        return self + 1
"#,
    );
    let classes: Vec<_> = units
        .iter()
        .filter(|u| u.kind == dupes_core::code_unit::CodeUnitKind::Class)
        .collect();
    assert_eq!(classes.len(), 1);
    assert_eq!(classes[0].name, "Child");
}
