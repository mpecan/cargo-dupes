//! Tests for dupes-core modules that require syn for test data construction.
//! These tests exercise fingerprint, similarity, grouper, and extractor functionality
//! using syn-parsed Rust code, so they live in cargo-dupes (which depends on syn).

use std::fs;

use dupes_core::fingerprint::Fingerprint;
use dupes_core::node::{NodeKind, NormalizedNode};
use dupes_core::similarity::similarity_score;
use tempfile::TempDir;

use cargo_dupes::normalizer::{
    NormalizationContext, normalize_expr, normalize_item_fn, reindex_placeholders,
};
use cargo_dupes::parser::{self, CodeUnit, CodeUnitKind};

// ── Helpers ───────────────────────────────────────────────────────────────

fn parse_fn(code: &str) -> syn::ItemFn {
    syn::parse_str::<syn::ItemFn>(code).unwrap()
}

fn parse_expr(code: &str) -> syn::Expr {
    syn::parse_str::<syn::Expr>(code).unwrap()
}

fn fn_body_similarity(code1: &str, code2: &str) -> f64 {
    let f1 = parse_fn(code1);
    let f2 = parse_fn(code2);
    let (_, b1) = normalize_item_fn(&f1);
    let (_, b2) = normalize_item_fn(&f2);
    similarity_score(&b1, &b2)
}

fn expr_similarity(code1: &str, code2: &str) -> f64 {
    let e1 = parse_expr(code1);
    let e2 = parse_expr(code2);
    let mut ctx1 = NormalizationContext::new();
    let mut ctx2 = NormalizationContext::new();
    let n1 = normalize_expr(&e1, &mut ctx1);
    let n2 = normalize_expr(&e2, &mut ctx2);
    similarity_score(&n1, &n2)
}

fn make_units(code: &str) -> Vec<CodeUnit> {
    let tmp = TempDir::new().unwrap();
    let file = tmp.path().join("test.rs");
    fs::write(&file, code).unwrap();
    parser::parse_file(&file, 1, 0, false).unwrap()
}

fn parse_and_extract_body(code: &str) -> NormalizedNode {
    let f = parse_fn(code);
    let (_, body) = normalize_item_fn(&f);
    body
}

// ── Fingerprint tests (syn-dependent) ─────────────────────────────────────

#[test]
fn identical_functions_same_fingerprint() {
    let code1 = "fn foo(x: i32) -> i32 { x + 1 }";
    let code2 = "fn bar(a: i32) -> i32 { a + 1 }";
    let f1 = parse_fn(code1);
    let f2 = parse_fn(code2);
    let (sig1, body1) = normalize_item_fn(&f1);
    let (sig2, body2) = normalize_item_fn(&f2);
    let fp1 = Fingerprint::from_sig_and_body(&sig1, &body1);
    let fp2 = Fingerprint::from_sig_and_body(&sig2, &body2);
    assert_eq!(fp1, fp2);
}

#[test]
fn different_functions_different_fingerprint() {
    let code1 = "fn foo(x: i32) -> i32 { x + 1 }";
    let code2 = "fn foo(x: i32) -> i32 { x * 2 }";
    let f1 = parse_fn(code1);
    let f2 = parse_fn(code2);
    let (sig1, body1) = normalize_item_fn(&f1);
    let (sig2, body2) = normalize_item_fn(&f2);
    let fp1 = Fingerprint::from_sig_and_body(&sig1, &body1);
    let fp2 = Fingerprint::from_sig_and_body(&sig2, &body2);
    assert_ne!(fp1, fp2);
}

#[test]
fn fingerprint_from_node() {
    let expr = parse_expr("x + 1");
    let mut ctx = NormalizationContext::new();
    let node = normalize_expr(&expr, &mut ctx);
    let fp = Fingerprint::from_node(&node);
    assert_ne!(fp.value(), 0);
}

#[test]
fn fingerprint_stability() {
    let code = "fn foo(x: i32) -> i32 { x + 1 }";
    let f = parse_fn(code);
    let (sig1, body1) = normalize_item_fn(&f);
    let (sig2, body2) = normalize_item_fn(&f);
    let fp1 = Fingerprint::from_sig_and_body(&sig1, &body1);
    let fp2 = Fingerprint::from_sig_and_body(&sig2, &body2);
    assert_eq!(fp1, fp2);
}

#[test]
fn fingerprint_discriminates_operators() {
    let code1 = "fn f(x: i32) -> i32 { x + 1 }";
    let code2 = "fn f(x: i32) -> i32 { x - 1 }";
    let f1 = parse_fn(code1);
    let f2 = parse_fn(code2);
    let (s1, b1) = normalize_item_fn(&f1);
    let (s2, b2) = normalize_item_fn(&f2);
    assert_ne!(
        Fingerprint::from_sig_and_body(&s1, &b1),
        Fingerprint::from_sig_and_body(&s2, &b2)
    );
}

#[test]
fn fingerprint_ignores_variable_names() {
    let code1 = "fn compute(value: i32) -> i32 { value * value + value }";
    let code2 = "fn calculate(num: i32) -> i32 { num * num + num }";
    let f1 = parse_fn(code1);
    let f2 = parse_fn(code2);
    let (s1, b1) = normalize_item_fn(&f1);
    let (s2, b2) = normalize_item_fn(&f2);
    assert_eq!(
        Fingerprint::from_sig_and_body(&s1, &b1),
        Fingerprint::from_sig_and_body(&s2, &b2)
    );
}

// ── Similarity tests ──────────────────────────────────────────────────────

#[test]
fn identical_trees_score_one() {
    let score = fn_body_similarity(
        "fn foo(x: i32) -> i32 { x + 1 }",
        "fn bar(a: i32) -> i32 { a + 1 }",
    );
    assert!((score - 1.0).abs() < f64::EPSILON);
}

#[test]
fn completely_different_trees_score_low() {
    let score = fn_body_similarity(
        "fn foo(x: i32) -> i32 { x + 1 }",
        "fn bar(x: bool) { if x { loop { break; } } }",
    );
    assert!(score < 0.3);
}

#[test]
fn single_expression_difference_high_score() {
    let score = fn_body_similarity(
        "fn foo(x: i32) -> i32 { let a = x + 1; let b = a * 2; a + b }",
        "fn bar(x: i32) -> i32 { let a = x + 1; let b = a * 3; a + b }",
    );
    assert!(score > 0.8);
}

#[test]
fn structural_difference_low_score() {
    let score = fn_body_similarity(
        "fn foo(x: i32) -> i32 { x + 1 }",
        "fn bar(x: i32) -> i32 { if x > 0 { x + 1 } else { x - 1 } }",
    );
    assert!(score < 0.7);
}

#[test]
fn empty_trees_score_one() {
    let score = fn_body_similarity("fn foo() {}", "fn bar() {}");
    assert!((score - 1.0).abs() < f64::EPSILON);
}

#[test]
fn simple_expr_identical() {
    let score = expr_similarity("x + 1", "y + 1");
    assert!((score - 1.0).abs() < f64::EPSILON);
}

#[test]
fn simple_expr_different_op() {
    let score = expr_similarity("x + 1", "x - 1");
    assert!(score > 0.5);
    assert!(score < 1.0);
}

#[test]
fn near_duplicate_complex_fn() {
    let score = fn_body_similarity(
        r#"
        fn process(data: Vec<i32>) -> i32 {
            let mut sum = 0;
            for item in data.iter() {
                if *item > 0 {
                    sum += *item;
                }
            }
            sum
        }
        "#,
        r#"
        fn compute(values: Vec<i32>) -> i32 {
            let mut total = 0;
            for val in values.iter() {
                if *val > 0 {
                    total += *val;
                }
            }
            total
        }
        "#,
    );
    assert!((score - 1.0).abs() < f64::EPSILON);
}

#[test]
fn similarity_is_symmetric() {
    let score1 = fn_body_similarity(
        "fn foo(x: i32) -> i32 { x + 1 }",
        "fn bar(x: i32) -> i32 { x * 2 + 1 }",
    );
    let score2 = fn_body_similarity(
        "fn bar(x: i32) -> i32 { x * 2 + 1 }",
        "fn foo(x: i32) -> i32 { x + 1 }",
    );
    assert!((score1 - score2).abs() < f64::EPSILON);
}

#[test]
fn if_vs_match_low_similarity() {
    let score = expr_similarity(
        "if x > 0 { x } else { -x }",
        "match x > 0 { true => x, false => -x }",
    );
    assert!(score < 0.5);
}

#[test]
fn method_call_similarity() {
    let score = expr_similarity("x.foo(y, z)", "a.foo(b, c)");
    assert!((score - 1.0).abs() < f64::EPSILON);
}

#[test]
fn closure_similarity() {
    let score = expr_similarity("|x| x + 1", "|y| y + 1");
    assert!((score - 1.0).abs() < f64::EPSILON);
}

#[test]
fn same_macro_same_args_score_one() {
    let score = expr_similarity("println!(\"hello\")", "println!(\"world\")");
    assert!((score - 1.0).abs() < f64::EPSILON);
}

#[test]
fn different_macro_names_score_zero() {
    let score = expr_similarity("println!(\"hello\")", "eprintln!(\"hello\")");
    assert!(score < f64::EPSILON);
}

#[test]
fn same_macro_different_arg_count_partial_similarity() {
    let score = expr_similarity("println!(\"a\")", "println!(\"a\", \"b\")");
    assert!(score > 0.0);
    assert!(score < 1.0);
}

// ── Grouper tests (syn-dependent) ─────────────────────────────────────────

#[test]
fn exact_duplicates_grouped() {
    let units = make_units(
        r#"
        fn foo(x: i32) -> i32 {
            let y = x + 1;
            y * 2
        }
        fn bar(a: i32) -> i32 {
            let b = a + 1;
            b * 2
        }
        fn unique(x: i32) -> i32 {
            x * x * x
        }
        "#,
    );
    let groups = dupes_core::grouper::group_exact_duplicates(&units);
    assert_eq!(groups.len(), 1);
    assert_eq!(groups[0].members.len(), 2);
    assert!((groups[0].similarity - 1.0).abs() < f64::EPSILON);
}

#[test]
fn no_duplicates_no_groups() {
    let units = make_units(
        r#"
        fn add(x: i32) -> i32 { x + 1 }
        fn mul(x: i32) -> i32 { x * 2 }
        fn sub(x: i32) -> i32 { x - 3 }
        "#,
    );
    let groups = dupes_core::grouper::group_exact_duplicates(&units);
    assert!(groups.is_empty());
}

#[test]
fn multiple_exact_groups() {
    let units = make_units(
        r#"
        fn a1(x: i32) -> i32 { x + 1 }
        fn a2(y: i32) -> i32 { y + 1 }
        fn b1(x: i32) -> i32 { x * 2 }
        fn b2(y: i32) -> i32 { y * 2 }
        "#,
    );
    let groups = dupes_core::grouper::group_exact_duplicates(&units);
    assert_eq!(groups.len(), 2);
}

#[test]
fn near_duplicates_found() {
    let units = make_units(
        r#"
        fn process(data: i32) -> i32 {
            let a = data + 1;
            let b = a * 2;
            let c = b - 3;
            a + b + c
        }
        fn compute(value: i32) -> i32 {
            let a = value + 1;
            let b = a * 2;
            let c = b - 4;
            a + b + c
        }
        "#,
    );
    let exact = dupes_core::grouper::group_exact_duplicates(&units);
    let exact_fps: Vec<_> = exact.iter().map(|g| g.fingerprint).collect();
    let near = dupes_core::grouper::find_near_duplicates(&units, 0.7, &exact_fps);
    assert!(exact.len() + near.len() >= 1);
}

#[test]
fn stats_computation() {
    let units = make_units(
        r#"
        fn a(x: i32) -> i32 { x + 1 }
        fn b(y: i32) -> i32 { y + 1 }
        fn c(x: i32) -> i32 { x * 2 }
        "#,
    );
    let exact = dupes_core::grouper::group_exact_duplicates(&units);
    let stats = dupes_core::grouper::compute_stats(&units, &exact, &[]);
    assert_eq!(stats.total_code_units, 3);
    assert_eq!(stats.exact_duplicate_groups, 1);
    assert_eq!(stats.exact_duplicate_units, 2);
    assert!(stats.total_lines > 0);
}

#[test]
fn single_unit_no_groups() {
    let units = make_units("fn solo(x: i32) -> i32 { x + 1 }");
    let groups = dupes_core::grouper::group_exact_duplicates(&units);
    assert!(groups.is_empty());
}

#[test]
fn exact_groups_sorted_by_size() {
    let units = make_units(
        r#"
        fn a1(x: i32) -> i32 { x + 1 }
        fn a2(y: i32) -> i32 { y + 1 }
        fn a3(z: i32) -> i32 { z + 1 }
        fn b1(x: i32) -> i32 { x * 2 }
        fn b2(y: i32) -> i32 { y * 2 }
        "#,
    );
    let groups = dupes_core::grouper::group_exact_duplicates(&units);
    assert_eq!(groups.len(), 2);
    assert!(groups[0].members.len() >= groups[1].members.len());
}

#[test]
fn near_duplicates_exclude_exact() {
    let units = make_units(
        r#"
        fn a(x: i32) -> i32 { x + 1 }
        fn b(y: i32) -> i32 { y + 1 }
        "#,
    );
    let exact = dupes_core::grouper::group_exact_duplicates(&units);
    let exact_fps: Vec<_> = exact.iter().map(|g| g.fingerprint).collect();
    let near = dupes_core::grouper::find_near_duplicates(&units, 0.7, &exact_fps);
    assert!(near.is_empty());
}

#[test]
fn duplicate_group_has_fingerprint() {
    let units = make_units(
        r#"
        fn a(x: i32) -> i32 { x + 1 }
        fn b(y: i32) -> i32 { y + 1 }
        "#,
    );
    let groups = dupes_core::grouper::group_exact_duplicates(&units);
    assert_eq!(groups.len(), 1);
    assert_ne!(groups[0].fingerprint.value(), 0);
}

#[test]
fn stats_with_near_duplicates() {
    let units = make_units(
        r#"
        fn a(x: i32) -> i32 { x + 1 }
        fn b(y: i32) -> i32 { y * 2 }
        "#,
    );
    let composite_fp = Fingerprint::from_fingerprints(&[Fingerprint::from_node(
        &NormalizedNode::leaf(NodeKind::Opaque),
    )]);
    let near_group = dupes_core::grouper::DuplicateGroup {
        fingerprint: composite_fp,
        members: vec![],
        similarity: 0.85,
    };
    let stats = dupes_core::grouper::compute_stats(&units, &[], &[near_group]);
    assert_eq!(stats.total_code_units, units.len());
    assert_eq!(stats.near_duplicate_groups, 1);
}

#[test]
fn stats_includes_line_counts() {
    let units = make_units(
        r#"
        fn foo(x: i32) -> i32 {
            let y = x + 1;
            y * 2
        }
        fn bar(a: i32) -> i32 {
            let b = a + 1;
            b * 2
        }
        "#,
    );
    let exact = dupes_core::grouper::group_exact_duplicates(&units);
    let stats = dupes_core::grouper::compute_stats(&units, &exact, &[]);
    assert!(stats.exact_duplicate_lines > 0);
    assert_eq!(stats.near_duplicate_lines, 0);
}

#[test]
fn stats_total_lines_computed() {
    let units = make_units(
        r#"
        fn foo(x: i32) -> i32 {
            let y = x + 1;
            y * 2
        }
        fn bar(a: i32) -> i32 {
            let b = a + 1;
            b * 2
        }
        "#,
    );
    let stats = dupes_core::grouper::compute_stats(&units, &[], &[]);
    assert!(stats.total_lines > 0);
}

// ── Extractor tests ───────────────────────────────────────────────────────

#[test]
fn extracts_if_branches() {
    let body = parse_and_extract_body(
        "fn foo(x: i32) -> i32 { if x > 0 { let y = x + 1; y * 2 } else { let z = x - 1; z * 3 } }",
    );
    let subs = dupes_core::extractor::extract_sub_units(&body, 1);
    let if_branches: Vec<_> = subs
        .iter()
        .filter(|s| s.kind == CodeUnitKind::IfBranch)
        .collect();
    assert_eq!(if_branches.len(), 2);
}

#[test]
fn extracts_match_arms() {
    let body = parse_and_extract_body(
        r#"fn foo(x: i32) -> i32 {
            match x {
                0 => { let a = 1; a + 1 },
                1 => { let b = 2; b + 2 },
                _ => { let c = 3; c + 3 },
            }
        }"#,
    );
    let subs = dupes_core::extractor::extract_sub_units(&body, 1);
    let match_arms: Vec<_> = subs
        .iter()
        .filter(|s| s.kind == CodeUnitKind::MatchArm)
        .collect();
    assert_eq!(match_arms.len(), 3);
}

#[test]
fn extracts_loop_bodies() {
    let body =
        parse_and_extract_body("fn foo(x: i32) { for i in 0..10 { let y = i + x; let _ = y; } }");
    let subs = dupes_core::extractor::extract_sub_units(&body, 1);
    let loops: Vec<_> = subs
        .iter()
        .filter(|s| s.kind == CodeUnitKind::LoopBody)
        .collect();
    assert_eq!(loops.len(), 1);
}

#[test]
fn respects_min_node_count() {
    let body =
        parse_and_extract_body("fn foo(x: i32) -> i32 { if x > 0 { x + 1 } else { x - 1 } }");
    let subs_low = dupes_core::extractor::extract_sub_units(&body, 1);
    let subs_high = dupes_core::extractor::extract_sub_units(&body, 100);
    assert!(!subs_low.is_empty());
    assert!(subs_high.is_empty());
}

#[test]
fn identical_branches_from_different_functions_match() {
    let body1 = parse_and_extract_body(
        "fn foo(unused: i32, x: i32) -> i32 { if x > 0 { let y = x + 1; y * 2 } else { x } }",
    );
    let body2 = parse_and_extract_body(
        "fn bar(a: i32) -> i32 { if a > 0 { let b = a + 1; b * 2 } else { a } }",
    );

    let subs1 = dupes_core::extractor::extract_sub_units(&body1, 1);
    let subs2 = dupes_core::extractor::extract_sub_units(&body2, 1);

    let then1 = subs1
        .iter()
        .find(|s| s.description == "if-then branch")
        .unwrap();
    let then2 = subs2
        .iter()
        .find(|s| s.description == "if-then branch")
        .unwrap();

    assert_eq!(then1.node, then2.node);
}

#[test]
fn sub_units_are_reindexed() {
    let body = parse_and_extract_body(
        "fn foo(a: i32, b: i32, c: i32) -> i32 { if c > 0 { let d = c + 1; d } else { c } }",
    );
    let subs = dupes_core::extractor::extract_sub_units(&body, 1);
    let then_branch = subs
        .iter()
        .find(|s| s.description == "if-then branch")
        .unwrap();

    let mut ctx = NormalizationContext::new();
    let fresh_expr = normalize_expr(
        &syn::parse_str::<syn::Expr>("{ let d = c + 1; d }").unwrap(),
        &mut ctx,
    );
    let reindexed_fresh = reindex_placeholders(&fresh_expr);
    assert_eq!(then_branch.node, reindexed_fresh);
}

#[test]
fn nested_structures_extracted_recursively() {
    let body = parse_and_extract_body(
        r#"fn foo(x: i32) -> i32 {
            if x > 0 {
                for i in 0..x {
                    let y = i + 1;
                    let _ = y;
                }
                x
            } else {
                x
            }
        }"#,
    );
    let subs = dupes_core::extractor::extract_sub_units(&body, 1);
    let if_branches: Vec<_> = subs
        .iter()
        .filter(|s| s.kind == CodeUnitKind::IfBranch)
        .collect();
    let loops: Vec<_> = subs
        .iter()
        .filter(|s| s.kind == CodeUnitKind::LoopBody)
        .collect();
    assert_eq!(if_branches.len(), 2);
    assert_eq!(loops.len(), 1);
}
