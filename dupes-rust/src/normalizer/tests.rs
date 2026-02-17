use super::*;

fn parse_expr(code: &str) -> syn::Expr {
    syn::parse_str::<syn::Expr>(code).unwrap()
}

fn parse_fn(code: &str) -> syn::ItemFn {
    syn::parse_str::<syn::ItemFn>(code).unwrap()
}

fn normalize_code_expr(code: &str) -> NormalizedNode {
    let expr = parse_expr(code);
    let mut ctx = NormalizationContext::new();
    normalize_expr(&expr, &mut ctx)
}

#[test]
fn renamed_variables_produce_identical_trees() {
    let code1 = "fn foo(x: i32) -> i32 { let y = x + 1; y }";
    let code2 = "fn bar(a: i32) -> i32 { let b = a + 1; b }";
    let f1 = parse_fn(code1);
    let f2 = parse_fn(code2);
    let (sig1, body1) = normalize_item_fn(&f1);
    let (sig2, body2) = normalize_item_fn(&f2);
    assert_eq!(sig1, sig2);
    assert_eq!(body1, body2);
}

#[test]
fn structural_changes_produce_different_trees() {
    let code1 = "fn foo(x: i32) -> i32 { x + 1 }";
    let code2 = "fn foo(x: i32) -> i32 { x * 1 }";
    let f1 = parse_fn(code1);
    let f2 = parse_fn(code2);
    let (_, body1) = normalize_item_fn(&f1);
    let (_, body2) = normalize_item_fn(&f2);
    assert_ne!(body1, body2);
}

#[test]
fn literal_kind_preserved_but_value_erased() {
    let n1 = normalize_code_expr("42");
    let n2 = normalize_code_expr("99");
    let n3 = normalize_code_expr("3.14");
    assert_eq!(n1, n2); // both are Int
    assert_ne!(n1, n3); // Int vs Float
}

#[test]
fn string_literals_are_equal() {
    let n1 = normalize_code_expr("\"hello\"");
    let n2 = normalize_code_expr("\"world\"");
    assert_eq!(n1, n2);
}

#[test]
fn bool_literals_normalize_as_placeholders() {
    let n1 = normalize_code_expr("true");
    let n2 = normalize_code_expr("false");
    assert_eq!(n1, n2);
}

#[test]
fn binary_ops_preserved() {
    let n1 = normalize_code_expr("a + b");
    let n2 = normalize_code_expr("a - b");
    assert_ne!(n1, n2);
}

#[test]
fn method_calls_normalized() {
    let code1 = "x.foo(y)";
    let code2 = "a.foo(b)";
    let n1 = normalize_code_expr(code1);
    let n2 = normalize_code_expr(code2);
    assert_eq!(n1, n2);
}

#[test]
fn if_else_structure_preserved() {
    let code1 = "if x > 0 { x } else { -x }";
    let code2 = "if a > 0 { a } else { -a }";
    let n1 = normalize_code_expr(code1);
    let n2 = normalize_code_expr(code2);
    assert_eq!(n1, n2);
}

#[test]
fn if_vs_if_else_different() {
    let code1 = "if x > 0 { x }";
    let code2 = "if x > 0 { x } else { 0 }";
    let n1 = normalize_code_expr(code1);
    let n2 = normalize_code_expr(code2);
    assert_ne!(n1, n2);
}

#[test]
fn match_arms_normalized() {
    let code = r#"match x { 0 => "zero", _ => "other" }"#;
    let n = normalize_code_expr(code);
    // Match -> [expr, arm0, arm1]
    assert_eq!(n.kind, NodeKind::Match);
    // children[0] is expr, children[1..] are arms
    assert_eq!(n.children.len(), 3); // expr + 2 arms
}

#[test]
fn closures_normalized() {
    let code1 = "|x| x + 1";
    let code2 = "|y| y + 1";
    let n1 = normalize_code_expr(code1);
    let n2 = normalize_code_expr(code2);
    assert_eq!(n1, n2);
}

#[test]
fn for_loops_normalized() {
    let code1 = "for i in 0..10 { println!(\"hello\") }";
    let code2 = "for j in 0..10 { println!(\"world\") }";
    let n1 = normalize_code_expr(code1);
    let n2 = normalize_code_expr(code2);
    assert_eq!(n1, n2);
}

#[test]
fn node_counting_works() {
    let code = "fn foo(x: i32) -> i32 { x + 1 }";
    let f = parse_fn(code);
    let (sig, body) = normalize_item_fn(&f);
    let sig_count = count_nodes(&sig);
    let body_count = count_nodes(&body);
    assert!(sig_count > 0);
    assert!(body_count > 0);
}

#[test]
fn tuple_pattern_normalized() {
    let code1 = "fn foo() { let (a, b) = (1, 2); }";
    let code2 = "fn bar() { let (x, y) = (1, 2); }";
    let f1 = parse_fn(code1);
    let f2 = parse_fn(code2);
    let (_, body1) = normalize_item_fn(&f1);
    let (_, body2) = normalize_item_fn(&f2);
    assert_eq!(body1, body2);
}

#[test]
fn reference_expressions_normalized() {
    let n1 = normalize_code_expr("&x");
    let n2 = normalize_code_expr("&mut x");
    assert_ne!(n1, n2); // mutability matters
}

#[test]
fn impl_block_methods_normalized() {
    let code = r#"
        impl Foo {
            fn bar(&self) -> i32 { self.x + 1 }
            fn baz(&mut self, val: i32) { self.x = val; }
        }
    "#;
    let item: syn::ItemImpl = syn::parse_str(code).unwrap();
    let methods = normalize_impl_block(&item);
    assert_eq!(methods.len(), 2);
    assert_eq!(methods[0].0, "bar");
    assert_eq!(methods[1].0, "baz");
}

#[test]
fn cast_expression_normalized() {
    let n = normalize_code_expr("x as f64");
    assert_eq!(n.kind, NodeKind::Cast);
}

#[test]
fn index_expression_normalized() {
    let n = normalize_code_expr("arr[0]");
    assert_eq!(n.kind, NodeKind::Index);
}

#[test]
fn await_expression_normalized() {
    let n = normalize_code_expr("fut.await");
    assert_eq!(n.kind, NodeKind::Await);
}

#[test]
fn try_expression_normalized() {
    let n = normalize_code_expr("result?");
    assert_eq!(n.kind, NodeKind::Try);
}

#[test]
fn range_expression_normalized() {
    let n = normalize_code_expr("0..10");
    // Range -> [from_or_None, to_or_None]
    assert_eq!(n.kind, NodeKind::Range);
    assert_eq!(n.children.len(), 2);
    assert!(!n.children[0].is_none());
    assert!(!n.children[1].is_none());
}

#[test]
fn complex_function_normalization() {
    let code1 = r#"
        fn process(data: Vec<i32>) -> Result<i32, String> {
            let mut sum = 0;
            for item in data.iter() {
                if *item > 0 {
                    sum += *item;
                }
            }
            Ok(sum)
        }
    "#;
    let code2 = r#"
        fn compute(values: Vec<i32>) -> Result<i32, String> {
            let mut total = 0;
            for val in values.iter() {
                if *val > 0 {
                    total += *val;
                }
            }
            Ok(total)
        }
    "#;
    let f1 = parse_fn(code1);
    let f2 = parse_fn(code2);
    let (sig1, body1) = normalize_item_fn(&f1);
    let (sig2, body2) = normalize_item_fn(&f2);
    assert_eq!(sig1, sig2);
    assert_eq!(body1, body2);
}

#[test]
fn macro_invocations_produce_macro_call() {
    let n = normalize_code_expr("println!(\"hello\")");
    match &n.kind {
        NodeKind::MacroCall { name } => {
            assert_eq!(name, "println");
            assert_eq!(n.children.len(), 1);
            assert_eq!(
                n.children[0],
                NormalizedNode::leaf(NodeKind::Literal(LiteralKind::Str))
            );
        }
        _ => panic!("Expected MacroCall node, got {:?}", n),
    }
}

#[test]
fn different_macro_names_produce_different_nodes() {
    let n1 = normalize_code_expr("println!(\"hello\")");
    let n2 = normalize_code_expr("eprintln!(\"hello\")");
    assert_ne!(n1, n2);
}

#[test]
fn same_macro_different_literal_values_are_equal() {
    let n1 = normalize_code_expr("println!(\"hello\")");
    let n2 = normalize_code_expr("println!(\"world\")");
    assert_eq!(n1, n2);
}

#[test]
fn same_macro_different_arg_count_are_different() {
    let n1 = normalize_code_expr("println!(\"a\")");
    let n2 = normalize_code_expr("println!(\"a\", \"b\")");
    assert_ne!(n1, n2);
}

#[test]
fn vec_macro_normalized() {
    let n = normalize_code_expr("vec![1, 2, 3]");
    match &n.kind {
        NodeKind::MacroCall { name } => {
            assert_eq!(name, "vec");
            assert_eq!(n.children.len(), 3);
        }
        _ => panic!("Expected MacroCall node, got {:?}", n),
    }
}

#[test]
fn multi_segment_macro_path_uses_last_segment() {
    let n = normalize_code_expr("std::println!(\"hello\")");
    match &n.kind {
        NodeKind::MacroCall { name } => {
            assert_eq!(name, "println");
        }
        _ => panic!("Expected MacroCall node, got {:?}", n),
    }
}

#[test]
fn macro_call_node_count() {
    let n = normalize_code_expr("println!(\"a\", \"b\")");
    // 1 for MacroCall + 2 args (Literal each = 1)
    assert_eq!(count_nodes(&n), 3);
}

#[test]
fn unparseable_macro_args_produce_opaque() {
    let n = normalize_code_expr("vec![x; 10]");
    match &n.kind {
        NodeKind::MacroCall { name } => {
            assert_eq!(name, "vec");
            assert_eq!(n.children.len(), 1);
            assert_eq!(n.children[0], NormalizedNode::leaf(NodeKind::Opaque));
        }
        _ => panic!("Expected MacroCall node, got {:?}", n),
    }
}

#[test]
fn unparseable_macro_differs_from_no_args() {
    let n_empty = normalize_code_expr("my_macro!()");
    let n_unparseable = normalize_code_expr("vec![x; 10]");
    match (&n_empty.kind, &n_unparseable.kind) {
        (NodeKind::MacroCall { .. }, NodeKind::MacroCall { .. }) => {
            assert!(n_empty.children.is_empty());
            assert_eq!(n_unparseable.children.len(), 1);
            assert_eq!(
                n_unparseable.children[0],
                NormalizedNode::leaf(NodeKind::Opaque)
            );
        }
        _ => panic!("Expected MacroCall nodes"),
    }
}

#[test]
fn type_position_macro_normalized() {
    let code = "fn foo() -> my_type!(i32) {}";
    if let Ok(f) = syn::parse_str::<syn::ItemFn>(code) {
        let (sig, _) = normalize_item_fn(&f);
        assert!(count_nodes(&sig) > 0);
    }
}

#[test]
fn pat_macro_normalized() {
    let code = "fn foo(x: i32) { match x { my_pat!(x) => {} _ => {} } }";
    if let Ok(f) = syn::parse_str::<syn::ItemFn>(code) {
        let (_, body) = normalize_item_fn(&f);
        assert!(count_nodes(&body) > 0);
    }
}

#[test]
fn while_loop_normalized() {
    let code1 = "while x > 0 { x = x - 1; }";
    let code2 = "while a > 0 { a = a - 1; }";
    let n1 = normalize_code_expr(code1);
    let n2 = normalize_code_expr(code2);
    assert_eq!(n1, n2);
}

#[test]
fn return_expression_normalized() {
    let n1 = normalize_code_expr("return 42");
    let n2 = normalize_code_expr("return 99");
    assert_eq!(n1, n2); // both return Int literals
}

#[test]
fn assign_expression_normalized() {
    let n = normalize_code_expr("x = 5");
    assert_eq!(n.kind, NodeKind::Assign);
}

#[test]
fn struct_init_normalized() {
    let code1 = "Foo { x: 1, y: 2 }";
    let code2 = "Bar { a: 1, b: 2 }";
    let n1 = normalize_code_expr(code1);
    let n2 = normalize_code_expr(code2);
    // Both have StructInit; children[0] is rest_or_None, rest are fields
    assert_eq!(n1.kind, NodeKind::StructInit);
    assert_eq!(n2.kind, NodeKind::StructInit);
    // Same number of fields
    assert_eq!(n1.children.len(), n2.children.len());
}

#[test]
fn array_expression_normalized() {
    let n = normalize_code_expr("[1, 2, 3]");
    assert_eq!(n.kind, NodeKind::Array);
    assert_eq!(n.children.len(), 3);
}

#[test]
fn tuple_expression_normalized() {
    let n = normalize_code_expr("(1, 2, 3)");
    assert_eq!(n.kind, NodeKind::Tuple);
    assert_eq!(n.children.len(), 3);
}

#[test]
fn field_access_normalized() {
    let n = normalize_code_expr("foo.bar");
    assert_eq!(n.kind, NodeKind::FieldAccess);
}

#[test]
fn unary_ops_preserved() {
    let n1 = normalize_code_expr("!x");
    let n2 = normalize_code_expr("-x");
    assert_ne!(n1, n2);
}

#[test]
fn loop_normalized() {
    let code = "loop { break; }";
    let n = normalize_code_expr(code);
    assert_eq!(n.kind, NodeKind::Loop);
}

#[test]
fn empty_block_normalized() {
    let code = "fn foo() {}";
    let f = parse_fn(code);
    let (_, body) = normalize_item_fn(&f);
    assert_eq!(body.kind, NodeKind::Block);
    assert!(body.children.is_empty());
}

#[test]
fn reindex_from_real_function_subtrees() {
    let f1 = parse_fn("fn foo(x: i32, y: i32) -> i32 { if x > 0 { let z = y + 1; z } else { x } }");
    let f2 = parse_fn(
        "fn bar(unused: i32, a: i32, b: i32) -> i32 { if a > 0 { let c = b + 1; c } else { a } }",
    );
    let (_, body1) = normalize_item_fn(&f1);
    let (_, body2) = normalize_item_fn(&f2);

    // Extract the then_branch from each: Block -> stmts[0] -> If -> children[1]
    let then1 = match &body1.kind {
        NodeKind::Block => match &body1.children[0].kind {
            NodeKind::If => body1.children[0].children[1].clone(),
            _ => panic!("expected If"),
        },
        _ => panic!("expected Block"),
    };
    let then2 = match &body2.kind {
        NodeKind::Block => match &body2.children[0].kind {
            NodeKind::If => body2.children[0].children[1].clone(),
            _ => panic!("expected If"),
        },
        _ => panic!("expected Block"),
    };

    assert_ne!(then1, then2);
    assert_eq!(reindex_placeholders(&then1), reindex_placeholders(&then2));
}
