use std::path::PathBuf;

use dupes_core::code_unit::CodeUnitKind;
use dupes_core::config::AnalysisConfig;
use dupes_core::node::{
    BinOpKind, LiteralKind, NodeKind, NormalizationContext, NormalizedNode, PlaceholderKind,
    UnOpKind, count_nodes,
};
use dupes_treesitter::mapping::NodeMapping;
use dupes_treesitter::normalizer::normalize_ts_node;

/// Build a Python-flavored `NodeMapping`.
fn python_mapping() -> NodeMapping {
    NodeMapping::new()
        .identifiers(&["identifier"])
        .literals(&[
            ("integer", LiteralKind::Int),
            ("float", LiteralKind::Float),
            ("string", LiteralKind::Str),
            ("true", LiteralKind::Bool),
            ("false", LiteralKind::Bool),
        ])
        .binary_ops(&[
            ("+", BinOpKind::Add),
            ("-", BinOpKind::Sub),
            ("*", BinOpKind::Mul),
            ("/", BinOpKind::Div),
            ("%", BinOpKind::Rem),
            ("==", BinOpKind::Eq),
            ("!=", BinOpKind::Ne),
            ("<", BinOpKind::Lt),
            (">", BinOpKind::Gt),
            ("<=", BinOpKind::Le),
            (">=", BinOpKind::Ge),
            ("and", BinOpKind::And),
            ("or", BinOpKind::Or),
            ("&", BinOpKind::BitAnd),
            ("|", BinOpKind::BitOr),
            ("^", BinOpKind::BitXor),
            ("<<", BinOpKind::Shl),
            (">>", BinOpKind::Shr),
        ])
        .unary_ops(&[
            ("not", UnOpKind::Not),
            ("-", UnOpKind::Neg),
            ("~", UnOpKind::Other),
        ])
        .skip(&["comment", "decorator"])
        .blocks(&["block"])
        .calls(&["call"])
        .returns(&["return_statement"])
        .ifs(&["if_statement"])
        .for_loops(&["for_statement"])
        .while_loops(&["while_statement"])
        .matches(&["match_statement"])
        .assignments(&["assignment"])
        .function_defs(&["function_definition"])
        .binary_op_kinds(&["binary_operator", "boolean_operator", "comparison_operator"])
        .unary_op_kinds(&["not_operator", "unary_operator"])
        .match_arms(&["case_clause"])
}

/// Parse Python source and return the tree.
fn parse_python(source: &str) -> tree_sitter::Tree {
    let mut parser = tree_sitter::Parser::new();
    let language = tree_sitter_python::LANGUAGE;
    parser
        .set_language(&language.into())
        .expect("Failed to set Python language");
    parser.parse(source, None).expect("Failed to parse")
}

/// Normalize a Python function body (first function_definition's body block).
fn normalize_python_body(source: &str) -> NormalizedNode {
    let tree = parse_python(source);
    let mapping = python_mapping();
    let mut ctx = NormalizationContext::new();
    let root = tree.root_node();

    // Find first function_definition, then its body
    fn find_body(node: tree_sitter::Node) -> Option<tree_sitter::Node> {
        if node.kind() == "function_definition" {
            return node.child_by_field_name("body");
        }
        let cursor = &mut node.walk();
        for child in node.named_children(cursor) {
            if let Some(body) = find_body(child) {
                return Some(body);
            }
        }
        None
    }

    let body = find_body(root).expect("No function body found");
    normalize_ts_node(body, source.as_bytes(), &mapping, &mut ctx)
}

// -- Tests --

#[test]
fn identifier_normalization() {
    // x = 1 → Assignment [Placeholder(Variable, 0), Literal(Int)]
    let tree = parse_python("x = 1\n");
    let mapping = python_mapping();
    let mut ctx = NormalizationContext::new();
    let root = tree.root_node();

    // The root is "module", and its first named child is "expression_statement" or "assignment"
    let cursor = &mut root.walk();
    let first_stmt = root.named_children(cursor).next().unwrap();
    let node = normalize_ts_node(first_stmt, "x = 1\n".as_bytes(), &mapping, &mut ctx);

    assert_eq!(node.kind, NodeKind::Assign);
    assert_eq!(node.children.len(), 2);
    assert_eq!(
        node.children[0].kind,
        NodeKind::Placeholder(PlaceholderKind::Variable, 0)
    );
    assert_eq!(node.children[1].kind, NodeKind::Literal(LiteralKind::Int));
}

#[test]
fn renamed_variables_produce_identical_bodies() {
    let source_a = "def foo(a, b):\n    return a + b\n";
    let source_b = "def bar(x, y):\n    return x + y\n";

    let body_a = normalize_python_body(source_a);
    let body_b = normalize_python_body(source_b);

    // Both should produce the same normalized body since variables are positional
    assert_eq!(body_a, body_b);
}

#[test]
fn different_structure_produces_different_trees() {
    let source_add = "def foo(a, b):\n    return a + b\n";
    let source_mul = "def foo(a, b):\n    return a * b\n";

    let body_add = normalize_python_body(source_add);
    let body_mul = normalize_python_body(source_mul);

    assert_ne!(body_add, body_mul);
}

#[test]
fn literal_kind_preserved_value_erased() {
    let tree42 = parse_python("42\n");
    let tree99 = parse_python("99\n");
    let mapping = python_mapping();

    let mut ctx42 = NormalizationContext::new();
    let mut ctx99 = NormalizationContext::new();

    // Find the integer node in each
    fn find_first_named(node: tree_sitter::Node) -> tree_sitter::Node {
        let cursor = &mut node.walk();
        node.named_children(cursor)
            .next()
            .and_then(|n| {
                let c = &mut n.walk();
                n.named_children(c).next()
            })
            .unwrap_or(node)
    }

    let int42 = find_first_named(tree42.root_node());
    let int99 = find_first_named(tree99.root_node());

    let norm42 = normalize_ts_node(int42, "42\n".as_bytes(), &mapping, &mut ctx42);
    let norm99 = normalize_ts_node(int99, "99\n".as_bytes(), &mapping, &mut ctx99);

    assert_eq!(norm42.kind, NodeKind::Literal(LiteralKind::Int));
    assert_eq!(norm99.kind, NodeKind::Literal(LiteralKind::Int));
    assert_eq!(norm42, norm99);
}

#[test]
fn binary_operator_detection() {
    let source = "a + b\n";
    let tree = parse_python(source);
    let mapping = python_mapping();
    let mut ctx = NormalizationContext::new();

    // Root → module → expression_statement → binary_operator
    let root = tree.root_node();
    let expr_stmt = root.named_child(0).unwrap();
    let bin_op = expr_stmt.named_child(0).unwrap();

    let node = normalize_ts_node(bin_op, source.as_bytes(), &mapping, &mut ctx);
    assert_eq!(node.kind, NodeKind::BinaryOp(BinOpKind::Add));
    assert_eq!(node.children.len(), 2);
    assert_eq!(
        node.children[0].kind,
        NodeKind::Placeholder(PlaceholderKind::Variable, 0)
    );
    assert_eq!(
        node.children[1].kind,
        NodeKind::Placeholder(PlaceholderKind::Variable, 1)
    );
}

#[test]
fn if_else_normalization() {
    let source = "if x:\n    y = 1\nelse:\n    y = 2\n";
    let tree = parse_python(source);
    let mapping = python_mapping();
    let mut ctx = NormalizationContext::new();

    let root = tree.root_node();
    let if_node = root.named_child(0).unwrap();
    assert_eq!(if_node.kind(), "if_statement");

    let node = normalize_ts_node(if_node, source.as_bytes(), &mapping, &mut ctx);
    assert_eq!(node.kind, NodeKind::If);
    assert_eq!(node.children.len(), 3); // condition, then, else
    assert!(!node.children[2].is_none()); // else branch present
}

#[test]
fn code_unit_extraction() {
    let source = r#"
def add(a, b):
    result = a + b
    return result

def subtract(a, b):
    result = a - b
    return result
"#;
    let tree = parse_python(source);
    let mapping = python_mapping();
    let language = tree_sitter_python::LANGUAGE;
    let query = tree_sitter::Query::new(
        &language.into(),
        r#"
        (function_definition
            name: (identifier) @name
            parameters: (parameters) @parameters
            body: (block) @body
        ) @definition
        "#,
    )
    .expect("Invalid query");

    let config = AnalysisConfig {
        min_nodes: 1,
        min_lines: 1,
    };

    let units = dupes_treesitter::extract_code_units(
        &tree,
        source.as_bytes(),
        &PathBuf::from("test.py"),
        &query,
        &mapping,
        &config,
        &CodeUnitKind::Function,
        |_, _| false,
    );

    assert_eq!(units.len(), 2);
    assert_eq!(units[0].name, "add");
    assert_eq!(units[1].name, "subtract");
    assert!(units[0].line_start >= 2);
    assert!(units[1].line_start >= 6);
}

#[test]
fn min_nodes_filter() {
    let source = "def tiny():\n    pass\n";
    let tree = parse_python(source);
    let mapping = python_mapping();
    let language = tree_sitter_python::LANGUAGE;
    let query = tree_sitter::Query::new(
        &language.into(),
        r#"
        (function_definition
            name: (identifier) @name
            parameters: (parameters) @parameters
            body: (block) @body
        ) @definition
        "#,
    )
    .expect("Invalid query");

    let config = AnalysisConfig {
        min_nodes: 100, // Very high threshold
        min_lines: 1,
    };

    let units = dupes_treesitter::extract_code_units(
        &tree,
        source.as_bytes(),
        &PathBuf::from("test.py"),
        &query,
        &mapping,
        &config,
        &CodeUnitKind::Function,
        |_, _| false,
    );

    assert!(
        units.is_empty(),
        "Small function should be filtered out by min_nodes"
    );
}

#[test]
fn min_lines_filter() {
    let source = "def one_liner(): return 1\n";
    let tree = parse_python(source);
    let mapping = python_mapping();
    let language = tree_sitter_python::LANGUAGE;
    let query = tree_sitter::Query::new(
        &language.into(),
        r#"
        (function_definition
            name: (identifier) @name
            parameters: (parameters) @parameters
            body: (block) @body
        ) @definition
        "#,
    )
    .expect("Invalid query");

    let config = AnalysisConfig {
        min_nodes: 1,
        min_lines: 5, // Requires 5+ lines
    };

    let units = dupes_treesitter::extract_code_units(
        &tree,
        source.as_bytes(),
        &PathBuf::from("test.py"),
        &query,
        &mapping,
        &config,
        &CodeUnitKind::Function,
        |_, _| false,
    );

    assert!(
        units.is_empty(),
        "Single-line function should be filtered out by min_lines"
    );
}

#[test]
fn duplicate_fingerprints() {
    let source = r#"
def add(a, b):
    result = a + b
    return result

def add2(x, y):
    result = x + y
    return result
"#;
    let tree = parse_python(source);
    let mapping = python_mapping();
    let language = tree_sitter_python::LANGUAGE;
    let query = tree_sitter::Query::new(
        &language.into(),
        r#"
        (function_definition
            name: (identifier) @name
            parameters: (parameters) @parameters
            body: (block) @body
        ) @definition
        "#,
    )
    .expect("Invalid query");

    let config = AnalysisConfig {
        min_nodes: 1,
        min_lines: 1,
    };

    let units = dupes_treesitter::extract_code_units(
        &tree,
        source.as_bytes(),
        &PathBuf::from("test.py"),
        &query,
        &mapping,
        &config,
        &CodeUnitKind::Function,
        |_, _| false,
    );

    assert_eq!(units.len(), 2);
    assert_eq!(
        units[0].fingerprint, units[1].fingerprint,
        "Structurally identical functions should have the same fingerprint"
    );
}

#[test]
fn different_fingerprints() {
    let source = r#"
def add(a, b):
    return a + b

def mul(a, b):
    return a * b
"#;
    let tree = parse_python(source);
    let mapping = python_mapping();
    let language = tree_sitter_python::LANGUAGE;
    let query = tree_sitter::Query::new(
        &language.into(),
        r#"
        (function_definition
            name: (identifier) @name
            parameters: (parameters) @parameters
            body: (block) @body
        ) @definition
        "#,
    )
    .expect("Invalid query");

    let config = AnalysisConfig {
        min_nodes: 1,
        min_lines: 1,
    };

    let units = dupes_treesitter::extract_code_units(
        &tree,
        source.as_bytes(),
        &PathBuf::from("test.py"),
        &query,
        &mapping,
        &config,
        &CodeUnitKind::Function,
        |_, _| false,
    );

    assert_eq!(units.len(), 2);
    assert_ne!(
        units[0].fingerprint, units[1].fingerprint,
        "Structurally different functions should have different fingerprints"
    );
}

#[test]
fn error_node_becomes_opaque() {
    // Malformed Python source — tree-sitter will produce ERROR nodes
    // Use severely broken syntax to force ERROR node creation
    let source = "((( @@@ )))  def\n";
    let tree = parse_python(source);
    let mapping = python_mapping();
    let mut ctx = NormalizationContext::new();

    let root = tree.root_node();
    assert!(root.has_error(), "Test source should trigger parse errors");
    let node = normalize_ts_node(root, source.as_bytes(), &mapping, &mut ctx);

    // The tree should contain Opaque nodes for error parts
    fn contains_opaque(node: &NormalizedNode) -> bool {
        if node.kind == NodeKind::Opaque {
            return true;
        }
        node.children.iter().any(contains_opaque)
    }

    assert!(
        contains_opaque(&node),
        "Malformed source should produce at least one Opaque node"
    );
}

#[test]
fn nested_calls() {
    let source = "f(g(x))\n";
    let tree = parse_python(source);
    let mapping = python_mapping();
    let mut ctx = NormalizationContext::new();

    // Navigate to the call expression
    let root = tree.root_node();
    let expr_stmt = root.named_child(0).unwrap();
    let call = expr_stmt.named_child(0).unwrap();

    let node = normalize_ts_node(call, source.as_bytes(), &mapping, &mut ctx);

    assert_eq!(node.kind, NodeKind::Call);
    // First child is the function name, second is the argument
    assert_eq!(
        node.children[0].kind,
        NodeKind::Placeholder(PlaceholderKind::Variable, 0)
    );
    // The argument should itself be a Call
    assert_eq!(node.children[1].kind, NodeKind::Call);
}

#[test]
fn empty_function_body() {
    let source = "def nothing():\n    pass\n";
    let body = normalize_python_body(source);
    // pass produces a minimal tree
    let count = count_nodes(&body);
    assert!(count >= 1, "Even 'pass' should produce at least 1 node");
}

#[test]
fn while_loop_normalization() {
    let source = "while x:\n    y = 1\n";
    let tree = parse_python(source);
    let mapping = python_mapping();
    let mut ctx = NormalizationContext::new();

    let root = tree.root_node();
    let while_node = root.named_child(0).unwrap();

    let node = normalize_ts_node(while_node, source.as_bytes(), &mapping, &mut ctx);
    assert_eq!(node.kind, NodeKind::While);
    assert_eq!(node.children.len(), 2); // condition, body
}

#[test]
fn for_loop_normalization() {
    let source = "for x in items:\n    print(x)\n";
    let tree = parse_python(source);
    let mapping = python_mapping();
    let mut ctx = NormalizationContext::new();

    let root = tree.root_node();
    let for_node = root.named_child(0).unwrap();

    let node = normalize_ts_node(for_node, source.as_bytes(), &mapping, &mut ctx);
    assert_eq!(node.kind, NodeKind::ForLoop);
    assert_eq!(node.children.len(), 3); // pattern, iterable, body
}
