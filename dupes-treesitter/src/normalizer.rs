//! Tree-sitter node normalization.
//!
//! Converts tree-sitter CST nodes into `dupes-core` [`NormalizedNode`] trees
//! using a table-driven [`NodeMapping`]. The primary entry point is
//! [`normalize_ts_node`].

use dupes_core::node::{
    BinOpKind, NodeKind, NormalizationContext, NormalizedNode, PlaceholderKind, UnOpKind,
};

use crate::mapping::NodeMapping;

/// Normalize a tree-sitter node into a `NormalizedNode` using the provided mapping.
///
/// The mapping table drives classification of tree-sitter node kinds into
/// the dupes-core normalized representation. Unknown named nodes are recursively
/// normalized and wrapped in a `Block`.
#[must_use]
pub fn normalize_ts_node(
    node: tree_sitter::Node,
    source: &[u8],
    mapping: &NodeMapping,
    ctx: &mut NormalizationContext,
) -> NormalizedNode {
    let kind = node.kind();

    // 1. Error/missing nodes → Opaque
    if kind == "ERROR" || kind == "MISSING" {
        return NormalizedNode::leaf(NodeKind::Opaque);
    }

    // 2. Skip kinds → shouldn't be called directly, but return Opaque as safety
    if mapping.skip_kinds.contains(kind) {
        return NormalizedNode::leaf(NodeKind::Opaque);
    }

    // 3. Opaque kinds → leaf
    if mapping.opaque_kinds.contains(kind) {
        return NormalizedNode::leaf(NodeKind::Opaque);
    }

    // 4. Identifiers → Placeholder
    if mapping.identifier_kinds.contains(kind) {
        let text = node_text(node, source);
        let idx = ctx.placeholder(&text, PlaceholderKind::Variable);
        return NormalizedNode::leaf(NodeKind::Placeholder(PlaceholderKind::Variable, idx));
    }

    // 5. Literals → Literal(kind)
    if let Some(lit_kind) = mapping.literal_kinds.get(kind) {
        return NormalizedNode::leaf(NodeKind::Literal(lit_kind.clone()));
    }

    // 6. Structural kinds

    // If/conditional
    if mapping.if_kinds.contains(kind) {
        return normalize_if(node, source, mapping, ctx);
    }

    // For loop
    if mapping.for_kinds.contains(kind) {
        return normalize_for(node, source, mapping, ctx);
    }

    // While loop
    if mapping.while_kinds.contains(kind) {
        return normalize_while(node, source, mapping, ctx);
    }

    // Infinite loop
    if mapping.loop_kinds.contains(kind) {
        let body = get_field_or_none(node, "body", source, mapping, ctx);
        return NormalizedNode::with_children(NodeKind::Loop, vec![body]);
    }

    // Match/switch
    if mapping.match_kinds.contains(kind) {
        return normalize_match(node, source, mapping, ctx);
    }

    // Call
    if mapping.call_kinds.contains(kind) {
        return normalize_call(node, source, mapping, ctx);
    }

    // Return
    if mapping.return_kinds.contains(kind) {
        return normalize_return(node, source, mapping, ctx);
    }

    // Block
    if mapping.block_kinds.contains(kind) {
        let children = normalize_named_children(node, source, mapping, ctx);
        return NormalizedNode::with_children(NodeKind::Block, children);
    }

    // Assignment
    if mapping.assignment_kinds.contains(kind) {
        return normalize_assignment(node, source, mapping, ctx);
    }

    // Function definitions (nested)
    if mapping.function_def_kinds.contains(kind) {
        let children = normalize_named_children(node, source, mapping, ctx);
        return NormalizedNode::with_children(NodeKind::Block, children);
    }

    // 7. Binary/unary expressions — driven by mapping
    if mapping.binary_op_kinds.contains(kind) {
        return normalize_binary_op(node, source, mapping, ctx);
    }

    if mapping.unary_op_kinds.contains(kind) {
        return normalize_unary_op(node, source, mapping, ctx);
    }

    // 8. Anonymous nodes → skip (shouldn't reach here normally)
    if !node.is_named() {
        return NormalizedNode::leaf(NodeKind::Opaque);
    }

    // 9. Unknown named nodes → recursively normalize children, wrap in Block
    let mut children = normalize_named_children(node, source, mapping, ctx);
    if children.is_empty() {
        return NormalizedNode::leaf(NodeKind::Opaque);
    }
    if children.len() == 1 {
        return children.swap_remove(0);
    }
    NormalizedNode::with_children(NodeKind::Block, children)
}

/// Normalize all named children of a node, skipping those in `skip_kinds`.
#[must_use]
pub(crate) fn normalize_named_children(
    node: tree_sitter::Node,
    source: &[u8],
    mapping: &NodeMapping,
    ctx: &mut NormalizationContext,
) -> Vec<NormalizedNode> {
    let mut children = Vec::new();
    let cursor = &mut node.walk();
    for child in node.named_children(cursor) {
        if mapping.skip_kinds.contains(child.kind()) {
            continue;
        }
        children.push(normalize_ts_node(child, source, mapping, ctx));
    }
    children
}

/// Get a field child by name, normalizing it, or return `NormalizedNode::none()`.
fn get_field_or_none(
    node: tree_sitter::Node,
    field: &str,
    source: &[u8],
    mapping: &NodeMapping,
    ctx: &mut NormalizationContext,
) -> NormalizedNode {
    node.child_by_field_name(field)
        .map_or_else(NormalizedNode::none, |child| {
            normalize_ts_node(child, source, mapping, ctx)
        })
}

/// Find the text of the first anonymous child (used for operator detection).
fn find_operator_text(node: tree_sitter::Node, source: &[u8]) -> Option<String> {
    let cursor = &mut node.walk();
    for child in node.children(cursor) {
        if !child.is_named() {
            let text = node_text(child, source);
            if !text.is_empty() {
                return Some(text);
            }
        }
    }
    None
}

/// Get the text of a tree-sitter node.
fn node_text(node: tree_sitter::Node, source: &[u8]) -> String {
    node.utf8_text(source).unwrap_or("").to_string()
}

/// Normalize an if/conditional construct.
/// Produces: [condition, then_branch, else_or_None]
fn normalize_if(
    node: tree_sitter::Node,
    source: &[u8],
    mapping: &NodeMapping,
    ctx: &mut NormalizationContext,
) -> NormalizedNode {
    let condition = get_field_or_none(node, "condition", source, mapping, ctx);
    let consequence = get_field_or_none(node, "consequence", source, mapping, ctx);
    let alternative = get_field_or_none(node, "alternative", source, mapping, ctx);
    NormalizedNode::with_children(NodeKind::If, vec![condition, consequence, alternative])
}

/// Normalize a for-loop construct.
/// Produces: [pattern, iterable, body]
fn normalize_for(
    node: tree_sitter::Node,
    source: &[u8],
    mapping: &NodeMapping,
    ctx: &mut NormalizationContext,
) -> NormalizedNode {
    // Python uses "left" for pattern and "right" for iterable
    let pattern = node
        .child_by_field_name("left")
        .map_or_else(NormalizedNode::none, |c| {
            normalize_ts_node(c, source, mapping, ctx)
        });
    let iterable = node
        .child_by_field_name("right")
        .map_or_else(NormalizedNode::none, |c| {
            normalize_ts_node(c, source, mapping, ctx)
        });
    let body = get_field_or_none(node, "body", source, mapping, ctx);
    NormalizedNode::with_children(NodeKind::ForLoop, vec![pattern, iterable, body])
}

/// Normalize a while-loop construct.
/// Produces: [condition, body]
fn normalize_while(
    node: tree_sitter::Node,
    source: &[u8],
    mapping: &NodeMapping,
    ctx: &mut NormalizationContext,
) -> NormalizedNode {
    let condition = get_field_or_none(node, "condition", source, mapping, ctx);
    let body = get_field_or_none(node, "body", source, mapping, ctx);
    NormalizedNode::with_children(NodeKind::While, vec![condition, body])
}

/// Normalize a match/switch construct.
/// Produces: `[subject, arm0, arm1, ...]` where each arm is
/// `MatchArm [pattern, guard_or_None, body]` per dupes-core convention.
fn normalize_match(
    node: tree_sitter::Node,
    source: &[u8],
    mapping: &NodeMapping,
    ctx: &mut NormalizationContext,
) -> NormalizedNode {
    let subject = get_field_or_none(node, "subject", source, mapping, ctx);
    let mut children = vec![subject];

    // Collect match arms/cases using mapping-driven kind detection
    let cursor = &mut node.walk();
    for child in node.named_children(cursor) {
        if mapping.match_arm_kinds.contains(child.kind()) {
            let pattern = get_field_or_none(child, "pattern", source, mapping, ctx);
            let guard = get_field_or_none(child, "guard", source, mapping, ctx);
            let body = get_field_or_none(child, "consequence", source, mapping, ctx);
            children.push(NormalizedNode::with_children(
                NodeKind::MatchArm,
                vec![pattern, guard, body],
            ));
        }
    }

    NormalizedNode::with_children(NodeKind::Match, children)
}

/// Normalize a function/method call.
/// Produces: [func, arg0, arg1, ...]
fn normalize_call(
    node: tree_sitter::Node,
    source: &[u8],
    mapping: &NodeMapping,
    ctx: &mut NormalizationContext,
) -> NormalizedNode {
    let func = get_field_or_none(node, "function", source, mapping, ctx);
    let mut children = vec![func];

    // Collect arguments
    if let Some(args) = node.child_by_field_name("arguments") {
        let cursor = &mut args.walk();
        for arg in args.named_children(cursor) {
            if !mapping.skip_kinds.contains(arg.kind()) {
                children.push(normalize_ts_node(arg, source, mapping, ctx));
            }
        }
    }

    NormalizedNode::with_children(NodeKind::Call, children)
}

/// Normalize a return statement.
/// Produces: [] or [value]
fn normalize_return(
    node: tree_sitter::Node,
    source: &[u8],
    mapping: &NodeMapping,
    ctx: &mut NormalizationContext,
) -> NormalizedNode {
    let children = normalize_named_children(node, source, mapping, ctx);
    NormalizedNode::with_children(NodeKind::Return, children)
}

/// Normalize an assignment.
/// Produces: [target, value]
fn normalize_assignment(
    node: tree_sitter::Node,
    source: &[u8],
    mapping: &NodeMapping,
    ctx: &mut NormalizationContext,
) -> NormalizedNode {
    let left = get_field_or_none(node, "left", source, mapping, ctx);
    let right = get_field_or_none(node, "right", source, mapping, ctx);
    NormalizedNode::with_children(NodeKind::Assign, vec![left, right])
}

/// Normalize a binary operation.
/// Produces: BinaryOp(kind) [left, right]
fn normalize_binary_op(
    node: tree_sitter::Node,
    source: &[u8],
    mapping: &NodeMapping,
    ctx: &mut NormalizationContext,
) -> NormalizedNode {
    let op_kind = find_operator_text(node, source)
        .and_then(|text| mapping.binary_op_map.get(text.as_str()).cloned())
        .unwrap_or(BinOpKind::Other);

    let left = get_field_or_none(node, "left", source, mapping, ctx);
    let right = get_field_or_none(node, "right", source, mapping, ctx);
    NormalizedNode::with_children(NodeKind::BinaryOp(op_kind), vec![left, right])
}

/// Normalize a unary operation.
/// Produces: UnaryOp(kind) [operand]
fn normalize_unary_op(
    node: tree_sitter::Node,
    source: &[u8],
    mapping: &NodeMapping,
    ctx: &mut NormalizationContext,
) -> NormalizedNode {
    let op_kind = find_operator_text(node, source)
        .and_then(|text| mapping.unary_op_map.get(text.as_str()).cloned())
        .unwrap_or(UnOpKind::Other);

    let operand_node = node
        .child_by_field_name("argument")
        .or_else(|| node.child_by_field_name("operand"))
        .or_else(|| {
            let cursor = &mut node.walk();
            node.named_children(cursor).next()
        });
    let operand = operand_node.map_or_else(NormalizedNode::none, |c| {
        normalize_ts_node(c, source, mapping, ctx)
    });

    NormalizedNode::with_children(NodeKind::UnaryOp(op_kind), vec![operand])
}

#[cfg(test)]
mod tests {
    use dupes_core::node::LiteralKind;

    use super::*;

    /// Build a Python-flavored mapping for normalizer unit tests.
    fn test_mapping() -> NodeMapping {
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
                ("==", BinOpKind::Eq),
                ("and", BinOpKind::And),
                ("or", BinOpKind::Or),
            ])
            .unary_ops(&[("not", UnOpKind::Not), ("-", UnOpKind::Neg)])
            .skip(&["comment", "decorator"])
            .opaque(&["type"])
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
    fn parse(source: &str) -> tree_sitter::Tree {
        let mut parser = tree_sitter::Parser::new();
        let lang = tree_sitter_python::LANGUAGE;
        parser.set_language(&lang.into()).unwrap();
        parser.parse(source, None).unwrap()
    }

    /// Get the first named child of the root (typically a statement).
    fn first_stmt<'a>(tree: &'a tree_sitter::Tree) -> tree_sitter::Node<'a> {
        let root = tree.root_node();
        let cursor = &mut root.walk();
        root.named_children(cursor).next().unwrap()
    }

    #[test]
    fn node_kind_classification() {
        let mapping = NodeMapping::new()
            .identifiers(&["identifier"])
            .literals(&[("integer", LiteralKind::Int)]);

        assert!(mapping.identifier_kinds.contains("identifier"));
        assert!(mapping.literal_kinds.contains_key("integer"));
    }

    #[test]
    fn identifier_becomes_placeholder() {
        let src = "x\n";
        let tree = parse(src);
        let mapping = test_mapping();
        let mut ctx = NormalizationContext::new();

        let stmt = first_stmt(&tree);
        // expression_statement -> identifier
        let ident = stmt.named_child(0).unwrap();
        let node = normalize_ts_node(ident, src.as_bytes(), &mapping, &mut ctx);
        assert_eq!(
            node.kind,
            NodeKind::Placeholder(PlaceholderKind::Variable, 0)
        );
    }

    #[test]
    fn two_identifiers_get_different_indices() {
        let src = "x\ny\n";
        let tree = parse(src);
        let mapping = test_mapping();
        let mut ctx = NormalizationContext::new();

        let root = tree.root_node();
        let cursor = &mut root.walk();
        let mut stmts = root.named_children(cursor);

        let x = stmts.next().unwrap().named_child(0).unwrap();
        let y = stmts.next().unwrap().named_child(0).unwrap();

        let nx = normalize_ts_node(x, src.as_bytes(), &mapping, &mut ctx);
        let ny = normalize_ts_node(y, src.as_bytes(), &mapping, &mut ctx);

        assert_eq!(nx.kind, NodeKind::Placeholder(PlaceholderKind::Variable, 0));
        assert_eq!(ny.kind, NodeKind::Placeholder(PlaceholderKind::Variable, 1));
    }

    #[test]
    fn same_identifier_reuses_index() {
        let src = "x\nx\n";
        let tree = parse(src);
        let mapping = test_mapping();
        let mut ctx = NormalizationContext::new();

        let root = tree.root_node();
        let cursor = &mut root.walk();
        let mut stmts = root.named_children(cursor);

        let x1 = stmts.next().unwrap().named_child(0).unwrap();
        let x2 = stmts.next().unwrap().named_child(0).unwrap();

        let n1 = normalize_ts_node(x1, src.as_bytes(), &mapping, &mut ctx);
        let n2 = normalize_ts_node(x2, src.as_bytes(), &mapping, &mut ctx);

        assert_eq!(n1.kind, n2.kind);
    }

    #[test]
    fn integer_literal() {
        let src = "42\n";
        let tree = parse(src);
        let mapping = test_mapping();
        let mut ctx = NormalizationContext::new();

        let stmt = first_stmt(&tree);
        let lit = stmt.named_child(0).unwrap();
        let node = normalize_ts_node(lit, src.as_bytes(), &mapping, &mut ctx);
        assert_eq!(node.kind, NodeKind::Literal(LiteralKind::Int));
    }

    #[test]
    fn string_literal() {
        let src = "\"hello\"\n";
        let tree = parse(src);
        let mapping = test_mapping();
        let mut ctx = NormalizationContext::new();

        let stmt = first_stmt(&tree);
        let lit = stmt.named_child(0).unwrap();
        let node = normalize_ts_node(lit, src.as_bytes(), &mapping, &mut ctx);
        assert_eq!(node.kind, NodeKind::Literal(LiteralKind::Str));
    }

    #[test]
    fn literal_values_erased() {
        let mapping = test_mapping();

        let tree_a = parse("42\n");
        let tree_b = parse("99\n");
        let mut ctx_a = NormalizationContext::new();
        let mut ctx_b = NormalizationContext::new();

        let a = first_stmt(&tree_a).named_child(0).unwrap();
        let b = first_stmt(&tree_b).named_child(0).unwrap();

        let na = normalize_ts_node(a, "42\n".as_bytes(), &mapping, &mut ctx_a);
        let nb = normalize_ts_node(b, "99\n".as_bytes(), &mapping, &mut ctx_b);
        assert_eq!(na, nb);
    }

    #[test]
    fn binary_add() {
        let src = "a + b\n";
        let tree = parse(src);
        let mapping = test_mapping();
        let mut ctx = NormalizationContext::new();

        let stmt = first_stmt(&tree);
        let bin = stmt.named_child(0).unwrap();
        let node = normalize_ts_node(bin, src.as_bytes(), &mapping, &mut ctx);
        assert_eq!(node.kind, NodeKind::BinaryOp(BinOpKind::Add));
        assert_eq!(node.children.len(), 2);
    }

    #[test]
    fn binary_eq() {
        let src = "a == b\n";
        let tree = parse(src);
        let mapping = test_mapping();
        let mut ctx = NormalizationContext::new();

        let stmt = first_stmt(&tree);
        let bin = stmt.named_child(0).unwrap();
        let node = normalize_ts_node(bin, src.as_bytes(), &mapping, &mut ctx);
        assert_eq!(node.kind, NodeKind::BinaryOp(BinOpKind::Eq));
    }

    #[test]
    fn boolean_and() {
        let src = "a and b\n";
        let tree = parse(src);
        let mapping = test_mapping();
        let mut ctx = NormalizationContext::new();

        let stmt = first_stmt(&tree);
        let bin = stmt.named_child(0).unwrap();
        let node = normalize_ts_node(bin, src.as_bytes(), &mapping, &mut ctx);
        assert_eq!(node.kind, NodeKind::BinaryOp(BinOpKind::And));
    }

    #[test]
    fn unknown_binary_op_falls_back_to_other() {
        let src = "a ** b\n";
        let tree = parse(src);
        let mapping = test_mapping();
        let mut ctx = NormalizationContext::new();

        let stmt = first_stmt(&tree);
        let bin = stmt.named_child(0).unwrap();
        let node = normalize_ts_node(bin, src.as_bytes(), &mapping, &mut ctx);
        assert_eq!(node.kind, NodeKind::BinaryOp(BinOpKind::Other));
    }

    #[test]
    fn unary_not() {
        let src = "not x\n";
        let tree = parse(src);
        let mapping = test_mapping();
        let mut ctx = NormalizationContext::new();

        let stmt = first_stmt(&tree);
        let un = stmt.named_child(0).unwrap();
        let node = normalize_ts_node(un, src.as_bytes(), &mapping, &mut ctx);
        assert_eq!(node.kind, NodeKind::UnaryOp(UnOpKind::Not));
        assert_eq!(node.children.len(), 1);
    }

    #[test]
    fn unary_neg() {
        let src = "-x\n";
        let tree = parse(src);
        let mapping = test_mapping();
        let mut ctx = NormalizationContext::new();

        let stmt = first_stmt(&tree);
        let un = stmt.named_child(0).unwrap();
        let node = normalize_ts_node(un, src.as_bytes(), &mapping, &mut ctx);
        assert_eq!(node.kind, NodeKind::UnaryOp(UnOpKind::Neg));
    }

    #[test]
    fn assignment_normalization() {
        let src = "x = 1\n";
        let tree = parse(src);
        let mapping = test_mapping();
        let mut ctx = NormalizationContext::new();

        let stmt = first_stmt(&tree);
        let node = normalize_ts_node(stmt, src.as_bytes(), &mapping, &mut ctx);
        assert_eq!(node.kind, NodeKind::Assign);
        assert_eq!(node.children.len(), 2);
        assert_eq!(
            node.children[0].kind,
            NodeKind::Placeholder(PlaceholderKind::Variable, 0)
        );
        assert_eq!(node.children[1].kind, NodeKind::Literal(LiteralKind::Int));
    }

    #[test]
    fn return_with_value() {
        let src = "def f():\n    return 42\n";
        let tree = parse(src);
        let mapping = test_mapping();
        let mut ctx = NormalizationContext::new();

        // function_definition -> body -> block -> return_statement
        let func = first_stmt(&tree);
        let body = func.child_by_field_name("body").unwrap();
        let cursor = &mut body.walk();
        let ret = body.named_children(cursor).next().unwrap();

        let node = normalize_ts_node(ret, src.as_bytes(), &mapping, &mut ctx);
        assert_eq!(node.kind, NodeKind::Return);
        assert_eq!(node.children.len(), 1);
        assert_eq!(node.children[0].kind, NodeKind::Literal(LiteralKind::Int));
    }

    #[test]
    fn return_without_value() {
        let src = "def f():\n    return\n";
        let tree = parse(src);
        let mapping = test_mapping();
        let mut ctx = NormalizationContext::new();

        let func = first_stmt(&tree);
        let body = func.child_by_field_name("body").unwrap();
        let cursor = &mut body.walk();
        let ret = body.named_children(cursor).next().unwrap();

        let node = normalize_ts_node(ret, src.as_bytes(), &mapping, &mut ctx);
        assert_eq!(node.kind, NodeKind::Return);
        assert!(node.children.is_empty());
    }

    #[test]
    fn if_with_else() {
        let src = "if x:\n    y = 1\nelse:\n    y = 2\n";
        let tree = parse(src);
        let mapping = test_mapping();
        let mut ctx = NormalizationContext::new();

        let if_node = first_stmt(&tree);
        let node = normalize_ts_node(if_node, src.as_bytes(), &mapping, &mut ctx);
        assert_eq!(node.kind, NodeKind::If);
        assert_eq!(node.children.len(), 3);
        // else branch should not be None
        assert!(!node.children[2].is_none());
    }

    #[test]
    fn if_without_else() {
        let src = "if x:\n    y = 1\n";
        let tree = parse(src);
        let mapping = test_mapping();
        let mut ctx = NormalizationContext::new();

        let if_node = first_stmt(&tree);
        let node = normalize_ts_node(if_node, src.as_bytes(), &mapping, &mut ctx);
        assert_eq!(node.kind, NodeKind::If);
        assert_eq!(node.children.len(), 3);
        // else branch should be None
        assert!(node.children[2].is_none());
    }

    #[test]
    fn while_loop() {
        let src = "while x:\n    y = 1\n";
        let tree = parse(src);
        let mapping = test_mapping();
        let mut ctx = NormalizationContext::new();

        let whl = first_stmt(&tree);
        let node = normalize_ts_node(whl, src.as_bytes(), &mapping, &mut ctx);
        assert_eq!(node.kind, NodeKind::While);
        assert_eq!(node.children.len(), 2);
    }

    #[test]
    fn for_loop() {
        let src = "for x in items:\n    pass\n";
        let tree = parse(src);
        let mapping = test_mapping();
        let mut ctx = NormalizationContext::new();

        let for_node = first_stmt(&tree);
        let node = normalize_ts_node(for_node, src.as_bytes(), &mapping, &mut ctx);
        assert_eq!(node.kind, NodeKind::ForLoop);
        assert_eq!(node.children.len(), 3);
    }

    #[test]
    fn call_with_args() {
        let src = "f(a, b)\n";
        let tree = parse(src);
        let mapping = test_mapping();
        let mut ctx = NormalizationContext::new();

        let stmt = first_stmt(&tree);
        let call = stmt.named_child(0).unwrap();
        let node = normalize_ts_node(call, src.as_bytes(), &mapping, &mut ctx);
        assert_eq!(node.kind, NodeKind::Call);
        // func + 2 args = 3 children
        assert_eq!(node.children.len(), 3);
    }

    #[test]
    fn call_no_args() {
        let src = "f()\n";
        let tree = parse(src);
        let mapping = test_mapping();
        let mut ctx = NormalizationContext::new();

        let stmt = first_stmt(&tree);
        let call = stmt.named_child(0).unwrap();
        let node = normalize_ts_node(call, src.as_bytes(), &mapping, &mut ctx);
        assert_eq!(node.kind, NodeKind::Call);
        // just the function name
        assert_eq!(node.children.len(), 1);
    }

    #[test]
    fn skip_kinds_are_filtered() {
        // Comments should be skipped by normalize_named_children
        let src = "# comment\nx = 1\n";
        let tree = parse(src);
        let mapping = test_mapping();
        let mut ctx = NormalizationContext::new();

        let root = tree.root_node();
        let children = normalize_named_children(root, src.as_bytes(), &mapping, &mut ctx);
        // Only the assignment should remain, not the comment
        assert_eq!(children.len(), 1);
        assert_eq!(children[0].kind, NodeKind::Assign);
    }

    #[test]
    fn error_node_becomes_opaque() {
        let src = "((( @@@ )))\n";
        let tree = parse(src);
        let mapping = test_mapping();
        let mut ctx = NormalizationContext::new();

        let root = tree.root_node();
        assert!(root.has_error());
        let node = normalize_ts_node(root, src.as_bytes(), &mapping, &mut ctx);

        fn has_opaque(n: &NormalizedNode) -> bool {
            n.kind == NodeKind::Opaque || n.children.iter().any(has_opaque)
        }
        assert!(has_opaque(&node));
    }

    #[test]
    fn block_normalization() {
        let src = "def f():\n    x = 1\n    y = 2\n";
        let tree = parse(src);
        let mapping = test_mapping();
        let mut ctx = NormalizationContext::new();

        let func = first_stmt(&tree);
        let body = func.child_by_field_name("body").unwrap();
        let node = normalize_ts_node(body, src.as_bytes(), &mapping, &mut ctx);
        assert_eq!(node.kind, NodeKind::Block);
        assert_eq!(node.children.len(), 2);
    }

    #[test]
    fn single_child_unwrap() {
        // When an unknown named node has exactly one named child,
        // the single child should be returned directly (no wrapping Block)
        let src = "42\n";
        let tree = parse(src);
        let mapping = test_mapping();
        let mut ctx = NormalizationContext::new();

        // expression_statement has one named child (integer)
        let stmt = first_stmt(&tree);
        let node = normalize_ts_node(stmt, src.as_bytes(), &mapping, &mut ctx);
        // Should unwrap to the literal directly, not wrap in Block
        assert_eq!(node.kind, NodeKind::Literal(LiteralKind::Int));
    }
}
