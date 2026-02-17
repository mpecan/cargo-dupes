use crate::code_unit::CodeUnitKind;
use crate::node::{self, NodeKind, NormalizedNode};

/// A sub-unit extracted from a normalized function body.
pub struct SubUnit {
    pub kind: CodeUnitKind,
    pub node: NormalizedNode,
    pub node_count: usize,
    pub description: String,
}

/// Extract candidate sub-units from a normalized AST node.
/// Walks the tree recursively and extracts natural compound structures
/// (if branches, match arm bodies, loop bodies, closure bodies).
/// Each sub-tree is re-indexed to canonical placeholder form.
/// Only sub-trees meeting `min_node_count` are returned.
#[must_use]
pub fn extract_sub_units(node: &NormalizedNode, min_node_count: usize) -> Vec<SubUnit> {
    let mut results = Vec::new();
    extract_recursive(node, min_node_count, &mut results);
    results
}

fn extract_recursive(node: &NormalizedNode, min_node_count: usize, results: &mut Vec<SubUnit>) {
    match &node.kind {
        // If -> [condition, then_branch, else_or_None]
        NodeKind::If => {
            if let Some(then_branch) = node.children.get(1) {
                try_add(
                    then_branch,
                    CodeUnitKind::IfBranch,
                    "if-then branch",
                    min_node_count,
                    results,
                );
            }
            if let Some(else_br) = node.children.get(2)
                && !else_br.is_none()
            {
                try_add(
                    else_br,
                    CodeUnitKind::IfBranch,
                    "if-else branch",
                    min_node_count,
                    results,
                );
            }
        }
        // Match -> [expr, arm0, arm1, ...]
        // Each arm is MatchArm -> [pattern, guard_or_None, body]
        NodeKind::Match => {
            for (i, arm) in node.children.iter().skip(1).enumerate() {
                if let Some(body) = arm.children.get(2) {
                    let desc = format!("match arm {}", i + 1);
                    try_add(body, CodeUnitKind::MatchArm, &desc, min_node_count, results);
                }
            }
        }
        // Loop -> [body]
        NodeKind::Loop => {
            if let Some(body) = node.children.first() {
                try_add(
                    body,
                    CodeUnitKind::LoopBody,
                    "loop body",
                    min_node_count,
                    results,
                );
            }
        }
        // While -> [condition, body]
        NodeKind::While => {
            if let Some(body) = node.children.get(1) {
                try_add(
                    body,
                    CodeUnitKind::LoopBody,
                    "while body",
                    min_node_count,
                    results,
                );
            }
        }
        // ForLoop -> [pat, iter, body]
        NodeKind::ForLoop => {
            if let Some(body) = node.children.get(2) {
                try_add(
                    body,
                    CodeUnitKind::LoopBody,
                    "for body",
                    min_node_count,
                    results,
                );
            }
        }
        // Closure -> [body, param0, ...]
        NodeKind::Closure => {
            if let Some(body) = node.children.first() {
                try_add(
                    body,
                    CodeUnitKind::Block,
                    "closure body",
                    min_node_count,
                    results,
                );
            }
        }
        _ => {}
    }

    // Always recurse into all children
    for child in &node.children {
        extract_recursive(child, min_node_count, results);
    }
}

fn try_add(
    node: &NormalizedNode,
    kind: CodeUnitKind,
    description: &str,
    min_node_count: usize,
    results: &mut Vec<SubUnit>,
) {
    let reindexed = node::reindex_placeholders(node);
    let node_count = node::count_nodes(&reindexed);
    if node_count >= min_node_count {
        results.push(SubUnit {
            kind,
            node: reindexed,
            node_count,
            description: description.to_string(),
        });
    }
}
