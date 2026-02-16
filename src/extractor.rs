use crate::normalizer::{self, NormalizedNode};
use crate::parser::CodeUnitKind;

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
pub fn extract_sub_units(node: &NormalizedNode, min_node_count: usize) -> Vec<SubUnit> {
    let mut results = Vec::new();
    extract_recursive(node, min_node_count, &mut results);
    results
}

fn extract_recursive(node: &NormalizedNode, min_node_count: usize, results: &mut Vec<SubUnit>) {
    match node {
        NormalizedNode::If {
            condition,
            then_branch,
            else_branch,
        } => {
            try_add(
                then_branch,
                CodeUnitKind::IfBranch,
                "if-then branch",
                min_node_count,
                results,
            );
            if let Some(else_br) = else_branch {
                try_add(
                    else_br,
                    CodeUnitKind::IfBranch,
                    "if-else branch",
                    min_node_count,
                    results,
                );
            }
            // Recurse into children
            extract_recursive(condition, min_node_count, results);
            extract_recursive(then_branch, min_node_count, results);
            if let Some(else_br) = else_branch {
                extract_recursive(else_br, min_node_count, results);
            }
        }
        NormalizedNode::Match { expr, arms } => {
            for (i, arm) in arms.iter().enumerate() {
                let desc = format!("match arm {}", i + 1);
                try_add(
                    &arm.body,
                    CodeUnitKind::MatchArm,
                    &desc,
                    min_node_count,
                    results,
                );
            }
            // Recurse into children
            extract_recursive(expr, min_node_count, results);
            for arm in arms {
                extract_recursive(&arm.pattern, min_node_count, results);
                if let Some(g) = &arm.guard {
                    extract_recursive(g, min_node_count, results);
                }
                extract_recursive(&arm.body, min_node_count, results);
            }
        }
        NormalizedNode::Loop(body) => {
            try_add(
                body,
                CodeUnitKind::LoopBody,
                "loop body",
                min_node_count,
                results,
            );
            extract_recursive(body, min_node_count, results);
        }
        NormalizedNode::While { condition, body } => {
            try_add(
                body,
                CodeUnitKind::LoopBody,
                "while body",
                min_node_count,
                results,
            );
            extract_recursive(condition, min_node_count, results);
            extract_recursive(body, min_node_count, results);
        }
        NormalizedNode::ForLoop { pat, iter, body } => {
            try_add(
                body,
                CodeUnitKind::LoopBody,
                "for body",
                min_node_count,
                results,
            );
            extract_recursive(pat, min_node_count, results);
            extract_recursive(iter, min_node_count, results);
            extract_recursive(body, min_node_count, results);
        }
        NormalizedNode::Closure { params, body } => {
            try_add(
                body,
                CodeUnitKind::Block,
                "closure body",
                min_node_count,
                results,
            );
            for p in params {
                extract_recursive(p, min_node_count, results);
            }
            extract_recursive(body, min_node_count, results);
        }
        // For blocks that are not the top-level function body, extract them
        // We only recurse into block contents; top-level block is the function body itself
        NormalizedNode::Block(stmts) => {
            for s in stmts {
                extract_recursive(s, min_node_count, results);
            }
        }
        // Recurse into other compound nodes
        NormalizedNode::LetBinding { pattern, ty, init } => {
            extract_recursive(pattern, min_node_count, results);
            if let Some(t) = ty {
                extract_recursive(t, min_node_count, results);
            }
            if let Some(i) = init {
                extract_recursive(i, min_node_count, results);
            }
        }
        NormalizedNode::BinaryOp { left, right, .. } | NormalizedNode::Assign { left, right } => {
            extract_recursive(left, min_node_count, results);
            extract_recursive(right, min_node_count, results);
        }
        NormalizedNode::UnaryOp { operand, .. } => {
            extract_recursive(operand, min_node_count, results)
        }
        NormalizedNode::Call { func, args } => {
            extract_recursive(func, min_node_count, results);
            for a in args {
                extract_recursive(a, min_node_count, results);
            }
        }
        NormalizedNode::MethodCall {
            receiver,
            method,
            args,
        } => {
            extract_recursive(receiver, min_node_count, results);
            extract_recursive(method, min_node_count, results);
            for a in args {
                extract_recursive(a, min_node_count, results);
            }
        }
        NormalizedNode::FieldAccess { base, field } => {
            extract_recursive(base, min_node_count, results);
            extract_recursive(field, min_node_count, results);
        }
        NormalizedNode::Index { base, index } => {
            extract_recursive(base, min_node_count, results);
            extract_recursive(index, min_node_count, results);
        }
        NormalizedNode::Reference { expr, .. } => extract_recursive(expr, min_node_count, results),
        NormalizedNode::Tuple(elems) | NormalizedNode::Array(elems) => {
            for e in elems {
                extract_recursive(e, min_node_count, results);
            }
        }
        NormalizedNode::Repeat { elem, len } => {
            extract_recursive(elem, min_node_count, results);
            extract_recursive(len, min_node_count, results);
        }
        NormalizedNode::Cast { expr, ty } => {
            extract_recursive(expr, min_node_count, results);
            extract_recursive(ty, min_node_count, results);
        }
        NormalizedNode::StructInit { fields, rest } => {
            for f in fields {
                extract_recursive(f, min_node_count, results);
            }
            if let Some(r) = rest {
                extract_recursive(r, min_node_count, results);
            }
        }
        NormalizedNode::Await(e)
        | NormalizedNode::Try(e)
        | NormalizedNode::Paren(e)
        | NormalizedNode::Semi(e) => {
            extract_recursive(e, min_node_count, results);
        }
        NormalizedNode::Return(e) | NormalizedNode::Break(e) => {
            if let Some(e) = e {
                extract_recursive(e, min_node_count, results);
            }
        }
        NormalizedNode::FnSignature {
            params,
            return_type,
        } => {
            for p in params {
                extract_recursive(p, min_node_count, results);
            }
            if let Some(r) = return_type {
                extract_recursive(r, min_node_count, results);
            }
        }
        NormalizedNode::FieldValue { name, value } => {
            extract_recursive(name, min_node_count, results);
            extract_recursive(value, min_node_count, results);
        }
        NormalizedNode::Range { from, to } => {
            if let Some(f) = from {
                extract_recursive(f, min_node_count, results);
            }
            if let Some(t) = to {
                extract_recursive(t, min_node_count, results);
            }
        }
        NormalizedNode::Path(segments) => {
            for s in segments {
                extract_recursive(s, min_node_count, results);
            }
        }
        NormalizedNode::LetExpr { pat, expr } => {
            extract_recursive(pat, min_node_count, results);
            extract_recursive(expr, min_node_count, results);
        }
        NormalizedNode::MacroCall { args, .. } => {
            for a in args {
                extract_recursive(a, min_node_count, results);
            }
        }
        NormalizedNode::PatTuple(elems)
        | NormalizedNode::PatStruct(elems)
        | NormalizedNode::PatOr(elems)
        | NormalizedNode::PatSlice(elems) => {
            for e in elems {
                extract_recursive(e, min_node_count, results);
            }
        }
        NormalizedNode::PatLiteral(e) => extract_recursive(e, min_node_count, results),
        NormalizedNode::PatReference { pat, .. } => extract_recursive(pat, min_node_count, results),
        NormalizedNode::PatRange { lo, hi } => {
            if let Some(l) = lo {
                extract_recursive(l, min_node_count, results);
            }
            if let Some(h) = hi {
                extract_recursive(h, min_node_count, results);
            }
        }
        NormalizedNode::TypeReference { elem, .. } | NormalizedNode::TypeSlice(elem) => {
            extract_recursive(elem, min_node_count, results);
        }
        NormalizedNode::TypeTuple(elems)
        | NormalizedNode::TypePath(elems)
        | NormalizedNode::TypeImplTrait(elems) => {
            for e in elems {
                extract_recursive(e, min_node_count, results);
            }
        }
        NormalizedNode::TypeArray { elem, len } => {
            extract_recursive(elem, min_node_count, results);
            extract_recursive(len, min_node_count, results);
        }
        // Leaf nodes — nothing to extract
        NormalizedNode::Literal(_)
        | NormalizedNode::Placeholder(_, _)
        | NormalizedNode::PatPlaceholder(_, _)
        | NormalizedNode::TypePlaceholder(_, _)
        | NormalizedNode::Continue
        | NormalizedNode::PatWild
        | NormalizedNode::PatRest
        | NormalizedNode::TypeInfer
        | NormalizedNode::TypeUnit
        | NormalizedNode::TypeNever
        | NormalizedNode::Opaque => {}
    }
}

fn try_add(
    node: &NormalizedNode,
    kind: CodeUnitKind,
    description: &str,
    min_node_count: usize,
    results: &mut Vec<SubUnit>,
) {
    let reindexed = normalizer::reindex_placeholders(node);
    let node_count = normalizer::count_nodes(&reindexed);
    if node_count >= min_node_count {
        results.push(SubUnit {
            kind,
            node: reindexed,
            node_count,
            description: description.to_string(),
        });
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::normalizer::{NormalizationContext, normalize_expr, normalize_item_fn};

    fn parse_fn(code: &str) -> syn::ItemFn {
        syn::parse_str::<syn::ItemFn>(code).unwrap()
    }

    fn parse_and_extract_body(code: &str) -> NormalizedNode {
        let f = parse_fn(code);
        let (_, body) = normalize_item_fn(&f);
        body
    }

    #[test]
    fn extracts_if_branches() {
        let body = parse_and_extract_body(
            "fn foo(x: i32) -> i32 { if x > 0 { let y = x + 1; y * 2 } else { let z = x - 1; z * 3 } }",
        );
        let subs = extract_sub_units(&body, 1);
        let if_branches: Vec<_> = subs
            .iter()
            .filter(|s| s.kind == CodeUnitKind::IfBranch)
            .collect();
        assert_eq!(if_branches.len(), 2); // then + else
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
        let subs = extract_sub_units(&body, 1);
        let match_arms: Vec<_> = subs
            .iter()
            .filter(|s| s.kind == CodeUnitKind::MatchArm)
            .collect();
        assert_eq!(match_arms.len(), 3);
    }

    #[test]
    fn extracts_loop_bodies() {
        let body = parse_and_extract_body(
            "fn foo(x: i32) { for i in 0..10 { let y = i + x; let _ = y; } }",
        );
        let subs = extract_sub_units(&body, 1);
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
        let subs_low = extract_sub_units(&body, 1);
        let subs_high = extract_sub_units(&body, 100);
        assert!(!subs_low.is_empty());
        assert!(subs_high.is_empty());
    }

    #[test]
    fn identical_branches_from_different_functions_match() {
        // Two functions with identical if-then branches but different variable names
        let body1 = parse_and_extract_body(
            "fn foo(unused: i32, x: i32) -> i32 { if x > 0 { let y = x + 1; y * 2 } else { x } }",
        );
        let body2 = parse_and_extract_body(
            "fn bar(a: i32) -> i32 { if a > 0 { let b = a + 1; b * 2 } else { a } }",
        );

        let subs1 = extract_sub_units(&body1, 1);
        let subs2 = extract_sub_units(&body2, 1);

        let then1 = subs1
            .iter()
            .find(|s| s.description == "if-then branch")
            .unwrap();
        let then2 = subs2
            .iter()
            .find(|s| s.description == "if-then branch")
            .unwrap();

        // After re-indexing (done by extract_sub_units), they should be structurally identical
        assert_eq!(then1.node, then2.node);
    }

    #[test]
    fn sub_units_are_reindexed() {
        // Verify that extracted sub-units have placeholder indices starting from 0
        let body = parse_and_extract_body(
            "fn foo(a: i32, b: i32, c: i32) -> i32 { if c > 0 { let d = c + 1; d } else { c } }",
        );
        let subs = extract_sub_units(&body, 1);
        let then_branch = subs
            .iter()
            .find(|s| s.description == "if-then branch")
            .unwrap();

        // The then branch uses 'c' and 'd' from parent context.
        // After re-indexing, these should become 0 and 1 (not 2 and 3).
        // Check by comparing with a hand-built expected tree.
        let mut ctx = NormalizationContext::new();
        let fresh_expr = normalize_expr(
            &syn::parse_str::<syn::Expr>("{ let d = c + 1; d }").unwrap(),
            &mut ctx,
        );
        let reindexed_fresh = normalizer::reindex_placeholders(&fresh_expr);
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
        let subs = extract_sub_units(&body, 1);
        // Should find: if-then branch, if-else branch, for body (nested inside if-then)
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
}
