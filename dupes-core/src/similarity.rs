use crate::node::{NodeKind, NormalizedNode};

/// Compute a similarity score between two normalized trees using the Dice coefficient.
/// Returns a value between 0.0 (completely different) and 1.0 (identical).
///
/// score = (2 * matching_nodes) / (nodes_a + nodes_b)
pub fn similarity_score(a: &NormalizedNode, b: &NormalizedNode) -> f64 {
    let nodes_a = crate::node::count_nodes(a);
    let nodes_b = crate::node::count_nodes(b);
    if nodes_a == 0 && nodes_b == 0 {
        return 1.0;
    }
    let matching = count_matching(a, b);
    (2.0 * matching as f64) / (nodes_a + nodes_b) as f64
}

/// Count matching nodes between two trees by traversing in parallel.
/// Two nodes "match" if their kind (discriminant + immediate data) are equal.
/// None sentinel nodes contribute 0 to matching.
fn count_matching(a: &NormalizedNode, b: &NormalizedNode) -> usize {
    if a.is_none() || b.is_none() {
        return 0;
    }
    if std::mem::discriminant(&a.kind) != std::mem::discriminant(&b.kind) {
        return 0;
    }
    // MacroCall: different names = no match (no recursion into children)
    if let (NodeKind::MacroCall { name: na }, NodeKind::MacroCall { name: nb }) = (&a.kind, &b.kind)
        && na != nb
    {
        return 0;
    }
    let self_match = usize::from(a.kind == b.kind);
    self_match
        + a.children
            .iter()
            .zip(&b.children)
            .map(|(ca, cb)| count_matching(ca, cb))
            .sum::<usize>()
}
