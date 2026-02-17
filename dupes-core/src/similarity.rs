use crate::node::NormalizedNode;

/// Compute a similarity score between two normalized trees using the Dice coefficient.
/// Returns a value between 0.0 (completely different) and 1.0 (identical).
///
/// score = (2 * matching_nodes) / (nodes_a + nodes_b)
pub fn similarity_score(a: &NormalizedNode, b: &NormalizedNode) -> f64 {
    let nodes_a = count_all_nodes(a);
    let nodes_b = count_all_nodes(b);
    if nodes_a == 0 && nodes_b == 0 {
        return 1.0;
    }
    let matching = count_matching(a, b);
    (2.0 * matching as f64) / (nodes_a + nodes_b) as f64
}

/// Count total nodes in a tree.
fn count_all_nodes(node: &NormalizedNode) -> usize {
    crate::node::count_nodes(node)
}

/// Count matching nodes between two trees by traversing in parallel.
/// Two nodes "match" if their variant and immediate data (operator kind, literal kind,
/// placeholder indices, mutability) are equal, regardless of children.
fn count_matching(a: &NormalizedNode, b: &NormalizedNode) -> usize {
    use NormalizedNode::*;

    match (a, b) {
        (Block(sa), Block(sb)) => 1 + pairwise_matching(sa, sb),
        (
            LetBinding {
                pattern: pa,
                ty: ta,
                init: ia,
            },
            LetBinding {
                pattern: pb,
                ty: tb,
                init: ib,
            },
        ) => {
            1 + count_matching(pa, pb)
                + match (ta, tb) {
                    (Some(a), Some(b)) => count_matching(a, b),
                    (None, None) => 0,
                    _ => 0,
                }
                + match (ia, ib) {
                    (Some(a), Some(b)) => count_matching(a, b),
                    (None, None) => 0,
                    _ => 0,
                }
        }
        (Literal(ka), Literal(kb)) => {
            if ka == kb {
                1
            } else {
                0
            }
        }
        (Placeholder(ka, ia), Placeholder(kb, ib)) => {
            if ka == kb && ia == ib {
                1
            } else {
                0
            }
        }
        (
            BinaryOp {
                op: oa,
                left: la,
                right: ra,
            },
            BinaryOp {
                op: ob,
                left: lb,
                right: rb,
            },
        ) => {
            if oa == ob {
                1 + count_matching(la, lb) + count_matching(ra, rb)
            } else {
                count_matching(la, lb) + count_matching(ra, rb)
            }
        }
        (
            UnaryOp {
                op: oa,
                operand: ea,
            },
            UnaryOp {
                op: ob,
                operand: eb,
            },
        ) => (if oa == ob { 1 } else { 0 }) + count_matching(ea, eb),
        (Call { func: fa, args: aa }, Call { func: fb, args: ab }) => {
            1 + count_matching(fa, fb) + pairwise_matching(aa, ab)
        }
        (
            MethodCall {
                receiver: ra,
                method: ma,
                args: aa,
            },
            MethodCall {
                receiver: rb,
                method: mb,
                args: ab,
            },
        ) => 1 + count_matching(ra, rb) + count_matching(ma, mb) + pairwise_matching(aa, ab),
        (
            FieldAccess {
                base: ba,
                field: fa,
            },
            FieldAccess {
                base: bb,
                field: fb,
            },
        ) => 1 + count_matching(ba, bb) + count_matching(fa, fb),
        (
            Index {
                base: ba,
                index: ia,
            },
            Index {
                base: bb,
                index: ib,
            },
        ) => 1 + count_matching(ba, bb) + count_matching(ia, ib),
        (
            Closure {
                params: pa,
                body: ba,
            },
            Closure {
                params: pb,
                body: bb,
            },
        ) => 1 + pairwise_matching(pa, pb) + count_matching(ba, bb),
        (
            FnSignature {
                params: pa,
                return_type: ra,
            },
            FnSignature {
                params: pb,
                return_type: rb,
            },
        ) => {
            1 + pairwise_matching(pa, pb)
                + match (ra, rb) {
                    (Some(a), Some(b)) => count_matching(a, b),
                    (None, None) => 0,
                    _ => 0,
                }
        }
        (Return(ea), Return(eb)) => {
            1 + match (ea, eb) {
                (Some(a), Some(b)) => count_matching(a, b),
                (None, None) => 0,
                _ => 0,
            }
        }
        (Break(ea), Break(eb)) => {
            1 + match (ea, eb) {
                (Some(a), Some(b)) => count_matching(a, b),
                (None, None) => 0,
                _ => 0,
            }
        }
        (Continue, Continue) => 1,
        (
            Assign {
                left: la,
                right: ra,
            },
            Assign {
                left: lb,
                right: rb,
            },
        ) => 1 + count_matching(la, lb) + count_matching(ra, rb),
        (
            Reference {
                mutable: ma,
                expr: ea,
            },
            Reference {
                mutable: mb,
                expr: eb,
            },
        ) => (if ma == mb { 1 } else { 0 }) + count_matching(ea, eb),
        (Tuple(ea), Tuple(eb)) | (Array(ea), Array(eb)) => 1 + pairwise_matching(ea, eb),
        (Repeat { elem: ea, len: la }, Repeat { elem: eb, len: lb }) => {
            1 + count_matching(ea, eb) + count_matching(la, lb)
        }
        (Cast { expr: ea, ty: ta }, Cast { expr: eb, ty: tb }) => {
            1 + count_matching(ea, eb) + count_matching(ta, tb)
        }
        (
            StructInit {
                fields: fa,
                rest: ra,
            },
            StructInit {
                fields: fb,
                rest: rb,
            },
        ) => {
            1 + pairwise_matching(fa, fb)
                + match (ra, rb) {
                    (Some(a), Some(b)) => count_matching(a, b),
                    (None, None) => 0,
                    _ => 0,
                }
        }
        (Await(ea), Await(eb)) | (Try(ea), Try(eb)) => 1 + count_matching(ea, eb),
        (
            If {
                condition: ca,
                then_branch: ta,
                else_branch: ea,
            },
            If {
                condition: cb,
                then_branch: tb,
                else_branch: eb,
            },
        ) => {
            1 + count_matching(ca, cb)
                + count_matching(ta, tb)
                + match (ea, eb) {
                    (Some(a), Some(b)) => count_matching(a, b),
                    (None, None) => 0,
                    _ => 0,
                }
        }
        (Match { expr: ea, arms: aa }, Match { expr: eb, arms: ab }) => {
            1 + count_matching(ea, eb)
                + aa.iter()
                    .zip(ab.iter())
                    .map(|(a, b)| {
                        count_matching(&a.pattern, &b.pattern)
                            + match (&a.guard, &b.guard) {
                                (Some(ga), Some(gb)) => count_matching(ga, gb),
                                _ => 0,
                            }
                            + count_matching(&a.body, &b.body)
                    })
                    .sum::<usize>()
        }
        (Loop(ba), Loop(bb)) => 1 + count_matching(ba, bb),
        (
            While {
                condition: ca,
                body: ba,
            },
            While {
                condition: cb,
                body: bb,
            },
        ) => 1 + count_matching(ca, cb) + count_matching(ba, bb),
        (
            ForLoop {
                pat: pa,
                iter: ia,
                body: ba,
            },
            ForLoop {
                pat: pb,
                iter: ib,
                body: bb,
            },
        ) => 1 + count_matching(pa, pb) + count_matching(ia, ib) + count_matching(ba, bb),
        // Patterns
        (PatWild, PatWild) | (PatRest, PatRest) => 1,
        (PatPlaceholder(ka, ia), PatPlaceholder(kb, ib)) => {
            if ka == kb && ia == ib {
                1
            } else {
                0
            }
        }
        (PatTuple(ea), PatTuple(eb))
        | (PatStruct(ea), PatStruct(eb))
        | (PatOr(ea), PatOr(eb))
        | (PatSlice(ea), PatSlice(eb)) => 1 + pairwise_matching(ea, eb),
        (PatLiteral(ea), PatLiteral(eb)) => 1 + count_matching(ea, eb),
        (
            PatReference {
                mutable: ma,
                pat: pa,
            },
            PatReference {
                mutable: mb,
                pat: pb,
            },
        ) => (if ma == mb { 1 } else { 0 }) + count_matching(pa, pb),
        (PatRange { lo: la, hi: ha }, PatRange { lo: lb, hi: hb }) => {
            1 + match (la, lb) {
                (Some(a), Some(b)) => count_matching(a, b),
                _ => 0,
            } + match (ha, hb) {
                (Some(a), Some(b)) => count_matching(a, b),
                _ => 0,
            }
        }
        // Types
        (TypePlaceholder(ka, ia), TypePlaceholder(kb, ib)) => {
            if ka == kb && ia == ib {
                1
            } else {
                0
            }
        }
        (
            TypeReference {
                mutable: ma,
                elem: ea,
            },
            TypeReference {
                mutable: mb,
                elem: eb,
            },
        ) => (if ma == mb { 1 } else { 0 }) + count_matching(ea, eb),
        (TypeTuple(ea), TypeTuple(eb))
        | (TypePath(ea), TypePath(eb))
        | (TypeImplTrait(ea), TypeImplTrait(eb)) => 1 + pairwise_matching(ea, eb),
        (TypeSlice(ea), TypeSlice(eb)) => 1 + count_matching(ea, eb),
        (TypeArray { elem: ea, len: la }, TypeArray { elem: eb, len: lb }) => {
            1 + count_matching(ea, eb) + count_matching(la, lb)
        }
        (TypeInfer, TypeInfer) | (TypeUnit, TypeUnit) | (TypeNever, TypeNever) => 1,
        (
            FieldValue {
                name: na,
                value: va,
            },
            FieldValue {
                name: nb,
                value: vb,
            },
        ) => 1 + count_matching(na, nb) + count_matching(va, vb),
        (MacroCall { name: na, args: aa }, MacroCall { name: nb, args: ab }) => {
            if na == nb {
                1 + pairwise_matching(aa, ab)
            } else {
                0
            }
        }
        (Opaque, Opaque) => 1,
        (Range { from: fa, to: ta }, Range { from: fb, to: tb }) => {
            1 + match (fa, fb) {
                (Some(a), Some(b)) => count_matching(a, b),
                _ => 0,
            } + match (ta, tb) {
                (Some(a), Some(b)) => count_matching(a, b),
                _ => 0,
            }
        }
        (Path(sa), Path(sb)) => 1 + pairwise_matching(sa, sb),
        (LetExpr { pat: pa, expr: ea }, LetExpr { pat: pb, expr: eb }) => {
            1 + count_matching(pa, pb) + count_matching(ea, eb)
        }
        (Paren(ea), Paren(eb)) => 1 + count_matching(ea, eb),
        (Semi(ea), Semi(eb)) => 1 + count_matching(ea, eb),
        // Different node kinds — no match
        _ => 0,
    }
}

/// Count matching nodes pairwise between two slices.
fn pairwise_matching(a: &[NormalizedNode], b: &[NormalizedNode]) -> usize {
    a.iter()
        .zip(b.iter())
        .map(|(x, y)| count_matching(x, y))
        .sum()
}
