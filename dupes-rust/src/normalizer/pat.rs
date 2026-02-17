use dupes_core::node::{NodeKind, NormalizationContext, NormalizedNode, PlaceholderKind};

use super::expr::normalize_expr;
use super::helpers::{member_to_string, normalize_lit, normalize_macro};

pub fn normalize_type(ty: &syn::Type, ctx: &mut NormalizationContext) -> NormalizedNode {
    match ty {
        syn::Type::Path(tp) => {
            // Single-segment paths become type placeholders
            if tp.qself.is_none() && tp.path.segments.len() == 1 {
                let seg = &tp.path.segments[0];
                let idx = ctx.placeholder(&seg.ident.to_string(), PlaceholderKind::Type);
                NormalizedNode::leaf(NodeKind::TypePlaceholder(PlaceholderKind::Type, idx))
            } else {
                let segments: Vec<NormalizedNode> = tp
                    .path
                    .segments
                    .iter()
                    .map(|seg| {
                        let idx = ctx.placeholder(&seg.ident.to_string(), PlaceholderKind::Type);
                        NormalizedNode::leaf(NodeKind::TypePlaceholder(PlaceholderKind::Type, idx))
                    })
                    .collect();
                NormalizedNode::with_children(NodeKind::TypePath, segments)
            }
        }
        syn::Type::Reference(r) => NormalizedNode::with_children(
            NodeKind::TypeReference {
                mutable: r.mutability.is_some(),
            },
            vec![normalize_type(&r.elem, ctx)],
        ),
        syn::Type::Tuple(t) => {
            if t.elems.is_empty() {
                NormalizedNode::leaf(NodeKind::TypeUnit)
            } else {
                NormalizedNode::with_children(
                    NodeKind::TypeTuple,
                    t.elems.iter().map(|e| normalize_type(e, ctx)).collect(),
                )
            }
        }
        syn::Type::Slice(s) => {
            NormalizedNode::with_children(NodeKind::TypeSlice, vec![normalize_type(&s.elem, ctx)])
        }
        syn::Type::Array(a) => NormalizedNode::with_children(
            NodeKind::TypeArray,
            vec![normalize_type(&a.elem, ctx), normalize_expr(&a.len, ctx)],
        ),
        syn::Type::ImplTrait(i) => NormalizedNode::with_children(
            NodeKind::TypeImplTrait,
            i.bounds
                .iter()
                .filter_map(|b| {
                    if let syn::TypeParamBound::Trait(t) = b {
                        let segments: Vec<NormalizedNode> = t
                            .path
                            .segments
                            .iter()
                            .map(|seg| {
                                let idx =
                                    ctx.placeholder(&seg.ident.to_string(), PlaceholderKind::Type);
                                NormalizedNode::leaf(NodeKind::TypePlaceholder(
                                    PlaceholderKind::Type,
                                    idx,
                                ))
                            })
                            .collect();
                        Some(if segments.len() == 1 {
                            segments.into_iter().next().unwrap()
                        } else {
                            NormalizedNode::with_children(NodeKind::TypePath, segments)
                        })
                    } else {
                        None
                    }
                })
                .collect(),
        ),
        syn::Type::Infer(_) => NormalizedNode::leaf(NodeKind::TypeInfer),
        syn::Type::Never(_) => NormalizedNode::leaf(NodeKind::TypeNever),
        syn::Type::Paren(p) => normalize_type(&p.elem, ctx),
        syn::Type::Macro(tm) => normalize_macro(&tm.mac, ctx),
        _ => NormalizedNode::leaf(NodeKind::Opaque),
    }
}

pub fn normalize_pat(pat: &syn::Pat, ctx: &mut NormalizationContext) -> NormalizedNode {
    match pat {
        syn::Pat::Ident(pi) => {
            let idx = ctx.placeholder(&pi.ident.to_string(), PlaceholderKind::Variable);
            NormalizedNode::leaf(NodeKind::PatPlaceholder(PlaceholderKind::Variable, idx))
        }
        syn::Pat::Wild(_) => NormalizedNode::leaf(NodeKind::PatWild),
        syn::Pat::Tuple(pt) => NormalizedNode::with_children(
            NodeKind::PatTuple,
            pt.elems.iter().map(|p| normalize_pat(p, ctx)).collect(),
        ),
        syn::Pat::TupleStruct(pts) => NormalizedNode::with_children(
            NodeKind::PatStruct,
            pts.elems.iter().map(|p| normalize_pat(p, ctx)).collect(),
        ),
        syn::Pat::Struct(ps) => NormalizedNode::with_children(
            NodeKind::PatStruct,
            ps.fields
                .iter()
                .map(|f| {
                    let value = normalize_pat(&f.pat, ctx);
                    let name_idx =
                        ctx.placeholder(&member_to_string(&f.member), PlaceholderKind::Variable);
                    NormalizedNode::with_children(
                        NodeKind::FieldValue,
                        vec![
                            NormalizedNode::leaf(NodeKind::PatPlaceholder(
                                PlaceholderKind::Variable,
                                name_idx,
                            )),
                            value,
                        ],
                    )
                })
                .collect(),
        ),
        syn::Pat::Or(po) => NormalizedNode::with_children(
            NodeKind::PatOr,
            po.cases.iter().map(|p| normalize_pat(p, ctx)).collect(),
        ),
        syn::Pat::Lit(pl) => {
            NormalizedNode::with_children(NodeKind::PatLiteral, vec![normalize_lit(&pl.lit)])
        }
        syn::Pat::Reference(pr) => NormalizedNode::with_children(
            NodeKind::PatReference {
                mutable: pr.mutability.is_some(),
            },
            vec![normalize_pat(&pr.pat, ctx)],
        ),
        syn::Pat::Slice(ps) => NormalizedNode::with_children(
            NodeKind::PatSlice,
            ps.elems.iter().map(|p| normalize_pat(p, ctx)).collect(),
        ),
        syn::Pat::Rest(_) => NormalizedNode::leaf(NodeKind::PatRest),
        // PatRange -> [from_or_None, to_or_None]
        syn::Pat::Range(pr) => NormalizedNode::with_children(
            NodeKind::PatRange,
            vec![
                NormalizedNode::opt(pr.start.as_ref().map(|e| normalize_expr(e, ctx))),
                NormalizedNode::opt(pr.end.as_ref().map(|e| normalize_expr(e, ctx))),
            ],
        ),
        syn::Pat::Path(pp) => {
            if pp.path.segments.len() == 1 {
                let seg = &pp.path.segments[0];
                let idx = ctx.placeholder(&seg.ident.to_string(), PlaceholderKind::Variable);
                NormalizedNode::leaf(NodeKind::PatPlaceholder(PlaceholderKind::Variable, idx))
            } else {
                NormalizedNode::with_children(
                    NodeKind::PatStruct,
                    pp.path
                        .segments
                        .iter()
                        .map(|seg| {
                            let idx =
                                ctx.placeholder(&seg.ident.to_string(), PlaceholderKind::Variable);
                            NormalizedNode::leaf(NodeKind::PatPlaceholder(
                                PlaceholderKind::Variable,
                                idx,
                            ))
                        })
                        .collect(),
                )
            }
        }
        syn::Pat::Type(pt) => normalize_pat(&pt.pat, ctx),
        syn::Pat::Macro(pm) => normalize_macro(&pm.mac, ctx),
        _ => NormalizedNode::leaf(NodeKind::Opaque),
    }
}
