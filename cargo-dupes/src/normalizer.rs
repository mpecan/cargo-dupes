// Re-export all core types so `use crate::normalizer::*` still works
pub use dupes_core::node::*;

use syn::punctuated::Punctuated;

// -- Helpers ------------------------------------------------------------------

fn member_to_string(member: &syn::Member) -> String {
    match member {
        syn::Member::Named(ident) => ident.to_string(),
        syn::Member::Unnamed(idx) => idx.index.to_string(),
    }
}

fn normalize_macro(mac: &syn::Macro, ctx: &mut NormalizationContext) -> NormalizedNode {
    let name = mac
        .path
        .segments
        .last()
        .map(|s| s.ident.to_string())
        .unwrap_or_default();
    let args = if mac.tokens.is_empty() {
        Vec::new()
    } else {
        match mac.parse_body_with(Punctuated::<syn::Expr, syn::Token![,]>::parse_terminated) {
            Ok(punct) => punct.into_iter().map(|e| normalize_expr(&e, ctx)).collect(),
            Err(_) => vec![NormalizedNode::leaf(NodeKind::Opaque)],
        }
    };
    NormalizedNode::with_children(NodeKind::MacroCall { name }, args)
}

// -- Normalization functions --------------------------------------------------

pub fn normalize_lit(lit: &syn::Lit) -> NormalizedNode {
    match lit {
        syn::Lit::Str(_) => NormalizedNode::leaf(NodeKind::Literal(LiteralKind::Str)),
        syn::Lit::ByteStr(_) => NormalizedNode::leaf(NodeKind::Literal(LiteralKind::ByteStr)),
        syn::Lit::CStr(_) => NormalizedNode::leaf(NodeKind::Literal(LiteralKind::CStr)),
        syn::Lit::Byte(_) => NormalizedNode::leaf(NodeKind::Literal(LiteralKind::Byte)),
        syn::Lit::Char(_) => NormalizedNode::leaf(NodeKind::Literal(LiteralKind::Char)),
        syn::Lit::Int(_) => NormalizedNode::leaf(NodeKind::Literal(LiteralKind::Int)),
        syn::Lit::Float(_) => NormalizedNode::leaf(NodeKind::Literal(LiteralKind::Float)),
        syn::Lit::Bool(_) => NormalizedNode::leaf(NodeKind::Literal(LiteralKind::Bool)),
        _ => NormalizedNode::leaf(NodeKind::Opaque),
    }
}

pub fn normalize_bin_op(op: &syn::BinOp) -> BinOpKind {
    match op {
        syn::BinOp::Add(_) => BinOpKind::Add,
        syn::BinOp::Sub(_) => BinOpKind::Sub,
        syn::BinOp::Mul(_) => BinOpKind::Mul,
        syn::BinOp::Div(_) => BinOpKind::Div,
        syn::BinOp::Rem(_) => BinOpKind::Rem,
        syn::BinOp::And(_) => BinOpKind::And,
        syn::BinOp::Or(_) => BinOpKind::Or,
        syn::BinOp::BitXor(_) => BinOpKind::BitXor,
        syn::BinOp::BitAnd(_) => BinOpKind::BitAnd,
        syn::BinOp::BitOr(_) => BinOpKind::BitOr,
        syn::BinOp::Shl(_) => BinOpKind::Shl,
        syn::BinOp::Shr(_) => BinOpKind::Shr,
        syn::BinOp::Eq(_) => BinOpKind::Eq,
        syn::BinOp::Lt(_) => BinOpKind::Lt,
        syn::BinOp::Le(_) => BinOpKind::Le,
        syn::BinOp::Ne(_) => BinOpKind::Ne,
        syn::BinOp::Ge(_) => BinOpKind::Ge,
        syn::BinOp::Gt(_) => BinOpKind::Gt,
        syn::BinOp::AddAssign(_) => BinOpKind::AddAssign,
        syn::BinOp::SubAssign(_) => BinOpKind::SubAssign,
        syn::BinOp::MulAssign(_) => BinOpKind::MulAssign,
        syn::BinOp::DivAssign(_) => BinOpKind::DivAssign,
        syn::BinOp::RemAssign(_) => BinOpKind::RemAssign,
        syn::BinOp::BitXorAssign(_) => BinOpKind::BitXorAssign,
        syn::BinOp::BitAndAssign(_) => BinOpKind::BitAndAssign,
        syn::BinOp::BitOrAssign(_) => BinOpKind::BitOrAssign,
        syn::BinOp::ShlAssign(_) => BinOpKind::ShlAssign,
        syn::BinOp::ShrAssign(_) => BinOpKind::ShrAssign,
        _ => BinOpKind::Other,
    }
}

pub fn normalize_un_op(op: &syn::UnOp) -> UnOpKind {
    match op {
        syn::UnOp::Deref(_) => UnOpKind::Deref,
        syn::UnOp::Not(_) => UnOpKind::Not,
        syn::UnOp::Neg(_) => UnOpKind::Neg,
        _ => UnOpKind::Other,
    }
}

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

pub fn normalize_expr(expr: &syn::Expr, ctx: &mut NormalizationContext) -> NormalizedNode {
    match expr {
        syn::Expr::Lit(el) => normalize_lit(&el.lit),
        syn::Expr::Path(ep) => {
            if ep.path.segments.len() == 1 {
                let seg = &ep.path.segments[0];
                let ident = seg.ident.to_string();
                let kind = if ident.chars().next().is_some_and(|c| c.is_uppercase()) {
                    PlaceholderKind::Type
                } else {
                    PlaceholderKind::Variable
                };
                let idx = ctx.placeholder(&ident, kind);
                NormalizedNode::leaf(NodeKind::Placeholder(kind, idx))
            } else {
                let segments: Vec<NormalizedNode> = ep
                    .path
                    .segments
                    .iter()
                    .map(|seg| {
                        let ident = seg.ident.to_string();
                        let kind = if ident.chars().next().is_some_and(|c| c.is_uppercase()) {
                            PlaceholderKind::Type
                        } else {
                            PlaceholderKind::Variable
                        };
                        let idx = ctx.placeholder(&ident, kind);
                        NormalizedNode::leaf(NodeKind::Placeholder(kind, idx))
                    })
                    .collect();
                NormalizedNode::with_children(NodeKind::Path, segments)
            }
        }
        // BinaryOp -> [left, right]
        syn::Expr::Binary(eb) => NormalizedNode::with_children(
            NodeKind::BinaryOp(normalize_bin_op(&eb.op)),
            vec![
                normalize_expr(&eb.left, ctx),
                normalize_expr(&eb.right, ctx),
            ],
        ),
        // UnaryOp -> [operand]
        syn::Expr::Unary(eu) => NormalizedNode::with_children(
            NodeKind::UnaryOp(normalize_un_op(&eu.op)),
            vec![normalize_expr(&eu.expr, ctx)],
        ),
        // Call -> [func, arg0, arg1, ...]
        syn::Expr::Call(ec) => {
            let mut children = vec![normalize_expr(&ec.func, ctx)];
            children.extend(ec.args.iter().map(|a| normalize_expr(a, ctx)));
            NormalizedNode::with_children(NodeKind::Call, children)
        }
        // MethodCall -> [receiver, method, arg0, ...]
        syn::Expr::MethodCall(emc) => {
            let method_idx = ctx.placeholder(&emc.method.to_string(), PlaceholderKind::Function);
            let mut children = vec![
                normalize_expr(&emc.receiver, ctx),
                NormalizedNode::leaf(NodeKind::Placeholder(PlaceholderKind::Function, method_idx)),
            ];
            children.extend(emc.args.iter().map(|a| normalize_expr(a, ctx)));
            NormalizedNode::with_children(NodeKind::MethodCall, children)
        }
        // FieldAccess -> [base, field]
        syn::Expr::Field(ef) => {
            let name = match &ef.member {
                syn::Member::Named(ident) => ident.to_string(),
                syn::Member::Unnamed(idx) => idx.index.to_string(),
            };
            let field_idx = ctx.placeholder(&name, PlaceholderKind::Variable);
            NormalizedNode::with_children(
                NodeKind::FieldAccess,
                vec![
                    normalize_expr(&ef.base, ctx),
                    NormalizedNode::leaf(NodeKind::Placeholder(
                        PlaceholderKind::Variable,
                        field_idx,
                    )),
                ],
            )
        }
        // Index -> [base, index]
        syn::Expr::Index(ei) => NormalizedNode::with_children(
            NodeKind::Index,
            vec![
                normalize_expr(&ei.expr, ctx),
                normalize_expr(&ei.index, ctx),
            ],
        ),
        // Closure -> [body, param0, param1, ...]
        syn::Expr::Closure(ec) => {
            let mut children = vec![normalize_expr(&ec.body, ctx)];
            children.extend(ec.inputs.iter().map(|p| normalize_pat(p, ctx)));
            NormalizedNode::with_children(NodeKind::Closure, children)
        }
        // Return -> [] or [value]
        syn::Expr::Return(er) => {
            let children: Vec<_> = er
                .expr
                .as_ref()
                .map(|e| vec![normalize_expr(e, ctx)])
                .unwrap_or_default();
            NormalizedNode::with_children(NodeKind::Return, children)
        }
        // Break -> [] or [value]
        syn::Expr::Break(eb) => {
            let children: Vec<_> = eb
                .expr
                .as_ref()
                .map(|e| vec![normalize_expr(e, ctx)])
                .unwrap_or_default();
            NormalizedNode::with_children(NodeKind::Break, children)
        }
        syn::Expr::Continue(_) => NormalizedNode::leaf(NodeKind::Continue),
        // Assign -> [left, right]
        syn::Expr::Assign(ea) => NormalizedNode::with_children(
            NodeKind::Assign,
            vec![
                normalize_expr(&ea.left, ctx),
                normalize_expr(&ea.right, ctx),
            ],
        ),
        // Reference -> [expr]
        syn::Expr::Reference(er) => NormalizedNode::with_children(
            NodeKind::Reference {
                mutable: er.mutability.is_some(),
            },
            vec![normalize_expr(&er.expr, ctx)],
        ),
        syn::Expr::Tuple(et) => NormalizedNode::with_children(
            NodeKind::Tuple,
            et.elems.iter().map(|e| normalize_expr(e, ctx)).collect(),
        ),
        syn::Expr::Array(ea) => NormalizedNode::with_children(
            NodeKind::Array,
            ea.elems.iter().map(|e| normalize_expr(e, ctx)).collect(),
        ),
        // Repeat -> [elem, len]
        syn::Expr::Repeat(er) => NormalizedNode::with_children(
            NodeKind::Repeat,
            vec![normalize_expr(&er.expr, ctx), normalize_expr(&er.len, ctx)],
        ),
        // Cast -> [expr, ty]
        syn::Expr::Cast(ec) => NormalizedNode::with_children(
            NodeKind::Cast,
            vec![normalize_expr(&ec.expr, ctx), normalize_type(&ec.ty, ctx)],
        ),
        // StructInit -> [rest_or_None, field0, field1, ...]
        syn::Expr::Struct(es) => {
            let mut children = vec![NormalizedNode::opt(
                es.rest.as_ref().map(|e| normalize_expr(e, ctx)),
            )];
            children.extend(es.fields.iter().map(|f| {
                let name = match &f.member {
                    syn::Member::Named(ident) => ident.to_string(),
                    syn::Member::Unnamed(idx) => idx.index.to_string(),
                };
                let field_idx = ctx.placeholder(&name, PlaceholderKind::Variable);
                NormalizedNode::with_children(
                    NodeKind::FieldValue,
                    vec![
                        NormalizedNode::leaf(NodeKind::Placeholder(
                            PlaceholderKind::Variable,
                            field_idx,
                        )),
                        normalize_expr(&f.expr, ctx),
                    ],
                )
            }));
            NormalizedNode::with_children(NodeKind::StructInit, children)
        }
        // Await -> [expr]
        syn::Expr::Await(ea) => {
            NormalizedNode::with_children(NodeKind::Await, vec![normalize_expr(&ea.base, ctx)])
        }
        // Try -> [expr]
        syn::Expr::Try(et) => {
            NormalizedNode::with_children(NodeKind::Try, vec![normalize_expr(&et.expr, ctx)])
        }
        // If -> [condition, then_branch, else_or_None]
        syn::Expr::If(ei) => NormalizedNode::with_children(
            NodeKind::If,
            vec![
                normalize_expr(&ei.cond, ctx),
                normalize_block(&ei.then_branch, ctx),
                NormalizedNode::opt(ei.else_branch.as_ref().map(|(_, e)| normalize_expr(e, ctx))),
            ],
        ),
        // Match -> [expr, arm0, arm1, ...]
        // Each arm is MatchArm -> [pattern, guard_or_None, body]
        syn::Expr::Match(em) => {
            let mut children = vec![normalize_expr(&em.expr, ctx)];
            children.extend(em.arms.iter().map(|arm| {
                NormalizedNode::with_children(
                    NodeKind::MatchArm,
                    vec![
                        normalize_pat(&arm.pat, ctx),
                        NormalizedNode::opt(
                            arm.guard.as_ref().map(|(_, g)| normalize_expr(g, ctx)),
                        ),
                        normalize_expr(&arm.body, ctx),
                    ],
                )
            }));
            NormalizedNode::with_children(NodeKind::Match, children)
        }
        // Loop -> [body]
        syn::Expr::Loop(el) => {
            NormalizedNode::with_children(NodeKind::Loop, vec![normalize_block(&el.body, ctx)])
        }
        // While -> [condition, body]
        syn::Expr::While(ew) => NormalizedNode::with_children(
            NodeKind::While,
            vec![
                normalize_expr(&ew.cond, ctx),
                normalize_block(&ew.body, ctx),
            ],
        ),
        // ForLoop -> [pat, iter, body]
        syn::Expr::ForLoop(ef) => NormalizedNode::with_children(
            NodeKind::ForLoop,
            vec![
                normalize_pat(&ef.pat, ctx),
                normalize_expr(&ef.expr, ctx),
                normalize_block(&ef.body, ctx),
            ],
        ),
        syn::Expr::Block(eb) => normalize_block(&eb.block, ctx),
        // Paren -> [expr]
        syn::Expr::Paren(ep) => {
            NormalizedNode::with_children(NodeKind::Paren, vec![normalize_expr(&ep.expr, ctx)])
        }
        // Range -> [from_or_None, to_or_None]
        syn::Expr::Range(er) => NormalizedNode::with_children(
            NodeKind::Range,
            vec![
                NormalizedNode::opt(er.start.as_ref().map(|e| normalize_expr(e, ctx))),
                NormalizedNode::opt(er.end.as_ref().map(|e| normalize_expr(e, ctx))),
            ],
        ),
        // LetExpr -> [pat, expr]
        syn::Expr::Let(el) => NormalizedNode::with_children(
            NodeKind::LetExpr,
            vec![normalize_pat(&el.pat, ctx), normalize_expr(&el.expr, ctx)],
        ),
        syn::Expr::Macro(em) => normalize_macro(&em.mac, ctx),
        syn::Expr::Group(eg) => normalize_expr(&eg.expr, ctx),
        syn::Expr::Unsafe(eu) => normalize_block(&eu.block, ctx),
        syn::Expr::Const(ec) => normalize_block(&ec.block, ctx),
        _ => NormalizedNode::leaf(NodeKind::Opaque),
    }
}

pub fn normalize_stmt(stmt: &syn::Stmt, ctx: &mut NormalizationContext) -> NormalizedNode {
    match stmt {
        // LetBinding -> [pattern, type_or_None, init_or_None]
        syn::Stmt::Local(local) => NormalizedNode::with_children(
            NodeKind::LetBinding,
            vec![
                normalize_pat(&local.pat, ctx),
                NormalizedNode::none(), // type annotations on let bindings are part of the pattern in syn
                NormalizedNode::opt(
                    local
                        .init
                        .as_ref()
                        .map(|init| normalize_expr(&init.expr, ctx)),
                ),
            ],
        ),
        syn::Stmt::Expr(expr, semi) => {
            let normalized = normalize_expr(expr, ctx);
            if semi.is_some() {
                NormalizedNode::with_children(NodeKind::Semi, vec![normalized])
            } else {
                normalized
            }
        }
        syn::Stmt::Item(_) => NormalizedNode::leaf(NodeKind::Opaque),
        syn::Stmt::Macro(sm) => {
            let normalized = normalize_macro(&sm.mac, ctx);
            if sm.semi_token.is_some() {
                NormalizedNode::with_children(NodeKind::Semi, vec![normalized])
            } else {
                normalized
            }
        }
    }
}

pub fn normalize_block(block: &syn::Block, ctx: &mut NormalizationContext) -> NormalizedNode {
    NormalizedNode::with_children(
        NodeKind::Block,
        block.stmts.iter().map(|s| normalize_stmt(s, ctx)).collect(),
    )
}

pub fn normalize_signature(sig: &syn::Signature, ctx: &mut NormalizationContext) -> NormalizedNode {
    // FnSignature -> [return_type_or_None, param0, param1, ...]
    let return_type = match &sig.output {
        syn::ReturnType::Default => NormalizedNode::none(),
        syn::ReturnType::Type(_, ty) => normalize_type(ty, ctx),
    };
    let mut children = vec![return_type];
    children.extend(sig.inputs.iter().map(|arg| match arg {
        syn::FnArg::Receiver(r) => {
            let idx = ctx.placeholder("self", PlaceholderKind::Variable);
            let self_node =
                NormalizedNode::leaf(NodeKind::Placeholder(PlaceholderKind::Variable, idx));
            if r.reference.is_some() {
                NormalizedNode::with_children(
                    NodeKind::Reference {
                        mutable: r.mutability.is_some(),
                    },
                    vec![self_node],
                )
            } else {
                self_node
            }
        }
        syn::FnArg::Typed(pt) => {
            let pat = normalize_pat(&pt.pat, ctx);
            let ty = normalize_type(&pt.ty, ctx);
            NormalizedNode::with_children(NodeKind::FieldValue, vec![pat, ty])
        }
    }));
    NormalizedNode::with_children(NodeKind::FnSignature, children)
}

// -- Public entry points ------------------------------------------------------

/// Normalize a top-level function.
pub fn normalize_item_fn(func: &syn::ItemFn) -> (NormalizedNode, NormalizedNode) {
    let mut ctx = NormalizationContext::new();
    let sig = normalize_signature(&func.sig, &mut ctx);
    let body = normalize_block(&func.block, &mut ctx);
    (sig, body)
}

/// Normalize a method within an impl block.
pub fn normalize_impl_item_fn(method: &syn::ImplItemFn) -> (NormalizedNode, NormalizedNode) {
    let mut ctx = NormalizationContext::new();
    let sig = normalize_signature(&method.sig, &mut ctx);
    let body = normalize_block(&method.block, &mut ctx);
    (sig, body)
}

/// Normalize a closure expression.
pub fn normalize_closure_expr(closure: &syn::ExprClosure) -> NormalizedNode {
    let mut ctx = NormalizationContext::new();
    normalize_expr(&syn::Expr::Closure(closure.clone()), &mut ctx)
}

/// Normalize an impl block -- normalizes each method body.
pub fn normalize_impl_block(imp: &syn::ItemImpl) -> Vec<(String, NormalizedNode, NormalizedNode)> {
    imp.items
        .iter()
        .filter_map(|item| {
            if let syn::ImplItem::Fn(method) = item {
                let name = method.sig.ident.to_string();
                let (sig, body) = normalize_impl_item_fn(method);
                Some((name, sig, body))
            } else {
                None
            }
        })
        .collect()
}

#[cfg(test)]
mod tests {
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
        let f1 =
            parse_fn("fn foo(x: i32, y: i32) -> i32 { if x > 0 { let z = y + 1; z } else { x } }");
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
}
