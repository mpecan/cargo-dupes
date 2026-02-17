// Re-export all core types so `use crate::normalizer::*` still works
pub use dupes_core::node::*;

use syn::punctuated::Punctuated;

// ── Helpers ──────────────────────────────────────────────────────────────

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
            Err(_) => vec![NormalizedNode::Opaque],
        }
    };
    NormalizedNode::MacroCall { name, args }
}

// ── Normalization functions ──────────────────────────────────────────────

pub fn normalize_lit(lit: &syn::Lit) -> NormalizedNode {
    match lit {
        syn::Lit::Str(_) => NormalizedNode::Literal(LiteralKind::Str),
        syn::Lit::ByteStr(_) => NormalizedNode::Literal(LiteralKind::ByteStr),
        syn::Lit::CStr(_) => NormalizedNode::Literal(LiteralKind::CStr),
        syn::Lit::Byte(_) => NormalizedNode::Literal(LiteralKind::Byte),
        syn::Lit::Char(_) => NormalizedNode::Literal(LiteralKind::Char),
        syn::Lit::Int(_) => NormalizedNode::Literal(LiteralKind::Int),
        syn::Lit::Float(_) => NormalizedNode::Literal(LiteralKind::Float),
        syn::Lit::Bool(_) => NormalizedNode::Literal(LiteralKind::Bool),
        _ => NormalizedNode::Opaque,
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
                NormalizedNode::TypePlaceholder(PlaceholderKind::Type, idx)
            } else {
                let segments: Vec<NormalizedNode> = tp
                    .path
                    .segments
                    .iter()
                    .map(|seg| {
                        let idx = ctx.placeholder(&seg.ident.to_string(), PlaceholderKind::Type);
                        NormalizedNode::TypePlaceholder(PlaceholderKind::Type, idx)
                    })
                    .collect();
                NormalizedNode::TypePath(segments)
            }
        }
        syn::Type::Reference(r) => NormalizedNode::TypeReference {
            mutable: r.mutability.is_some(),
            elem: Box::new(normalize_type(&r.elem, ctx)),
        },
        syn::Type::Tuple(t) => {
            if t.elems.is_empty() {
                NormalizedNode::TypeUnit
            } else {
                NormalizedNode::TypeTuple(t.elems.iter().map(|e| normalize_type(e, ctx)).collect())
            }
        }
        syn::Type::Slice(s) => NormalizedNode::TypeSlice(Box::new(normalize_type(&s.elem, ctx))),
        syn::Type::Array(a) => NormalizedNode::TypeArray {
            elem: Box::new(normalize_type(&a.elem, ctx)),
            len: Box::new(normalize_expr(&a.len, ctx)),
        },
        syn::Type::ImplTrait(i) => NormalizedNode::TypeImplTrait(
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
                                NormalizedNode::TypePlaceholder(PlaceholderKind::Type, idx)
                            })
                            .collect();
                        Some(if segments.len() == 1 {
                            segments.into_iter().next().unwrap()
                        } else {
                            NormalizedNode::TypePath(segments)
                        })
                    } else {
                        None
                    }
                })
                .collect(),
        ),
        syn::Type::Infer(_) => NormalizedNode::TypeInfer,
        syn::Type::Never(_) => NormalizedNode::TypeNever,
        syn::Type::Paren(p) => normalize_type(&p.elem, ctx),
        syn::Type::Macro(tm) => normalize_macro(&tm.mac, ctx),
        _ => NormalizedNode::Opaque,
    }
}

pub fn normalize_pat(pat: &syn::Pat, ctx: &mut NormalizationContext) -> NormalizedNode {
    match pat {
        syn::Pat::Ident(pi) => {
            let idx = ctx.placeholder(&pi.ident.to_string(), PlaceholderKind::Variable);
            NormalizedNode::PatPlaceholder(PlaceholderKind::Variable, idx)
        }
        syn::Pat::Wild(_) => NormalizedNode::PatWild,
        syn::Pat::Tuple(pt) => {
            NormalizedNode::PatTuple(pt.elems.iter().map(|p| normalize_pat(p, ctx)).collect())
        }
        syn::Pat::TupleStruct(pts) => {
            NormalizedNode::PatStruct(pts.elems.iter().map(|p| normalize_pat(p, ctx)).collect())
        }
        syn::Pat::Struct(ps) => NormalizedNode::PatStruct(
            ps.fields
                .iter()
                .map(|f| {
                    let value = normalize_pat(&f.pat, ctx);
                    let name_idx =
                        ctx.placeholder(&member_to_string(&f.member), PlaceholderKind::Variable);
                    NormalizedNode::FieldValue {
                        name: Box::new(NormalizedNode::PatPlaceholder(
                            PlaceholderKind::Variable,
                            name_idx,
                        )),
                        value: Box::new(value),
                    }
                })
                .collect(),
        ),
        syn::Pat::Or(po) => {
            NormalizedNode::PatOr(po.cases.iter().map(|p| normalize_pat(p, ctx)).collect())
        }
        syn::Pat::Lit(pl) => NormalizedNode::PatLiteral(Box::new(normalize_lit(&pl.lit))),
        syn::Pat::Reference(pr) => NormalizedNode::PatReference {
            mutable: pr.mutability.is_some(),
            pat: Box::new(normalize_pat(&pr.pat, ctx)),
        },
        syn::Pat::Slice(ps) => {
            NormalizedNode::PatSlice(ps.elems.iter().map(|p| normalize_pat(p, ctx)).collect())
        }
        syn::Pat::Rest(_) => NormalizedNode::PatRest,
        syn::Pat::Range(pr) => NormalizedNode::PatRange {
            lo: pr.start.as_ref().map(|e| Box::new(normalize_expr(e, ctx))),
            hi: pr.end.as_ref().map(|e| Box::new(normalize_expr(e, ctx))),
        },
        syn::Pat::Path(pp) => {
            if pp.path.segments.len() == 1 {
                let seg = &pp.path.segments[0];
                let idx = ctx.placeholder(&seg.ident.to_string(), PlaceholderKind::Variable);
                NormalizedNode::PatPlaceholder(PlaceholderKind::Variable, idx)
            } else {
                NormalizedNode::PatStruct(
                    pp.path
                        .segments
                        .iter()
                        .map(|seg| {
                            let idx =
                                ctx.placeholder(&seg.ident.to_string(), PlaceholderKind::Variable);
                            NormalizedNode::PatPlaceholder(PlaceholderKind::Variable, idx)
                        })
                        .collect(),
                )
            }
        }
        syn::Pat::Type(pt) => normalize_pat(&pt.pat, ctx),
        syn::Pat::Macro(pm) => normalize_macro(&pm.mac, ctx),
        _ => NormalizedNode::Opaque,
    }
}

pub fn normalize_expr(expr: &syn::Expr, ctx: &mut NormalizationContext) -> NormalizedNode {
    match expr {
        syn::Expr::Lit(el) => normalize_lit(&el.lit),
        syn::Expr::Path(ep) => {
            if ep.path.segments.len() == 1 {
                let seg = &ep.path.segments[0];
                let ident = seg.ident.to_string();
                // Check if it looks like a type/variant (starts with uppercase)
                let kind = if ident.chars().next().is_some_and(|c| c.is_uppercase()) {
                    PlaceholderKind::Type
                } else {
                    PlaceholderKind::Variable
                };
                let idx = ctx.placeholder(&ident, kind);
                NormalizedNode::Placeholder(kind, idx)
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
                        NormalizedNode::Placeholder(kind, idx)
                    })
                    .collect();
                NormalizedNode::Path(segments)
            }
        }
        syn::Expr::Binary(eb) => NormalizedNode::BinaryOp {
            op: normalize_bin_op(&eb.op),
            left: Box::new(normalize_expr(&eb.left, ctx)),
            right: Box::new(normalize_expr(&eb.right, ctx)),
        },
        syn::Expr::Unary(eu) => NormalizedNode::UnaryOp {
            op: normalize_un_op(&eu.op),
            operand: Box::new(normalize_expr(&eu.expr, ctx)),
        },
        syn::Expr::Call(ec) => NormalizedNode::Call {
            func: Box::new(normalize_expr(&ec.func, ctx)),
            args: ec.args.iter().map(|a| normalize_expr(a, ctx)).collect(),
        },
        syn::Expr::MethodCall(emc) => NormalizedNode::MethodCall {
            receiver: Box::new(normalize_expr(&emc.receiver, ctx)),
            method: Box::new({
                let idx = ctx.placeholder(&emc.method.to_string(), PlaceholderKind::Function);
                NormalizedNode::Placeholder(PlaceholderKind::Function, idx)
            }),
            args: emc.args.iter().map(|a| normalize_expr(a, ctx)).collect(),
        },
        syn::Expr::Field(ef) => NormalizedNode::FieldAccess {
            base: Box::new(normalize_expr(&ef.base, ctx)),
            field: Box::new({
                let name = match &ef.member {
                    syn::Member::Named(ident) => ident.to_string(),
                    syn::Member::Unnamed(idx) => idx.index.to_string(),
                };
                let idx = ctx.placeholder(&name, PlaceholderKind::Variable);
                NormalizedNode::Placeholder(PlaceholderKind::Variable, idx)
            }),
        },
        syn::Expr::Index(ei) => NormalizedNode::Index {
            base: Box::new(normalize_expr(&ei.expr, ctx)),
            index: Box::new(normalize_expr(&ei.index, ctx)),
        },
        syn::Expr::Closure(ec) => NormalizedNode::Closure {
            params: ec.inputs.iter().map(|p| normalize_pat(p, ctx)).collect(),
            body: Box::new(normalize_expr(&ec.body, ctx)),
        },
        syn::Expr::Return(er) => {
            NormalizedNode::Return(er.expr.as_ref().map(|e| Box::new(normalize_expr(e, ctx))))
        }
        syn::Expr::Break(eb) => {
            NormalizedNode::Break(eb.expr.as_ref().map(|e| Box::new(normalize_expr(e, ctx))))
        }
        syn::Expr::Continue(_) => NormalizedNode::Continue,
        syn::Expr::Assign(ea) => NormalizedNode::Assign {
            left: Box::new(normalize_expr(&ea.left, ctx)),
            right: Box::new(normalize_expr(&ea.right, ctx)),
        },
        syn::Expr::Reference(er) => NormalizedNode::Reference {
            mutable: er.mutability.is_some(),
            expr: Box::new(normalize_expr(&er.expr, ctx)),
        },
        syn::Expr::Tuple(et) => {
            NormalizedNode::Tuple(et.elems.iter().map(|e| normalize_expr(e, ctx)).collect())
        }
        syn::Expr::Array(ea) => {
            NormalizedNode::Array(ea.elems.iter().map(|e| normalize_expr(e, ctx)).collect())
        }
        syn::Expr::Repeat(er) => NormalizedNode::Repeat {
            elem: Box::new(normalize_expr(&er.expr, ctx)),
            len: Box::new(normalize_expr(&er.len, ctx)),
        },
        syn::Expr::Cast(ec) => NormalizedNode::Cast {
            expr: Box::new(normalize_expr(&ec.expr, ctx)),
            ty: Box::new(normalize_type(&ec.ty, ctx)),
        },
        syn::Expr::Struct(es) => NormalizedNode::StructInit {
            fields: es
                .fields
                .iter()
                .map(|f| NormalizedNode::FieldValue {
                    name: Box::new({
                        let name = match &f.member {
                            syn::Member::Named(ident) => ident.to_string(),
                            syn::Member::Unnamed(idx) => idx.index.to_string(),
                        };
                        let idx = ctx.placeholder(&name, PlaceholderKind::Variable);
                        NormalizedNode::Placeholder(PlaceholderKind::Variable, idx)
                    }),
                    value: Box::new(normalize_expr(&f.expr, ctx)),
                })
                .collect(),
            rest: es.rest.as_ref().map(|e| Box::new(normalize_expr(e, ctx))),
        },
        syn::Expr::Await(ea) => NormalizedNode::Await(Box::new(normalize_expr(&ea.base, ctx))),
        syn::Expr::Try(et) => NormalizedNode::Try(Box::new(normalize_expr(&et.expr, ctx))),
        syn::Expr::If(ei) => NormalizedNode::If {
            condition: Box::new(normalize_expr(&ei.cond, ctx)),
            then_branch: Box::new(normalize_block(&ei.then_branch, ctx)),
            else_branch: ei
                .else_branch
                .as_ref()
                .map(|(_, e)| Box::new(normalize_expr(e, ctx))),
        },
        syn::Expr::Match(em) => NormalizedNode::Match {
            expr: Box::new(normalize_expr(&em.expr, ctx)),
            arms: em
                .arms
                .iter()
                .map(|arm| MatchArm {
                    pattern: normalize_pat(&arm.pat, ctx),
                    guard: arm
                        .guard
                        .as_ref()
                        .map(|(_, g)| Box::new(normalize_expr(g, ctx))),
                    body: Box::new(normalize_expr(&arm.body, ctx)),
                })
                .collect(),
        },
        syn::Expr::Loop(el) => NormalizedNode::Loop(Box::new(normalize_block(&el.body, ctx))),
        syn::Expr::While(ew) => NormalizedNode::While {
            condition: Box::new(normalize_expr(&ew.cond, ctx)),
            body: Box::new(normalize_block(&ew.body, ctx)),
        },
        syn::Expr::ForLoop(ef) => NormalizedNode::ForLoop {
            pat: Box::new(normalize_pat(&ef.pat, ctx)),
            iter: Box::new(normalize_expr(&ef.expr, ctx)),
            body: Box::new(normalize_block(&ef.body, ctx)),
        },
        syn::Expr::Block(eb) => normalize_block(&eb.block, ctx),
        syn::Expr::Paren(ep) => NormalizedNode::Paren(Box::new(normalize_expr(&ep.expr, ctx))),
        syn::Expr::Range(er) => NormalizedNode::Range {
            from: er.start.as_ref().map(|e| Box::new(normalize_expr(e, ctx))),
            to: er.end.as_ref().map(|e| Box::new(normalize_expr(e, ctx))),
        },
        syn::Expr::Let(el) => NormalizedNode::LetExpr {
            pat: Box::new(normalize_pat(&el.pat, ctx)),
            expr: Box::new(normalize_expr(&el.expr, ctx)),
        },
        syn::Expr::Macro(em) => normalize_macro(&em.mac, ctx),
        syn::Expr::Group(eg) => normalize_expr(&eg.expr, ctx),
        syn::Expr::Unsafe(eu) => normalize_block(&eu.block, ctx),
        syn::Expr::Const(ec) => normalize_block(&ec.block, ctx),
        _ => NormalizedNode::Opaque,
    }
}

pub fn normalize_stmt(stmt: &syn::Stmt, ctx: &mut NormalizationContext) -> NormalizedNode {
    match stmt {
        syn::Stmt::Local(local) => NormalizedNode::LetBinding {
            pattern: Box::new(normalize_pat(&local.pat, ctx)),
            ty: None, // type annotations on let bindings are part of the pattern in syn
            init: local
                .init
                .as_ref()
                .map(|init| Box::new(normalize_expr(&init.expr, ctx))),
        },
        syn::Stmt::Expr(expr, semi) => {
            let normalized = normalize_expr(expr, ctx);
            if semi.is_some() {
                NormalizedNode::Semi(Box::new(normalized))
            } else {
                normalized
            }
        }
        syn::Stmt::Item(_) => NormalizedNode::Opaque,
        syn::Stmt::Macro(sm) => {
            let normalized = normalize_macro(&sm.mac, ctx);
            if sm.semi_token.is_some() {
                NormalizedNode::Semi(Box::new(normalized))
            } else {
                normalized
            }
        }
    }
}

pub fn normalize_block(block: &syn::Block, ctx: &mut NormalizationContext) -> NormalizedNode {
    NormalizedNode::Block(block.stmts.iter().map(|s| normalize_stmt(s, ctx)).collect())
}

pub fn normalize_signature(sig: &syn::Signature, ctx: &mut NormalizationContext) -> NormalizedNode {
    let params = sig
        .inputs
        .iter()
        .map(|arg| match arg {
            syn::FnArg::Receiver(r) => {
                let idx = ctx.placeholder("self", PlaceholderKind::Variable);
                let self_node = NormalizedNode::Placeholder(PlaceholderKind::Variable, idx);
                if r.reference.is_some() {
                    NormalizedNode::Reference {
                        mutable: r.mutability.is_some(),
                        expr: Box::new(self_node),
                    }
                } else {
                    self_node
                }
            }
            syn::FnArg::Typed(pt) => {
                let pat = normalize_pat(&pt.pat, ctx);
                let ty = normalize_type(&pt.ty, ctx);
                NormalizedNode::FieldValue {
                    name: Box::new(pat),
                    value: Box::new(ty),
                }
            }
        })
        .collect();
    let return_type = match &sig.output {
        syn::ReturnType::Default => None,
        syn::ReturnType::Type(_, ty) => Some(Box::new(normalize_type(ty, ctx))),
    };
    NormalizedNode::FnSignature {
        params,
        return_type,
    }
}

// ── Public entry points ──────────────────────────────────────────────────

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

/// Normalize an impl block — normalizes each method body.
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
        // syn parses `true` and `false` as path expressions, not as Lit::Bool.
        // They normalize to Placeholder(Variable, 0) since they are single-segment paths
        // with lowercase names. This means `true` and `false` are indistinguishable from
        // any variable name in the normalized form — a known limitation.
        let n1 = normalize_code_expr("true");
        let n2 = normalize_code_expr("false");
        // Both normalize to the same placeholder shape (Variable, 0)
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
        match n {
            NormalizedNode::Match { arms, .. } => {
                assert_eq!(arms.len(), 2);
            }
            _ => panic!("Expected Match node"),
        }
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
        // Same macro name with erased literal values, so loop bodies match
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
        match n {
            NormalizedNode::Cast { .. } => {}
            _ => panic!("Expected Cast node"),
        }
    }

    #[test]
    fn index_expression_normalized() {
        let n = normalize_code_expr("arr[0]");
        match n {
            NormalizedNode::Index { .. } => {}
            _ => panic!("Expected Index node"),
        }
    }

    #[test]
    fn await_expression_normalized() {
        let n = normalize_code_expr("fut.await");
        match n {
            NormalizedNode::Await(_) => {}
            _ => panic!("Expected Await node"),
        }
    }

    #[test]
    fn try_expression_normalized() {
        let n = normalize_code_expr("result?");
        match n {
            NormalizedNode::Try(_) => {}
            _ => panic!("Expected Try node"),
        }
    }

    #[test]
    fn range_expression_normalized() {
        let n = normalize_code_expr("0..10");
        match n {
            NormalizedNode::Range {
                from: Some(_),
                to: Some(_),
            } => {}
            _ => panic!("Expected Range node with from and to"),
        }
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
        match &n {
            NormalizedNode::MacroCall { name, args } => {
                assert_eq!(name, "println");
                assert_eq!(args.len(), 1);
                assert_eq!(args[0], NormalizedNode::Literal(LiteralKind::Str));
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
        match &n {
            NormalizedNode::MacroCall { name, args } => {
                assert_eq!(name, "vec");
                assert_eq!(args.len(), 3);
            }
            _ => panic!("Expected MacroCall node, got {:?}", n),
        }
    }

    #[test]
    fn multi_segment_macro_path_uses_last_segment() {
        let n = normalize_code_expr("std::println!(\"hello\")");
        match &n {
            NormalizedNode::MacroCall { name, .. } => {
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
        // vec![x; n] uses semicolon syntax, which can't be parsed as comma-separated exprs
        let n = normalize_code_expr("vec![x; 10]");
        match &n {
            NormalizedNode::MacroCall { name, args } => {
                assert_eq!(name, "vec");
                assert_eq!(args.len(), 1);
                assert_eq!(args[0], NormalizedNode::Opaque);
            }
            _ => panic!("Expected MacroCall node, got {:?}", n),
        }
    }

    #[test]
    fn unparseable_macro_differs_from_no_args() {
        let n_empty = normalize_code_expr("my_macro!()");
        let n_unparseable = normalize_code_expr("vec![x; 10]");
        match (&n_empty, &n_unparseable) {
            (
                NormalizedNode::MacroCall {
                    args: args_empty, ..
                },
                NormalizedNode::MacroCall {
                    args: args_unparseable,
                    ..
                },
            ) => {
                assert!(args_empty.is_empty());
                assert_eq!(args_unparseable.len(), 1);
                assert_eq!(args_unparseable[0], NormalizedNode::Opaque);
            }
            _ => panic!("Expected MacroCall nodes"),
        }
    }

    #[test]
    fn type_position_macro_normalized() {
        // Type::Macro is rare but exists — we need to parse it via a function signature
        let code = "fn foo() -> my_type!(i32) {}";
        // syn may not parse this as Type::Macro in all cases; verify it at least doesn't panic
        if let Ok(f) = syn::parse_str::<syn::ItemFn>(code) {
            let (sig, _) = normalize_item_fn(&f);
            // Just verify it produces something (doesn't panic)
            assert!(count_nodes(&sig) > 0);
        }
    }

    #[test]
    fn pat_macro_normalized() {
        // Pat::Macro appears as a macro in pattern position, e.g. in a match arm
        // This is quite rare, but verify it at least normalizes correctly
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
        match n {
            NormalizedNode::Assign { .. } => {}
            _ => panic!("Expected Assign node"),
        }
    }

    #[test]
    fn struct_init_normalized() {
        let code1 = "Foo { x: 1, y: 2 }";
        let code2 = "Bar { a: 1, b: 2 }";
        let n1 = normalize_code_expr(code1);
        let n2 = normalize_code_expr(code2);
        // Struct names differ, so paths differ, but field structure is the same
        // Both have StructInit with 2 fields
        match (&n1, &n2) {
            (
                NormalizedNode::StructInit { fields: f1, .. },
                NormalizedNode::StructInit { fields: f2, .. },
            ) => {
                assert_eq!(f1.len(), f2.len());
            }
            _ => panic!("Expected StructInit nodes"),
        }
    }

    #[test]
    fn array_expression_normalized() {
        let n = normalize_code_expr("[1, 2, 3]");
        match n {
            NormalizedNode::Array(elems) => assert_eq!(elems.len(), 3),
            _ => panic!("Expected Array node"),
        }
    }

    #[test]
    fn tuple_expression_normalized() {
        let n = normalize_code_expr("(1, 2, 3)");
        match n {
            NormalizedNode::Tuple(elems) => assert_eq!(elems.len(), 3),
            _ => panic!("Expected Tuple node"),
        }
    }

    #[test]
    fn field_access_normalized() {
        let n = normalize_code_expr("foo.bar");
        match n {
            NormalizedNode::FieldAccess { .. } => {}
            _ => panic!("Expected FieldAccess node"),
        }
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
        match n {
            NormalizedNode::Loop(_) => {}
            _ => panic!("Expected Loop node"),
        }
    }

    #[test]
    fn empty_block_normalized() {
        let code = "fn foo() {}";
        let f = parse_fn(code);
        let (_, body) = normalize_item_fn(&f);
        match body {
            NormalizedNode::Block(stmts) => assert!(stmts.is_empty()),
            _ => panic!("Expected empty Block"),
        }
    }

    // NOTE: reindex_remaps_from_zero, reindex_preserves_same_placeholder_identity,
    // reindex_makes_equivalent_subtrees_equal, and reindex_handles_multiple_placeholder_kinds
    // are tested in dupes-core/src/node.rs tests (no syn dependency needed).

    #[test]
    fn reindex_from_real_function_subtrees() {
        // In fn1, if-then uses x (idx 0) and y (idx 1) — then introduces z (idx 2)
        let f1 =
            parse_fn("fn foo(x: i32, y: i32) -> i32 { if x > 0 { let z = y + 1; z } else { x } }");
        // In fn2, if-then uses a (idx 0) then introduces c (idx 2), uses b (idx 1) — same structure
        // But here the then_branch references a(0) and b(1) differently
        let f2 = parse_fn(
            "fn bar(unused: i32, a: i32, b: i32) -> i32 { if a > 0 { let c = b + 1; c } else { a } }",
        );
        let (_, body1) = normalize_item_fn(&f1);
        let (_, body2) = normalize_item_fn(&f2);

        // Extract the then_branch from each
        let then1 = match &body1 {
            NormalizedNode::Block(stmts) => match &stmts[0] {
                NormalizedNode::If { then_branch, .. } => then_branch.as_ref().clone(),
                _ => panic!("expected If"),
            },
            _ => panic!("expected Block"),
        };
        let then2 = match &body2 {
            NormalizedNode::Block(stmts) => match &stmts[0] {
                NormalizedNode::If { then_branch, .. } => then_branch.as_ref().clone(),
                _ => panic!("expected If"),
            },
            _ => panic!("expected Block"),
        };

        // Before re-indexing, they have different placeholder indices due to different parent contexts
        assert_ne!(then1, then2);
        // After re-indexing, they should be equal (both: let $0 = $1 + 1; $0)
        assert_eq!(reindex_placeholders(&then1), reindex_placeholders(&then2));
    }
}
