use dupes_core::node::{NodeKind, NormalizationContext, NormalizedNode, PlaceholderKind};

use super::helpers::{
    member_to_string, normalize_bin_op, normalize_lit, normalize_macro, normalize_un_op,
};
use super::pat::{normalize_pat, normalize_type};

#[allow(clippy::too_many_lines)]
pub fn normalize_expr(expr: &syn::Expr, ctx: &mut NormalizationContext) -> NormalizedNode {
    match expr {
        syn::Expr::Lit(el) => normalize_lit(&el.lit),
        syn::Expr::Path(ep) => {
            if ep.path.segments.len() == 1 {
                let seg = &ep.path.segments[0];
                let ident = seg.ident.to_string();
                let kind = if ident.chars().next().is_some_and(char::is_uppercase) {
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
                        let kind = if ident.chars().next().is_some_and(char::is_uppercase) {
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
            let field_idx =
                ctx.placeholder(&member_to_string(&ef.member), PlaceholderKind::Variable);
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
                let field_idx =
                    ctx.placeholder(&member_to_string(&f.member), PlaceholderKind::Variable);
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
        // LetBinding -> [pattern, type_or_None, init_or_None, diverge_or_None]
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
                NormalizedNode::opt(
                    local
                        .init
                        .as_ref()
                        .and_then(|init| init.diverge.as_ref())
                        .map(|(_, expr)| normalize_expr(expr, ctx)),
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
