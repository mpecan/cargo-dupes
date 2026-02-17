use dupes_core::node::{
    BinOpKind, LiteralKind, NodeKind, NormalizationContext, NormalizedNode, UnOpKind,
};
use syn::punctuated::Punctuated;

use super::expr::normalize_expr;

pub fn member_to_string(member: &syn::Member) -> String {
    match member {
        syn::Member::Named(ident) => ident.to_string(),
        syn::Member::Unnamed(idx) => idx.index.to_string(),
    }
}

pub fn normalize_macro(mac: &syn::Macro, ctx: &mut NormalizationContext) -> NormalizedNode {
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

#[must_use]
pub const fn normalize_lit(lit: &syn::Lit) -> NormalizedNode {
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

#[must_use]
pub const fn normalize_bin_op(op: &syn::BinOp) -> BinOpKind {
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

#[must_use]
pub const fn normalize_un_op(op: &syn::UnOp) -> UnOpKind {
    match op {
        syn::UnOp::Deref(_) => UnOpKind::Deref,
        syn::UnOp::Not(_) => UnOpKind::Not,
        syn::UnOp::Neg(_) => UnOpKind::Neg,
        _ => UnOpKind::Other,
    }
}
