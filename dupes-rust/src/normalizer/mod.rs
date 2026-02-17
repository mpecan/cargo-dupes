// Re-export core node types used by the normalizer's public API and tests.
pub use dupes_core::node::{
    BinOpKind, LiteralKind, NodeKind, NormalizationContext, NormalizedNode, PlaceholderKind,
    UnOpKind, count_nodes, reindex_placeholders,
};

mod expr;
mod helpers;
mod pat;

pub use expr::{normalize_block, normalize_expr, normalize_stmt};
pub use helpers::{normalize_bin_op, normalize_lit, normalize_un_op};
pub use pat::{normalize_pat, normalize_type};

pub fn normalize_signature(sig: &syn::Signature, ctx: &mut NormalizationContext) -> NormalizedNode {
    // FnSignature -> [return_type_or_None, param0, param1, ...]
    let return_type = match &sig.output {
        syn::ReturnType::Default => NormalizedNode::none(),
        syn::ReturnType::Type(_, ty) => pat::normalize_type(ty, ctx),
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
            let pat = pat::normalize_pat(&pt.pat, ctx);
            let ty = pat::normalize_type(&pt.ty, ctx);
            NormalizedNode::with_children(NodeKind::FieldValue, vec![pat, ty])
        }
    }));
    NormalizedNode::with_children(NodeKind::FnSignature, children)
}

// -- Public entry points ------------------------------------------------------

fn normalize_fn_parts(
    sig: &syn::Signature,
    block: &syn::Block,
) -> (NormalizedNode, NormalizedNode) {
    let mut ctx = NormalizationContext::new();
    let sig = normalize_signature(sig, &mut ctx);
    let body = normalize_block(block, &mut ctx);
    (sig, body)
}

/// Normalize a top-level function.
#[must_use]
pub fn normalize_item_fn(func: &syn::ItemFn) -> (NormalizedNode, NormalizedNode) {
    normalize_fn_parts(&func.sig, &func.block)
}

/// Normalize a method within an impl block.
#[must_use]
pub fn normalize_impl_item_fn(method: &syn::ImplItemFn) -> (NormalizedNode, NormalizedNode) {
    normalize_fn_parts(&method.sig, &method.block)
}

/// Normalize a closure expression.
#[must_use]
pub fn normalize_closure_expr(closure: &syn::ExprClosure) -> NormalizedNode {
    let mut ctx = NormalizationContext::new();
    let mut children = vec![normalize_expr(&closure.body, &mut ctx)];
    children.extend(
        closure
            .inputs
            .iter()
            .map(|p| pat::normalize_pat(p, &mut ctx)),
    );
    NormalizedNode::with_children(NodeKind::Closure, children)
}

/// Normalize an impl block -- normalizes each method body.
#[must_use]
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
mod tests;
