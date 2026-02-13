use std::collections::hash_map::DefaultHasher;
use std::fmt;
use std::hash::{Hash, Hasher};

use crate::normalizer::NormalizedNode;

/// A fingerprint of a normalized AST node, wrapping a u64 hash.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Fingerprint(u64);

impl Fingerprint {
    /// Compute a fingerprint from a normalized node.
    pub fn from_node(node: &NormalizedNode) -> Self {
        let mut hasher = DefaultHasher::new();
        node.hash(&mut hasher);
        Fingerprint(hasher.finish())
    }

    /// Compute a fingerprint from a signature + body pair.
    pub fn from_sig_and_body(sig: &NormalizedNode, body: &NormalizedNode) -> Self {
        let mut hasher = DefaultHasher::new();
        sig.hash(&mut hasher);
        body.hash(&mut hasher);
        Fingerprint(hasher.finish())
    }

    /// Compute a composite fingerprint from a set of fingerprints.
    /// Sorts by u64 value for order-independence, then hashes the sorted sequence.
    pub fn from_fingerprints(fps: &[Fingerprint]) -> Self {
        let mut sorted: Vec<u64> = fps.iter().map(|fp| fp.0).collect();
        sorted.sort_unstable();
        let mut hasher = DefaultHasher::new();
        for v in &sorted {
            v.hash(&mut hasher);
        }
        Fingerprint(hasher.finish())
    }

    /// Get the raw u64 value.
    pub fn value(self) -> u64 {
        self.0
    }

    /// Convert to hex string.
    pub fn to_hex(self) -> String {
        format!("{:016x}", self.0)
    }

    /// Parse from hex string.
    pub fn from_hex(s: &str) -> Option<Self> {
        u64::from_str_radix(s, 16).ok().map(Fingerprint)
    }
}

impl fmt::Display for Fingerprint {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:016x}", self.0)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::normalizer::{NormalizationContext, normalize_expr, normalize_item_fn};

    fn parse_fn(code: &str) -> syn::ItemFn {
        syn::parse_str::<syn::ItemFn>(code).unwrap()
    }

    fn parse_expr(code: &str) -> syn::Expr {
        syn::parse_str::<syn::Expr>(code).unwrap()
    }

    #[test]
    fn identical_functions_same_fingerprint() {
        let code1 = "fn foo(x: i32) -> i32 { x + 1 }";
        let code2 = "fn bar(a: i32) -> i32 { a + 1 }";
        let f1 = parse_fn(code1);
        let f2 = parse_fn(code2);
        let (sig1, body1) = normalize_item_fn(&f1);
        let (sig2, body2) = normalize_item_fn(&f2);
        let fp1 = Fingerprint::from_sig_and_body(&sig1, &body1);
        let fp2 = Fingerprint::from_sig_and_body(&sig2, &body2);
        assert_eq!(fp1, fp2);
    }

    #[test]
    fn different_functions_different_fingerprint() {
        let code1 = "fn foo(x: i32) -> i32 { x + 1 }";
        let code2 = "fn foo(x: i32) -> i32 { x * 2 }";
        let f1 = parse_fn(code1);
        let f2 = parse_fn(code2);
        let (sig1, body1) = normalize_item_fn(&f1);
        let (sig2, body2) = normalize_item_fn(&f2);
        let fp1 = Fingerprint::from_sig_and_body(&sig1, &body1);
        let fp2 = Fingerprint::from_sig_and_body(&sig2, &body2);
        assert_ne!(fp1, fp2);
    }

    #[test]
    fn fingerprint_from_node() {
        let expr = parse_expr("x + 1");
        let mut ctx = NormalizationContext::new();
        let node = normalize_expr(&expr, &mut ctx);
        let fp = Fingerprint::from_node(&node);
        assert_ne!(fp.value(), 0);
    }

    #[test]
    fn hex_roundtrip() {
        let fp = Fingerprint(0xdeadbeef12345678);
        let hex = fp.to_hex();
        assert_eq!(hex, "deadbeef12345678");
        let fp2 = Fingerprint::from_hex(&hex).unwrap();
        assert_eq!(fp, fp2);
    }

    #[test]
    fn display_format() {
        let fp = Fingerprint(0x0000000000000042);
        assert_eq!(format!("{fp}"), "0000000000000042");
    }

    #[test]
    fn from_hex_invalid() {
        assert!(Fingerprint::from_hex("not_hex").is_none());
    }

    #[test]
    fn fingerprint_stability() {
        // Same code should always produce the same fingerprint
        let code = "fn foo(x: i32) -> i32 { x + 1 }";
        let f = parse_fn(code);
        let (sig1, body1) = normalize_item_fn(&f);
        let (sig2, body2) = normalize_item_fn(&f);
        let fp1 = Fingerprint::from_sig_and_body(&sig1, &body1);
        let fp2 = Fingerprint::from_sig_and_body(&sig2, &body2);
        assert_eq!(fp1, fp2);
    }

    #[test]
    fn fingerprint_discriminates_operators() {
        let code1 = "fn f(x: i32) -> i32 { x + 1 }";
        let code2 = "fn f(x: i32) -> i32 { x - 1 }";
        let f1 = parse_fn(code1);
        let f2 = parse_fn(code2);
        let (s1, b1) = normalize_item_fn(&f1);
        let (s2, b2) = normalize_item_fn(&f2);
        assert_ne!(
            Fingerprint::from_sig_and_body(&s1, &b1),
            Fingerprint::from_sig_and_body(&s2, &b2)
        );
    }

    #[test]
    fn composite_fingerprint_order_independent() {
        let fp1 = Fingerprint(1);
        let fp2 = Fingerprint(2);
        let fp3 = Fingerprint(3);
        assert_eq!(
            Fingerprint::from_fingerprints(&[fp1, fp2, fp3]),
            Fingerprint::from_fingerprints(&[fp3, fp1, fp2])
        );
    }

    #[test]
    fn composite_fingerprint_different_sets_differ() {
        let fp1 = Fingerprint(1);
        let fp2 = Fingerprint(2);
        let fp3 = Fingerprint(3);
        assert_ne!(
            Fingerprint::from_fingerprints(&[fp1, fp2]),
            Fingerprint::from_fingerprints(&[fp2, fp3])
        );
    }

    #[test]
    fn composite_fingerprint_deterministic() {
        let fp1 = Fingerprint(42);
        let fp2 = Fingerprint(99);
        let a = Fingerprint::from_fingerprints(&[fp1, fp2]);
        let b = Fingerprint::from_fingerprints(&[fp1, fp2]);
        assert_eq!(a, b);
    }

    #[test]
    fn fingerprint_ignores_variable_names() {
        let code1 = "fn compute(value: i32) -> i32 { value * value + value }";
        let code2 = "fn calculate(num: i32) -> i32 { num * num + num }";
        let f1 = parse_fn(code1);
        let f2 = parse_fn(code2);
        let (s1, b1) = normalize_item_fn(&f1);
        let (s2, b2) = normalize_item_fn(&f2);
        assert_eq!(
            Fingerprint::from_sig_and_body(&s1, &b1),
            Fingerprint::from_sig_and_body(&s2, &b2)
        );
    }
}
