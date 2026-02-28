//! Tree-sitter normalization bridge for `dupes-core`.
//!
//! This crate provides shared utilities for building tree-sitter-backed
//! language analyzers compatible with the `dupes-core` duplicate detection
//! pipeline. Language-specific crates supply a [`NodeMapping`] table and a
//! tree-sitter query; this crate handles normalization, fingerprinting, and
//! `CodeUnit` extraction.
//!
//! # Quick start
//!
//! For the simplest integration, construct a [`TreeSitterAnalyzer`] with your
//! grammar, extraction query, and mapping, then pass it to `dupes_core::analyze()`.
//!
//! For lower-level access, call [`extract_code_units`] or [`normalize_ts_node`]
//! directly.

pub mod analyzer;
pub mod extractor;
pub mod mapping;
pub mod normalizer;

// Re-export primary types for convenience
pub use analyzer::TreeSitterAnalyzer;
pub use extractor::extract_code_units;
pub use mapping::NodeMapping;
pub use normalizer::normalize_ts_node;

// Re-export commonly needed dupes-core types
pub use dupes_core::code_unit::{CodeUnit, CodeUnitKind};
pub use dupes_core::config::AnalysisConfig;
pub use dupes_core::fingerprint::Fingerprint;
pub use dupes_core::node::{
    BinOpKind, LiteralKind, NodeKind, NormalizationContext, NormalizedNode, PlaceholderKind,
    UnOpKind,
};
