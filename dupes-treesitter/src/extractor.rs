//! Query-based `CodeUnit` extraction from tree-sitter parse trees.
//!
//! Uses tree-sitter queries with named captures to identify function definitions,
//! then normalizes and fingerprints them into `CodeUnit`s for duplicate detection.

use std::path::Path;

use dupes_core::code_unit::{CodeUnit, CodeUnitKind};
use dupes_core::config::AnalysisConfig;
use dupes_core::fingerprint::Fingerprint;
use dupes_core::node::{NormalizationContext, NormalizedNode, count_nodes};
use tree_sitter::StreamingIterator;

use crate::mapping::NodeMapping;
use crate::normalizer::normalize_ts_node;

/// Extract `CodeUnit`s from a tree-sitter parse tree using a query.
///
/// The query should use capture names to identify parts of function definitions:
/// - `@definition` — the entire function node (for line range)
/// - `@name` — the function name identifier
/// - `@body` — the function body block
/// - `@parameters` — the parameter list (optional)
///
/// # Parameters
///
/// - `unit_kind` — the `CodeUnitKind` to assign to extracted units (e.g., `Function`, `Method`)
/// - `is_test` — callback to determine whether a definition node represents test code;
///   receives the function name and the `@definition` node
///
/// Each captured function is normalized, fingerprinted, and filtered by
/// `min_nodes` and `min_lines` from the `AnalysisConfig`.
#[must_use]
#[allow(clippy::too_many_arguments)]
pub fn extract_code_units(
    tree: &tree_sitter::Tree,
    source: &[u8],
    file_path: &Path,
    query: &tree_sitter::Query,
    mapping: &NodeMapping,
    config: &AnalysisConfig,
    unit_kind: &CodeUnitKind,
    is_test: impl Fn(&str, tree_sitter::Node) -> bool,
) -> Vec<CodeUnit> {
    let root = tree.root_node();
    let mut cursor = tree_sitter::QueryCursor::new();
    let mut matches = cursor.matches(query, root, source);

    // Resolve capture indices by name
    let def_idx = query.capture_index_for_name("definition");
    let name_idx = query.capture_index_for_name("name");
    let body_idx = query.capture_index_for_name("body");
    let params_idx = query.capture_index_for_name("parameters");

    let mut units = Vec::new();

    while let Some(m) = matches.next() {
        let def_node: Option<tree_sitter::Node> =
            def_idx.and_then(|idx| m.captures.iter().find(|c| c.index == idx).map(|c| c.node));
        let name_node: Option<tree_sitter::Node> =
            name_idx.and_then(|idx| m.captures.iter().find(|c| c.index == idx).map(|c| c.node));
        let body_node: Option<tree_sitter::Node> =
            body_idx.and_then(|idx| m.captures.iter().find(|c| c.index == idx).map(|c| c.node));
        let params_node: Option<tree_sitter::Node> =
            params_idx.and_then(|idx| m.captures.iter().find(|c| c.index == idx).map(|c| c.node));

        // We need at least a definition and body to create a code unit
        let Some(def) = def_node else { continue };
        let Some(body) = body_node else { continue };

        let name = name_node
            .and_then(|n: tree_sitter::Node| n.utf8_text(source).ok())
            .unwrap_or("<anonymous>")
            .to_string();

        // Line numbers: tree-sitter is 0-based, dupes-core is 1-based
        let line_start = def.start_position().row + 1;
        let line_end = def.end_position().row + 1;

        // Filter by min_lines
        let line_count = line_end.saturating_sub(line_start) + 1;
        if line_count < config.min_lines {
            continue;
        }

        // Normalize with a fresh context per code unit
        let mut ctx = NormalizationContext::new();

        // Build signature from parameters
        let signature = if let Some(params) = params_node {
            normalize_ts_node(params, source, mapping, &mut ctx)
        } else {
            NormalizedNode::none()
        };

        let normalized_body = normalize_ts_node(body, source, mapping, &mut ctx);

        // Count nodes (sig + body) for consistency with other analyzers
        let node_count = count_nodes(&signature) + count_nodes(&normalized_body);
        if node_count < config.min_nodes {
            continue;
        }

        let fingerprint = Fingerprint::from_sig_and_body(&signature, &normalized_body);
        let test_code = is_test(&name, def);

        units.push(CodeUnit {
            kind: unit_kind.clone(),
            name,
            file: file_path.to_path_buf(),
            line_start,
            line_end,
            signature,
            body: normalized_body,
            fingerprint,
            node_count,
            parent_name: None,
            is_test: test_code,
        });
    }

    units
}
