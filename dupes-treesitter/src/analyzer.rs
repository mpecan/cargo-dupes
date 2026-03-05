//! Generic tree-sitter-backed `LanguageAnalyzer` implementation.
//!
//! Provides [`TreeSitterAnalyzer`], a ready-made adapter that implements
//! `dupes_core::analyzer::LanguageAnalyzer` using a [`NodeMapping`], a tree-sitter
//! query, and a tree-sitter [`Language`]. Language-specific crates can construct
//! one with their grammar and mapping to get full duplication analysis without
//! writing custom parsing logic.

use std::path::Path;

use dupes_core::analyzer::LanguageAnalyzer;
use dupes_core::code_unit::{CodeUnit, CodeUnitKind};
use dupes_core::config::AnalysisConfig;

use crate::extractor::{KindResolver, extract_code_units};
use crate::mapping::NodeMapping;

/// A tree-sitter-backed language analyzer.
///
/// Wraps a tree-sitter [`Language`](tree_sitter::Language), extraction query,
/// [`NodeMapping`], and file extensions to implement [`LanguageAnalyzer`].
///
/// # Example
///
/// ```ignore
/// let analyzer = TreeSitterAnalyzer::new(
///     tree_sitter_python::LANGUAGE.into(),
///     &["py"],
///     "(function_definition name: (identifier) @name
///         parameters: (parameters) @parameters
///         body: (block) @body) @definition",
///     python_mapping(),
/// ).expect("invalid query");
/// ```
pub struct TreeSitterAnalyzer {
    language: tree_sitter::Language,
    extensions: Vec<&'static str>,
    query: tree_sitter::Query,
    mapping: NodeMapping,
    kind_resolver: KindResolver,
    #[allow(clippy::type_complexity)]
    is_test: Box<dyn Fn(&str, tree_sitter::Node) -> bool + Send + Sync>,
}

impl TreeSitterAnalyzer {
    /// Create a new tree-sitter analyzer.
    ///
    /// # Errors
    ///
    /// Returns an error if `query_source` is not a valid tree-sitter query
    /// for the given language.
    pub fn new(
        language: tree_sitter::Language,
        extensions: &[&'static str],
        query_source: &str,
        mapping: NodeMapping,
    ) -> Result<Self, tree_sitter::QueryError> {
        let query = tree_sitter::Query::new(&language, query_source)?;
        Ok(Self {
            language,
            extensions: extensions.to_vec(),
            query,
            mapping,
            kind_resolver: Box::new(|_| CodeUnitKind::Function),
            is_test: Box::new(|_, _| false),
        })
    }

    /// Set a callback to resolve `CodeUnitKind` from the tree-sitter node kind.
    ///
    /// The callback receives the `@definition` node's `kind()` string (e.g.,
    /// `"function_definition"`, `"lambda"`, `"class_definition"`) and should
    /// return the corresponding `CodeUnitKind`.
    ///
    /// Default: all nodes map to `CodeUnitKind::Function`.
    #[must_use]
    pub fn with_kind_resolver(
        mut self,
        f: impl Fn(&str) -> CodeUnitKind + Send + Sync + 'static,
    ) -> Self {
        self.kind_resolver = Box::new(f);
        self
    }

    /// Set a callback to detect test code.
    ///
    /// The callback receives the function name and the `@definition` tree-sitter
    /// node, and should return `true` if the code unit is test code.
    #[must_use]
    pub fn with_test_detector(
        mut self,
        f: impl Fn(&str, tree_sitter::Node) -> bool + Send + Sync + 'static,
    ) -> Self {
        self.is_test = Box::new(f);
        self
    }
}

impl LanguageAnalyzer for TreeSitterAnalyzer {
    fn file_extensions(&self) -> &[&str] {
        &self.extensions
    }

    fn parse_file(
        &self,
        path: &Path,
        source: &str,
        config: &AnalysisConfig,
    ) -> Result<Vec<CodeUnit>, Box<dyn std::error::Error + Send + Sync>> {
        let mut parser = tree_sitter::Parser::new();
        parser.set_language(&self.language)?;
        let tree = parser
            .parse(source, None)
            .ok_or("tree-sitter failed to parse source")?;
        Ok(extract_code_units(
            &tree,
            source.as_bytes(),
            path,
            &self.query,
            &self.mapping,
            config,
            &self.kind_resolver,
            &self.is_test,
        ))
    }
}

impl std::fmt::Debug for TreeSitterAnalyzer {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("TreeSitterAnalyzer")
            .field("extensions", &self.extensions)
            .field("kind_resolver", &"<closure>")
            .finish_non_exhaustive()
    }
}
