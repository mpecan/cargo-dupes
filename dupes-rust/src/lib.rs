//! Rust language analyzer for the `dupes-core` duplicate detection framework.
//!
//! This crate provides [`RustAnalyzer`], which implements the
//! [`dupes_core::analyzer::LanguageAnalyzer`] trait using `syn` for AST parsing
//! and normalization.

pub mod normalizer;
pub mod parser;

use std::path::Path;

use dupes_core::analyzer::LanguageAnalyzer;
use dupes_core::code_unit::CodeUnit;
use dupes_core::config::AnalysisConfig;

/// Rust language analyzer using syn for AST parsing.
pub struct RustAnalyzer;

impl RustAnalyzer {
    pub fn new() -> Self {
        Self
    }
}

impl Default for RustAnalyzer {
    fn default() -> Self {
        Self::new()
    }
}

impl LanguageAnalyzer for RustAnalyzer {
    fn file_extensions(&self) -> &[&str] {
        &["rs"]
    }

    fn parse_file(
        &self,
        path: &Path,
        source: &str,
        config: &AnalysisConfig,
    ) -> Result<Vec<CodeUnit>, Box<dyn std::error::Error + Send + Sync>> {
        parser::parse_source(path, source, config.min_nodes, config.min_lines)
            .map_err(|e| -> Box<dyn std::error::Error + Send + Sync> { e.into() })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::path::PathBuf;

    #[test]
    fn rust_analyzer_through_trait() {
        let analyzer = RustAnalyzer::new();
        let config = AnalysisConfig {
            min_nodes: 1,
            min_lines: 0,
        };
        let source = r#"
            fn foo(x: i32) -> i32 {
                let y = x + 1;
                y * 2
            }
            #[test]
            fn test_foo() {
                let z = 1;
                let w = z + 1;
                assert_eq!(w, 2);
            }
        "#;
        let path = PathBuf::from("test.rs");
        let units = analyzer.parse_file(&path, source, &config).unwrap();

        // Both units should be present (filtering is done by analyze())
        assert!(units.len() >= 2);

        // Production code should not be tagged as test
        let prod: Vec<_> = units.iter().filter(|u| u.name == "foo").collect();
        assert_eq!(prod.len(), 1);
        assert!(!prod[0].is_test);

        // Test code should be tagged
        let test: Vec<_> = units.iter().filter(|u| u.name == "test_foo").collect();
        assert_eq!(test.len(), 1);
        assert!(test[0].is_test);

        // Default is_test_code() delegates to is_test field
        assert!(!analyzer.is_test_code(prod[0]));
    }
}
