use std::path::Path;

use crate::code_unit::CodeUnit;
use crate::config::AnalysisConfig;

/// Trait for language-specific code analysis.
///
/// Implementors provide file-extension detection and parsing logic,
/// allowing `dupes-core` to work with any language.
///
/// **Test code handling:** Analyzers should set [`CodeUnit::is_test`] to `true`
/// for test functions, test modules, etc. The [`crate::analyze`] function will
/// filter them out when `Config::exclude_tests` is enabled, using [`is_test_code`].
pub trait LanguageAnalyzer: Send + Sync {
    /// File extensions this analyzer handles (without the leading dot).
    fn file_extensions(&self) -> &[&str];

    /// Parse a single source file into code units.
    ///
    /// `path` is the file's location (for diagnostics), `source` is the file content,
    /// and `config` carries min-node / min-line thresholds.
    ///
    /// Analyzers should tag test code via [`CodeUnit::is_test`] rather than
    /// filtering it out; the caller handles exclusion.
    fn parse_file(
        &self,
        path: &Path,
        source: &str,
        config: &AnalysisConfig,
    ) -> Result<Vec<CodeUnit>, Box<dyn std::error::Error + Send + Sync>>;

    /// Check whether a code unit represents test code.
    ///
    /// The default implementation delegates to [`CodeUnit::is_test`],
    /// which language analyzers set during parsing.
    fn is_test_code(&self, unit: &CodeUnit) -> bool {
        unit.is_test
    }
}
