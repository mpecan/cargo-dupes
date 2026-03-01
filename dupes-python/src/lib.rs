//! Python language analyzer for `dupes-core`.
//!
//! Provides [`PythonAnalyzer`], a thin wrapper around
//! [`dupes_treesitter::TreeSitterAnalyzer`] configured with Python-specific
//! node mappings, extraction query, and test detection.

use std::path::Path;

use dupes_core::analyzer::LanguageAnalyzer;
use dupes_core::code_unit::CodeUnit;
use dupes_core::config::AnalysisConfig;
use dupes_core::node::{BinOpKind, LiteralKind, NodeKind, UnOpKind};
use dupes_treesitter::TreeSitterAnalyzer;
use dupes_treesitter::mapping::NodeMapping;

/// Tree-sitter query for extracting Python functions and methods.
///
/// Matches `function_definition` at any depth, so it captures both top-level
/// functions and methods inside classes.
const PYTHON_QUERY: &str = r"
(function_definition
    name: (identifier) @name
    parameters: (parameters) @parameters
    body: (block) @body
) @definition
";

/// Python language analyzer backed by tree-sitter.
///
/// Detects duplicate and near-duplicate code in Python source files (`.py`, `.pyi`).
/// Test functions are identified by the `test_` name prefix (pytest convention).
#[derive(Debug)]
pub struct PythonAnalyzer {
    inner: TreeSitterAnalyzer,
}

impl PythonAnalyzer {
    /// Create a new `PythonAnalyzer`.
    ///
    /// # Panics
    ///
    /// Panics if the built-in tree-sitter query is invalid (should never happen).
    #[must_use]
    pub fn new() -> Self {
        let inner = TreeSitterAnalyzer::new(
            tree_sitter_python::LANGUAGE.into(),
            &["py", "pyi"],
            PYTHON_QUERY,
            python_mapping(),
        )
        .expect("built-in Python query should be valid")
        .with_test_detector(|name, _node| name.starts_with("test_"));

        Self { inner }
    }
}

impl Default for PythonAnalyzer {
    fn default() -> Self {
        Self::new()
    }
}

impl LanguageAnalyzer for PythonAnalyzer {
    fn file_extensions(&self) -> &[&str] {
        self.inner.file_extensions()
    }

    fn parse_file(
        &self,
        path: &Path,
        source: &str,
        config: &AnalysisConfig,
    ) -> Result<Vec<CodeUnit>, Box<dyn std::error::Error + Send + Sync>> {
        self.inner.parse_file(path, source, config)
    }
}

/// Build the Python-specific [`NodeMapping`].
///
/// # Covered constructs
///
/// - Identifiers, literals (int, float, str, bool, None)
/// - Binary operators (+, -, *, /, %, ==, !=, <, >, <=, >=, and, or, bitwise, augmented assignment)
/// - Unary operators (not, -, ~)
/// - Control flow: if, for, while, match/case, return, break, continue
/// - Calls, assignments (plain + augmented), blocks, function definitions
/// - Containers: tuple, list, set, dictionary
/// - Access: attribute (field access), subscript (index)
/// - Async: await, yield
///
/// # Not yet covered (intentional omissions for initial implementation)
///
/// - `lambda` — falls through to generic Block normalization (acceptable; not extracted as code units)
/// - `with_statement` / `try_statement` / `raise_statement` / `assert_statement` —
///   fall through to generic recursion (structure is preserved, semantic kind is lost)
/// - `conditional_expression` (ternary `x if cond else y`) — falls through to Block
/// - List/dict/set comprehensions — children are recursively normalized
/// - `global_statement` / `nonlocal_statement` — fall through
/// - f-string interpolations — the `string` node is treated as `Literal(Str)`,
///   so interpolated expressions inside f-strings are not captured
/// - `pass_statement` / `ellipsis` — become Opaque leaves (acceptable no-ops)
/// - Decorator-based test detection (e.g., `@pytest.fixture`) — only name-based `test_` prefix
#[must_use]
pub fn python_mapping() -> NodeMapping {
    NodeMapping::new()
        .identifiers(&["identifier"])
        .literals(&[
            ("integer", LiteralKind::Int),
            ("float", LiteralKind::Float),
            ("string", LiteralKind::Str),
            ("true", LiteralKind::Bool),
            ("false", LiteralKind::Bool),
            ("none", LiteralKind::Null),
        ])
        .binary_ops(&[
            // Arithmetic
            ("+", BinOpKind::Add),
            ("-", BinOpKind::Sub),
            ("*", BinOpKind::Mul),
            ("/", BinOpKind::Div),
            ("%", BinOpKind::Rem),
            // Comparison
            ("==", BinOpKind::Eq),
            ("!=", BinOpKind::Ne),
            ("<", BinOpKind::Lt),
            (">", BinOpKind::Gt),
            ("<=", BinOpKind::Le),
            (">=", BinOpKind::Ge),
            // Logical
            ("and", BinOpKind::And),
            ("or", BinOpKind::Or),
            // Bitwise
            ("&", BinOpKind::BitAnd),
            ("|", BinOpKind::BitOr),
            ("^", BinOpKind::BitXor),
            ("<<", BinOpKind::Shl),
            (">>", BinOpKind::Shr),
            // Augmented assignment operators
            ("+=", BinOpKind::AddAssign),
            ("-=", BinOpKind::SubAssign),
            ("*=", BinOpKind::MulAssign),
            ("/=", BinOpKind::DivAssign),
            ("%=", BinOpKind::RemAssign),
            ("&=", BinOpKind::BitAndAssign),
            ("|=", BinOpKind::BitOrAssign),
            ("^=", BinOpKind::BitXorAssign),
            ("<<=", BinOpKind::ShlAssign),
            (">>=", BinOpKind::ShrAssign),
            // Power, floor div, matrix multiply — fall back to Other
        ])
        .unary_ops(&[
            ("not", UnOpKind::Not),
            ("-", UnOpKind::Neg),
            ("~", UnOpKind::Other),
        ])
        .skip(&["comment", "decorator"])
        .blocks(&["block"])
        .calls(&["call"])
        .returns(&["return_statement"])
        .ifs(&["if_statement"])
        .for_loops(&["for_statement"])
        .while_loops(&["while_statement"])
        .matches(&["match_statement"])
        .assignments(&["assignment"])
        .function_defs(&["function_definition"])
        .binary_op_kinds(&[
            "binary_operator",
            "boolean_operator",
            "comparison_operator",
            "augmented_assignment",
        ])
        .unary_op_kinds(&["not_operator", "unary_operator"])
        .match_arms(&["case_clause"])
        .node_kinds(&[
            // Control flow leaves
            ("break_statement", NodeKind::Break),
            ("continue_statement", NodeKind::Continue),
            // Async
            ("await", NodeKind::Await),
            ("yield", NodeKind::Yield),
            // Containers
            ("tuple", NodeKind::Tuple),
            ("list", NodeKind::Array),
            ("set", NodeKind::Array),
            ("dictionary", NodeKind::StructInit),
            // Access
            ("attribute", NodeKind::FieldAccess),
            ("subscript", NodeKind::Index),
        ])
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn new_does_not_panic() {
        let _ = PythonAnalyzer::new();
    }

    #[test]
    fn default_does_not_panic() {
        let _ = PythonAnalyzer::default();
    }

    #[test]
    fn file_extensions() {
        let analyzer = PythonAnalyzer::new();
        assert_eq!(analyzer.file_extensions(), &["py", "pyi"]);
    }
}
