use std::path::PathBuf;

use crate::fingerprint::Fingerprint;
use crate::node::NormalizedNode;

/// The kind of code unit extracted from source.
#[derive(Debug, Clone, PartialEq, Eq, Hash, serde::Serialize)]
pub enum CodeUnitKind {
    Function,
    Method,
    Closure,
    ImplBlock,
    TraitImplBlock,
    // Sub-function kinds
    IfBranch,
    MatchArm,
    LoopBody,
    Block,
}

impl std::fmt::Display for CodeUnitKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Function => write!(f, "function"),
            Self::Method => write!(f, "method"),
            Self::Closure => write!(f, "closure"),
            Self::ImplBlock => write!(f, "impl block"),
            Self::TraitImplBlock => write!(f, "trait impl block"),
            Self::IfBranch => write!(f, "if branch"),
            Self::MatchArm => write!(f, "match arm"),
            Self::LoopBody => write!(f, "loop body"),
            Self::Block => write!(f, "block"),
        }
    }
}

/// A unit of code extracted and normalized for duplication analysis.
#[derive(Debug, Clone)]
pub struct CodeUnit {
    pub kind: CodeUnitKind,
    pub name: String,
    pub file: PathBuf,
    pub line_start: usize,
    pub line_end: usize,
    pub signature: NormalizedNode,
    pub body: NormalizedNode,
    pub fingerprint: Fingerprint,
    pub node_count: usize,
    /// For sub-function units, the name of the parent function.
    pub parent_name: Option<String>,
    /// Whether this code unit was identified as test code by the language analyzer.
    pub is_test: bool,
}
