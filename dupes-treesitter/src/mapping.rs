use std::collections::{HashMap, HashSet};

use dupes_core::node::{BinOpKind, LiteralKind, UnOpKind};

/// Table-driven mapping from tree-sitter node kinds to dupes-core concepts.
///
/// Each language populates its own instance with the relevant node kind strings
/// from its tree-sitter grammar. The normalizer and extractor use this mapping
/// to convert tree-sitter CST nodes into `NormalizedNode` trees.
#[derive(Debug, Clone)]
pub struct NodeMapping {
    /// Node kinds that represent identifiers (variables, function names, etc.).
    pub identifier_kinds: HashSet<&'static str>,
    /// Node kinds that represent literals, mapped to their `LiteralKind`.
    pub literal_kinds: HashMap<&'static str, LiteralKind>,
    /// Node kinds for binary operators, mapped to `BinOpKind`.
    pub binary_op_map: HashMap<&'static str, BinOpKind>,
    /// Node kinds for unary operators, mapped to `UnOpKind`.
    pub unary_op_map: HashMap<&'static str, UnOpKind>,
    /// Node kinds to skip entirely (e.g., comments, decorators).
    pub skip_kinds: HashSet<&'static str>,
    /// Node kinds to treat as opaque leaves (no recursive normalization).
    pub opaque_kinds: HashSet<&'static str>,
    /// Node kinds representing block/suite constructs.
    pub block_kinds: HashSet<&'static str>,
    /// Node kinds representing function/method calls.
    pub call_kinds: HashSet<&'static str>,
    /// Node kinds representing return statements.
    pub return_kinds: HashSet<&'static str>,
    /// Node kinds representing if/conditional constructs.
    pub if_kinds: HashSet<&'static str>,
    /// Node kinds representing infinite loop constructs.
    pub loop_kinds: HashSet<&'static str>,
    /// Node kinds representing for-loop constructs.
    pub for_kinds: HashSet<&'static str>,
    /// Node kinds representing while-loop constructs.
    pub while_kinds: HashSet<&'static str>,
    /// Node kinds representing match/switch constructs.
    pub match_kinds: HashSet<&'static str>,
    /// Node kinds representing assignment statements.
    pub assignment_kinds: HashSet<&'static str>,
    /// Node kinds representing function definitions.
    pub function_def_kinds: HashSet<&'static str>,
    /// Node kinds representing binary operator expressions (e.g., `"binary_operator"`,
    /// `"binary_expression"`, `"boolean_operator"`, `"comparison_operator"`).
    /// The operator text is looked up in `binary_op_map`.
    pub binary_op_kinds: HashSet<&'static str>,
    /// Node kinds representing unary operator expressions (e.g., `"unary_operator"`,
    /// `"unary_expression"`, `"not_operator"`).
    /// The operator text is looked up in `unary_op_map`.
    pub unary_op_kinds: HashSet<&'static str>,
    /// Node kinds representing match/case arm entries within a match statement.
    /// Used for fixed-position child extraction: `[pattern, guard_or_None, body]`.
    pub match_arm_kinds: HashSet<&'static str>,
}

impl NodeMapping {
    /// Create an empty mapping. Use the builder methods to populate it.
    #[must_use]
    pub fn new() -> Self {
        Self {
            identifier_kinds: HashSet::new(),
            literal_kinds: HashMap::new(),
            binary_op_map: HashMap::new(),
            unary_op_map: HashMap::new(),
            skip_kinds: HashSet::new(),
            opaque_kinds: HashSet::new(),
            block_kinds: HashSet::new(),
            call_kinds: HashSet::new(),
            return_kinds: HashSet::new(),
            if_kinds: HashSet::new(),
            loop_kinds: HashSet::new(),
            for_kinds: HashSet::new(),
            while_kinds: HashSet::new(),
            match_kinds: HashSet::new(),
            assignment_kinds: HashSet::new(),
            function_def_kinds: HashSet::new(),
            binary_op_kinds: HashSet::new(),
            unary_op_kinds: HashSet::new(),
            match_arm_kinds: HashSet::new(),
        }
    }

    /// Add identifier node kinds.
    #[must_use]
    pub fn identifiers(mut self, kinds: &[&'static str]) -> Self {
        self.identifier_kinds.extend(kinds);
        self
    }

    /// Add literal node kinds with their `LiteralKind`.
    #[must_use]
    pub fn literals(mut self, mappings: &[(&'static str, LiteralKind)]) -> Self {
        for (kind, lit) in mappings {
            self.literal_kinds.insert(kind, lit.clone());
        }
        self
    }

    /// Add binary operator mappings (operator text → `BinOpKind`).
    #[must_use]
    pub fn binary_ops(mut self, mappings: &[(&'static str, BinOpKind)]) -> Self {
        for (text, op) in mappings {
            self.binary_op_map.insert(text, op.clone());
        }
        self
    }

    /// Add unary operator mappings (operator text → `UnOpKind`).
    #[must_use]
    pub fn unary_ops(mut self, mappings: &[(&'static str, UnOpKind)]) -> Self {
        for (text, op) in mappings {
            self.unary_op_map.insert(text, op.clone());
        }
        self
    }

    /// Add node kinds to skip entirely.
    #[must_use]
    pub fn skip(mut self, kinds: &[&'static str]) -> Self {
        self.skip_kinds.extend(kinds);
        self
    }

    /// Add node kinds to treat as opaque leaves.
    #[must_use]
    pub fn opaque(mut self, kinds: &[&'static str]) -> Self {
        self.opaque_kinds.extend(kinds);
        self
    }

    /// Add block/suite node kinds.
    #[must_use]
    pub fn blocks(mut self, kinds: &[&'static str]) -> Self {
        self.block_kinds.extend(kinds);
        self
    }

    /// Add call node kinds.
    #[must_use]
    pub fn calls(mut self, kinds: &[&'static str]) -> Self {
        self.call_kinds.extend(kinds);
        self
    }

    /// Add return statement node kinds.
    #[must_use]
    pub fn returns(mut self, kinds: &[&'static str]) -> Self {
        self.return_kinds.extend(kinds);
        self
    }

    /// Add if/conditional node kinds.
    #[must_use]
    pub fn ifs(mut self, kinds: &[&'static str]) -> Self {
        self.if_kinds.extend(kinds);
        self
    }

    /// Add infinite loop node kinds.
    #[must_use]
    pub fn loops(mut self, kinds: &[&'static str]) -> Self {
        self.loop_kinds.extend(kinds);
        self
    }

    /// Add for-loop node kinds.
    #[must_use]
    pub fn for_loops(mut self, kinds: &[&'static str]) -> Self {
        self.for_kinds.extend(kinds);
        self
    }

    /// Add while-loop node kinds.
    #[must_use]
    pub fn while_loops(mut self, kinds: &[&'static str]) -> Self {
        self.while_kinds.extend(kinds);
        self
    }

    /// Add match/switch node kinds.
    #[must_use]
    pub fn matches(mut self, kinds: &[&'static str]) -> Self {
        self.match_kinds.extend(kinds);
        self
    }

    /// Add assignment node kinds.
    #[must_use]
    pub fn assignments(mut self, kinds: &[&'static str]) -> Self {
        self.assignment_kinds.extend(kinds);
        self
    }

    /// Add function definition node kinds.
    #[must_use]
    pub fn function_defs(mut self, kinds: &[&'static str]) -> Self {
        self.function_def_kinds.extend(kinds);
        self
    }

    /// Add binary operator expression node kinds (e.g., `"binary_operator"`,
    /// `"binary_expression"`). These are the tree-sitter node kinds that contain
    /// a binary operation; the operator text is looked up in `binary_op_map`.
    #[must_use]
    pub fn binary_op_kinds(mut self, kinds: &[&'static str]) -> Self {
        self.binary_op_kinds.extend(kinds);
        self
    }

    /// Add unary operator expression node kinds (e.g., `"unary_operator"`,
    /// `"not_operator"`). These are the tree-sitter node kinds that contain
    /// a unary operation; the operator text is looked up in `unary_op_map`.
    #[must_use]
    pub fn unary_op_kinds(mut self, kinds: &[&'static str]) -> Self {
        self.unary_op_kinds.extend(kinds);
        self
    }

    /// Add match/case arm node kinds for fixed-position extraction.
    #[must_use]
    pub fn match_arms(mut self, kinds: &[&'static str]) -> Self {
        self.match_arm_kinds.extend(kinds);
        self
    }
}

impl Default for NodeMapping {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn empty_mapping() {
        let m = NodeMapping::new();
        assert!(m.identifier_kinds.is_empty());
        assert!(m.literal_kinds.is_empty());
        assert!(m.binary_op_map.is_empty());
    }

    #[test]
    fn builder_api() {
        let m = NodeMapping::new()
            .identifiers(&["identifier", "name"])
            .literals(&[("integer", LiteralKind::Int), ("string", LiteralKind::Str)])
            .binary_ops(&[("+", BinOpKind::Add), ("-", BinOpKind::Sub)])
            .skip(&["comment"])
            .blocks(&["block"])
            .calls(&["call"])
            .returns(&["return_statement"])
            .ifs(&["if_statement"])
            .for_loops(&["for_statement"])
            .while_loops(&["while_statement"])
            .assignments(&["assignment"])
            .function_defs(&["function_definition"]);

        assert!(m.identifier_kinds.contains("identifier"));
        assert!(m.identifier_kinds.contains("name"));
        assert_eq!(m.literal_kinds.get("integer"), Some(&LiteralKind::Int));
        assert_eq!(m.binary_op_map.get("+"), Some(&BinOpKind::Add));
        assert!(m.skip_kinds.contains("comment"));
        assert!(m.block_kinds.contains("block"));
        assert!(m.call_kinds.contains("call"));
        assert!(m.return_kinds.contains("return_statement"));
        assert!(m.if_kinds.contains("if_statement"));
        assert!(m.for_kinds.contains("for_statement"));
        assert!(m.while_kinds.contains("while_statement"));
        assert!(m.assignment_kinds.contains("assignment"));
        assert!(m.function_def_kinds.contains("function_definition"));
    }
}
