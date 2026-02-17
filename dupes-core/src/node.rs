use std::collections::HashMap;

/// Kinds of literals — preserves type but erases value.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum LiteralKind {
    Int,
    Float,
    Str,
    ByteStr,
    CStr,
    Byte,
    Char,
    Bool,
}

/// Kinds of placeholders — what the original identifier referred to.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum PlaceholderKind {
    Variable,
    Function,
    Type,
    Lifetime,
    Label,
}

/// Binary operators.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum BinOpKind {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    And,
    Or,
    BitXor,
    BitAnd,
    BitOr,
    Shl,
    Shr,
    Eq,
    Lt,
    Le,
    Ne,
    Ge,
    Gt,
    AddAssign,
    SubAssign,
    MulAssign,
    DivAssign,
    RemAssign,
    BitXorAssign,
    BitAndAssign,
    BitOrAssign,
    ShlAssign,
    ShrAssign,
    Other,
}

/// Unary operators.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum UnOpKind {
    Deref,
    Not,
    Neg,
    Other,
}

/// The kind of a normalized AST node. Carries only non-child data
/// (operator kinds, literal kinds, placeholder indices, mutability flags, macro names).
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum NodeKind {
    // Blocks and statements
    Block,
    LetBinding,

    // Literals and identifiers
    Literal(LiteralKind),
    Placeholder(PlaceholderKind, usize),

    // Operations
    BinaryOp(BinOpKind),
    UnaryOp(UnOpKind),

    // Calls and access
    Call,
    MethodCall,
    FieldAccess,
    Index,

    // Closures and functions
    Closure,
    FnSignature,

    // Control flow
    Return,
    Break,
    Continue,
    Assign,

    // References and pointers
    Reference {
        mutable: bool,
    },

    // Compound types
    Tuple,
    Array,
    Repeat,

    // Type operations
    Cast,
    StructInit,

    // Async/error
    Await,
    Try,

    // Control flow structures
    If,
    Match,
    MatchArm,
    Loop,
    While,
    ForLoop,

    // Patterns
    PatWild,
    PatPlaceholder(PlaceholderKind, usize),
    PatTuple,
    PatStruct,
    PatOr,
    PatLiteral,
    PatReference {
        mutable: bool,
    },
    PatSlice,
    PatRest,
    PatRange,

    // Types
    TypePlaceholder(PlaceholderKind, usize),
    TypeReference {
        mutable: bool,
    },
    TypeTuple,
    TypeSlice,
    TypeArray,
    TypePath,
    TypeImplTrait,
    TypeInfer,
    TypeUnit,
    TypeNever,

    // Field initializer (name = value)
    FieldValue,

    // Macro invocations
    MacroCall {
        name: String,
    },

    // Opaque — unsupported constructs
    Opaque,

    // Range expressions
    Range,

    // Path expression
    Path,

    // Let-in-condition (if let / while let)
    LetExpr,

    // Grouped expression (parenthesized)
    Paren,

    // Semicolon expression (expression statement)
    Semi,

    /// Sentinel for absent optional children, ensuring fixed child positions
    /// for correct zip alignment in similarity comparison.
    None,
}

/// A normalized AST node. Uses a data-driven `{ kind, children }` representation
/// instead of a large enum with differently-shaped variants. This allows generic
/// traversal algorithms (count_nodes, reindex, count_matching, extract) to work
/// without exhaustive matching on every variant.
///
/// ## Child ordering conventions
///
/// - **Fixed with None sentinels** (always same child count):
///   - `If` -> [condition, then_branch, else_or_None]
///   - `LetBinding` -> [pattern, type_or_None, init_or_None]
///   - `Range` / `PatRange` -> [from_or_None, to_or_None]
///   - `MatchArm` -> [pattern, guard_or_None, body]
/// - **Fixed children first, variable after** (for zip alignment):
///   - `Call` -> [func, arg0, arg1, ...]
///   - `MethodCall` -> [receiver, method, arg0, ...]
///   - `Closure` -> [body, param0, ...]
///   - `FnSignature` -> [return_type_or_None, param0, ...]
///   - `Match` -> [expr, arm0, arm1, ...]
///   - `StructInit` -> [rest_or_None, field0, field1, ...]
///   - `MacroCall` -> [arg0, arg1, ...]
/// - **Variable-length (0 or 1)**: `Return`, `Break` -> [] or [value]
/// - **Homogeneous**: `Block`, `Tuple`, `Array`, `Path`, `PatTuple`, etc. -> [elem0, ...]
/// - **All other fixed**: e.g. `BinaryOp` -> [left, right], `ForLoop` -> [pat, iter, body]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct NormalizedNode {
    pub kind: NodeKind,
    pub children: Vec<NormalizedNode>,
}

impl NormalizedNode {
    /// Create a leaf node (no children).
    pub fn leaf(kind: NodeKind) -> Self {
        Self {
            kind,
            children: vec![],
        }
    }

    /// Create a node with children.
    pub fn with_children(kind: NodeKind, children: Vec<NormalizedNode>) -> Self {
        Self { kind, children }
    }

    /// Create a None sentinel node.
    pub fn none() -> Self {
        Self::leaf(NodeKind::None)
    }

    /// Convert an Option<NormalizedNode> to a node, using None sentinel for absent values.
    pub fn opt(node: Option<NormalizedNode>) -> Self {
        node.unwrap_or_else(Self::none)
    }

    /// Check if this is a None sentinel node.
    pub fn is_none(&self) -> bool {
        matches!(self.kind, NodeKind::None)
    }
}

/// Tracks identifier-to-placeholder mappings during normalization.
pub struct NormalizationContext {
    /// Maps (identifier_string, kind) -> placeholder index
    mappings: HashMap<(String, PlaceholderKind), usize>,
    /// Per-kind counters
    counters: HashMap<PlaceholderKind, usize>,
}

impl NormalizationContext {
    pub fn new() -> Self {
        Self {
            mappings: HashMap::new(),
            counters: HashMap::new(),
        }
    }

    /// Get or assign a placeholder index for the given identifier and kind.
    pub fn placeholder(&mut self, name: &str, kind: PlaceholderKind) -> usize {
        let key = (name.to_string(), kind);
        if let Some(&idx) = self.mappings.get(&key) {
            return idx;
        }
        let counter = self.counters.entry(kind).or_insert(0);
        let idx = *counter;
        *counter += 1;
        self.mappings.insert(key, idx);
        idx
    }
}

impl Default for NormalizationContext {
    fn default() -> Self {
        Self::new()
    }
}

// -- Placeholder re-indexing --------------------------------------------------

/// Collects all placeholder occurrences in depth-first order, building
/// a mapping from (kind, old_index) -> new_sequential_index.
fn collect_placeholder_order(
    node: &NormalizedNode,
    order: &mut Vec<(PlaceholderKind, usize)>,
    seen: &mut std::collections::HashSet<(PlaceholderKind, usize)>,
) {
    match &node.kind {
        NodeKind::Placeholder(kind, idx)
        | NodeKind::PatPlaceholder(kind, idx)
        | NodeKind::TypePlaceholder(kind, idx) => {
            if seen.insert((*kind, *idx)) {
                order.push((*kind, *idx));
            }
        }
        _ => {}
    }
    for child in &node.children {
        collect_placeholder_order(child, order, seen);
    }
}

/// Applies the reindex mapping to a node, returning a new node with remapped indices.
fn apply_reindex(
    node: &NormalizedNode,
    mapping: &HashMap<(PlaceholderKind, usize), usize>,
) -> NormalizedNode {
    let kind = match &node.kind {
        NodeKind::Placeholder(kind, idx) => {
            let new_idx = mapping.get(&(*kind, *idx)).copied().unwrap_or(*idx);
            NodeKind::Placeholder(*kind, new_idx)
        }
        NodeKind::PatPlaceholder(kind, idx) => {
            let new_idx = mapping.get(&(*kind, *idx)).copied().unwrap_or(*idx);
            NodeKind::PatPlaceholder(*kind, new_idx)
        }
        NodeKind::TypePlaceholder(kind, idx) => {
            let new_idx = mapping.get(&(*kind, *idx)).copied().unwrap_or(*idx);
            NodeKind::TypePlaceholder(*kind, new_idx)
        }
        other => other.clone(),
    };
    let children = node
        .children
        .iter()
        .map(|c| apply_reindex(c, mapping))
        .collect();
    NormalizedNode { kind, children }
}

/// Re-index all placeholders in a sub-tree so that indices start from 0
/// per kind, assigned by first-occurrence depth-first order.
/// This allows comparing sub-trees extracted from different function contexts.
pub fn reindex_placeholders(node: &NormalizedNode) -> NormalizedNode {
    let mut order = Vec::new();
    let mut seen = std::collections::HashSet::new();
    collect_placeholder_order(node, &mut order, &mut seen);

    // Build mapping: (kind, old_index) -> new sequential index per kind
    let mut counters: HashMap<PlaceholderKind, usize> = HashMap::new();
    let mut mapping: HashMap<(PlaceholderKind, usize), usize> = HashMap::new();
    for (kind, old_idx) in order {
        let counter = counters.entry(kind).or_insert(0);
        mapping.insert((kind, old_idx), *counter);
        *counter += 1;
    }

    apply_reindex(node, &mapping)
}

/// Count the number of nodes in a normalized tree.
/// None sentinel nodes are not counted.
pub fn count_nodes(node: &NormalizedNode) -> usize {
    if node.is_none() {
        return 0;
    }
    1 + node.children.iter().map(count_nodes).sum::<usize>()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn reindex_remaps_from_zero() {
        let node = NormalizedNode::with_children(
            NodeKind::BinaryOp(BinOpKind::Add),
            vec![
                NormalizedNode::leaf(NodeKind::Placeholder(PlaceholderKind::Variable, 5)),
                NormalizedNode::leaf(NodeKind::Placeholder(PlaceholderKind::Variable, 8)),
            ],
        );
        let reindexed = reindex_placeholders(&node);
        let expected = NormalizedNode::with_children(
            NodeKind::BinaryOp(BinOpKind::Add),
            vec![
                NormalizedNode::leaf(NodeKind::Placeholder(PlaceholderKind::Variable, 0)),
                NormalizedNode::leaf(NodeKind::Placeholder(PlaceholderKind::Variable, 1)),
            ],
        );
        assert_eq!(reindexed, expected);
    }

    #[test]
    fn reindex_preserves_same_placeholder_identity() {
        let node = NormalizedNode::with_children(
            NodeKind::BinaryOp(BinOpKind::Add),
            vec![
                NormalizedNode::leaf(NodeKind::Placeholder(PlaceholderKind::Variable, 3)),
                NormalizedNode::leaf(NodeKind::Placeholder(PlaceholderKind::Variable, 3)),
            ],
        );
        let reindexed = reindex_placeholders(&node);
        let expected = NormalizedNode::with_children(
            NodeKind::BinaryOp(BinOpKind::Add),
            vec![
                NormalizedNode::leaf(NodeKind::Placeholder(PlaceholderKind::Variable, 0)),
                NormalizedNode::leaf(NodeKind::Placeholder(PlaceholderKind::Variable, 0)),
            ],
        );
        assert_eq!(reindexed, expected);
    }

    #[test]
    fn reindex_makes_equivalent_subtrees_equal() {
        let subtree1 = NormalizedNode::with_children(
            NodeKind::Block,
            vec![
                NormalizedNode::with_children(
                    NodeKind::LetBinding,
                    vec![
                        NormalizedNode::leaf(NodeKind::PatPlaceholder(
                            PlaceholderKind::Variable,
                            2,
                        )),
                        NormalizedNode::none(),
                        NormalizedNode::with_children(
                            NodeKind::BinaryOp(BinOpKind::Add),
                            vec![
                                NormalizedNode::leaf(NodeKind::Placeholder(
                                    PlaceholderKind::Variable,
                                    0,
                                )),
                                NormalizedNode::leaf(NodeKind::Literal(LiteralKind::Int)),
                            ],
                        ),
                    ],
                ),
                NormalizedNode::leaf(NodeKind::Placeholder(PlaceholderKind::Variable, 2)),
            ],
        );
        let subtree2 = NormalizedNode::with_children(
            NodeKind::Block,
            vec![
                NormalizedNode::with_children(
                    NodeKind::LetBinding,
                    vec![
                        NormalizedNode::leaf(NodeKind::PatPlaceholder(
                            PlaceholderKind::Variable,
                            7,
                        )),
                        NormalizedNode::none(),
                        NormalizedNode::with_children(
                            NodeKind::BinaryOp(BinOpKind::Add),
                            vec![
                                NormalizedNode::leaf(NodeKind::Placeholder(
                                    PlaceholderKind::Variable,
                                    5,
                                )),
                                NormalizedNode::leaf(NodeKind::Literal(LiteralKind::Int)),
                            ],
                        ),
                    ],
                ),
                NormalizedNode::leaf(NodeKind::Placeholder(PlaceholderKind::Variable, 7)),
            ],
        );

        assert_ne!(subtree1, subtree2);
        assert_eq!(
            reindex_placeholders(&subtree1),
            reindex_placeholders(&subtree2)
        );
    }

    #[test]
    fn reindex_handles_multiple_placeholder_kinds() {
        let node = NormalizedNode::with_children(
            NodeKind::Call,
            vec![
                NormalizedNode::leaf(NodeKind::Placeholder(PlaceholderKind::Function, 3)),
                NormalizedNode::leaf(NodeKind::Placeholder(PlaceholderKind::Variable, 5)),
                NormalizedNode::with_children(
                    NodeKind::Cast,
                    vec![
                        NormalizedNode::leaf(NodeKind::Placeholder(PlaceholderKind::Variable, 5)),
                        NormalizedNode::leaf(NodeKind::TypePlaceholder(PlaceholderKind::Type, 2)),
                    ],
                ),
            ],
        );
        let reindexed = reindex_placeholders(&node);
        let expected = NormalizedNode::with_children(
            NodeKind::Call,
            vec![
                NormalizedNode::leaf(NodeKind::Placeholder(PlaceholderKind::Function, 0)),
                NormalizedNode::leaf(NodeKind::Placeholder(PlaceholderKind::Variable, 0)),
                NormalizedNode::with_children(
                    NodeKind::Cast,
                    vec![
                        NormalizedNode::leaf(NodeKind::Placeholder(PlaceholderKind::Variable, 0)),
                        NormalizedNode::leaf(NodeKind::TypePlaceholder(PlaceholderKind::Type, 0)),
                    ],
                ),
            ],
        );
        assert_eq!(reindexed, expected);
    }

    #[test]
    fn count_nodes_skips_none_sentinels() {
        let node = NormalizedNode::with_children(
            NodeKind::If,
            vec![
                NormalizedNode::leaf(NodeKind::Placeholder(PlaceholderKind::Variable, 0)),
                NormalizedNode::with_children(NodeKind::Block, vec![]),
                NormalizedNode::none(),
            ],
        );
        // If(1) + Placeholder(1) + Block(1) = 3 (None is not counted)
        assert_eq!(count_nodes(&node), 3);
    }

    #[test]
    fn count_nodes_basic() {
        let node = NormalizedNode::with_children(
            NodeKind::BinaryOp(BinOpKind::Add),
            vec![
                NormalizedNode::leaf(NodeKind::Placeholder(PlaceholderKind::Variable, 0)),
                NormalizedNode::leaf(NodeKind::Literal(LiteralKind::Int)),
            ],
        );
        assert_eq!(count_nodes(&node), 3);
    }
}
