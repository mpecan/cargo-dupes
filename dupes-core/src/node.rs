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

/// A match arm in normalized form.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct MatchArm {
    pub pattern: NormalizedNode,
    pub guard: Option<Box<NormalizedNode>>,
    pub body: Box<NormalizedNode>,
}

/// A normalized AST node. Mirrors syn's AST but with identifiers replaced
/// by positional placeholders and literal values erased.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum NormalizedNode {
    // Blocks and statements
    Block(Vec<NormalizedNode>),
    LetBinding {
        pattern: Box<NormalizedNode>,
        ty: Option<Box<NormalizedNode>>,
        init: Option<Box<NormalizedNode>>,
    },

    // Literals and identifiers
    Literal(LiteralKind),
    Placeholder(PlaceholderKind, usize),

    // Operations
    BinaryOp {
        op: BinOpKind,
        left: Box<NormalizedNode>,
        right: Box<NormalizedNode>,
    },
    UnaryOp {
        op: UnOpKind,
        operand: Box<NormalizedNode>,
    },

    // Calls and access
    Call {
        func: Box<NormalizedNode>,
        args: Vec<NormalizedNode>,
    },
    MethodCall {
        receiver: Box<NormalizedNode>,
        method: Box<NormalizedNode>,
        args: Vec<NormalizedNode>,
    },
    FieldAccess {
        base: Box<NormalizedNode>,
        field: Box<NormalizedNode>,
    },
    Index {
        base: Box<NormalizedNode>,
        index: Box<NormalizedNode>,
    },

    // Closures and functions
    Closure {
        params: Vec<NormalizedNode>,
        body: Box<NormalizedNode>,
    },
    FnSignature {
        params: Vec<NormalizedNode>,
        return_type: Option<Box<NormalizedNode>>,
    },

    // Control flow
    Return(Option<Box<NormalizedNode>>),
    Break(Option<Box<NormalizedNode>>),
    Continue,
    Assign {
        left: Box<NormalizedNode>,
        right: Box<NormalizedNode>,
    },

    // References and pointers
    Reference {
        mutable: bool,
        expr: Box<NormalizedNode>,
    },

    // Compound types
    Tuple(Vec<NormalizedNode>),
    Array(Vec<NormalizedNode>),
    Repeat {
        elem: Box<NormalizedNode>,
        len: Box<NormalizedNode>,
    },

    // Type operations
    Cast {
        expr: Box<NormalizedNode>,
        ty: Box<NormalizedNode>,
    },
    StructInit {
        fields: Vec<NormalizedNode>,
        rest: Option<Box<NormalizedNode>>,
    },

    // Async/error
    Await(Box<NormalizedNode>),
    Try(Box<NormalizedNode>),

    // Control flow structures
    If {
        condition: Box<NormalizedNode>,
        then_branch: Box<NormalizedNode>,
        else_branch: Option<Box<NormalizedNode>>,
    },
    Match {
        expr: Box<NormalizedNode>,
        arms: Vec<MatchArm>,
    },
    Loop(Box<NormalizedNode>),
    While {
        condition: Box<NormalizedNode>,
        body: Box<NormalizedNode>,
    },
    ForLoop {
        pat: Box<NormalizedNode>,
        iter: Box<NormalizedNode>,
        body: Box<NormalizedNode>,
    },

    // Patterns
    PatWild,
    PatPlaceholder(PlaceholderKind, usize),
    PatTuple(Vec<NormalizedNode>),
    PatStruct(Vec<NormalizedNode>),
    PatOr(Vec<NormalizedNode>),
    PatLiteral(Box<NormalizedNode>),
    PatReference {
        mutable: bool,
        pat: Box<NormalizedNode>,
    },
    PatSlice(Vec<NormalizedNode>),
    PatRest,
    PatRange {
        lo: Option<Box<NormalizedNode>>,
        hi: Option<Box<NormalizedNode>>,
    },

    // Types
    TypePlaceholder(PlaceholderKind, usize),
    TypeReference {
        mutable: bool,
        elem: Box<NormalizedNode>,
    },
    TypeTuple(Vec<NormalizedNode>),
    TypeSlice(Box<NormalizedNode>),
    TypeArray {
        elem: Box<NormalizedNode>,
        len: Box<NormalizedNode>,
    },
    TypePath(Vec<NormalizedNode>),
    TypeImplTrait(Vec<NormalizedNode>),
    TypeInfer,
    TypeUnit,
    TypeNever,

    // Field initializer (name = value)
    FieldValue {
        name: Box<NormalizedNode>,
        value: Box<NormalizedNode>,
    },

    // Macro invocations — preserves macro name and best-effort parsed args
    MacroCall {
        name: String,
        args: Vec<NormalizedNode>,
    },

    // Opaque — unsupported constructs
    Opaque,

    // Range expressions
    Range {
        from: Option<Box<NormalizedNode>>,
        to: Option<Box<NormalizedNode>>,
    },

    // Path expression (e.g. std::io::Error, or just a variable ident used as expr)
    Path(Vec<NormalizedNode>),

    // Let-in-condition (if let / while let)
    LetExpr {
        pat: Box<NormalizedNode>,
        expr: Box<NormalizedNode>,
    },

    // Grouped expression (parenthesized)
    Paren(Box<NormalizedNode>),

    // Semicolon expression (expression statement)
    Semi(Box<NormalizedNode>),
}

/// Tracks identifier-to-placeholder mappings during normalization.
pub struct NormalizationContext {
    /// Maps (identifier_string, kind) → placeholder index
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

// ── Placeholder re-indexing ──────────────────────────────────────────────

/// Collects all placeholder occurrences in depth-first order, building
/// a mapping from (kind, old_index) → new_sequential_index.
fn collect_placeholder_order(
    node: &NormalizedNode,
    order: &mut Vec<(PlaceholderKind, usize)>,
    seen: &mut std::collections::HashSet<(PlaceholderKind, usize)>,
) {
    match node {
        NormalizedNode::Placeholder(kind, idx)
        | NormalizedNode::PatPlaceholder(kind, idx)
        | NormalizedNode::TypePlaceholder(kind, idx) => {
            if seen.insert((*kind, *idx)) {
                order.push((*kind, *idx));
            }
        }
        NormalizedNode::Block(stmts) => {
            for s in stmts {
                collect_placeholder_order(s, order, seen);
            }
        }
        NormalizedNode::LetBinding { pattern, ty, init } => {
            collect_placeholder_order(pattern, order, seen);
            if let Some(t) = ty {
                collect_placeholder_order(t, order, seen);
            }
            if let Some(i) = init {
                collect_placeholder_order(i, order, seen);
            }
        }
        NormalizedNode::BinaryOp { left, right, .. } | NormalizedNode::Assign { left, right } => {
            collect_placeholder_order(left, order, seen);
            collect_placeholder_order(right, order, seen);
        }
        NormalizedNode::UnaryOp { operand, .. } => collect_placeholder_order(operand, order, seen),
        NormalizedNode::Call { func, args } => {
            collect_placeholder_order(func, order, seen);
            for a in args {
                collect_placeholder_order(a, order, seen);
            }
        }
        NormalizedNode::MethodCall {
            receiver,
            method,
            args,
        } => {
            collect_placeholder_order(receiver, order, seen);
            collect_placeholder_order(method, order, seen);
            for a in args {
                collect_placeholder_order(a, order, seen);
            }
        }
        NormalizedNode::FieldAccess { base, field } => {
            collect_placeholder_order(base, order, seen);
            collect_placeholder_order(field, order, seen);
        }
        NormalizedNode::Index { base, index } => {
            collect_placeholder_order(base, order, seen);
            collect_placeholder_order(index, order, seen);
        }
        NormalizedNode::Closure { params, body } => {
            for p in params {
                collect_placeholder_order(p, order, seen);
            }
            collect_placeholder_order(body, order, seen);
        }
        NormalizedNode::FnSignature {
            params,
            return_type,
        } => {
            for p in params {
                collect_placeholder_order(p, order, seen);
            }
            if let Some(r) = return_type {
                collect_placeholder_order(r, order, seen);
            }
        }
        NormalizedNode::Return(e) | NormalizedNode::Break(e) => {
            if let Some(e) = e {
                collect_placeholder_order(e, order, seen);
            }
        }
        NormalizedNode::Reference { expr, .. } => collect_placeholder_order(expr, order, seen),
        NormalizedNode::Tuple(elems) | NormalizedNode::Array(elems) => {
            for e in elems {
                collect_placeholder_order(e, order, seen);
            }
        }
        NormalizedNode::Repeat { elem, len } => {
            collect_placeholder_order(elem, order, seen);
            collect_placeholder_order(len, order, seen);
        }
        NormalizedNode::Cast { expr, ty } => {
            collect_placeholder_order(expr, order, seen);
            collect_placeholder_order(ty, order, seen);
        }
        NormalizedNode::StructInit { fields, rest } => {
            for f in fields {
                collect_placeholder_order(f, order, seen);
            }
            if let Some(r) = rest {
                collect_placeholder_order(r, order, seen);
            }
        }
        NormalizedNode::Await(e)
        | NormalizedNode::Try(e)
        | NormalizedNode::Paren(e)
        | NormalizedNode::Semi(e) => {
            collect_placeholder_order(e, order, seen);
        }
        NormalizedNode::If {
            condition,
            then_branch,
            else_branch,
        } => {
            collect_placeholder_order(condition, order, seen);
            collect_placeholder_order(then_branch, order, seen);
            if let Some(e) = else_branch {
                collect_placeholder_order(e, order, seen);
            }
        }
        NormalizedNode::Match { expr, arms } => {
            collect_placeholder_order(expr, order, seen);
            for arm in arms {
                collect_placeholder_order(&arm.pattern, order, seen);
                if let Some(g) = &arm.guard {
                    collect_placeholder_order(g, order, seen);
                }
                collect_placeholder_order(&arm.body, order, seen);
            }
        }
        NormalizedNode::Loop(body) => collect_placeholder_order(body, order, seen),
        NormalizedNode::While { condition, body } => {
            collect_placeholder_order(condition, order, seen);
            collect_placeholder_order(body, order, seen);
        }
        NormalizedNode::ForLoop { pat, iter, body } => {
            collect_placeholder_order(pat, order, seen);
            collect_placeholder_order(iter, order, seen);
            collect_placeholder_order(body, order, seen);
        }
        NormalizedNode::PatTuple(elems)
        | NormalizedNode::PatStruct(elems)
        | NormalizedNode::PatOr(elems)
        | NormalizedNode::PatSlice(elems) => {
            for e in elems {
                collect_placeholder_order(e, order, seen);
            }
        }
        NormalizedNode::PatLiteral(e) => collect_placeholder_order(e, order, seen),
        NormalizedNode::PatReference { pat, .. } => collect_placeholder_order(pat, order, seen),
        NormalizedNode::PatRange { lo, hi } => {
            if let Some(l) = lo {
                collect_placeholder_order(l, order, seen);
            }
            if let Some(h) = hi {
                collect_placeholder_order(h, order, seen);
            }
        }
        NormalizedNode::TypeReference { elem, .. } | NormalizedNode::TypeSlice(elem) => {
            collect_placeholder_order(elem, order, seen);
        }
        NormalizedNode::TypeTuple(elems)
        | NormalizedNode::TypePath(elems)
        | NormalizedNode::TypeImplTrait(elems) => {
            for e in elems {
                collect_placeholder_order(e, order, seen);
            }
        }
        NormalizedNode::TypeArray { elem, len } => {
            collect_placeholder_order(elem, order, seen);
            collect_placeholder_order(len, order, seen);
        }
        NormalizedNode::FieldValue { name, value } => {
            collect_placeholder_order(name, order, seen);
            collect_placeholder_order(value, order, seen);
        }
        NormalizedNode::Range { from, to } => {
            if let Some(f) = from {
                collect_placeholder_order(f, order, seen);
            }
            if let Some(t) = to {
                collect_placeholder_order(t, order, seen);
            }
        }
        NormalizedNode::Path(segments) => {
            for s in segments {
                collect_placeholder_order(s, order, seen);
            }
        }
        NormalizedNode::LetExpr { pat, expr } => {
            collect_placeholder_order(pat, order, seen);
            collect_placeholder_order(expr, order, seen);
        }
        NormalizedNode::MacroCall { args, .. } => {
            for a in args {
                collect_placeholder_order(a, order, seen);
            }
        }
        NormalizedNode::Literal(_)
        | NormalizedNode::Continue
        | NormalizedNode::PatWild
        | NormalizedNode::PatRest
        | NormalizedNode::TypeInfer
        | NormalizedNode::TypeUnit
        | NormalizedNode::TypeNever
        | NormalizedNode::Opaque => {}
    }
}

/// Applies the reindex mapping to a node, returning a new node with remapped indices.
fn apply_reindex(
    node: &NormalizedNode,
    mapping: &HashMap<(PlaceholderKind, usize), usize>,
) -> NormalizedNode {
    match node {
        NormalizedNode::Placeholder(kind, idx) => {
            let new_idx = mapping.get(&(*kind, *idx)).copied().unwrap_or(*idx);
            NormalizedNode::Placeholder(*kind, new_idx)
        }
        NormalizedNode::PatPlaceholder(kind, idx) => {
            let new_idx = mapping.get(&(*kind, *idx)).copied().unwrap_or(*idx);
            NormalizedNode::PatPlaceholder(*kind, new_idx)
        }
        NormalizedNode::TypePlaceholder(kind, idx) => {
            let new_idx = mapping.get(&(*kind, *idx)).copied().unwrap_or(*idx);
            NormalizedNode::TypePlaceholder(*kind, new_idx)
        }
        NormalizedNode::Block(stmts) => {
            NormalizedNode::Block(stmts.iter().map(|s| apply_reindex(s, mapping)).collect())
        }
        NormalizedNode::LetBinding { pattern, ty, init } => NormalizedNode::LetBinding {
            pattern: Box::new(apply_reindex(pattern, mapping)),
            ty: ty.as_ref().map(|t| Box::new(apply_reindex(t, mapping))),
            init: init.as_ref().map(|i| Box::new(apply_reindex(i, mapping))),
        },
        NormalizedNode::BinaryOp { op, left, right } => NormalizedNode::BinaryOp {
            op: op.clone(),
            left: Box::new(apply_reindex(left, mapping)),
            right: Box::new(apply_reindex(right, mapping)),
        },
        NormalizedNode::UnaryOp { op, operand } => NormalizedNode::UnaryOp {
            op: op.clone(),
            operand: Box::new(apply_reindex(operand, mapping)),
        },
        NormalizedNode::Call { func, args } => NormalizedNode::Call {
            func: Box::new(apply_reindex(func, mapping)),
            args: args.iter().map(|a| apply_reindex(a, mapping)).collect(),
        },
        NormalizedNode::MethodCall {
            receiver,
            method,
            args,
        } => NormalizedNode::MethodCall {
            receiver: Box::new(apply_reindex(receiver, mapping)),
            method: Box::new(apply_reindex(method, mapping)),
            args: args.iter().map(|a| apply_reindex(a, mapping)).collect(),
        },
        NormalizedNode::FieldAccess { base, field } => NormalizedNode::FieldAccess {
            base: Box::new(apply_reindex(base, mapping)),
            field: Box::new(apply_reindex(field, mapping)),
        },
        NormalizedNode::Index { base, index } => NormalizedNode::Index {
            base: Box::new(apply_reindex(base, mapping)),
            index: Box::new(apply_reindex(index, mapping)),
        },
        NormalizedNode::Closure { params, body } => NormalizedNode::Closure {
            params: params.iter().map(|p| apply_reindex(p, mapping)).collect(),
            body: Box::new(apply_reindex(body, mapping)),
        },
        NormalizedNode::FnSignature {
            params,
            return_type,
        } => NormalizedNode::FnSignature {
            params: params.iter().map(|p| apply_reindex(p, mapping)).collect(),
            return_type: return_type
                .as_ref()
                .map(|r| Box::new(apply_reindex(r, mapping))),
        },
        NormalizedNode::Return(e) => {
            NormalizedNode::Return(e.as_ref().map(|e| Box::new(apply_reindex(e, mapping))))
        }
        NormalizedNode::Break(e) => {
            NormalizedNode::Break(e.as_ref().map(|e| Box::new(apply_reindex(e, mapping))))
        }
        NormalizedNode::Assign { left, right } => NormalizedNode::Assign {
            left: Box::new(apply_reindex(left, mapping)),
            right: Box::new(apply_reindex(right, mapping)),
        },
        NormalizedNode::Reference { mutable, expr } => NormalizedNode::Reference {
            mutable: *mutable,
            expr: Box::new(apply_reindex(expr, mapping)),
        },
        NormalizedNode::Tuple(elems) => {
            NormalizedNode::Tuple(elems.iter().map(|e| apply_reindex(e, mapping)).collect())
        }
        NormalizedNode::Array(elems) => {
            NormalizedNode::Array(elems.iter().map(|e| apply_reindex(e, mapping)).collect())
        }
        NormalizedNode::Repeat { elem, len } => NormalizedNode::Repeat {
            elem: Box::new(apply_reindex(elem, mapping)),
            len: Box::new(apply_reindex(len, mapping)),
        },
        NormalizedNode::Cast { expr, ty } => NormalizedNode::Cast {
            expr: Box::new(apply_reindex(expr, mapping)),
            ty: Box::new(apply_reindex(ty, mapping)),
        },
        NormalizedNode::StructInit { fields, rest } => NormalizedNode::StructInit {
            fields: fields.iter().map(|f| apply_reindex(f, mapping)).collect(),
            rest: rest.as_ref().map(|r| Box::new(apply_reindex(r, mapping))),
        },
        NormalizedNode::Await(e) => NormalizedNode::Await(Box::new(apply_reindex(e, mapping))),
        NormalizedNode::Try(e) => NormalizedNode::Try(Box::new(apply_reindex(e, mapping))),
        NormalizedNode::Paren(e) => NormalizedNode::Paren(Box::new(apply_reindex(e, mapping))),
        NormalizedNode::Semi(e) => NormalizedNode::Semi(Box::new(apply_reindex(e, mapping))),
        NormalizedNode::If {
            condition,
            then_branch,
            else_branch,
        } => NormalizedNode::If {
            condition: Box::new(apply_reindex(condition, mapping)),
            then_branch: Box::new(apply_reindex(then_branch, mapping)),
            else_branch: else_branch
                .as_ref()
                .map(|e| Box::new(apply_reindex(e, mapping))),
        },
        NormalizedNode::Match { expr, arms } => NormalizedNode::Match {
            expr: Box::new(apply_reindex(expr, mapping)),
            arms: arms
                .iter()
                .map(|arm| MatchArm {
                    pattern: apply_reindex(&arm.pattern, mapping),
                    guard: arm
                        .guard
                        .as_ref()
                        .map(|g| Box::new(apply_reindex(g, mapping))),
                    body: Box::new(apply_reindex(&arm.body, mapping)),
                })
                .collect(),
        },
        NormalizedNode::Loop(body) => NormalizedNode::Loop(Box::new(apply_reindex(body, mapping))),
        NormalizedNode::While { condition, body } => NormalizedNode::While {
            condition: Box::new(apply_reindex(condition, mapping)),
            body: Box::new(apply_reindex(body, mapping)),
        },
        NormalizedNode::ForLoop { pat, iter, body } => NormalizedNode::ForLoop {
            pat: Box::new(apply_reindex(pat, mapping)),
            iter: Box::new(apply_reindex(iter, mapping)),
            body: Box::new(apply_reindex(body, mapping)),
        },
        NormalizedNode::PatTuple(elems) => {
            NormalizedNode::PatTuple(elems.iter().map(|e| apply_reindex(e, mapping)).collect())
        }
        NormalizedNode::PatStruct(elems) => {
            NormalizedNode::PatStruct(elems.iter().map(|e| apply_reindex(e, mapping)).collect())
        }
        NormalizedNode::PatOr(elems) => {
            NormalizedNode::PatOr(elems.iter().map(|e| apply_reindex(e, mapping)).collect())
        }
        NormalizedNode::PatSlice(elems) => {
            NormalizedNode::PatSlice(elems.iter().map(|e| apply_reindex(e, mapping)).collect())
        }
        NormalizedNode::PatLiteral(e) => {
            NormalizedNode::PatLiteral(Box::new(apply_reindex(e, mapping)))
        }
        NormalizedNode::PatReference { mutable, pat } => NormalizedNode::PatReference {
            mutable: *mutable,
            pat: Box::new(apply_reindex(pat, mapping)),
        },
        NormalizedNode::PatRange { lo, hi } => NormalizedNode::PatRange {
            lo: lo.as_ref().map(|l| Box::new(apply_reindex(l, mapping))),
            hi: hi.as_ref().map(|h| Box::new(apply_reindex(h, mapping))),
        },
        NormalizedNode::TypeReference { mutable, elem } => NormalizedNode::TypeReference {
            mutable: *mutable,
            elem: Box::new(apply_reindex(elem, mapping)),
        },
        NormalizedNode::TypeSlice(elem) => {
            NormalizedNode::TypeSlice(Box::new(apply_reindex(elem, mapping)))
        }
        NormalizedNode::TypeTuple(elems) => {
            NormalizedNode::TypeTuple(elems.iter().map(|e| apply_reindex(e, mapping)).collect())
        }
        NormalizedNode::TypePath(elems) => {
            NormalizedNode::TypePath(elems.iter().map(|e| apply_reindex(e, mapping)).collect())
        }
        NormalizedNode::TypeImplTrait(elems) => {
            NormalizedNode::TypeImplTrait(elems.iter().map(|e| apply_reindex(e, mapping)).collect())
        }
        NormalizedNode::TypeArray { elem, len } => NormalizedNode::TypeArray {
            elem: Box::new(apply_reindex(elem, mapping)),
            len: Box::new(apply_reindex(len, mapping)),
        },
        NormalizedNode::FieldValue { name, value } => NormalizedNode::FieldValue {
            name: Box::new(apply_reindex(name, mapping)),
            value: Box::new(apply_reindex(value, mapping)),
        },
        NormalizedNode::Range { from, to } => NormalizedNode::Range {
            from: from.as_ref().map(|f| Box::new(apply_reindex(f, mapping))),
            to: to.as_ref().map(|t| Box::new(apply_reindex(t, mapping))),
        },
        NormalizedNode::Path(segments) => {
            NormalizedNode::Path(segments.iter().map(|s| apply_reindex(s, mapping)).collect())
        }
        NormalizedNode::LetExpr { pat, expr } => NormalizedNode::LetExpr {
            pat: Box::new(apply_reindex(pat, mapping)),
            expr: Box::new(apply_reindex(expr, mapping)),
        },
        NormalizedNode::MacroCall { name, args } => NormalizedNode::MacroCall {
            name: name.clone(),
            args: args.iter().map(|a| apply_reindex(a, mapping)).collect(),
        },
        // Leaf nodes that contain no placeholders — clone as-is
        NormalizedNode::Literal(_)
        | NormalizedNode::Continue
        | NormalizedNode::PatWild
        | NormalizedNode::PatRest
        | NormalizedNode::TypeInfer
        | NormalizedNode::TypeUnit
        | NormalizedNode::TypeNever
        | NormalizedNode::Opaque => node.clone(),
    }
}

/// Re-index all placeholders in a sub-tree so that indices start from 0
/// per kind, assigned by first-occurrence depth-first order.
/// This allows comparing sub-trees extracted from different function contexts.
pub fn reindex_placeholders(node: &NormalizedNode) -> NormalizedNode {
    let mut order = Vec::new();
    let mut seen = std::collections::HashSet::new();
    collect_placeholder_order(node, &mut order, &mut seen);

    // Build mapping: (kind, old_index) → new sequential index per kind
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
pub fn count_nodes(node: &NormalizedNode) -> usize {
    match node {
        NormalizedNode::Block(stmts) => 1 + stmts.iter().map(count_nodes).sum::<usize>(),
        NormalizedNode::LetBinding { pattern, ty, init } => {
            1 + count_nodes(pattern)
                + ty.as_ref().map_or(0, |t| count_nodes(t))
                + init.as_ref().map_or(0, |i| count_nodes(i))
        }
        NormalizedNode::Literal(_) => 1,
        NormalizedNode::Placeholder(_, _) => 1,
        NormalizedNode::BinaryOp { left, right, .. } => 1 + count_nodes(left) + count_nodes(right),
        NormalizedNode::UnaryOp { operand, .. } => 1 + count_nodes(operand),
        NormalizedNode::Call { func, args } => {
            1 + count_nodes(func) + args.iter().map(count_nodes).sum::<usize>()
        }
        NormalizedNode::MethodCall {
            receiver,
            method,
            args,
        } => {
            1 + count_nodes(receiver)
                + count_nodes(method)
                + args.iter().map(count_nodes).sum::<usize>()
        }
        NormalizedNode::FieldAccess { base, field } => 1 + count_nodes(base) + count_nodes(field),
        NormalizedNode::Index { base, index } => 1 + count_nodes(base) + count_nodes(index),
        NormalizedNode::Closure { params, body } => {
            1 + params.iter().map(count_nodes).sum::<usize>() + count_nodes(body)
        }
        NormalizedNode::FnSignature {
            params,
            return_type,
        } => {
            1 + params.iter().map(count_nodes).sum::<usize>()
                + return_type.as_ref().map_or(0, |t| count_nodes(t))
        }
        NormalizedNode::Return(e) => 1 + e.as_ref().map_or(0, |e| count_nodes(e)),
        NormalizedNode::Break(e) => 1 + e.as_ref().map_or(0, |e| count_nodes(e)),
        NormalizedNode::Continue => 1,
        NormalizedNode::Assign { left, right } => 1 + count_nodes(left) + count_nodes(right),
        NormalizedNode::Reference { expr, .. } => 1 + count_nodes(expr),
        NormalizedNode::Tuple(elems) | NormalizedNode::Array(elems) => {
            1 + elems.iter().map(count_nodes).sum::<usize>()
        }
        NormalizedNode::Repeat { elem, len } => 1 + count_nodes(elem) + count_nodes(len),
        NormalizedNode::Cast { expr, ty } => 1 + count_nodes(expr) + count_nodes(ty),
        NormalizedNode::StructInit { fields, rest } => {
            1 + fields.iter().map(count_nodes).sum::<usize>()
                + rest.as_ref().map_or(0, |r| count_nodes(r))
        }
        NormalizedNode::Await(e) | NormalizedNode::Try(e) => 1 + count_nodes(e),
        NormalizedNode::If {
            condition,
            then_branch,
            else_branch,
        } => {
            1 + count_nodes(condition)
                + count_nodes(then_branch)
                + else_branch.as_ref().map_or(0, |e| count_nodes(e))
        }
        NormalizedNode::Match { expr, arms } => {
            1 + count_nodes(expr)
                + arms
                    .iter()
                    .map(|a| {
                        count_nodes(&a.pattern)
                            + a.guard.as_ref().map_or(0, |g| count_nodes(g))
                            + count_nodes(&a.body)
                    })
                    .sum::<usize>()
        }
        NormalizedNode::Loop(body) => 1 + count_nodes(body),
        NormalizedNode::While { condition, body } => 1 + count_nodes(condition) + count_nodes(body),
        NormalizedNode::ForLoop { pat, iter, body } => {
            1 + count_nodes(pat) + count_nodes(iter) + count_nodes(body)
        }
        NormalizedNode::PatWild | NormalizedNode::PatRest => 1,
        NormalizedNode::PatPlaceholder(_, _) => 1,
        NormalizedNode::PatTuple(elems)
        | NormalizedNode::PatStruct(elems)
        | NormalizedNode::PatOr(elems)
        | NormalizedNode::PatSlice(elems) => 1 + elems.iter().map(count_nodes).sum::<usize>(),
        NormalizedNode::PatLiteral(e) => 1 + count_nodes(e),
        NormalizedNode::PatReference { pat, .. } => 1 + count_nodes(pat),
        NormalizedNode::PatRange { lo, hi } => {
            1 + lo.as_ref().map_or(0, |e| count_nodes(e))
                + hi.as_ref().map_or(0, |e| count_nodes(e))
        }
        NormalizedNode::TypePlaceholder(_, _)
        | NormalizedNode::TypeInfer
        | NormalizedNode::TypeUnit
        | NormalizedNode::TypeNever => 1,
        NormalizedNode::TypeReference { elem, .. } | NormalizedNode::TypeSlice(elem) => {
            1 + count_nodes(elem)
        }
        NormalizedNode::TypeTuple(elems)
        | NormalizedNode::TypePath(elems)
        | NormalizedNode::TypeImplTrait(elems) => 1 + elems.iter().map(count_nodes).sum::<usize>(),
        NormalizedNode::TypeArray { elem, len } => 1 + count_nodes(elem) + count_nodes(len),
        NormalizedNode::FieldValue { name, value } => 1 + count_nodes(name) + count_nodes(value),
        NormalizedNode::MacroCall { args, .. } => 1 + args.iter().map(count_nodes).sum::<usize>(),
        NormalizedNode::Opaque => 1,
        NormalizedNode::Range { from, to } => {
            1 + from.as_ref().map_or(0, |e| count_nodes(e))
                + to.as_ref().map_or(0, |e| count_nodes(e))
        }
        NormalizedNode::Path(segments) => 1 + segments.iter().map(count_nodes).sum::<usize>(),
        NormalizedNode::LetExpr { pat, expr } => 1 + count_nodes(pat) + count_nodes(expr),
        NormalizedNode::Paren(e) => 1 + count_nodes(e),
        NormalizedNode::Semi(e) => 1 + count_nodes(e),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn reindex_remaps_from_zero() {
        let node = NormalizedNode::BinaryOp {
            op: BinOpKind::Add,
            left: Box::new(NormalizedNode::Placeholder(PlaceholderKind::Variable, 5)),
            right: Box::new(NormalizedNode::Placeholder(PlaceholderKind::Variable, 8)),
        };
        let reindexed = reindex_placeholders(&node);
        let expected = NormalizedNode::BinaryOp {
            op: BinOpKind::Add,
            left: Box::new(NormalizedNode::Placeholder(PlaceholderKind::Variable, 0)),
            right: Box::new(NormalizedNode::Placeholder(PlaceholderKind::Variable, 1)),
        };
        assert_eq!(reindexed, expected);
    }

    #[test]
    fn reindex_preserves_same_placeholder_identity() {
        let node = NormalizedNode::BinaryOp {
            op: BinOpKind::Add,
            left: Box::new(NormalizedNode::Placeholder(PlaceholderKind::Variable, 3)),
            right: Box::new(NormalizedNode::Placeholder(PlaceholderKind::Variable, 3)),
        };
        let reindexed = reindex_placeholders(&node);
        let expected = NormalizedNode::BinaryOp {
            op: BinOpKind::Add,
            left: Box::new(NormalizedNode::Placeholder(PlaceholderKind::Variable, 0)),
            right: Box::new(NormalizedNode::Placeholder(PlaceholderKind::Variable, 0)),
        };
        assert_eq!(reindexed, expected);
    }

    #[test]
    fn reindex_makes_equivalent_subtrees_equal() {
        let subtree1 = NormalizedNode::Block(vec![
            NormalizedNode::LetBinding {
                pattern: Box::new(NormalizedNode::PatPlaceholder(PlaceholderKind::Variable, 2)),
                ty: None,
                init: Some(Box::new(NormalizedNode::BinaryOp {
                    op: BinOpKind::Add,
                    left: Box::new(NormalizedNode::Placeholder(PlaceholderKind::Variable, 0)),
                    right: Box::new(NormalizedNode::Literal(LiteralKind::Int)),
                })),
            },
            NormalizedNode::Placeholder(PlaceholderKind::Variable, 2),
        ]);
        let subtree2 = NormalizedNode::Block(vec![
            NormalizedNode::LetBinding {
                pattern: Box::new(NormalizedNode::PatPlaceholder(PlaceholderKind::Variable, 7)),
                ty: None,
                init: Some(Box::new(NormalizedNode::BinaryOp {
                    op: BinOpKind::Add,
                    left: Box::new(NormalizedNode::Placeholder(PlaceholderKind::Variable, 5)),
                    right: Box::new(NormalizedNode::Literal(LiteralKind::Int)),
                })),
            },
            NormalizedNode::Placeholder(PlaceholderKind::Variable, 7),
        ]);

        assert_ne!(subtree1, subtree2);
        assert_eq!(
            reindex_placeholders(&subtree1),
            reindex_placeholders(&subtree2)
        );
    }

    #[test]
    fn reindex_handles_multiple_placeholder_kinds() {
        let node = NormalizedNode::Call {
            func: Box::new(NormalizedNode::Placeholder(PlaceholderKind::Function, 3)),
            args: vec![
                NormalizedNode::Placeholder(PlaceholderKind::Variable, 5),
                NormalizedNode::Cast {
                    expr: Box::new(NormalizedNode::Placeholder(PlaceholderKind::Variable, 5)),
                    ty: Box::new(NormalizedNode::TypePlaceholder(PlaceholderKind::Type, 2)),
                },
            ],
        };
        let reindexed = reindex_placeholders(&node);
        let expected = NormalizedNode::Call {
            func: Box::new(NormalizedNode::Placeholder(PlaceholderKind::Function, 0)),
            args: vec![
                NormalizedNode::Placeholder(PlaceholderKind::Variable, 0),
                NormalizedNode::Cast {
                    expr: Box::new(NormalizedNode::Placeholder(PlaceholderKind::Variable, 0)),
                    ty: Box::new(NormalizedNode::TypePlaceholder(PlaceholderKind::Type, 0)),
                },
            ],
        };
        assert_eq!(reindexed, expected);
    }
}
