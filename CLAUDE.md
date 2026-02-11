# CLAUDE.md — cargo-dupes

## Project Overview

`cargo-dupes` is a cargo subcommand that detects duplicate and near-duplicate code blocks in Rust codebases. It works by normalizing Rust AST into a custom representation where identifiers are replaced with positional placeholders and literal values are erased, then uses fingerprinting (hashing) for exact duplicate detection and Dice coefficient tree comparison for near-duplicate detection.

**Edition:** 2024 (Rust 1.85+). Uses let chains natively.

## Build & Test

```sh
cargo build          # Build
cargo test           # Run all 131 tests (112 unit + 19 integration)
cargo clippy         # Lint (must be clean)
cargo fmt --check    # Format check
```

Pre-commit hooks (via cargo-husky) enforce `cargo clippy -- -D warnings` and `cargo fmt -- --check`.

## Architecture

### Analysis Pipeline (`lib.rs::analyze()`)

```
scan_files → parse_files → group_exact_duplicates → find_near_duplicates → filter_ignored → compute_stats
```

### Module Map

| Module | File | Responsibility |
|--------|------|---------------|
| **normalizer** | `src/normalizer.rs` | Core AST normalization. Defines `NormalizedNode` enum (~30 variants), `NormalizationContext` for identifier-to-placeholder mapping, and normalization functions for expressions, statements, patterns, types, and signatures. |
| **fingerprint** | `src/fingerprint.rs` | `Fingerprint` struct wrapping `u64` from `DefaultHasher`. Supports hex serialization. |
| **parser** | `src/parser.rs` | `CodeUnit` extraction using `syn::visit::Visit`. Visits `ItemFn`, `ItemImpl`, `ExprClosure`. Extracts units above `min_node_count` and `min_line_count` thresholds. |
| **scanner** | `src/scanner.rs` | File discovery via `walkdir`. Skips `target/` and hidden directories. Respects exclude patterns. |
| **similarity** | `src/similarity.rs` | Recursive tree comparison using Dice coefficient: `score = (2 * matching) / (nodes_a + nodes_b)`. |
| **grouper** | `src/grouper.rs` | Exact duplicate grouping via `HashMap<Fingerprint, Vec<CodeUnit>>`. Near-duplicate detection with size-bucket pre-filtering and union-find for transitive closure. |
| **output** | `src/output/` | `Reporter` trait with `TextReporter` and `JsonReporter`. |
| **config** | `src/config.rs` | Config loading: `dupes.toml` > `Cargo.toml [package.metadata.dupes]` > defaults. CLI overrides applied on top. |
| **ignore** | `src/ignore.rs` | TOML-based ignore file (`.dupes-ignore.toml`). Add/remove/filter by fingerprint. |
| **error** | `src/error.rs` | Error types via `thiserror`. |
| **CLI** | `src/main.rs` | `clap` derive CLI. Subcommands: `stats`, `report` (default), `check`, `ignore`, `ignored`. |

### Key Design: NormalizedNode

The `NormalizedNode` enum mirrors syn's AST but:
- Replaces all identifiers with `Placeholder(kind, positional_index)` — assigned by first-occurrence order
- Preserves literal *kind* but erases *values* (`42` and `99` both become `Literal(Int)`)
- Preserves control flow structure exactly (if/match/loop/for)
- Maps macro invocations to `Opaque`

This enables `derive(Hash)` for fingerprinting and recursive tree comparison for similarity scoring.

### Key Types

- `CodeUnit` — A function, method, closure, or impl block extracted from source. Contains normalized signature + body, fingerprint, file location, line numbers.
- `DuplicateGroup` — A group of code units with the same fingerprint (exact) or above the similarity threshold (near).
- `DuplicationStats` — Statistics including group/unit counts and duplicated line counts (exact and near).
- `Config` — All analysis parameters (min_nodes, min_lines, similarity_threshold, excludes, CI thresholds).

## CLI Subcommands

- **`report`** (default) — Full report: stats + exact groups + near groups
- **`stats`** — Summary statistics only
- **`check`** — CI mode. Exits 1 if `--max-exact` or `--max-near` thresholds exceeded. Exits 0 on pass. Exits 2 on errors.
- **`ignore <fingerprint>`** — Add fingerprint to `.dupes-ignore.toml`
- **`ignored`** — List all ignored fingerprints

## Testing

- **Unit tests** are colocated in each module (`#[cfg(test)] mod tests`).
- **Integration tests** are in `tests/cli_integration.rs` using `assert_cmd` + `predicates`.
- **Fixtures** are in `tests/fixtures/` with 4 minimal Rust projects: `exact_dupes`, `near_dupes`, `no_dupes`, `mixed`.

## syn 2 Gotchas

- `Pat::Lit` wraps `ExprLit` which has a `lit: Lit` field (not `expr: Expr`)
- `Member` does not implement `Display`; use a match to extract ident/index strings
- `ExprMatch` arm guards are `Option<(If, Box<Expr>)>` — a tuple, not just `Option<Box<Expr>>`

## Platform Notes

- macOS `TempDir` paths may have components that look like hidden directories; the scanner skips the hidden-directory filter for the root path to avoid false filtering in tests.
