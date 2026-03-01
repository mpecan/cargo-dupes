# CLAUDE.md — cargo-dupes

## Project Overview

`cargo-dupes` / `code-dupes` detects duplicate and near-duplicate code blocks across multiple languages. It works by normalizing AST into a custom representation where identifiers are replaced with positional placeholders and literal values are erased, then uses fingerprinting (hashing) for exact duplicate detection and Dice coefficient tree comparison for near-duplicate detection.

**Edition:** 2024 (Rust 1.93+). Uses let chains natively.

## Workspace Structure

This is a Cargo workspace with six crates:

```
Cargo.toml                        # Workspace root
dupes-core/                       # Language-agnostic core library (no syn dependency)
  src/
    lib.rs                        # pub mod declarations, AnalysisResult, analyze(), analyze_units()
    analyzer.rs                   # LanguageAnalyzer trait
    node.rs                       # NormalizedNode enum (~30 variants), NormalizationContext, count_nodes, reindex
    fingerprint.rs                # Fingerprint (u64 hash wrapper, hex serialization)
    similarity.rs                 # Dice coefficient tree comparison
    grouper.rs                    # Exact grouping (HashMap) + near-duplicate (union-find)
    extractor.rs                  # Sub-function duplicate extraction
    code_unit.rs                  # CodeUnit, CodeUnitKind (data types only)
    config.rs                     # Config + AnalysisConfig, loading from dupes.toml / Cargo.toml metadata
    cli.rs                        # Shared CLI types (CliError, Command, CliOverrides, run_analysis)
    ignore.rs                     # .dupes-ignore.toml management
    scanner.rs                    # File discovery via walkdir (configurable extensions)
    error.rs                      # Error types via thiserror
    output/
      mod.rs                      # Reporter trait
      text.rs                     # TextReporter
      json.rs                     # JsonReporter
dupes-treesitter/                 # Tree-sitter normalization bridge (depends on dupes-core + tree-sitter)
  src/
    lib.rs                        # pub mod declarations
    analyzer.rs                   # TreeSitterAnalyzer implementing LanguageAnalyzer
    mapping.rs                    # NodeMapping table-driven config (builder pattern)
    normalizer.rs                 # normalize_ts_node(): tree-sitter CST → NormalizedNode
    extractor.rs                  # extract_code_units(): query-based CodeUnit extraction
  tests/
    python_integration.rs         # Test-only Python mapping for normalizer unit tests
dupes-python/                     # Python language analyzer (depends on dupes-treesitter + dupes-core)
  src/
    lib.rs                        # PythonAnalyzer wrapping TreeSitterAnalyzer, python_mapping()
  tests/
    integration.rs                # Python analyzer integration tests
    core_with_python_tests.rs     # Core pipeline tests using Python analyzer
dupes-rust/                       # Rust language analyzer (depends on syn + dupes-core)
  src/
    lib.rs                        # RustAnalyzer implementing LanguageAnalyzer
    normalizer.rs                 # syn AST → NormalizedNode conversion functions
    parser.rs                     # CodeUnit extraction via syn::visit::Visit
  tests/
    core_with_syn_tests.rs        # Core module tests that need syn to construct test data
cargo-dupes/                      # Cargo subcommand CLI (Rust only, depends on dupes-rust + dupes-core)
  src/
    main.rs                       # clap CLI with subcommands (cargo dupes ...)
  tests/
    cli_integration.rs            # CLI integration tests (assert_cmd + predicates)
    fixtures/                     # 6 minimal Rust projects for integration tests
code-dupes/                       # Multi-language CLI (depends on dupes-rust + dupes-python + dupes-core)
  src/
    main.rs                       # clap CLI with --language flag and auto-detection
  tests/
    language.rs                   # Language detection + Python CLI integration tests
    fixtures/                     # Python fixture directories (python_dupes, python_test_code, python_no_dupes)
```

## Build & Test

```sh
cargo build                       # Build all workspace members
cargo test                        # Run all ~340 tests
cargo test -p dupes-core          # 69 core unit tests
cargo test -p dupes-treesitter    # 44 tree-sitter tests (28 unit + 16 integration)
cargo test -p dupes-python        # 33 Python analyzer tests (22 integration + 9 core pipeline + 2 suites)
cargo test -p dupes-rust --lib    # 64 normalizer + parser + RustAnalyzer unit tests
cargo test -p dupes-rust --test core_with_syn_tests  # 45 syn-dependent core tests
cargo test -p cargo-dupes --tests # 35 cargo-dupes CLI integration tests
cargo test -p code-dupes --tests  # 50 code-dupes CLI integration tests
cargo clippy --workspace          # Lint (must be clean)
cargo fmt --all --check           # Format check
```

Pre-commit hooks (via cargo-husky) enforce `cargo clippy -- -D warnings` and `cargo fmt -- --check`.

## Architecture

### Analysis Pipeline

The CLIs (`cargo-dupes` for Rust, `code-dupes` for multi-language) orchestrate the pipeline:
1. Load config (`dupes_core::config`)
2. Create language analyzer (`RustAnalyzer` or `PythonAnalyzer`)
3. Scan for source files (`dupes_core::scanner` — extensions from analyzer)
4. Analyze: parse + group + compare + filter + stats (`dupes_core::analyze()`)

```
scan_files → analyze(analyzer, files, config) → AnalysisResult
               ├── analyzer.parse_file() per file
               ├── filter test code (optional)
               └── analyze_units()
                     ├── group_exact_duplicates
                     ├── find_near_duplicates
                     ├── extract_sub_units (sub-function detection)
                     ├── filter_ignored
                     └── compute_stats
```

### Module Map — dupes-core

| Module | File | Responsibility |
|--------|------|---------------|
| **analyzer** | `dupes-core/src/analyzer.rs` | `LanguageAnalyzer` trait (`Send + Sync`): `file_extensions()`, `parse_file()`, `is_test_code()`. Analyzers tag test code via `CodeUnit::is_test`; `analyze()` filters. |
| **node** | `dupes-core/src/node.rs` | `NormalizedNode` enum (~30 variants), `LiteralKind` (incl. `Null`), `BinOpKind` (incl. augmented assignments), `UnOpKind`, `NodeKind` (incl. `Yield`), `NormalizationContext`, `count_nodes()`, `reindex_placeholders()`. |
| **fingerprint** | `dupes-core/src/fingerprint.rs` | `Fingerprint` struct wrapping `u64` from `DefaultHasher`. Supports hex serialization. |
| **code_unit** | `dupes-core/src/code_unit.rs` | `CodeUnit` struct (with `is_test` field) and `CodeUnitKind` enum (data types only, no parsing logic). |
| **similarity** | `dupes-core/src/similarity.rs` | Recursive tree comparison using Dice coefficient: `score = (2 * matching) / (nodes_a + nodes_b)`. |
| **grouper** | `dupes-core/src/grouper.rs` | Exact duplicate grouping via `HashMap<Fingerprint, Vec<CodeUnit>>`. Near-duplicate detection with size-bucket pre-filtering and union-find for transitive closure. |
| **extractor** | `dupes-core/src/extractor.rs` | Sub-function duplicate detection: extracts inner blocks from `CodeUnit` bodies. |
| **scanner** | `dupes-core/src/scanner.rs` | File discovery via `walkdir`. Skips `target/` and hidden directories. Respects exclude patterns. Configurable file extensions. |
| **config** | `dupes-core/src/config.rs` | `Config` loading: `dupes.toml` > `Cargo.toml [package.metadata.dupes]` > defaults. `AnalysisConfig` (min_nodes, min_lines) for parsing-relevant subset. CLI overrides applied on top. |
| **cli** | `dupes-core/src/cli.rs` | Shared CLI types: `CliError` (incl. `AmbiguousLanguage`), `Command`, `CliOverrides`, `OutputFormat`, `run_analysis()`, command implementations (`cmd_report`, `cmd_check`, etc.). |
| **ignore** | `dupes-core/src/ignore.rs` | TOML-based ignore file (`.dupes-ignore.toml`). Add/remove/filter by fingerprint. Stale entry cleanup. |
| **error** | `dupes-core/src/error.rs` | Error types via `thiserror`. |
| **output** | `dupes-core/src/output/` | `Reporter` trait with `TextReporter` and `JsonReporter`. |

### Module Map — dupes-treesitter

| Module | File | Responsibility |
|--------|------|---------------|
| **TreeSitterAnalyzer** | `dupes-treesitter/src/analyzer.rs` | Generic `LanguageAnalyzer` impl using tree-sitter. Configured with `NodeMapping`, query, test detector, extensions. |
| **mapping** | `dupes-treesitter/src/mapping.rs` | `NodeMapping` — table-driven config for tree-sitter normalization. Builder pattern with `identifiers()`, `literals()`, `binary_ops()`, `unary_ops()`, `node_kinds()`, etc. |
| **normalizer** | `dupes-treesitter/src/normalizer.rs` | `normalize_ts_node()`: tree-sitter CST → `NormalizedNode`. Handles identifiers, literals, binary/unary ops, node kinds, structural nodes. |
| **extractor** | `dupes-treesitter/src/extractor.rs` | `extract_code_units()`: query-based `CodeUnit` extraction from tree-sitter parse tree. |

### Module Map — dupes-python

| Module | File | Responsibility |
|--------|------|---------------|
| **PythonAnalyzer** | `dupes-python/src/lib.rs` | Thin wrapper around `TreeSitterAnalyzer` with Python-specific `NodeMapping`, tree-sitter query, and `test_` prefix test detection. `python_mapping()` is public for reuse. |

### Module Map — dupes-rust

| Module | File | Responsibility |
|--------|------|---------------|
| **RustAnalyzer** | `dupes-rust/src/lib.rs` | Implements `LanguageAnalyzer` for Rust. Delegates to `parser::parse_source()`. |
| **normalizer** | `dupes-rust/src/normalizer.rs` | syn AST → `NormalizedNode` conversion. All `normalize_*` functions for expressions, statements, patterns, types, signatures. |
| **parser** | `dupes-rust/src/parser.rs` | `CodeUnitExtractor` using `syn::visit::Visit`. Visits `ItemFn`, `ItemImpl`, `ItemMod`, `ExprClosure`. `parse_source()` takes `&str`, `parse_file()` reads from disk. |

### Module Map — cargo-dupes

| Module | File | Responsibility |
|--------|------|---------------|
| **CLI** | `cargo-dupes/src/main.rs` | `clap` derive CLI (Rust only). Subcommands: `stats`, `report` (default), `check`, `ignore`, `ignored`, `cleanup`. Uses `RustAnalyzer` + `dupes_core::cli::run_analysis()`. |

### Module Map — code-dupes

| Module | File | Responsibility |
|--------|------|---------------|
| **CLI** | `code-dupes/src/main.rs` | `clap` derive CLI (multi-language). `--language` flag with auto-detection from file extensions. Ambiguous detection (mixed languages) returns an error. Uses `dupes_core::cli::run_analysis()`. |

### Key Design: NormalizedNode

The `NormalizedNode` enum provides a language-agnostic normalized AST:
- Replaces all identifiers with `Placeholder(kind, positional_index)` — assigned by first-occurrence order
- Preserves literal *kind* but erases *values* (`42` and `99` both become `Literal(Int)`)
- Preserves control flow structure exactly (if/match/loop/for)
- Maps language-specific constructs to shared `NodeKind` variants

This enables `derive(Hash)` for fingerprinting and recursive tree comparison for similarity scoring.

Two normalization backends exist:
- **syn-based** (dupes-rust): Direct Rust AST → `NormalizedNode` via syn's visitor pattern
- **tree-sitter-based** (dupes-treesitter): Generic CST → `NormalizedNode` via table-driven `NodeMapping`

### Key Types

- `LanguageAnalyzer` — Trait for language-specific parsing. Provides `file_extensions()`, `parse_file()`, `is_test_code()`.
- `AnalysisConfig` — Parsing-relevant config subset: `min_nodes`, `min_lines`.
- `CodeUnit` — A function, method, closure, or impl block extracted from source. Contains normalized signature + body, fingerprint, file location, line numbers, `is_test` flag.
- `DuplicateGroup` — A group of code units with the same fingerprint (exact) or above the similarity threshold (near). `fingerprint` is always set (non-optional).
- `DuplicationStats` — Statistics including group/unit counts, duplicated line counts (exact and near), total lines, and percentage helpers.
- `Config` — All analysis parameters (min_nodes, min_lines, similarity_threshold, excludes, exclude_tests, CI thresholds including percentage-based).
- `NodeMapping` — Table-driven configuration for tree-sitter normalization: identifier kinds, literal kinds, binary/unary op maps, node kinds, structural kinds.

## CLI Subcommands

Both `cargo-dupes` (Rust only) and `code-dupes` (multi-language) share the same subcommands:

- **`report`** (default) — Full report: stats + exact groups + near groups
- **`stats`** — Summary statistics only
- **`check`** — CI mode. Exits 1 if `--max-exact`, `--max-near`, `--max-exact-percent`, or `--max-near-percent` thresholds exceeded. Exits 0 on pass. Exits 2 on errors.
- **`ignore <fingerprint>`** — Add fingerprint to `.dupes-ignore.toml`
- **`ignored`** — List all ignored fingerprints
- **`cleanup`** — Remove stale entries from `.dupes-ignore.toml`

`code-dupes` adds `--language` flag (auto-detected from file extensions if omitted).

## Testing

- **dupes-core unit tests** (69) — Colocated in each module (`#[cfg(test)] mod tests`). No syn dependency.
- **dupes-treesitter tests** (44) — 28 unit tests + 16 integration tests (Python normalization).
- **dupes-python tests** (33) — 22 integration tests + 9 core pipeline tests + edge cases.
- **dupes-rust unit tests** (64) — Colocated in `normalizer.rs`, `parser.rs`, and `lib.rs`. Use `syn::parse_str` to construct test data.
- **syn-dependent core tests** (45) — In `dupes-rust/tests/core_with_syn_tests.rs`. Tests for grouper, extractor, similarity, fingerprint that require syn to build realistic test data.
- **cargo-dupes CLI tests** (35) — In `cargo-dupes/tests/cli_integration.rs` using `assert_cmd` + `predicates`.
- **code-dupes CLI tests** (50) — In `code-dupes/tests/` using `assert_cmd` + `predicates`. Covers language detection, Python analysis, ambiguous detection.
- **Fixtures** — `cargo-dupes/tests/fixtures/` (6 Rust projects), `code-dupes/tests/fixtures/` (Python projects: `python_dupes`, `python_test_code`, `python_no_dupes`).

## syn 2 Gotchas

- `Pat::Lit` wraps `ExprLit` which has a `lit: Lit` field (not `expr: Expr`)
- `Member` does not implement `Display`; use a match to extract ident/index strings
- `ExprMatch` arm guards are `Option<(If, Box<Expr>)>` — a tuple, not just `Option<Box<Expr>>`
- `true`/`false` are parsed as path expressions (become `Placeholder`), not as `Lit::Bool`

## tree-sitter Gotchas

- tree-sitter 0.25 `QueryMatches` uses `StreamingIterator` not `Iterator`; import via `tree_sitter::StreamingIterator`
- Use `while let Some(m) = matches.next()` instead of `for m in matches`
- Python `for_statement` uses `left`/`right` fields (not `pattern`/`iterable`)
- Python `comparison_operator` lacks `left`/`right` field names — uses positional children instead

## Platform Notes

- macOS `TempDir` paths may have components that look like hidden directories; the scanner skips the hidden-directory filter for the root path to avoid false filtering in tests.
