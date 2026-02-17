use std::io::{self, Write};
use std::path::{Path, PathBuf};

use crate::AnalysisResult;
use crate::analyzer::LanguageAnalyzer;
use crate::config::Config;
use crate::fingerprint::Fingerprint;
use crate::ignore::{self, IgnoreEntry};
use crate::output::Reporter;
use crate::output::json::JsonReporter;
use crate::output::text::TextReporter;

// ---------------------------------------------------------------------------
// Error types
// ---------------------------------------------------------------------------

/// Errors returned by CLI command functions.
#[derive(Debug)]
pub enum CliError {
    /// An I/O error (exit code 2).
    Io(io::Error),
    /// No source files found (exit code 2).
    NoSourceFiles(PathBuf),
    /// No recognized source files for language auto-detection (exit code 2).
    NoRecognizedFiles,
    /// Analysis pipeline failed (exit code 2).
    Analysis(crate::error::Error),
    /// Invalid fingerprint string (exit code 2).
    InvalidFingerprint(String),
    /// Check thresholds exceeded (exit code 1).
    CheckFailed,
}

impl CliError {
    /// Map to an appropriate process exit code.
    #[must_use]
    pub const fn exit_code(&self) -> i32 {
        match self {
            Self::CheckFailed => 1,
            Self::Io(_)
            | Self::NoSourceFiles(_)
            | Self::NoRecognizedFiles
            | Self::Analysis(_)
            | Self::InvalidFingerprint(_) => 2,
        }
    }
}

impl std::fmt::Display for CliError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Io(e) => write!(f, "{e}"),
            Self::NoSourceFiles(path) => {
                write!(f, "No source files found in {}", path.display())
            }
            Self::NoRecognizedFiles => {
                write!(
                    f,
                    "No recognized source files found. Use --language to specify the language."
                )
            }
            Self::Analysis(e) => write!(f, "{e}"),
            Self::InvalidFingerprint(fp) => write!(f, "Invalid fingerprint: {fp}"),
            Self::CheckFailed => write!(f, "Check failed"),
        }
    }
}

impl std::error::Error for CliError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            Self::Io(e) => Some(e),
            Self::Analysis(e) => Some(e),
            _ => None,
        }
    }
}

impl From<io::Error> for CliError {
    fn from(e: io::Error) -> Self {
        Self::Io(e)
    }
}

impl From<crate::error::Error> for CliError {
    fn from(e: crate::error::Error) -> Self {
        Self::Analysis(e)
    }
}

/// Result type for CLI operations.
pub type CliResult<T = ()> = Result<T, CliError>;

// ---------------------------------------------------------------------------
// Shared CLI types
// ---------------------------------------------------------------------------

/// Output format for CLI reports.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
#[cfg_attr(feature = "cli", derive(clap::ValueEnum))]
pub enum OutputFormat {
    #[default]
    Text,
    Json,
}

/// CLI subcommands shared between `cargo-dupes` and `code-dupes`.
#[derive(Debug, Clone)]
#[cfg_attr(feature = "cli", derive(clap::Subcommand))]
pub enum Command {
    /// Show duplication statistics only.
    Stats,
    /// Show full duplication report (default).
    Report,
    /// Check for duplicates and exit with non-zero if thresholds exceeded.
    Check {
        /// Maximum allowed exact duplicate groups (exit 1 if exceeded).
        #[cfg_attr(feature = "cli", arg(long))]
        max_exact: Option<usize>,
        /// Maximum allowed near duplicate groups (exit 1 if exceeded).
        #[cfg_attr(feature = "cli", arg(long))]
        max_near: Option<usize>,
        /// Maximum allowed exact duplicate percentage (exit 1 if exceeded).
        #[cfg_attr(feature = "cli", arg(long))]
        max_exact_percent: Option<f64>,
        /// Maximum allowed near duplicate percentage (exit 1 if exceeded).
        #[cfg_attr(feature = "cli", arg(long))]
        max_near_percent: Option<f64>,
    },
    /// Add a fingerprint to the ignore list.
    Ignore {
        /// The fingerprint to ignore (hex string).
        fingerprint: String,
        /// Reason for ignoring.
        #[cfg_attr(feature = "cli", arg(long))]
        reason: Option<String>,
    },
    /// List all ignored fingerprints.
    Ignored,
    /// Remove stale entries from the ignore file.
    Cleanup {
        /// Only list stale entries without removing them.
        #[cfg_attr(feature = "cli", arg(long))]
        dry_run: bool,
    },
}

/// Thresholds for the `check` subcommand.
#[derive(Debug, Clone, Default)]
pub struct CheckThresholds {
    pub max_exact: Option<usize>,
    pub max_near: Option<usize>,
    pub max_exact_percent: Option<f64>,
    pub max_near_percent: Option<f64>,
}

/// Optional CLI overrides applied on top of file-based config.
#[derive(Debug, Clone, Default)]
pub struct CliOverrides {
    pub min_nodes: Option<usize>,
    pub min_lines: Option<usize>,
    pub threshold: Option<f64>,
    pub exclude: Vec<String>,
    pub exclude_tests: Option<bool>,
    pub sub_function: Option<bool>,
    pub min_sub_nodes: Option<usize>,
}

/// Result of [`run_analysis`].
pub struct AnalysisOutput {
    pub config: Config,
    pub result: AnalysisResult,
    pub reporter: Box<dyn Reporter>,
}

// ---------------------------------------------------------------------------
// Config helpers
// ---------------------------------------------------------------------------

/// Apply CLI overrides to a loaded `Config`.
///
/// CLI `--exclude` patterns are *appended* to config-file excludes (not replaced).
pub fn apply_overrides(config: &mut Config, overrides: &CliOverrides) {
    if let Some(min_nodes) = overrides.min_nodes {
        config.min_nodes = min_nodes;
    }
    if let Some(min_lines) = overrides.min_lines {
        config.min_lines = min_lines;
    }
    if let Some(threshold) = overrides.threshold {
        config.similarity_threshold = threshold;
    }
    if !overrides.exclude.is_empty() {
        config.exclude.extend(overrides.exclude.iter().cloned());
    }
    if let Some(v) = overrides.exclude_tests {
        config.exclude_tests = v;
    }
    if let Some(v) = overrides.sub_function {
        config.sub_function = v;
    }
    if let Some(min_sub_nodes) = overrides.min_sub_nodes {
        config.min_sub_nodes = min_sub_nodes;
    }
}

/// Create a reporter for the given output format.
#[must_use]
pub fn create_reporter(format: OutputFormat, root: Option<&Path>) -> Box<dyn Reporter> {
    match format {
        OutputFormat::Text => Box::new(TextReporter::new(root.map(Path::to_path_buf))),
        OutputFormat::Json => Box::new(JsonReporter::new(root.map(Path::to_path_buf))),
    }
}

// ---------------------------------------------------------------------------
// Analysis
// ---------------------------------------------------------------------------

/// Scan files, run the analysis pipeline, and return the output.
///
/// Warnings are stored in [`AnalysisOutput::result`] but **not** printed;
/// the caller is responsible for writing them to stderr.
pub fn run_analysis(
    analyzer: &dyn LanguageAnalyzer,
    root: &Path,
    format: OutputFormat,
    overrides: &CliOverrides,
) -> CliResult<AnalysisOutput> {
    let mut config = Config::load(root);
    apply_overrides(&mut config, overrides);

    let scan_config = crate::scanner::ScanConfig::new(config.root.clone())
        .with_excludes(config.exclude.clone())
        .with_extensions(
            analyzer
                .file_extensions()
                .iter()
                .map(std::string::ToString::to_string)
                .collect(),
        );
    let files = crate::scanner::scan_files(&scan_config);

    if files.is_empty() {
        return Err(CliError::NoSourceFiles(config.root));
    }

    let result = crate::analyze(analyzer, &files, &config)?;
    let reporter = create_reporter(format, Some(root));

    Ok(AnalysisOutput {
        config,
        result,
        reporter,
    })
}

// ---------------------------------------------------------------------------
// Command implementations
// ---------------------------------------------------------------------------

/// Show duplication statistics only.
pub fn cmd_stats(
    result: &AnalysisResult,
    reporter: &dyn Reporter,
    writer: &mut impl Write,
) -> CliResult {
    reporter.report_stats(&result.stats, writer)?;
    Ok(())
}

/// Show a full duplication report (stats + groups).
pub fn cmd_report(
    result: &AnalysisResult,
    reporter: &dyn Reporter,
    writer: &mut impl Write,
) -> CliResult {
    reporter.report_stats(&result.stats, writer)?;
    writeln!(writer)?;
    reporter.report_exact(&result.exact_groups, writer)?;
    if !result.near_groups.is_empty() {
        reporter.report_near(&result.near_groups, writer)?;
    }
    if !result.sub_exact_groups.is_empty() {
        reporter.report_sub_exact(&result.sub_exact_groups, writer)?;
    }
    if !result.sub_near_groups.is_empty() {
        reporter.report_sub_near(&result.sub_near_groups, writer)?;
    }
    Ok(())
}

/// Check thresholds; returns `Err(CliError::CheckFailed)` if any are exceeded.
pub fn cmd_check(
    config: &Config,
    result: &AnalysisResult,
    reporter: &dyn Reporter,
    writer: &mut impl Write,
    thresholds: &CheckThresholds,
) -> CliResult {
    let max_exact = thresholds.max_exact.or(config.max_exact_duplicates);
    let max_near = thresholds.max_near.or(config.max_near_duplicates);
    let max_exact_pct = thresholds.max_exact_percent.or(config.max_exact_percent);
    let max_near_pct = thresholds.max_near_percent.or(config.max_near_percent);

    reporter.report_stats(&result.stats, writer)?;

    let mut failed = false;

    if let Some(threshold) = max_exact
        && result.stats.exact_duplicate_groups > threshold
    {
        writeln!(
            writer,
            "\nCheck FAILED: {} exact duplicate groups (max: {})",
            result.stats.exact_duplicate_groups, threshold
        )?;
        reporter.report_exact(&result.exact_groups, writer)?;
        failed = true;
    }

    if let Some(threshold) = max_near
        && result.stats.near_duplicate_groups > threshold
    {
        writeln!(
            writer,
            "\nCheck FAILED: {} near duplicate groups (max: {})",
            result.stats.near_duplicate_groups, threshold
        )?;
        reporter.report_near(&result.near_groups, writer)?;
        failed = true;
    }

    if let Some(threshold) = max_exact_pct {
        let actual = result.stats.exact_duplicate_percent();
        if actual > threshold {
            writeln!(
                writer,
                "\nCheck FAILED: {actual:.1}% exact duplicate lines (max: {threshold:.1}%)"
            )?;
            reporter.report_exact(&result.exact_groups, writer)?;
            failed = true;
        }
    }

    if let Some(threshold) = max_near_pct {
        let actual = result.stats.near_duplicate_percent();
        if actual > threshold {
            writeln!(
                writer,
                "\nCheck FAILED: {actual:.1}% near duplicate lines (max: {threshold:.1}%)"
            )?;
            reporter.report_near(&result.near_groups, writer)?;
            failed = true;
        }
    }

    if failed {
        Err(CliError::CheckFailed)
    } else {
        writeln!(writer, "\nCheck passed.")?;
        Ok(())
    }
}

/// Add a fingerprint to the ignore list.
pub fn cmd_ignore(
    root: &Path,
    fingerprint: &str,
    reason: Option<String>,
    writer: &mut impl Write,
) -> CliResult {
    let fp = Fingerprint::from_hex(fingerprint)
        .ok_or_else(|| CliError::InvalidFingerprint(fingerprint.to_string()))?;
    let mut ignore_file = ignore::load_ignore_file(root);
    ignore::add_ignore(&mut ignore_file, &fp, reason, vec![]);
    ignore::save_ignore_file(root, &ignore_file)?;
    writeln!(writer, "Added {fingerprint} to ignore list.")?;
    Ok(())
}

/// List all ignored fingerprints.
pub fn cmd_ignored(root: &Path, writer: &mut impl Write) -> CliResult {
    let ignore_file = ignore::load_ignore_file(root);
    if ignore_file.ignore.is_empty() {
        writeln!(writer, "No ignored fingerprints.")?;
    } else {
        writeln!(writer, "Ignored fingerprints:")?;
        for entry in &ignore_file.ignore {
            write_ignore_entry(writer, entry)?;
        }
    }
    Ok(())
}

/// Remove stale entries from the ignore file.
pub fn cmd_cleanup(
    root: &Path,
    result: &AnalysisResult,
    writer: &mut impl Write,
    dry_run: bool,
) -> CliResult {
    let mut ignore_file = ignore::load_ignore_file(root);

    if dry_run {
        let stale = ignore::find_stale_entries(&ignore_file, &result.all_fingerprints);
        if stale.is_empty() {
            writeln!(writer, "No stale entries found.")?;
        } else {
            writeln!(writer, "Stale entries (dry run):")?;
            for entry in &stale {
                write_ignore_entry(writer, entry)?;
            }
            writeln!(writer, "\n{} stale entries would be removed.", stale.len())?;
        }
    } else {
        let removed = ignore::remove_stale_entries(&mut ignore_file, &result.all_fingerprints);
        if removed.is_empty() {
            writeln!(writer, "No stale entries found.")?;
        } else {
            ignore::save_ignore_file(root, &ignore_file)?;
            writeln!(writer, "Removed stale entries:")?;
            for entry in &removed {
                write_ignore_entry(writer, entry)?;
            }
            writeln!(writer, "\nRemoved {} stale entries.", removed.len())?;
        }
    }
    Ok(())
}

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

fn write_ignore_entry(writer: &mut impl Write, entry: &IgnoreEntry) -> io::Result<()> {
    write!(writer, "  {}", entry.fingerprint)?;
    if let Some(reason) = &entry.reason {
        write!(writer, " (reason: {reason})")?;
    }
    if !entry.members.is_empty() {
        write!(writer, " [{}]", entry.members.join(", "))?;
    }
    writeln!(writer)
}
