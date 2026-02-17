use std::io::Write;
use std::path::{Path, PathBuf};
use std::process;

use clap::{Parser, Subcommand};

use dupes_core::AnalysisResult;
use dupes_core::analyzer::LanguageAnalyzer;
use dupes_core::config::Config;
use dupes_core::fingerprint::Fingerprint;
use dupes_core::ignore;
use dupes_core::output::Reporter;
use dupes_core::output::json::JsonReporter;
use dupes_core::output::text::TextReporter;
use dupes_rust::RustAnalyzer;

#[derive(Parser)]
#[command(
    name = "cargo-dupes",
    version,
    about = "Detect duplicate code in Rust codebases"
)]
struct Cli {
    /// When invoked as `cargo dupes`, cargo passes "dupes" as the first arg.
    #[arg(hide = true, default_value = "")]
    _cargo_subcommand: String,

    #[command(subcommand)]
    command: Option<Command>,

    /// Path to analyze (defaults to current directory).
    #[arg(short, long, global = true)]
    path: Option<PathBuf>,

    /// Minimum AST node count for analysis.
    #[arg(long, global = true)]
    min_nodes: Option<usize>,

    /// Minimum source line count for analysis.
    #[arg(long, global = true)]
    min_lines: Option<usize>,

    /// Similarity threshold (0.0-1.0).
    #[arg(long, global = true)]
    threshold: Option<f64>,

    /// Output format.
    #[arg(long, global = true, default_value = "text")]
    format: OutputFormat,

    /// Exclude patterns (can be repeated).
    #[arg(long, global = true)]
    exclude: Vec<String>,

    /// Exclude test code (#[test] functions and #[cfg(test)] modules).
    #[arg(long, global = true)]
    exclude_tests: bool,

    /// Enable sub-function duplicate detection (if branches, match arms, loop bodies).
    #[arg(long, short = 's', global = true)]
    sub_function: bool,

    /// Minimum AST node count for sub-function units.
    #[arg(long, global = true)]
    min_sub_nodes: Option<usize>,
}

#[derive(Clone, clap::ValueEnum)]
enum OutputFormat {
    Text,
    Json,
}

#[derive(Subcommand)]
enum Command {
    /// Show duplication statistics only.
    Stats,
    /// Show full duplication report (default).
    Report,
    /// Check for duplicates and exit with non-zero if thresholds exceeded.
    Check {
        /// Maximum allowed exact duplicate groups (exit 1 if exceeded).
        #[arg(long)]
        max_exact: Option<usize>,
        /// Maximum allowed near duplicate groups (exit 1 if exceeded).
        #[arg(long)]
        max_near: Option<usize>,
        /// Maximum allowed exact duplicate percentage (exit 1 if exceeded).
        #[arg(long)]
        max_exact_percent: Option<f64>,
        /// Maximum allowed near duplicate percentage (exit 1 if exceeded).
        #[arg(long)]
        max_near_percent: Option<f64>,
    },
    /// Add a fingerprint to the ignore list.
    Ignore {
        /// The fingerprint to ignore (hex string).
        fingerprint: String,
        /// Reason for ignoring.
        #[arg(long)]
        reason: Option<String>,
    },
    /// List all ignored fingerprints.
    Ignored,
    /// Remove stale entries from the ignore file.
    Cleanup {
        /// Only list stale entries without removing them.
        #[arg(long)]
        dry_run: bool,
    },
}

fn cmd_ignore(root: &Path, fingerprint: &str, reason: Option<String>) {
    let Some(fp) = Fingerprint::from_hex(fingerprint) else {
        eprintln!("Error: Invalid fingerprint: {fingerprint}");
        process::exit(2);
    };
    let mut ignore_file = ignore::load_ignore_file(root);
    ignore::add_ignore(&mut ignore_file, &fp, reason, vec![]);
    if let Err(e) = ignore::save_ignore_file(root, &ignore_file) {
        eprintln!("Error saving ignore file: {e}");
        process::exit(2);
    }
    println!("Added {fingerprint} to ignore list.");
}

fn cmd_ignored(root: &Path) {
    let ignore_file = ignore::load_ignore_file(root);
    if ignore_file.ignore.is_empty() {
        println!("No ignored fingerprints.");
    } else {
        println!("Ignored fingerprints:");
        for entry in &ignore_file.ignore {
            print!("  {}", entry.fingerprint);
            if let Some(reason) = &entry.reason {
                print!(" (reason: {reason})");
            }
            if !entry.members.is_empty() {
                print!(" [{}]", entry.members.join(", "));
            }
            println!();
        }
    }
}

fn run_analysis(cli: &Cli, root: &Path) -> (Config, AnalysisResult, Box<dyn Reporter>) {
    let mut config = Config::load(root);

    // Apply CLI overrides
    if let Some(min_nodes) = cli.min_nodes {
        config.min_nodes = min_nodes;
    }
    if let Some(min_lines) = cli.min_lines {
        config.min_lines = min_lines;
    }
    if let Some(threshold) = cli.threshold {
        config.similarity_threshold = threshold;
    }
    if !cli.exclude.is_empty() {
        config.exclude.clone_from(&cli.exclude);
    }
    if cli.exclude_tests {
        config.exclude_tests = true;
    }
    if cli.sub_function {
        config.sub_function = true;
    }
    if let Some(min_sub_nodes) = cli.min_sub_nodes {
        config.min_sub_nodes = min_sub_nodes;
    }

    // Set up the language analyzer
    let analyzer = RustAnalyzer::new();

    // Scan for files
    let scan_config = dupes_core::scanner::ScanConfig::new(config.root.clone())
        .with_excludes(config.exclude.clone())
        .with_extensions(
            analyzer
                .file_extensions()
                .iter()
                .map(std::string::ToString::to_string)
                .collect(),
        );
    let files = dupes_core::scanner::scan_files(&scan_config);

    if files.is_empty() {
        eprintln!(
            "Error: {}",
            dupes_core::error::Error::NoSourceFiles(config.root)
        );
        process::exit(2);
    }

    // Run the analysis pipeline
    let result = match dupes_core::analyze(&analyzer, &files, &config) {
        Ok(r) => r,
        Err(e) => {
            eprintln!("Error: {e}");
            process::exit(2);
        }
    };

    // Print warnings
    for warning in &result.warnings {
        eprintln!("Warning: {warning}");
    }

    let reporter: Box<dyn Reporter> = match cli.format {
        OutputFormat::Text => Box::new(TextReporter::new(Some(root.to_path_buf()))),
        OutputFormat::Json => Box::new(JsonReporter::new(Some(root.to_path_buf()))),
    };

    (config, result, reporter)
}

fn cmd_stats(result: &AnalysisResult, reporter: &dyn Reporter, writer: &mut impl Write) {
    reporter.report_stats(&result.stats, writer).unwrap();
}

fn cmd_report(result: &AnalysisResult, reporter: &dyn Reporter, writer: &mut impl Write) {
    reporter.report_stats(&result.stats, writer).unwrap();
    writeln!(writer).unwrap();
    reporter.report_exact(&result.exact_groups, writer).unwrap();
    if !result.near_groups.is_empty() {
        reporter.report_near(&result.near_groups, writer).unwrap();
    }
    if !result.sub_exact_groups.is_empty() {
        reporter
            .report_sub_exact(&result.sub_exact_groups, writer)
            .unwrap();
    }
    if !result.sub_near_groups.is_empty() {
        reporter
            .report_sub_near(&result.sub_near_groups, writer)
            .unwrap();
    }
}

struct CheckThresholds {
    exact: Option<usize>,
    near: Option<usize>,
    exact_percent: Option<f64>,
    near_percent: Option<f64>,
}

fn cmd_check(
    config: &Config,
    result: &AnalysisResult,
    reporter: &dyn Reporter,
    writer: &mut impl Write,
    thresholds: &CheckThresholds,
) {
    let max_exact = thresholds.exact.or(config.max_exact_duplicates);
    let max_near = thresholds.near.or(config.max_near_duplicates);
    let max_exact_pct = thresholds.exact_percent.or(config.max_exact_percent);
    let max_near_pct = thresholds.near_percent.or(config.max_near_percent);

    reporter.report_stats(&result.stats, writer).unwrap();

    let mut failed = false;

    if let Some(threshold) = max_exact
        && result.stats.exact_duplicate_groups > threshold
    {
        writeln!(
            writer,
            "\nCheck FAILED: {} exact duplicate groups (max: {})",
            result.stats.exact_duplicate_groups, threshold
        )
        .unwrap();
        reporter.report_exact(&result.exact_groups, writer).unwrap();
        failed = true;
    }

    if let Some(threshold) = max_near
        && result.stats.near_duplicate_groups > threshold
    {
        writeln!(
            writer,
            "\nCheck FAILED: {} near duplicate groups (max: {})",
            result.stats.near_duplicate_groups, threshold
        )
        .unwrap();
        reporter.report_near(&result.near_groups, writer).unwrap();
        failed = true;
    }

    if let Some(threshold) = max_exact_pct {
        let actual = result.stats.exact_duplicate_percent();
        if actual > threshold {
            writeln!(
                writer,
                "\nCheck FAILED: {actual:.1}% exact duplicate lines (max: {threshold:.1}%)"
            )
            .unwrap();
            reporter.report_exact(&result.exact_groups, writer).unwrap();
            failed = true;
        }
    }

    if let Some(threshold) = max_near_pct {
        let actual = result.stats.near_duplicate_percent();
        if actual > threshold {
            writeln!(
                writer,
                "\nCheck FAILED: {actual:.1}% near duplicate lines (max: {threshold:.1}%)"
            )
            .unwrap();
            reporter.report_near(&result.near_groups, writer).unwrap();
            failed = true;
        }
    }

    if failed {
        process::exit(1);
    } else {
        writeln!(writer, "\nCheck passed.").unwrap();
    }
}

fn cmd_cleanup(root: &Path, result: &AnalysisResult, writer: &mut impl Write, dry_run: bool) {
    let mut ignore_file = ignore::load_ignore_file(root);

    if dry_run {
        let stale = ignore::find_stale_entries(&ignore_file, &result.all_fingerprints);
        if stale.is_empty() {
            writeln!(writer, "No stale entries found.").unwrap();
        } else {
            writeln!(writer, "Stale entries (dry run):").unwrap();
            for entry in &stale {
                write!(writer, "  {}", entry.fingerprint).unwrap();
                if let Some(reason) = &entry.reason {
                    write!(writer, " (reason: {reason})").unwrap();
                }
                writeln!(writer).unwrap();
            }
            writeln!(writer, "\n{} stale entries would be removed.", stale.len()).unwrap();
        }
    } else {
        let removed = ignore::remove_stale_entries(&mut ignore_file, &result.all_fingerprints);
        if removed.is_empty() {
            writeln!(writer, "No stale entries found.").unwrap();
        } else {
            if let Err(e) = ignore::save_ignore_file(root, &ignore_file) {
                eprintln!("Error saving ignore file: {e}");
                process::exit(2);
            }
            writeln!(writer, "Removed stale entries:").unwrap();
            for entry in &removed {
                write!(writer, "  {}", entry.fingerprint).unwrap();
                if let Some(reason) = &entry.reason {
                    write!(writer, " (reason: {reason})").unwrap();
                }
                writeln!(writer).unwrap();
            }
            writeln!(writer, "\nRemoved {} stale entries.", removed.len()).unwrap();
        }
    }
}

fn main() {
    let cli = Cli::parse();

    let root = cli
        .path
        .clone()
        .unwrap_or_else(|| std::env::current_dir().unwrap_or_else(|_| PathBuf::from(".")));

    let command = cli.command.as_ref().unwrap_or(&Command::Report);

    // Handle commands that don't need full analysis
    match command {
        Command::Ignore {
            fingerprint,
            reason,
        } => {
            cmd_ignore(&root, fingerprint, reason.clone());
            return;
        }
        Command::Ignored => {
            cmd_ignored(&root);
            return;
        }
        _ => {}
    }

    // Commands that need analysis
    let (config, result, reporter) = run_analysis(&cli, &root);
    let stdout = std::io::stdout();
    let mut writer = stdout.lock();

    match command {
        Command::Stats => cmd_stats(&result, &*reporter, &mut writer),
        Command::Report => cmd_report(&result, &*reporter, &mut writer),
        Command::Check {
            max_exact,
            max_near,
            max_exact_percent,
            max_near_percent,
        } => cmd_check(
            &config,
            &result,
            &*reporter,
            &mut writer,
            &CheckThresholds {
                exact: *max_exact,
                near: *max_near,
                exact_percent: *max_exact_percent,
                near_percent: *max_near_percent,
            },
        ),
        Command::Cleanup { dry_run } => cmd_cleanup(&root, &result, &mut writer, *dry_run),
        Command::Ignore { .. } | Command::Ignored => unreachable!(),
    }
}
