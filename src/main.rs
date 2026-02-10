use std::io::Write;
use std::path::PathBuf;
use std::process;

use clap::{Parser, Subcommand};

use cargo_dupes::config::Config;
use cargo_dupes::fingerprint::Fingerprint;
use cargo_dupes::ignore;
use cargo_dupes::output::Reporter;
use cargo_dupes::output::json::JsonReporter;
use cargo_dupes::output::text::TextReporter;

#[derive(Parser)]
#[command(
    name = "cargo-dupes",
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

    /// Similarity threshold (0.0-1.0).
    #[arg(long, global = true)]
    threshold: Option<f64>,

    /// Output format.
    #[arg(long, global = true, default_value = "text")]
    format: OutputFormat,

    /// Exclude patterns (can be repeated).
    #[arg(long, global = true)]
    exclude: Vec<String>,
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
}

fn main() {
    let cli = Cli::parse();

    let root = cli
        .path
        .unwrap_or_else(|| std::env::current_dir().unwrap_or_else(|_| PathBuf::from(".")));

    // Handle ignore-related commands that don't need full analysis
    let command = cli.command.unwrap_or(Command::Report);

    match command {
        Command::Ignore {
            fingerprint,
            reason,
        } => {
            let Some(fp) = Fingerprint::from_hex(&fingerprint) else {
                eprintln!("Error: Invalid fingerprint: {fingerprint}");
                process::exit(2);
            };
            let mut ignore_file = ignore::load_ignore_file(&root);
            ignore::add_ignore(&mut ignore_file, &fp, reason, vec![]);
            if let Err(e) = ignore::save_ignore_file(&root, &ignore_file) {
                eprintln!("Error saving ignore file: {e}");
                process::exit(2);
            }
            println!("Added {fingerprint} to ignore list.");
            return;
        }
        Command::Ignored => {
            let ignore_file = ignore::load_ignore_file(&root);
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
            return;
        }
        _ => {}
    }

    let mut config = Config::load(&root);

    // Apply CLI overrides
    if let Some(min_nodes) = cli.min_nodes {
        config.min_nodes = min_nodes;
    }
    if let Some(threshold) = cli.threshold {
        config.similarity_threshold = threshold;
    }
    if !cli.exclude.is_empty() {
        config.exclude = cli.exclude;
    }

    let result = match cargo_dupes::analyze(&config) {
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

    let stdout = std::io::stdout();
    let mut writer = stdout.lock();

    let reporter: Box<dyn Reporter> = match cli.format {
        OutputFormat::Text => Box::new(TextReporter::new(Some(root.clone()))),
        OutputFormat::Json => Box::new(JsonReporter::new(Some(root.clone()))),
    };

    match command {
        Command::Stats => {
            reporter.report_stats(&result.stats, &mut writer).unwrap();
        }
        Command::Report => {
            reporter.report_stats(&result.stats, &mut writer).unwrap();
            writeln!(writer).unwrap();
            reporter
                .report_exact(&result.exact_groups, &mut writer)
                .unwrap();
            if !result.near_groups.is_empty() {
                reporter
                    .report_near(&result.near_groups, &mut writer)
                    .unwrap();
            }
        }
        Command::Check {
            max_exact,
            max_near,
        } => {
            let max_exact = max_exact.or(config.max_exact_duplicates).unwrap_or(0);
            let max_near = max_near
                .or(config.max_near_duplicates)
                .unwrap_or(usize::MAX);

            reporter.report_stats(&result.stats, &mut writer).unwrap();

            let mut failed = false;

            if result.stats.exact_duplicate_groups > max_exact {
                writeln!(
                    writer,
                    "\nCheck FAILED: {} exact duplicate groups (max: {})",
                    result.stats.exact_duplicate_groups, max_exact
                )
                .unwrap();
                reporter
                    .report_exact(&result.exact_groups, &mut writer)
                    .unwrap();
                failed = true;
            }

            if result.stats.near_duplicate_groups > max_near {
                writeln!(
                    writer,
                    "\nCheck FAILED: {} near duplicate groups (max: {})",
                    result.stats.near_duplicate_groups, max_near
                )
                .unwrap();
                reporter
                    .report_near(&result.near_groups, &mut writer)
                    .unwrap();
                failed = true;
            }

            if failed {
                process::exit(1);
            } else {
                writeln!(writer, "\nCheck passed.").unwrap();
            }
        }
        Command::Ignore { .. } | Command::Ignored => unreachable!(),
    }
}
