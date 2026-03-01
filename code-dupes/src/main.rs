use std::collections::HashSet;
use std::path::PathBuf;
use std::process;

use clap::{Parser, ValueEnum};
use walkdir::WalkDir;

use dupes_core::analyzer::LanguageAnalyzer;
use dupes_core::cli::{self, CliError, CliOverrides, Command, OutputFormat};
use dupes_python::PythonAnalyzer;
use dupes_rust::RustAnalyzer;

#[derive(Parser)]
#[command(
    name = "code-dupes",
    version,
    about = "Detect duplicate code across multiple languages"
)]
struct Cli {
    #[command(subcommand)]
    command: Option<Command>,

    /// Path to analyze (defaults to current directory).
    #[arg(short, long, global = true)]
    path: Option<PathBuf>,

    /// Language to analyze. Auto-detected from file extensions if omitted.
    #[arg(short, long, global = true)]
    language: Option<Language>,

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

    /// Exclude test code (language-specific detection).
    #[arg(long, global = true)]
    exclude_tests: bool,

    /// Enable sub-function duplicate detection (if branches, match arms, loop bodies).
    #[arg(long, short = 's', global = true)]
    sub_function: bool,

    /// Minimum AST node count for sub-function units.
    #[arg(long, global = true)]
    min_sub_nodes: Option<usize>,
}

#[derive(Clone, ValueEnum)]
enum Language {
    Rust,
    Python,
}

impl Language {
    /// Static file extension registry — avoids constructing analyzers for detection.
    const fn extensions(&self) -> &'static [&'static str] {
        match self {
            Self::Rust => &["rs"],
            Self::Python => &["py", "pyi"],
        }
    }

    const ALL: &[Self] = &[Self::Rust, Self::Python];
}

impl std::fmt::Display for Language {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Rust => write!(f, "rust"),
            Self::Python => write!(f, "python"),
        }
    }
}

/// Create a language analyzer for the given language.
fn resolve_analyzer(language: &Language) -> Box<dyn LanguageAnalyzer> {
    match language {
        Language::Rust => Box::new(RustAnalyzer::new()),
        Language::Python => Box::new(PythonAnalyzer::new()),
    }
}

/// Auto-detect language by scanning for files matching known extensions.
///
/// Performs a single directory walk (instead of one per language) and collects
/// file extensions, then matches against `Language::ALL`. Returns an error if
/// multiple languages are detected — the user must specify `--language` to
/// disambiguate.
fn auto_detect_language(root: &std::path::Path) -> Result<Language, CliError> {
    // Collect all file extensions found in a single walk.
    let mut found_extensions = HashSet::new();

    for entry in WalkDir::new(root)
        .into_iter()
        .filter_entry(|e| {
            let path = e.path();
            if path.is_dir()
                && let Some(name) = path.file_name().and_then(|n| n.to_str())
            {
                if name == "target" {
                    return false;
                }
                if name.starts_with('.') && path != root {
                    return false;
                }
            }
            true
        })
        .flatten()
    {
        let path = entry.path();
        if path.is_file()
            && let Some(ext) = path.extension().and_then(|e| e.to_str())
        {
            found_extensions.insert(ext.to_ascii_lowercase());
        }
    }

    // Match found extensions against known languages.
    let detected: Vec<Language> = Language::ALL
        .iter()
        .filter(|lang| {
            lang.extensions()
                .iter()
                .any(|ext| found_extensions.contains(*ext))
        })
        .cloned()
        .collect();

    match detected.len() {
        0 => Err(CliError::NoRecognizedFiles),
        1 => Ok(detected.into_iter().next().unwrap()),
        _ => Err(CliError::AmbiguousLanguage(
            detected.iter().map(ToString::to_string).collect(),
        )),
    }
}

fn main() {
    let Cli {
        command,
        path,
        language,
        min_nodes,
        min_lines,
        threshold,
        format,
        exclude,
        exclude_tests,
        sub_function,
        min_sub_nodes,
    } = Cli::parse();

    let root =
        path.unwrap_or_else(|| std::env::current_dir().unwrap_or_else(|_| PathBuf::from(".")));

    let command = command.unwrap_or(Command::Report);
    let stdout = std::io::stdout();
    let mut writer = stdout.lock();

    let result = match &command {
        Command::Ignore {
            fingerprint,
            reason,
        } => cli::cmd_ignore(&root, fingerprint, reason.clone(), &mut writer),
        Command::Ignored => cli::cmd_ignored(&root, &mut writer),
        _ => {
            let language = match language {
                Some(l) => l,
                None => match auto_detect_language(&root) {
                    Ok(l) => l,
                    Err(e) => {
                        eprintln!("Error: {e}");
                        process::exit(e.exit_code());
                    }
                },
            };
            let analyzer = resolve_analyzer(&language);

            let overrides = CliOverrides {
                min_nodes,
                min_lines,
                threshold,
                exclude,
                exclude_tests: if exclude_tests { Some(true) } else { None },
                sub_function: if sub_function { Some(true) } else { None },
                min_sub_nodes,
            };
            let output = match cli::run_analysis(analyzer.as_ref(), &root, format, &overrides) {
                Ok(o) => o,
                Err(e) => {
                    eprintln!("Error: {e}");
                    process::exit(e.exit_code());
                }
            };

            for warning in &output.result.warnings {
                eprintln!("Warning: {warning}");
            }

            let reporter: &dyn dupes_core::output::Reporter = &*output.reporter;

            match &command {
                Command::Stats => cli::cmd_stats(&output.result, reporter, &mut writer),
                Command::Report => cli::cmd_report(&output.result, reporter, &mut writer),
                Command::Check {
                    max_exact,
                    max_near,
                    max_exact_percent,
                    max_near_percent,
                } => cli::cmd_check(
                    &output.config,
                    &output.result,
                    reporter,
                    &mut writer,
                    &cli::CheckThresholds {
                        max_exact: *max_exact,
                        max_near: *max_near,
                        max_exact_percent: *max_exact_percent,
                        max_near_percent: *max_near_percent,
                    },
                ),
                Command::Cleanup { dry_run } => {
                    cli::cmd_cleanup(&root, &output.result, &mut writer, *dry_run)
                }
                Command::Ignore { .. } | Command::Ignored => unreachable!(),
            }
        }
    };

    if let Err(e) = result {
        if matches!(e, CliError::CheckFailed) {
            process::exit(1);
        } else {
            eprintln!("Error: {e}");
            process::exit(e.exit_code());
        }
    }
}
