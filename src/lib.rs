pub mod config;
pub mod error;
pub mod fingerprint;
pub mod grouper;
pub mod ignore;
pub mod normalizer;
pub mod output;
pub mod parser;
pub mod scanner;
pub mod similarity;

use config::Config;
use grouper::{DuplicateGroup, DuplicationStats};

/// The result of a full analysis run.
pub struct AnalysisResult {
    pub stats: DuplicationStats,
    pub exact_groups: Vec<DuplicateGroup>,
    pub near_groups: Vec<DuplicateGroup>,
    pub warnings: Vec<String>,
}

/// Run the full analysis pipeline.
pub fn analyze(config: &Config) -> error::Result<AnalysisResult> {
    // 1. Scan for files
    let scan_config =
        scanner::ScanConfig::new(config.root.clone()).with_excludes(config.exclude.clone());
    let files = scanner::scan_files(&scan_config);

    if files.is_empty() {
        return Err(error::Error::NoSourceFiles(config.root.clone()));
    }

    // 2. Parse all files
    let (units, warnings) = parser::parse_files(&files, config.min_nodes, config.min_lines);

    // 3. Group exact duplicates
    let exact_groups = grouper::group_exact_duplicates(&units);

    // 4. Find near-duplicates
    let exact_fps: Vec<_> = exact_groups.iter().filter_map(|g| g.fingerprint).collect();
    let near_groups =
        grouper::find_near_duplicates(&units, config.similarity_threshold, &exact_fps);

    // 5. Apply ignore filtering
    let ignore_file = ignore::load_ignore_file(&config.root);
    let exact_groups = ignore::filter_ignored(exact_groups, &ignore_file);
    let near_groups = ignore::filter_ignored(near_groups, &ignore_file);

    // 6. Compute stats
    let stats = grouper::compute_stats(units.len(), &exact_groups, &near_groups);

    Ok(AnalysisResult {
        stats,
        exact_groups,
        near_groups,
        warnings,
    })
}
