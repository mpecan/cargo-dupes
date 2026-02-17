use std::path::{Path, PathBuf};
use walkdir::WalkDir;

/// Configuration for scanning the filesystem for source files.
pub struct ScanConfig {
    /// Root directory to scan.
    pub root: PathBuf,
    /// Glob patterns to exclude (simple substring matching for now).
    pub exclude_patterns: Vec<String>,
    /// File extensions to include (without the leading dot). Defaults to `["rs"]`.
    pub extensions: Vec<String>,
}

impl ScanConfig {
    #[must_use]
    pub fn new(root: PathBuf) -> Self {
        Self {
            root,
            exclude_patterns: Vec::new(),
            extensions: vec!["rs".to_string()],
        }
    }

    #[must_use]
    pub fn with_excludes(mut self, patterns: Vec<String>) -> Self {
        self.exclude_patterns = patterns;
        self
    }

    #[must_use]
    pub fn with_extensions(mut self, extensions: Vec<String>) -> Self {
        self.extensions = extensions;
        self
    }
}

/// Scan for source files under the given config.
/// Always skips `target/` directories.
#[must_use]
pub fn scan_files(config: &ScanConfig) -> Vec<PathBuf> {
    let mut files = Vec::new();

    for entry in WalkDir::new(&config.root)
        .into_iter()
        .filter_entry(|e| {
            let path = e.path();
            // Only filter directories (not the root itself for hidden check)
            if path.is_dir()
                && let Some(name) = path.file_name().and_then(|n| n.to_str())
            {
                if name == "target" {
                    return false;
                }
                // Skip hidden directories, but not the root
                if name.starts_with('.') && path != config.root.as_path() {
                    return false;
                }
            }
            true
        })
        .flatten()
    {
        let path = entry.path();
        if path.is_file()
            && path
                .extension()
                .and_then(|ext| ext.to_str())
                .is_some_and(|ext| {
                    config
                        .extensions
                        .iter()
                        .any(|e| e.eq_ignore_ascii_case(ext))
                })
            && !is_excluded(path, &config.exclude_patterns)
        {
            files.push(path.to_path_buf());
        }
    }

    files
}

/// Check if a path should be excluded based on exclusion patterns.
#[must_use]
pub fn is_excluded(path: &Path, patterns: &[String]) -> bool {
    let path_str = path.to_string_lossy();
    patterns
        .iter()
        .any(|pattern| path_str.contains(pattern.as_str()))
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;
    use tempfile::TempDir;

    fn create_test_tree(dir: &Path) {
        fs::create_dir_all(dir.join("src")).unwrap();
        fs::create_dir_all(dir.join("src/utils")).unwrap();
        fs::create_dir_all(dir.join("target/debug")).unwrap();
        fs::create_dir_all(dir.join(".hidden")).unwrap();
        fs::write(dir.join("src/main.rs"), "fn main() {}").unwrap();
        fs::write(dir.join("src/lib.rs"), "pub mod utils;").unwrap();
        fs::write(dir.join("src/utils/helper.rs"), "pub fn help() {}").unwrap();
        fs::write(dir.join("target/debug/build.rs"), "fn build() {}").unwrap();
        fs::write(dir.join(".hidden/secret.rs"), "fn secret() {}").unwrap();
        fs::write(dir.join("src/readme.md"), "# README").unwrap();
    }

    #[test]
    fn scan_finds_rust_files() {
        let tmp = TempDir::new().unwrap();
        create_test_tree(tmp.path());
        let config = ScanConfig::new(tmp.path().to_path_buf());
        let files = scan_files(&config);
        assert_eq!(files.len(), 3);
        assert!(files.iter().all(|f| f.extension().unwrap() == "rs"));
    }

    #[test]
    fn scan_skips_target_directory() {
        let tmp = TempDir::new().unwrap();
        create_test_tree(tmp.path());
        let config = ScanConfig::new(tmp.path().to_path_buf());
        let files = scan_files(&config);
        assert!(!files.iter().any(|f| f.to_string_lossy().contains("target")));
    }

    #[test]
    fn scan_skips_hidden_directories() {
        let tmp = TempDir::new().unwrap();
        create_test_tree(tmp.path());
        let config = ScanConfig::new(tmp.path().to_path_buf());
        let files = scan_files(&config);
        assert!(
            !files
                .iter()
                .any(|f| f.to_string_lossy().contains(".hidden"))
        );
    }

    #[test]
    fn scan_respects_exclude_patterns() {
        let tmp = TempDir::new().unwrap();
        create_test_tree(tmp.path());
        let config =
            ScanConfig::new(tmp.path().to_path_buf()).with_excludes(vec!["utils".to_string()]);
        let files = scan_files(&config);
        assert!(!files.iter().any(|f| f.to_string_lossy().contains("utils")));
        assert_eq!(files.len(), 2);
    }

    #[test]
    fn scan_empty_directory() {
        let tmp = TempDir::new().unwrap();
        let config = ScanConfig::new(tmp.path().to_path_buf());
        let files = scan_files(&config);
        assert!(files.is_empty());
    }

    #[test]
    fn is_excluded_works() {
        let path = Path::new("/foo/bar/tests/test.rs");
        assert!(is_excluded(path, &["tests".to_string()]));
        assert!(!is_excluded(path, &["benches".to_string()]));
    }
}
