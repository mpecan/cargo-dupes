use std::path::{Path, PathBuf};

use serde::Deserialize;

/// Configuration for cargo-dupes analysis.
#[derive(Debug, Clone)]
pub struct Config {
    /// Minimum number of AST nodes for a code unit to be analyzed.
    pub min_nodes: usize,
    /// Similarity threshold for near-duplicates (0.0 to 1.0).
    pub similarity_threshold: f64,
    /// Path patterns to exclude from scanning.
    pub exclude: Vec<String>,
    /// Exit code threshold: fail if exact duplicate count exceeds this.
    pub max_exact_duplicates: Option<usize>,
    /// Exit code threshold: fail if near duplicate count exceeds this.
    pub max_near_duplicates: Option<usize>,
    /// Exit code threshold: fail if exact duplicate percentage exceeds this.
    pub max_exact_percent: Option<f64>,
    /// Exit code threshold: fail if near duplicate percentage exceeds this.
    pub max_near_percent: Option<f64>,
    /// Minimum number of source lines for a code unit to be analyzed.
    pub min_lines: usize,
    /// Exclude test code (#[test] functions and #[cfg(test)] modules).
    pub exclude_tests: bool,
    /// Root path to analyze.
    pub root: PathBuf,
}

impl Default for Config {
    fn default() -> Self {
        Self {
            min_nodes: 10,
            similarity_threshold: 0.8,
            exclude: Vec::new(),
            max_exact_duplicates: None,
            max_near_duplicates: None,
            max_exact_percent: None,
            max_near_percent: None,
            min_lines: 0,
            exclude_tests: false,
            root: PathBuf::from("."),
        }
    }
}

/// Config as stored in dupes.toml or Cargo.toml metadata.
#[derive(Debug, Deserialize, Default)]
#[serde(default)]
struct FileConfig {
    min_nodes: Option<usize>,
    similarity_threshold: Option<f64>,
    exclude: Option<Vec<String>>,
    max_exact_duplicates: Option<usize>,
    max_near_duplicates: Option<usize>,
    max_exact_percent: Option<f64>,
    max_near_percent: Option<f64>,
    min_lines: Option<usize>,
    exclude_tests: Option<bool>,
}

/// Cargo.toml metadata section.
#[derive(Debug, Deserialize)]
struct CargoMetadata {
    #[serde(default)]
    package: Option<CargoPackage>,
}

#[derive(Debug, Deserialize)]
struct CargoPackage {
    #[serde(default)]
    metadata: Option<CargoPackageMetadata>,
}

#[derive(Debug, Deserialize)]
struct CargoPackageMetadata {
    #[serde(default)]
    dupes: Option<FileConfig>,
}

impl Config {
    /// Load config with the following precedence:
    /// 1. CLI overrides (applied by the caller after this method)
    /// 2. dupes.toml in the project root
    /// 3. [package.metadata.dupes] in Cargo.toml
    /// 4. Defaults
    pub fn load(root: &Path) -> Self {
        let mut config = Config {
            root: root.to_path_buf(),
            ..Default::default()
        };

        // Try Cargo.toml metadata first (lowest priority file config)
        let cargo_toml = root.join("Cargo.toml");
        if cargo_toml.exists()
            && let Ok(content) = std::fs::read_to_string(&cargo_toml)
            && let Ok(cargo) = toml::from_str::<CargoMetadata>(&content)
            && let Some(pkg) = cargo.package
            && let Some(meta) = pkg.metadata
            && let Some(dupes) = meta.dupes
        {
            config.apply_file_config(&dupes);
        }

        // Try dupes.toml (higher priority)
        let dupes_toml = root.join("dupes.toml");
        if dupes_toml.exists()
            && let Ok(content) = std::fs::read_to_string(&dupes_toml)
            && let Ok(file_config) = toml::from_str::<FileConfig>(&content)
        {
            config.apply_file_config(&file_config);
        }

        config
    }

    fn apply_file_config(&mut self, fc: &FileConfig) {
        if let Some(v) = fc.min_nodes {
            self.min_nodes = v;
        }
        if let Some(v) = fc.similarity_threshold {
            self.similarity_threshold = v;
        }
        if let Some(ref v) = fc.exclude {
            self.exclude = v.clone();
        }
        if let Some(v) = fc.max_exact_duplicates {
            self.max_exact_duplicates = Some(v);
        }
        if let Some(v) = fc.max_near_duplicates {
            self.max_near_duplicates = Some(v);
        }
        if let Some(v) = fc.max_exact_percent {
            self.max_exact_percent = Some(v);
        }
        if let Some(v) = fc.max_near_percent {
            self.max_near_percent = Some(v);
        }
        if let Some(v) = fc.min_lines {
            self.min_lines = v;
        }
        if let Some(v) = fc.exclude_tests {
            self.exclude_tests = v;
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;
    use tempfile::TempDir;

    #[test]
    fn default_config() {
        let config = Config::default();
        assert_eq!(config.min_nodes, 10);
        assert!((config.similarity_threshold - 0.8).abs() < f64::EPSILON);
        assert!(config.exclude.is_empty());
    }

    #[test]
    fn load_from_dupes_toml() {
        let tmp = TempDir::new().unwrap();
        fs::write(
            tmp.path().join("dupes.toml"),
            r#"
            min_nodes = 20
            similarity_threshold = 0.9
            exclude = ["tests"]
            "#,
        )
        .unwrap();
        let config = Config::load(tmp.path());
        assert_eq!(config.min_nodes, 20);
        assert!((config.similarity_threshold - 0.9).abs() < f64::EPSILON);
        assert_eq!(config.exclude, vec!["tests".to_string()]);
    }

    #[test]
    fn load_from_cargo_toml_metadata() {
        let tmp = TempDir::new().unwrap();
        fs::write(
            tmp.path().join("Cargo.toml"),
            r#"
            [package]
            name = "test"
            version = "0.1.0"
            edition = "2021"

            [package.metadata.dupes]
            min_nodes = 15
            similarity_threshold = 0.75
            "#,
        )
        .unwrap();
        let config = Config::load(tmp.path());
        assert_eq!(config.min_nodes, 15);
        assert!((config.similarity_threshold - 0.75).abs() < f64::EPSILON);
    }

    #[test]
    fn dupes_toml_overrides_cargo_toml() {
        let tmp = TempDir::new().unwrap();
        fs::write(
            tmp.path().join("Cargo.toml"),
            r#"
            [package]
            name = "test"
            version = "0.1.0"
            edition = "2021"

            [package.metadata.dupes]
            min_nodes = 15
            "#,
        )
        .unwrap();
        fs::write(
            tmp.path().join("dupes.toml"),
            r#"
            min_nodes = 25
            "#,
        )
        .unwrap();
        let config = Config::load(tmp.path());
        assert_eq!(config.min_nodes, 25);
    }

    #[test]
    fn load_no_config_files() {
        let tmp = TempDir::new().unwrap();
        let config = Config::load(tmp.path());
        assert_eq!(config.min_nodes, 10); // default
    }

    #[test]
    fn config_with_thresholds() {
        let tmp = TempDir::new().unwrap();
        fs::write(
            tmp.path().join("dupes.toml"),
            r#"
            max_exact_duplicates = 0
            max_near_duplicates = 5
            "#,
        )
        .unwrap();
        let config = Config::load(tmp.path());
        assert_eq!(config.max_exact_duplicates, Some(0));
        assert_eq!(config.max_near_duplicates, Some(5));
    }

    #[test]
    fn config_with_exclude_tests() {
        let tmp = TempDir::new().unwrap();
        fs::write(
            tmp.path().join("dupes.toml"),
            r#"
            exclude_tests = true
            "#,
        )
        .unwrap();
        let config = Config::load(tmp.path());
        assert!(config.exclude_tests);
    }

    #[test]
    fn config_with_min_lines() {
        let tmp = TempDir::new().unwrap();
        fs::write(
            tmp.path().join("dupes.toml"),
            r#"
            min_lines = 5
            "#,
        )
        .unwrap();
        let config = Config::load(tmp.path());
        assert_eq!(config.min_lines, 5);
    }

    #[test]
    fn config_with_percentage_thresholds() {
        let tmp = TempDir::new().unwrap();
        fs::write(
            tmp.path().join("dupes.toml"),
            r#"
            max_exact_percent = 5.0
            max_near_percent = 10.5
            "#,
        )
        .unwrap();
        let config = Config::load(tmp.path());
        assert_eq!(config.max_exact_percent, Some(5.0));
        assert_eq!(config.max_near_percent, Some(10.5));
    }
}
