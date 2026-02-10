use std::path::{Path, PathBuf};

use serde::{Deserialize, Serialize};

use crate::fingerprint::Fingerprint;
use crate::grouper::DuplicateGroup;

const IGNORE_FILE_NAME: &str = ".dupes-ignore.toml";

/// An entry in the ignore file.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct IgnoreEntry {
    /// The fingerprint of the duplicated code.
    pub fingerprint: String,
    /// Optional reason for ignoring.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub reason: Option<String>,
    /// Names of the code units in the group (for documentation).
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub members: Vec<String>,
}

/// The ignore file structure.
#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct IgnoreFile {
    #[serde(default)]
    pub ignore: Vec<IgnoreEntry>,
}

/// Get the path to the ignore file for a project root.
pub fn ignore_file_path(root: &Path) -> PathBuf {
    root.join(IGNORE_FILE_NAME)
}

/// Load the ignore file from disk.
pub fn load_ignore_file(root: &Path) -> IgnoreFile {
    let path = ignore_file_path(root);
    if !path.exists() {
        return IgnoreFile::default();
    }
    match std::fs::read_to_string(&path) {
        Ok(content) => toml::from_str(&content).unwrap_or_default(),
        Err(_) => IgnoreFile::default(),
    }
}

/// Save the ignore file to disk.
pub fn save_ignore_file(root: &Path, ignore_file: &IgnoreFile) -> std::io::Result<()> {
    let path = ignore_file_path(root);
    let content = toml::to_string_pretty(ignore_file)
        .map_err(|e| std::io::Error::other(format!("Failed to serialize ignore file: {e}")))?;
    std::fs::write(path, content)
}

/// Add an ignore entry for a fingerprint.
pub fn add_ignore(
    ignore_file: &mut IgnoreFile,
    fingerprint: &Fingerprint,
    reason: Option<String>,
    members: Vec<String>,
) {
    let fp_hex = fingerprint.to_hex();
    // Don't add duplicates
    if ignore_file.ignore.iter().any(|e| e.fingerprint == fp_hex) {
        return;
    }
    ignore_file.ignore.push(IgnoreEntry {
        fingerprint: fp_hex,
        reason,
        members,
    });
}

/// Remove an ignore entry by fingerprint.
pub fn remove_ignore(ignore_file: &mut IgnoreFile, fingerprint: &str) -> bool {
    let initial_len = ignore_file.ignore.len();
    ignore_file.ignore.retain(|e| e.fingerprint != fingerprint);
    ignore_file.ignore.len() < initial_len
}

/// Check if a fingerprint is ignored.
pub fn is_ignored(ignore_file: &IgnoreFile, fingerprint: &Fingerprint) -> bool {
    let fp_hex = fingerprint.to_hex();
    ignore_file.ignore.iter().any(|e| e.fingerprint == fp_hex)
}

/// Filter out ignored groups from a list of duplicate groups.
pub fn filter_ignored(
    groups: Vec<DuplicateGroup>,
    ignore_file: &IgnoreFile,
) -> Vec<DuplicateGroup> {
    groups
        .into_iter()
        .filter(|g| {
            if let Some(fp) = g.fingerprint {
                !is_ignored(ignore_file, &fp)
            } else {
                true // near-duplicates without fingerprints are never ignored
            }
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::normalizer::NormalizedNode;
    use tempfile::TempDir;

    fn test_fingerprint() -> Fingerprint {
        Fingerprint::from_node(&NormalizedNode::Literal(
            crate::normalizer::LiteralKind::Int,
        ))
    }

    #[test]
    fn load_nonexistent_returns_default() {
        let tmp = TempDir::new().unwrap();
        let ignore = load_ignore_file(tmp.path());
        assert!(ignore.ignore.is_empty());
    }

    #[test]
    fn roundtrip_save_and_load() {
        let tmp = TempDir::new().unwrap();
        let fp = test_fingerprint();
        let mut ignore = IgnoreFile::default();
        add_ignore(
            &mut ignore,
            &fp,
            Some("test reason".to_string()),
            vec!["foo".to_string(), "bar".to_string()],
        );
        save_ignore_file(tmp.path(), &ignore).unwrap();
        let loaded = load_ignore_file(tmp.path());
        assert_eq!(loaded.ignore.len(), 1);
        assert_eq!(loaded.ignore[0].fingerprint, fp.to_hex());
        assert_eq!(loaded.ignore[0].reason, Some("test reason".to_string()));
        assert_eq!(loaded.ignore[0].members, vec!["foo", "bar"]);
    }

    #[test]
    fn add_ignore_deduplicates() {
        let fp = test_fingerprint();
        let mut ignore = IgnoreFile::default();
        add_ignore(&mut ignore, &fp, None, vec![]);
        add_ignore(&mut ignore, &fp, None, vec![]);
        assert_eq!(ignore.ignore.len(), 1);
    }

    #[test]
    fn remove_ignore_works() {
        let fp = test_fingerprint();
        let mut ignore = IgnoreFile::default();
        add_ignore(&mut ignore, &fp, None, vec![]);
        assert!(remove_ignore(&mut ignore, &fp.to_hex()));
        assert!(ignore.ignore.is_empty());
    }

    #[test]
    fn remove_nonexistent_returns_false() {
        let mut ignore = IgnoreFile::default();
        assert!(!remove_ignore(&mut ignore, "nonexistent"));
    }

    #[test]
    fn is_ignored_works() {
        let fp = test_fingerprint();
        let mut ignore = IgnoreFile::default();
        assert!(!is_ignored(&ignore, &fp));
        add_ignore(&mut ignore, &fp, None, vec![]);
        assert!(is_ignored(&ignore, &fp));
    }

    #[test]
    fn filter_ignored_removes_matching_groups() {
        let fp = test_fingerprint();
        let mut ignore = IgnoreFile::default();
        add_ignore(&mut ignore, &fp, None, vec![]);

        let groups = vec![
            DuplicateGroup {
                fingerprint: Some(fp),
                members: vec![],
                similarity: 1.0,
            },
            DuplicateGroup {
                fingerprint: Some(Fingerprint::from_node(&NormalizedNode::Opaque)),
                members: vec![],
                similarity: 1.0,
            },
        ];

        let filtered = filter_ignored(groups, &ignore);
        assert_eq!(filtered.len(), 1);
    }

    #[test]
    fn filter_ignored_keeps_near_duplicates() {
        let fp = test_fingerprint();
        let mut ignore = IgnoreFile::default();
        add_ignore(&mut ignore, &fp, None, vec![]);

        let groups = vec![DuplicateGroup {
            fingerprint: None,
            members: vec![],
            similarity: 0.85,
        }];

        let filtered = filter_ignored(groups, &ignore);
        assert_eq!(filtered.len(), 1);
    }

    #[test]
    fn ignore_file_path_is_correct() {
        let path = ignore_file_path(Path::new("/project"));
        assert_eq!(path, PathBuf::from("/project/.dupes-ignore.toml"));
    }
}
