use std::io;

use crate::grouper::{DuplicateGroup, DuplicationStats};
use crate::output::Reporter;

pub struct JsonReporter {
    pub base_path: Option<std::path::PathBuf>,
}

impl JsonReporter {
    pub fn new(base_path: Option<std::path::PathBuf>) -> Self {
        Self { base_path }
    }

    fn relative_path(&self, path: &std::path::Path) -> String {
        if let Some(base) = &self.base_path
            && let Ok(rel) = path.strip_prefix(base)
        {
            return rel.to_string_lossy().to_string();
        }
        path.to_string_lossy().to_string()
    }
}

#[derive(serde::Serialize)]
struct JsonStats {
    total_code_units: usize,
    total_lines: usize,
    exact_duplicate_groups: usize,
    exact_duplicate_units: usize,
    near_duplicate_groups: usize,
    near_duplicate_units: usize,
    exact_duplicate_lines: usize,
    near_duplicate_lines: usize,
    exact_duplicate_percent: f64,
    near_duplicate_percent: f64,
    #[serde(skip_serializing_if = "is_zero")]
    sub_exact_groups: usize,
    #[serde(skip_serializing_if = "is_zero")]
    sub_exact_units: usize,
    #[serde(skip_serializing_if = "is_zero")]
    sub_near_groups: usize,
    #[serde(skip_serializing_if = "is_zero")]
    sub_near_units: usize,
}

fn is_zero(v: &usize) -> bool {
    *v == 0
}

#[derive(serde::Serialize)]
struct JsonGroup {
    fingerprint: String,
    similarity: f64,
    members: Vec<JsonMember>,
}

#[derive(serde::Serialize)]
struct JsonMember {
    name: String,
    kind: String,
    file: String,
    line_start: usize,
    line_end: usize,
}

impl Reporter for JsonReporter {
    fn report_stats(&self, stats: &DuplicationStats, writer: &mut dyn io::Write) -> io::Result<()> {
        let json_stats = JsonStats {
            total_code_units: stats.total_code_units,
            total_lines: stats.total_lines,
            exact_duplicate_groups: stats.exact_duplicate_groups,
            exact_duplicate_units: stats.exact_duplicate_units,
            near_duplicate_groups: stats.near_duplicate_groups,
            near_duplicate_units: stats.near_duplicate_units,
            exact_duplicate_lines: stats.exact_duplicate_lines,
            near_duplicate_lines: stats.near_duplicate_lines,
            exact_duplicate_percent: stats.exact_duplicate_percent(),
            near_duplicate_percent: stats.near_duplicate_percent(),
            sub_exact_groups: stats.sub_exact_groups,
            sub_exact_units: stats.sub_exact_units,
            sub_near_groups: stats.sub_near_groups,
            sub_near_units: stats.sub_near_units,
        };
        let json = serde_json::to_string_pretty(&json_stats).map_err(io::Error::other)?;
        writeln!(writer, "{json}")
    }

    fn report_exact(
        &self,
        groups: &[DuplicateGroup],
        writer: &mut dyn io::Write,
    ) -> io::Result<()> {
        let json_groups: Vec<JsonGroup> = groups.iter().map(|g| self.to_json_group(g)).collect();
        let json = serde_json::to_string_pretty(&json_groups).map_err(io::Error::other)?;
        writeln!(writer, "{json}")
    }

    fn report_near(&self, groups: &[DuplicateGroup], writer: &mut dyn io::Write) -> io::Result<()> {
        let json_groups: Vec<JsonGroup> = groups.iter().map(|g| self.to_json_group(g)).collect();
        let json = serde_json::to_string_pretty(&json_groups).map_err(io::Error::other)?;
        writeln!(writer, "{json}")
    }

    fn report_sub_exact(
        &self,
        groups: &[DuplicateGroup],
        writer: &mut dyn io::Write,
    ) -> io::Result<()> {
        let json_groups: Vec<JsonGroup> = groups.iter().map(|g| self.to_json_group(g)).collect();
        let json = serde_json::to_string_pretty(&json_groups).map_err(io::Error::other)?;
        writeln!(writer, "{json}")
    }

    fn report_sub_near(
        &self,
        groups: &[DuplicateGroup],
        writer: &mut dyn io::Write,
    ) -> io::Result<()> {
        let json_groups: Vec<JsonGroup> = groups.iter().map(|g| self.to_json_group(g)).collect();
        let json = serde_json::to_string_pretty(&json_groups).map_err(io::Error::other)?;
        writeln!(writer, "{json}")
    }
}

impl JsonReporter {
    fn to_json_group(&self, group: &DuplicateGroup) -> JsonGroup {
        JsonGroup {
            fingerprint: group.fingerprint.to_hex(),
            similarity: group.similarity,
            members: group
                .members
                .iter()
                .map(|m| JsonMember {
                    name: m.name.clone(),
                    kind: m.kind.to_string(),
                    file: self.relative_path(&m.file),
                    line_start: m.line_start,
                    line_end: m.line_end,
                })
                .collect(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::code_unit::{CodeUnit, CodeUnitKind};
    use crate::fingerprint::Fingerprint;
    use crate::node::NormalizedNode;
    use std::path::PathBuf;

    fn make_unit(name: &str, file: &str, line_start: usize, line_end: usize) -> CodeUnit {
        CodeUnit {
            kind: CodeUnitKind::Function,
            name: name.to_string(),
            file: PathBuf::from(file),
            line_start,
            line_end,
            signature: NormalizedNode::Opaque,
            body: NormalizedNode::Block(vec![]),
            fingerprint: Fingerprint::from_node(&NormalizedNode::Opaque),
            node_count: 10,
            parent_name: None,
        }
    }

    #[test]
    fn json_report_stats() {
        let reporter = JsonReporter::new(None);
        let stats = DuplicationStats {
            total_code_units: 50,
            total_lines: 500,
            exact_duplicate_groups: 3,
            exact_duplicate_units: 8,
            near_duplicate_groups: 2,
            near_duplicate_units: 5,
            exact_duplicate_lines: 30,
            near_duplicate_lines: 20,
            sub_exact_groups: 0,
            sub_exact_units: 0,
            sub_near_groups: 0,
            sub_near_units: 0,
        };
        let mut buf = Vec::new();
        reporter.report_stats(&stats, &mut buf).unwrap();
        let output = String::from_utf8(buf).unwrap();
        let parsed: serde_json::Value = serde_json::from_str(&output).unwrap();
        assert_eq!(parsed["total_code_units"], 50);
        assert_eq!(parsed["exact_duplicate_groups"], 3);
    }

    #[test]
    fn json_report_exact_empty() {
        let reporter = JsonReporter::new(None);
        let mut buf = Vec::new();
        reporter.report_exact(&[], &mut buf).unwrap();
        let output = String::from_utf8(buf).unwrap();
        let parsed: serde_json::Value = serde_json::from_str(&output).unwrap();
        assert!(parsed.as_array().unwrap().is_empty());
    }

    #[test]
    fn json_report_exact_with_groups() {
        let reporter = JsonReporter::new(Some(PathBuf::from("/project")));
        let group = DuplicateGroup {
            fingerprint: Fingerprint::from_node(&NormalizedNode::Opaque),
            members: vec![
                make_unit("foo", "/project/src/a.rs", 10, 20),
                make_unit("bar", "/project/src/b.rs", 30, 40),
            ],
            similarity: 1.0,
        };
        let mut buf = Vec::new();
        reporter.report_exact(&[group], &mut buf).unwrap();
        let output = String::from_utf8(buf).unwrap();
        let parsed: serde_json::Value = serde_json::from_str(&output).unwrap();
        let groups = parsed.as_array().unwrap();
        assert_eq!(groups.len(), 1);
        assert_eq!(groups[0]["members"].as_array().unwrap().len(), 2);
        assert_eq!(groups[0]["similarity"], 1.0);
        assert!(groups[0]["fingerprint"].is_string());
    }

    #[test]
    fn json_report_near_with_groups() {
        let reporter = JsonReporter::new(None);
        let fp = Fingerprint::from_node(&NormalizedNode::Block(vec![]));
        let group = DuplicateGroup {
            fingerprint: fp,
            members: vec![
                make_unit("process", "/src/a.rs", 10, 25),
                make_unit("compute", "/src/b.rs", 30, 45),
            ],
            similarity: 0.85,
        };
        let mut buf = Vec::new();
        reporter.report_near(&[group], &mut buf).unwrap();
        let output = String::from_utf8(buf).unwrap();
        let parsed: serde_json::Value = serde_json::from_str(&output).unwrap();
        let groups = parsed.as_array().unwrap();
        assert_eq!(groups.len(), 1);
        assert_eq!(groups[0]["fingerprint"].as_str().unwrap(), fp.to_hex());
        assert_eq!(groups[0]["similarity"], 0.85);
    }

    #[test]
    fn json_is_valid() {
        let reporter = JsonReporter::new(Some(PathBuf::from("/project")));
        let group = DuplicateGroup {
            fingerprint: Fingerprint::from_node(&NormalizedNode::Opaque),
            members: vec![make_unit("foo", "/project/src/a.rs", 10, 20)],
            similarity: 1.0,
        };
        let mut buf = Vec::new();
        reporter.report_exact(&[group], &mut buf).unwrap();
        let output = String::from_utf8(buf).unwrap();
        // Should be valid JSON
        assert!(serde_json::from_str::<serde_json::Value>(&output).is_ok());
    }

    #[test]
    fn json_relative_paths() {
        let reporter = JsonReporter::new(Some(PathBuf::from("/home/user/project")));
        let fp = Fingerprint::from_node(&NormalizedNode::Block(vec![]));
        let group = DuplicateGroup {
            fingerprint: fp,
            members: vec![make_unit("foo", "/home/user/project/src/main.rs", 1, 10)],
            similarity: 0.9,
        };
        let mut buf = Vec::new();
        reporter.report_near(&[group], &mut buf).unwrap();
        let output = String::from_utf8(buf).unwrap();
        assert!(output.contains("src/main.rs"));
        assert!(!output.contains("/home/user/project"));
    }
}
