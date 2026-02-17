use std::io;

use crate::grouper::{DuplicateGroup, DuplicationStats};
use crate::output::{Reporter, display_path};

fn format_with_commas(n: usize) -> String {
    let s = n.to_string();
    let mut result = String::with_capacity(s.len() + s.len() / 3);
    for (i, c) in s.chars().enumerate() {
        if i > 0 && (s.len() - i).is_multiple_of(3) {
            result.push(',');
        }
        result.push(c);
    }
    result
}

pub struct TextReporter {
    /// Base path for displaying relative paths.
    pub base_path: Option<std::path::PathBuf>,
}

impl TextReporter {
    #[must_use]
    pub const fn new(base_path: Option<std::path::PathBuf>) -> Self {
        Self { base_path }
    }

    fn write_groups(
        &self,
        groups: &[DuplicateGroup],
        writer: &mut dyn io::Write,
        title: &str,
        empty_message: Option<&str>,
        show_similarity: bool,
        show_parent: bool,
    ) -> io::Result<()> {
        if groups.is_empty() {
            if let Some(msg) = empty_message {
                writeln!(writer, "{msg}")?;
            }
            return Ok(());
        }

        writeln!(writer, "{title}")?;
        writeln!(writer, "{}", "=".repeat(title.len()))?;
        writeln!(writer)?;

        for (i, group) in groups.iter().enumerate() {
            let fp = group.fingerprint.to_hex();
            if show_similarity {
                writeln!(
                    writer,
                    "Group {} (fingerprint: {}, similarity: {:.0}%, {} members):",
                    i + 1,
                    fp,
                    group.similarity * 100.0,
                    group.members.len()
                )?;
            } else {
                writeln!(
                    writer,
                    "Group {} (fingerprint: {}, {} members):",
                    i + 1,
                    fp,
                    group.members.len()
                )?;
            }
            for member in &group.members {
                let parent = if show_parent {
                    member
                        .parent_name
                        .as_deref()
                        .map(|p| format!(" in {p}"))
                        .unwrap_or_default()
                } else {
                    String::new()
                };
                writeln!(
                    writer,
                    "  - {} ({}){} at {}:{}-{}",
                    member.name,
                    member.kind,
                    parent,
                    display_path(self.base_path.as_deref(), &member.file),
                    member.line_start,
                    member.line_end,
                )?;
            }
            writeln!(writer)?;
        }
        Ok(())
    }
}

impl Reporter for TextReporter {
    fn report_stats(&self, stats: &DuplicationStats, writer: &mut dyn io::Write) -> io::Result<()> {
        writeln!(writer, "Duplication Statistics")?;
        writeln!(writer, "=====================")?;
        writeln!(
            writer,
            "Total code units analyzed: {}",
            stats.total_code_units
        )?;
        writeln!(writer)?;
        writeln!(
            writer,
            "Exact duplicates: {} groups ({} code units)",
            stats.exact_duplicate_groups, stats.exact_duplicate_units
        )?;
        writeln!(
            writer,
            "Near duplicates:  {} groups ({} code units)",
            stats.near_duplicate_groups, stats.near_duplicate_units
        )?;
        writeln!(writer)?;
        writeln!(
            writer,
            "Duplicated lines (exact): {}",
            stats.exact_duplicate_lines
        )?;
        writeln!(
            writer,
            "Duplicated lines (near):  {}",
            stats.near_duplicate_lines
        )?;
        writeln!(
            writer,
            "Duplication: {:.1}% exact, {:.1}% near (of {} total lines)",
            stats.exact_duplicate_percent(),
            stats.near_duplicate_percent(),
            format_with_commas(stats.total_lines),
        )?;
        if stats.sub_exact_groups > 0 || stats.sub_near_groups > 0 {
            writeln!(writer)?;
            writeln!(
                writer,
                "Sub-function exact: {} groups ({} units)",
                stats.sub_exact_groups, stats.sub_exact_units
            )?;
            writeln!(
                writer,
                "Sub-function near:  {} groups ({} units)",
                stats.sub_near_groups, stats.sub_near_units
            )?;
        }
        Ok(())
    }

    fn report_exact(
        &self,
        groups: &[DuplicateGroup],
        writer: &mut dyn io::Write,
    ) -> io::Result<()> {
        self.write_groups(
            groups,
            writer,
            "Exact Duplicates",
            Some("No exact duplicates found."),
            false,
            false,
        )
    }

    fn report_near(&self, groups: &[DuplicateGroup], writer: &mut dyn io::Write) -> io::Result<()> {
        self.write_groups(
            groups,
            writer,
            "Near Duplicates",
            Some("No near duplicates found."),
            true,
            false,
        )
    }

    fn report_sub_exact(
        &self,
        groups: &[DuplicateGroup],
        writer: &mut dyn io::Write,
    ) -> io::Result<()> {
        self.write_groups(
            groups,
            writer,
            "Sub-function Exact Duplicates",
            None,
            false,
            true,
        )
    }

    fn report_sub_near(
        &self,
        groups: &[DuplicateGroup],
        writer: &mut dyn io::Write,
    ) -> io::Result<()> {
        self.write_groups(
            groups,
            writer,
            "Sub-function Near Duplicates",
            None,
            true,
            true,
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::code_unit::{CodeUnit, CodeUnitKind};
    use crate::fingerprint::Fingerprint;
    use crate::node::{NodeKind, NormalizedNode};
    use std::path::PathBuf;

    fn make_unit(name: &str, file: &str, line_start: usize, line_end: usize) -> CodeUnit {
        CodeUnit {
            kind: CodeUnitKind::Function,
            name: name.to_string(),
            file: PathBuf::from(file),
            line_start,
            line_end,
            signature: NormalizedNode::leaf(NodeKind::Opaque),
            body: NormalizedNode::with_children(NodeKind::Block, vec![]),
            fingerprint: Fingerprint::from_node(&NormalizedNode::leaf(NodeKind::Opaque)),
            node_count: 10,
            parent_name: None,
            is_test: false,
        }
    }

    #[test]
    fn text_report_stats() {
        let reporter = TextReporter::new(None);
        let stats = DuplicationStats {
            total_code_units: 100,
            total_lines: 1000,
            exact_duplicate_groups: 5,
            exact_duplicate_units: 12,
            near_duplicate_groups: 3,
            near_duplicate_units: 8,
            exact_duplicate_lines: 60,
            near_duplicate_lines: 40,
            sub_exact_groups: 0,
            sub_exact_units: 0,
            sub_near_groups: 0,
            sub_near_units: 0,
        };
        let mut buf = Vec::new();
        reporter.report_stats(&stats, &mut buf).unwrap();
        let output = String::from_utf8(buf).unwrap();
        assert!(output.contains("100"));
        assert!(output.contains("5 groups"));
        assert!(output.contains("3 groups"));
    }

    #[test]
    fn text_report_exact_empty() {
        let reporter = TextReporter::new(None);
        let mut buf = Vec::new();
        reporter.report_exact(&[], &mut buf).unwrap();
        let output = String::from_utf8(buf).unwrap();
        assert!(output.contains("No exact duplicates"));
    }

    #[test]
    fn text_report_exact_with_groups() {
        let reporter = TextReporter::new(Some(PathBuf::from("/project")));
        let group = DuplicateGroup {
            fingerprint: Fingerprint::from_node(&NormalizedNode::leaf(NodeKind::Opaque)),
            members: vec![
                make_unit("foo", "/project/src/a.rs", 10, 20),
                make_unit("bar", "/project/src/b.rs", 30, 40),
            ],
            similarity: 1.0,
        };
        let mut buf = Vec::new();
        reporter.report_exact(&[group], &mut buf).unwrap();
        let output = String::from_utf8(buf).unwrap();
        assert!(output.contains("Group 1"));
        assert!(output.contains("foo"));
        assert!(output.contains("bar"));
        assert!(output.contains("src/a.rs"));
        assert!(output.contains("src/b.rs"));
    }

    #[test]
    fn text_report_near_with_groups() {
        let reporter = TextReporter::new(None);
        let fp = Fingerprint::from_node(&NormalizedNode::with_children(NodeKind::Block, vec![]));
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
        assert!(output.contains("fingerprint:"));
        assert!(output.contains(&fp.to_hex()));
        assert!(output.contains("85%"));
        assert!(output.contains("process"));
        assert!(output.contains("compute"));
    }

    #[test]
    fn text_report_near_empty() {
        let reporter = TextReporter::new(None);
        let mut buf = Vec::new();
        reporter.report_near(&[], &mut buf).unwrap();
        let output = String::from_utf8(buf).unwrap();
        assert!(output.contains("No near duplicates"));
    }

    #[test]
    fn relative_path_stripping() {
        let base = PathBuf::from("/home/user/project");
        let result = display_path(
            Some(base.as_path()),
            std::path::Path::new("/home/user/project/src/main.rs"),
        );
        assert_eq!(result, "src/main.rs");
    }
}
