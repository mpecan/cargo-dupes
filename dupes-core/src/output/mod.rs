pub mod json;
pub mod text;

use std::borrow::Cow;
use std::io;
use std::path::Path;

use crate::grouper::{DuplicateGroup, DuplicationStats};

/// Compute a display path relative to an optional base, falling back to the absolute path.
#[must_use]
pub fn display_path<'a>(base: Option<&Path>, path: &'a Path) -> Cow<'a, str> {
    if let Some(base) = base
        && let Ok(rel) = path.strip_prefix(base)
    {
        return rel.to_string_lossy();
    }
    path.to_string_lossy()
}

/// Trait for reporting analysis results.
pub trait Reporter {
    fn report_stats(&self, stats: &DuplicationStats, writer: &mut dyn io::Write) -> io::Result<()>;
    fn report_exact(&self, groups: &[DuplicateGroup], writer: &mut dyn io::Write)
    -> io::Result<()>;
    fn report_near(&self, groups: &[DuplicateGroup], writer: &mut dyn io::Write) -> io::Result<()>;
    fn report_sub_exact(
        &self,
        groups: &[DuplicateGroup],
        writer: &mut dyn io::Write,
    ) -> io::Result<()>;
    fn report_sub_near(
        &self,
        groups: &[DuplicateGroup],
        writer: &mut dyn io::Write,
    ) -> io::Result<()>;
}
