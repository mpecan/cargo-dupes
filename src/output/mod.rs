pub mod json;
pub mod text;

use std::io;

use crate::grouper::{DuplicateGroup, DuplicationStats};

/// Trait for reporting analysis results.
pub trait Reporter {
    fn report_stats(&self, stats: &DuplicationStats, writer: &mut dyn io::Write) -> io::Result<()>;
    fn report_exact(&self, groups: &[DuplicateGroup], writer: &mut dyn io::Write)
    -> io::Result<()>;
    fn report_near(&self, groups: &[DuplicateGroup], writer: &mut dyn io::Write) -> io::Result<()>;
}
