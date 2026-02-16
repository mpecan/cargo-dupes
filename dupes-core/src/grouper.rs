use std::collections::HashMap;

use crate::code_unit::{CodeUnit, CodeUnitKind};
use crate::fingerprint::Fingerprint;
use crate::similarity;

/// A group of duplicate code units.
#[derive(Debug, Clone)]
pub struct DuplicateGroup {
    /// Shared fingerprint for exact duplicates, or composite fingerprint
    /// (derived from sorted member fingerprints) for near-duplicate groups.
    pub fingerprint: Fingerprint,
    /// The code units in this group.
    pub members: Vec<CodeUnit>,
    /// Similarity score (1.0 for exact duplicates).
    pub similarity: f64,
}

/// Statistics about duplication in the analyzed codebase.
#[derive(Debug, Clone, serde::Serialize)]
pub struct DuplicationStats {
    pub total_code_units: usize,
    pub total_lines: usize,
    pub exact_duplicate_groups: usize,
    pub exact_duplicate_units: usize,
    pub near_duplicate_groups: usize,
    pub near_duplicate_units: usize,
    pub exact_duplicate_lines: usize,
    pub near_duplicate_lines: usize,
    // Sub-function stats
    pub sub_exact_groups: usize,
    pub sub_exact_units: usize,
    pub sub_near_groups: usize,
    pub sub_near_units: usize,
}

impl DuplicationStats {
    /// Percentage of total lines that are exact duplicates.
    pub fn exact_duplicate_percent(&self) -> f64 {
        if self.total_lines == 0 {
            0.0
        } else {
            self.exact_duplicate_lines as f64 / self.total_lines as f64 * 100.0
        }
    }

    /// Percentage of total lines that are near duplicates.
    pub fn near_duplicate_percent(&self) -> f64 {
        if self.total_lines == 0 {
            0.0
        } else {
            self.near_duplicate_lines as f64 / self.total_lines as f64 * 100.0
        }
    }
}

/// Group code units by exact fingerprint match.
pub fn group_exact_duplicates(units: &[CodeUnit]) -> Vec<DuplicateGroup> {
    let mut groups: HashMap<Fingerprint, Vec<CodeUnit>> = HashMap::new();

    for unit in units {
        groups
            .entry(unit.fingerprint)
            .or_default()
            .push(unit.clone());
    }

    let mut result: Vec<DuplicateGroup> = groups
        .into_iter()
        .filter(|(_, members)| members.len() > 1)
        .map(|(fp, members)| DuplicateGroup {
            fingerprint: fp,
            members,
            similarity: 1.0,
        })
        .collect();

    // Sort by group size (largest first), then by fingerprint for stability
    result.sort_by(|a, b| {
        b.members
            .len()
            .cmp(&a.members.len())
            .then_with(|| a.fingerprint.cmp(&b.fingerprint))
    });

    result
}

/// Find near-duplicate groups above the similarity threshold.
/// Pre-filters by CodeUnitKind and approximate size to reduce pairwise comparisons.
pub fn find_near_duplicates(
    units: &[CodeUnit],
    threshold: f64,
    exact_fingerprints: &[Fingerprint],
) -> Vec<DuplicateGroup> {
    // Build set of fingerprints that are already exact duplicates
    let exact_set: std::collections::HashSet<Fingerprint> =
        exact_fingerprints.iter().copied().collect();

    // Filter out units that are already in exact duplicate groups
    let candidates: Vec<&CodeUnit> = units
        .iter()
        .filter(|u| !exact_set.contains(&u.fingerprint))
        .collect();

    if candidates.len() < 2 {
        return Vec::new();
    }

    // Bucket by kind and approximate size range
    let mut buckets: HashMap<(CodeUnitKind, usize), Vec<&CodeUnit>> = HashMap::new();
    for unit in &candidates {
        // Size bucket: group units within 2x of each other
        let size_bucket = if unit.node_count == 0 {
            0
        } else {
            (unit.node_count as f64).log2().floor() as usize
        };
        buckets
            .entry((unit.kind.clone(), size_bucket))
            .or_default()
            .push(unit);
    }

    // Pairwise comparison within buckets
    let mut pairs: Vec<(usize, usize, f64)> = Vec::new();
    let unit_indices: HashMap<*const CodeUnit, usize> = candidates
        .iter()
        .enumerate()
        .map(|(i, u)| (*u as *const CodeUnit, i))
        .collect();

    for bucket in buckets.values() {
        if bucket.len() < 2 {
            continue;
        }
        for i in 0..bucket.len() {
            for j in (i + 1)..bucket.len() {
                let score = similarity::similarity_score(&bucket[i].body, &bucket[j].body);
                if score >= threshold {
                    let idx_i = unit_indices[&(bucket[i] as *const CodeUnit)];
                    let idx_j = unit_indices[&(bucket[j] as *const CodeUnit)];
                    pairs.push((idx_i, idx_j, score));
                }
            }
        }
    }

    // Build groups via transitive closure using union-find
    let mut parent: Vec<usize> = (0..candidates.len()).collect();
    let mut scores: HashMap<(usize, usize), f64> = HashMap::new();

    for &(i, j, score) in &pairs {
        union(&mut parent, i, j);
        let key = (i.min(j), i.max(j));
        scores.insert(key, score);
    }

    // Collect groups
    let mut group_map: HashMap<usize, Vec<usize>> = HashMap::new();
    for i in 0..candidates.len() {
        let root = find(&mut parent, i);
        group_map.entry(root).or_default().push(i);
    }

    let mut result: Vec<DuplicateGroup> = group_map
        .into_values()
        .filter(|members| members.len() > 1)
        .map(|member_indices| {
            // Compute minimum similarity within the group
            let mut min_score = f64::INFINITY;
            for &i in &member_indices {
                for &j in &member_indices {
                    if i < j
                        && let Some(&s) = scores.get(&(i, j))
                        && s < min_score
                    {
                        min_score = s;
                    }
                }
            }

            let members: Vec<CodeUnit> = member_indices
                .iter()
                .map(|&i| candidates[i].clone())
                .collect();

            let member_fps: Vec<Fingerprint> = members.iter().map(|m| m.fingerprint).collect();
            let composite_fp = Fingerprint::from_fingerprints(&member_fps);

            DuplicateGroup {
                fingerprint: composite_fp,
                members,
                similarity: if min_score.is_infinite() {
                    threshold
                } else {
                    min_score
                },
            }
        })
        .collect();

    result.sort_by(|a, b| {
        b.members
            .len()
            .cmp(&a.members.len())
            .then_with(|| {
                b.similarity
                    .partial_cmp(&a.similarity)
                    .unwrap_or(std::cmp::Ordering::Equal)
            })
            .then_with(|| a.fingerprint.cmp(&b.fingerprint))
    });

    result
}

/// Compute the total number of source lines in a duplicate group.
fn group_line_count(group: &DuplicateGroup) -> usize {
    group
        .members
        .iter()
        .map(|m| m.line_end.saturating_sub(m.line_start) + 1)
        .sum()
}

/// Compute duplication statistics.
pub fn compute_stats(
    units: &[CodeUnit],
    exact_groups: &[DuplicateGroup],
    near_groups: &[DuplicateGroup],
) -> DuplicationStats {
    let total_lines: usize = units
        .iter()
        .map(|u| u.line_end.saturating_sub(u.line_start) + 1)
        .sum();

    DuplicationStats {
        total_code_units: units.len(),
        total_lines,
        exact_duplicate_groups: exact_groups.len(),
        exact_duplicate_units: exact_groups.iter().map(|g| g.members.len()).sum(),
        near_duplicate_groups: near_groups.len(),
        near_duplicate_units: near_groups.iter().map(|g| g.members.len()).sum(),
        exact_duplicate_lines: exact_groups.iter().map(group_line_count).sum(),
        near_duplicate_lines: near_groups.iter().map(group_line_count).sum(),
        sub_exact_groups: 0,
        sub_exact_units: 0,
        sub_near_groups: 0,
        sub_near_units: 0,
    }
}

/// Compute duplication statistics including sub-function results.
pub fn compute_stats_with_sub(
    units: &[CodeUnit],
    exact_groups: &[DuplicateGroup],
    near_groups: &[DuplicateGroup],
    sub_exact_groups: &[DuplicateGroup],
    sub_near_groups: &[DuplicateGroup],
) -> DuplicationStats {
    let mut stats = compute_stats(units, exact_groups, near_groups);
    stats.sub_exact_groups = sub_exact_groups.len();
    stats.sub_exact_units = sub_exact_groups.iter().map(|g| g.members.len()).sum();
    stats.sub_near_groups = sub_near_groups.len();
    stats.sub_near_units = sub_near_groups.iter().map(|g| g.members.len()).sum();
    stats
}

// ── Union-Find helpers ──────────────────────────────────────────────────

fn find(parent: &mut [usize], i: usize) -> usize {
    if parent[i] != i {
        parent[i] = find(parent, parent[i]);
    }
    parent[i]
}

fn union(parent: &mut [usize], i: usize, j: usize) {
    let ri = find(parent, i);
    let rj = find(parent, j);
    if ri != rj {
        parent[ri] = rj;
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn empty_input_no_groups() {
        let groups = group_exact_duplicates(&[]);
        assert!(groups.is_empty());
    }

    #[test]
    fn percentage_helpers() {
        let stats = DuplicationStats {
            total_code_units: 10,
            total_lines: 200,
            exact_duplicate_groups: 2,
            exact_duplicate_units: 4,
            near_duplicate_groups: 1,
            near_duplicate_units: 3,
            exact_duplicate_lines: 50,
            near_duplicate_lines: 30,
            sub_exact_groups: 0,
            sub_exact_units: 0,
            sub_near_groups: 0,
            sub_near_units: 0,
        };
        assert!((stats.exact_duplicate_percent() - 25.0).abs() < f64::EPSILON);
        assert!((stats.near_duplicate_percent() - 15.0).abs() < f64::EPSILON);
    }

    #[test]
    fn percentage_helpers_zero_total() {
        let stats = DuplicationStats {
            total_code_units: 0,
            total_lines: 0,
            exact_duplicate_groups: 0,
            exact_duplicate_units: 0,
            near_duplicate_groups: 0,
            near_duplicate_units: 0,
            exact_duplicate_lines: 0,
            near_duplicate_lines: 0,
            sub_exact_groups: 0,
            sub_exact_units: 0,
            sub_near_groups: 0,
            sub_near_units: 0,
        };
        assert!((stats.exact_duplicate_percent() - 0.0).abs() < f64::EPSILON);
        assert!((stats.near_duplicate_percent() - 0.0).abs() < f64::EPSILON);
    }
}
