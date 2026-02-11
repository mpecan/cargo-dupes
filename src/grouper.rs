use std::collections::HashMap;

use crate::fingerprint::Fingerprint;
use crate::parser::{CodeUnit, CodeUnitKind};
use crate::similarity;

/// A group of duplicate code units.
#[derive(Debug, Clone)]
pub struct DuplicateGroup {
    /// Shared fingerprint (for exact duplicates) or None (for near-duplicates).
    pub fingerprint: Option<Fingerprint>,
    /// The code units in this group.
    pub members: Vec<CodeUnit>,
    /// Similarity score (1.0 for exact duplicates).
    pub similarity: f64,
}

/// Statistics about duplication in the analyzed codebase.
#[derive(Debug, Clone, serde::Serialize)]
pub struct DuplicationStats {
    pub total_code_units: usize,
    pub exact_duplicate_groups: usize,
    pub exact_duplicate_units: usize,
    pub near_duplicate_groups: usize,
    pub near_duplicate_units: usize,
    pub exact_duplicate_lines: usize,
    pub near_duplicate_lines: usize,
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
            fingerprint: Some(fp),
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

            DuplicateGroup {
                fingerprint: None,
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
        b.members.len().cmp(&a.members.len()).then_with(|| {
            b.similarity
                .partial_cmp(&a.similarity)
                .unwrap_or(std::cmp::Ordering::Equal)
        })
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
    total_units: usize,
    exact_groups: &[DuplicateGroup],
    near_groups: &[DuplicateGroup],
) -> DuplicationStats {
    DuplicationStats {
        total_code_units: total_units,
        exact_duplicate_groups: exact_groups.len(),
        exact_duplicate_units: exact_groups.iter().map(|g| g.members.len()).sum(),
        near_duplicate_groups: near_groups.len(),
        near_duplicate_units: near_groups.iter().map(|g| g.members.len()).sum(),
        exact_duplicate_lines: exact_groups.iter().map(group_line_count).sum(),
        near_duplicate_lines: near_groups.iter().map(group_line_count).sum(),
    }
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
    use crate::parser;
    use std::fs;
    use tempfile::TempDir;

    fn make_units(code: &str) -> Vec<CodeUnit> {
        let tmp = TempDir::new().unwrap();
        let file = tmp.path().join("test.rs");
        fs::write(&file, code).unwrap();
        parser::parse_file(&file, 1, 0).unwrap()
    }

    #[test]
    fn exact_duplicates_grouped() {
        let units = make_units(
            r#"
            fn foo(x: i32) -> i32 {
                let y = x + 1;
                y * 2
            }
            fn bar(a: i32) -> i32 {
                let b = a + 1;
                b * 2
            }
            fn unique(x: i32) -> i32 {
                x * x * x
            }
            "#,
        );
        let groups = group_exact_duplicates(&units);
        assert_eq!(groups.len(), 1);
        assert_eq!(groups[0].members.len(), 2);
        assert!((groups[0].similarity - 1.0).abs() < f64::EPSILON);
    }

    #[test]
    fn no_duplicates_no_groups() {
        let units = make_units(
            r#"
            fn add(x: i32) -> i32 { x + 1 }
            fn mul(x: i32) -> i32 { x * 2 }
            fn sub(x: i32) -> i32 { x - 3 }
            "#,
        );
        let groups = group_exact_duplicates(&units);
        assert!(groups.is_empty());
    }

    #[test]
    fn multiple_exact_groups() {
        let units = make_units(
            r#"
            fn a1(x: i32) -> i32 { x + 1 }
            fn a2(y: i32) -> i32 { y + 1 }
            fn b1(x: i32) -> i32 { x * 2 }
            fn b2(y: i32) -> i32 { y * 2 }
            "#,
        );
        let groups = group_exact_duplicates(&units);
        assert_eq!(groups.len(), 2);
    }

    #[test]
    fn near_duplicates_found() {
        let units = make_units(
            r#"
            fn process(data: i32) -> i32 {
                let a = data + 1;
                let b = a * 2;
                let c = b - 3;
                a + b + c
            }
            fn compute(value: i32) -> i32 {
                let a = value + 1;
                let b = a * 2;
                let c = b - 4;
                a + b + c
            }
            "#,
        );
        let exact = group_exact_duplicates(&units);
        let exact_fps: Vec<_> = exact.iter().filter_map(|g| g.fingerprint).collect();
        let near = find_near_duplicates(&units, 0.7, &exact_fps);
        // If they're exact they won't be in near, if slightly different they'll be near
        assert!(exact.len() + near.len() >= 1);
    }

    #[test]
    fn stats_computation() {
        let units = make_units(
            r#"
            fn a(x: i32) -> i32 { x + 1 }
            fn b(y: i32) -> i32 { y + 1 }
            fn c(x: i32) -> i32 { x * 2 }
            "#,
        );
        let exact = group_exact_duplicates(&units);
        let stats = compute_stats(units.len(), &exact, &[]);
        assert_eq!(stats.total_code_units, 3);
        assert_eq!(stats.exact_duplicate_groups, 1);
        assert_eq!(stats.exact_duplicate_units, 2);
    }

    #[test]
    fn empty_input_no_groups() {
        let groups = group_exact_duplicates(&[]);
        assert!(groups.is_empty());
    }

    #[test]
    fn single_unit_no_groups() {
        let units = make_units("fn solo(x: i32) -> i32 { x + 1 }");
        let groups = group_exact_duplicates(&units);
        assert!(groups.is_empty());
    }

    #[test]
    fn exact_groups_sorted_by_size() {
        let units = make_units(
            r#"
            fn a1(x: i32) -> i32 { x + 1 }
            fn a2(y: i32) -> i32 { y + 1 }
            fn a3(z: i32) -> i32 { z + 1 }
            fn b1(x: i32) -> i32 { x * 2 }
            fn b2(y: i32) -> i32 { y * 2 }
            "#,
        );
        let groups = group_exact_duplicates(&units);
        assert_eq!(groups.len(), 2);
        assert!(groups[0].members.len() >= groups[1].members.len());
    }

    #[test]
    fn near_duplicates_exclude_exact() {
        let units = make_units(
            r#"
            fn a(x: i32) -> i32 { x + 1 }
            fn b(y: i32) -> i32 { y + 1 }
            "#,
        );
        let exact = group_exact_duplicates(&units);
        let exact_fps: Vec<_> = exact.iter().filter_map(|g| g.fingerprint).collect();
        let near = find_near_duplicates(&units, 0.7, &exact_fps);
        // These are exact duplicates, so they should not appear in near
        assert!(near.is_empty());
    }

    #[test]
    fn duplicate_group_has_fingerprint() {
        let units = make_units(
            r#"
            fn a(x: i32) -> i32 { x + 1 }
            fn b(y: i32) -> i32 { y + 1 }
            "#,
        );
        let groups = group_exact_duplicates(&units);
        assert_eq!(groups.len(), 1);
        assert!(groups[0].fingerprint.is_some());
    }

    #[test]
    fn stats_with_near_duplicates() {
        let near_group = DuplicateGroup {
            fingerprint: None,
            members: vec![], // fake empty for stats test
            similarity: 0.85,
        };
        let stats = compute_stats(10, &[], &[near_group]);
        assert_eq!(stats.total_code_units, 10);
        assert_eq!(stats.near_duplicate_groups, 1);
    }

    #[test]
    fn stats_includes_line_counts() {
        let units = make_units(
            r#"
            fn foo(x: i32) -> i32 {
                let y = x + 1;
                y * 2
            }
            fn bar(a: i32) -> i32 {
                let b = a + 1;
                b * 2
            }
            "#,
        );
        let exact = group_exact_duplicates(&units);
        let stats = compute_stats(units.len(), &exact, &[]);
        assert!(stats.exact_duplicate_lines > 0);
        assert_eq!(stats.near_duplicate_lines, 0);
    }
}
