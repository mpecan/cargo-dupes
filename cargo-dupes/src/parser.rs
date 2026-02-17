use std::path::{Path, PathBuf};

use syn::visit::Visit;

use dupes_core::fingerprint::Fingerprint;
use dupes_core::node::NormalizedNode;

use crate::normalizer;

pub use dupes_core::code_unit::{CodeUnit, CodeUnitKind};

/// Check if attributes contain `#[test]`.
fn has_test_attr(attrs: &[syn::Attribute]) -> bool {
    attrs.iter().any(|attr| attr.path().is_ident("test"))
}

/// Check if attributes contain `#[cfg(test)]`.
fn has_cfg_test_attr(attrs: &[syn::Attribute]) -> bool {
    attrs.iter().any(|attr| {
        attr.path().is_ident("cfg")
            && attr
                .parse_args::<syn::Ident>()
                .is_ok_and(|ident| ident == "test")
    })
}

/// Extracts code units from a syn file by visiting the AST.
struct CodeUnitExtractor {
    file: PathBuf,
    min_node_count: usize,
    min_line_count: usize,
    exclude_tests: bool,
    units: Vec<CodeUnit>,
    /// Track current impl block context for method naming.
    current_impl: Option<String>,
    /// Track if we're in a trait impl
    in_trait_impl: bool,
}

impl CodeUnitExtractor {
    fn new(
        file: PathBuf,
        min_node_count: usize,
        min_line_count: usize,
        exclude_tests: bool,
    ) -> Self {
        Self {
            file,
            min_node_count,
            min_line_count,
            exclude_tests,
            units: Vec::new(),
            current_impl: None,
            in_trait_impl: false,
        }
    }

    fn add_unit(
        &mut self,
        kind: CodeUnitKind,
        name: String,
        line_start: usize,
        line_end: usize,
        sig: NormalizedNode,
        body: NormalizedNode,
    ) {
        let node_count = normalizer::count_nodes(&sig) + normalizer::count_nodes(&body);
        if node_count < self.min_node_count {
            return;
        }
        let line_count = line_end.saturating_sub(line_start) + 1;
        if self.min_line_count > 0 && line_count < self.min_line_count {
            return;
        }
        let fingerprint = Fingerprint::from_sig_and_body(&sig, &body);
        self.units.push(CodeUnit {
            kind,
            name,
            file: self.file.clone(),
            line_start,
            line_end,
            signature: sig,
            body,
            fingerprint,
            node_count,
            parent_name: None,
        });
    }
}

impl<'ast> Visit<'ast> for CodeUnitExtractor {
    fn visit_item_fn(&mut self, node: &'ast syn::ItemFn) {
        if self.exclude_tests && (has_test_attr(&node.attrs) || has_cfg_test_attr(&node.attrs)) {
            return;
        }

        let name = node.sig.ident.to_string();
        let line_start = node.sig.ident.span().start().line;
        let line_end = node.block.brace_token.span.close().end().line;
        let (sig, body) = normalizer::normalize_item_fn(node);
        self.add_unit(
            CodeUnitKind::Function,
            name,
            line_start,
            line_end,
            sig,
            body,
        );

        // Continue visiting nested items
        syn::visit::visit_item_fn(self, node);
    }

    fn visit_item_mod(&mut self, node: &'ast syn::ItemMod) {
        if self.exclude_tests && has_cfg_test_attr(&node.attrs) {
            return;
        }
        syn::visit::visit_item_mod(self, node);
    }

    fn visit_item_impl(&mut self, node: &'ast syn::ItemImpl) {
        if self.exclude_tests && has_cfg_test_attr(&node.attrs) {
            return;
        }

        let type_name = quote_type(&node.self_ty);
        let is_trait_impl = node.trait_.is_some();
        let trait_name = node
            .trait_
            .as_ref()
            .map(|(_, path, _)| {
                path.segments
                    .iter()
                    .map(|s| s.ident.to_string())
                    .collect::<Vec<_>>()
                    .join("::")
            })
            .unwrap_or_default();

        let prev_impl = self.current_impl.take();
        let prev_trait = self.in_trait_impl;

        self.current_impl = Some(type_name.clone());
        self.in_trait_impl = is_trait_impl;

        // Visit each method in the impl block
        for item in &node.items {
            if let syn::ImplItem::Fn(method) = item {
                let method_name = method.sig.ident.to_string();
                let full_name = if is_trait_impl {
                    format!("<{type_name} as {trait_name}>::{method_name}")
                } else {
                    format!("{type_name}::{method_name}")
                };

                let line_start = method.sig.ident.span().start().line;
                let line_end = method.block.brace_token.span.close().end().line;

                let (sig, body) = normalizer::normalize_impl_item_fn(method);
                let kind = if is_trait_impl {
                    CodeUnitKind::TraitImplBlock
                } else {
                    CodeUnitKind::Method
                };

                self.add_unit(kind, full_name, line_start, line_end, sig, body);
            }
        }

        self.current_impl = prev_impl;
        self.in_trait_impl = prev_trait;
    }

    fn visit_expr_closure(&mut self, node: &'ast syn::ExprClosure) {
        let line_start = node.or1_token.span.start().line;
        let line_end = line_start; // closures often span one expression

        let normalized = normalizer::normalize_closure_expr(node);
        let node_count = normalizer::count_nodes(&normalized);
        let line_count = line_end.saturating_sub(line_start) + 1;
        if node_count >= self.min_node_count
            && (self.min_line_count == 0 || line_count >= self.min_line_count)
        {
            let name = format!("closure at {}:{}", self.file.display(), line_start);
            let fingerprint = Fingerprint::from_node(&normalized);
            self.units.push(CodeUnit {
                kind: CodeUnitKind::Closure,
                name,
                file: self.file.clone(),
                line_start,
                line_end,
                signature: NormalizedNode::Opaque,
                body: normalized,
                fingerprint,
                node_count,
                parent_name: None,
            });
        }

        // Continue visiting nested closures
        syn::visit::visit_expr_closure(self, node);
    }
}

/// Get a simple string representation of a type for naming.
fn quote_type(ty: &syn::Type) -> String {
    match ty {
        syn::Type::Path(tp) => tp
            .path
            .segments
            .iter()
            .map(|s| s.ident.to_string())
            .collect::<Vec<_>>()
            .join("::"),
        _ => "Unknown".to_string(),
    }
}

/// Parse a single Rust file and extract code units.
pub fn parse_file(
    path: &Path,
    min_node_count: usize,
    min_line_count: usize,
    exclude_tests: bool,
) -> Result<Vec<CodeUnit>, String> {
    let content = std::fs::read_to_string(path)
        .map_err(|e| format!("Failed to read {}: {}", path.display(), e))?;

    let file = syn::parse_file(&content)
        .map_err(|e| format!("Failed to parse {}: {}", path.display(), e))?;

    let mut extractor = CodeUnitExtractor::new(
        path.to_path_buf(),
        min_node_count,
        min_line_count,
        exclude_tests,
    );
    extractor.visit_file(&file);

    Ok(extractor.units)
}

/// Parse multiple files and collect all code units, skipping files that fail to parse.
pub fn parse_files(
    paths: &[PathBuf],
    min_node_count: usize,
    min_line_count: usize,
    exclude_tests: bool,
) -> (Vec<CodeUnit>, Vec<String>) {
    let mut units = Vec::new();
    let mut warnings = Vec::new();

    for path in paths {
        match parse_file(path, min_node_count, min_line_count, exclude_tests) {
            Ok(file_units) => units.extend(file_units),
            Err(warning) => warnings.push(warning),
        }
    }

    (units, warnings)
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;
    use tempfile::TempDir;

    fn write_and_parse(code: &str, min_nodes: usize) -> Vec<CodeUnit> {
        let tmp = TempDir::new().unwrap();
        let file = tmp.path().join("test.rs");
        fs::write(&file, code).unwrap();
        parse_file(&file, min_nodes, 0, false).unwrap()
    }

    #[test]
    fn extracts_top_level_functions() {
        let units = write_and_parse(
            r#"
            fn foo(x: i32) -> i32 {
                let y = x + 1;
                y * 2
            }
            fn bar() {
                println!("hello");
            }
            "#,
            1,
        );
        let fns: Vec<_> = units
            .iter()
            .filter(|u| u.kind == CodeUnitKind::Function)
            .collect();
        assert_eq!(fns.len(), 2);
        assert_eq!(fns[0].name, "foo");
        assert_eq!(fns[1].name, "bar");
    }

    #[test]
    fn extracts_methods_from_impl() {
        let units = write_and_parse(
            r#"
            struct Foo;
            impl Foo {
                fn bar(&self) -> i32 {
                    42
                }
                fn baz(&mut self, val: i32) {
                    let _ = val + 1;
                }
            }
            "#,
            1,
        );
        let methods: Vec<_> = units
            .iter()
            .filter(|u| u.kind == CodeUnitKind::Method)
            .collect();
        assert_eq!(methods.len(), 2);
        assert!(methods[0].name.contains("Foo::bar"));
        assert!(methods[1].name.contains("Foo::baz"));
    }

    #[test]
    fn extracts_trait_impl_methods() {
        let units = write_and_parse(
            r#"
            struct Foo;
            trait MyTrait {
                fn do_thing(&self) -> i32;
            }
            impl MyTrait for Foo {
                fn do_thing(&self) -> i32 {
                    let x = 42;
                    x + 1
                }
            }
            "#,
            1,
        );
        let trait_impls: Vec<_> = units
            .iter()
            .filter(|u| u.kind == CodeUnitKind::TraitImplBlock)
            .collect();
        assert_eq!(trait_impls.len(), 1);
        assert!(trait_impls[0].name.contains("Foo"));
        assert!(trait_impls[0].name.contains("MyTrait"));
        assert!(trait_impls[0].name.contains("do_thing"));
    }

    #[test]
    fn respects_min_node_count() {
        let units_low = write_and_parse(
            r#"
            fn tiny() -> i32 { 1 }
            fn bigger(x: i32) -> i32 {
                let a = x + 1;
                let b = a * 2;
                a + b
            }
            "#,
            1,
        );
        let units_high = write_and_parse(
            r#"
            fn tiny() -> i32 { 1 }
            fn bigger(x: i32) -> i32 {
                let a = x + 1;
                let b = a * 2;
                a + b
            }
            "#,
            20,
        );
        assert!(units_low.len() >= units_high.len());
    }

    #[test]
    fn duplicate_functions_same_fingerprint() {
        let units = write_and_parse(
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
            1,
        );
        let fns: Vec<_> = units
            .iter()
            .filter(|u| u.kind == CodeUnitKind::Function)
            .collect();
        assert_eq!(fns.len(), 2);
        assert_eq!(fns[0].fingerprint, fns[1].fingerprint);
    }

    #[test]
    fn different_functions_different_fingerprint() {
        let units = write_and_parse(
            r#"
            fn add(x: i32) -> i32 {
                x + 1
            }
            fn mul(x: i32) -> i32 {
                x * 2
            }
            "#,
            1,
        );
        let fns: Vec<_> = units
            .iter()
            .filter(|u| u.kind == CodeUnitKind::Function)
            .collect();
        assert_eq!(fns.len(), 2);
        assert_ne!(fns[0].fingerprint, fns[1].fingerprint);
    }

    #[test]
    fn handles_parse_errors_gracefully() {
        let tmp = TempDir::new().unwrap();
        let file = tmp.path().join("broken.rs");
        fs::write(&file, "fn broken( { }").unwrap();
        let result = parse_file(&file, 1, 0, false);
        assert!(result.is_err());
    }

    #[test]
    fn parse_files_collects_warnings() {
        let tmp = TempDir::new().unwrap();
        let good = tmp.path().join("good.rs");
        let bad = tmp.path().join("bad.rs");
        fs::write(&good, "fn good() { let x = 1; }").unwrap();
        fs::write(&bad, "fn bad( {").unwrap();
        let (units, warnings) = parse_files(&[good, bad], 1, 0, false);
        assert!(!units.is_empty());
        assert_eq!(warnings.len(), 1);
    }

    #[test]
    fn code_unit_has_line_numbers() {
        let units = write_and_parse(
            r#"
fn first() {
    let x = 1;
}

fn second() {
    let y = 2;
}
            "#,
            1,
        );
        assert!(units.len() >= 2);
        // First function starts at line 2
        assert!(units[0].line_start > 0);
        assert!(units[0].line_end >= units[0].line_start);
    }

    #[test]
    fn code_unit_kind_display() {
        assert_eq!(CodeUnitKind::Function.to_string(), "function");
        assert_eq!(CodeUnitKind::Method.to_string(), "method");
        assert_eq!(CodeUnitKind::Closure.to_string(), "closure");
    }

    #[test]
    fn extracts_closures() {
        let units = write_and_parse(
            r#"
            fn foo() {
                let f = |x: i32, y: i32| {
                    let sum = x + y;
                    let product = x * y;
                    sum + product
                };
            }
            "#,
            1,
        );
        let closures: Vec<_> = units
            .iter()
            .filter(|u| u.kind == CodeUnitKind::Closure)
            .collect();
        assert!(!closures.is_empty());
    }

    #[test]
    fn min_line_count_filters_short_functions() {
        let code = r#"
fn short(x: i32) -> i32 {
    x + 1
}

fn longer(x: i32) -> i32 {
    let a = x + 1;
    let b = a * 2;
    let c = b - 3;
    let d = c + 4;
    a + b + c + d
}
        "#;
        let tmp = TempDir::new().unwrap();
        let file = tmp.path().join("test.rs");
        fs::write(&file, code).unwrap();

        // With min_line_count=0, both functions should appear
        let units_all = parse_file(&file, 1, 0, false).unwrap();
        assert!(units_all.len() >= 2);

        // With min_line_count=5, only the longer function should pass
        let units_filtered = parse_file(&file, 1, 5, false).unwrap();
        assert!(units_filtered.len() < units_all.len());
        for unit in &units_filtered {
            let lines = unit.line_end.saturating_sub(unit.line_start) + 1;
            assert!(lines >= 5, "unit {} has only {lines} lines", unit.name);
        }
    }

    fn write_and_parse_exclude(code: &str, min_nodes: usize, exclude_tests: bool) -> Vec<CodeUnit> {
        let tmp = TempDir::new().unwrap();
        let file = tmp.path().join("test.rs");
        fs::write(&file, code).unwrap();
        parse_file(&file, min_nodes, 0, exclude_tests).unwrap()
    }

    #[test]
    fn test_has_test_attr() {
        let file: syn::File = syn::parse_str(
            r#"
            #[test]
            fn my_test() {}
            fn normal() {}
            "#,
        )
        .unwrap();

        let items = &file.items;
        if let syn::Item::Fn(f) = &items[0] {
            assert!(has_test_attr(&f.attrs));
        } else {
            panic!("expected function");
        }
        if let syn::Item::Fn(f) = &items[1] {
            assert!(!has_test_attr(&f.attrs));
        } else {
            panic!("expected function");
        }
    }

    #[test]
    fn test_has_cfg_test_attr() {
        let file: syn::File = syn::parse_str(
            r#"
            #[cfg(test)]
            mod tests {}
            mod normal {}
            "#,
        )
        .unwrap();

        let items = &file.items;
        if let syn::Item::Mod(m) = &items[0] {
            assert!(has_cfg_test_attr(&m.attrs));
        } else {
            panic!("expected module");
        }
        if let syn::Item::Mod(m) = &items[1] {
            assert!(!has_cfg_test_attr(&m.attrs));
        } else {
            panic!("expected module");
        }
    }

    #[test]
    fn exclude_tests_skips_test_functions() {
        let code = r#"
            fn production(x: i32) -> i32 {
                let y = x + 1;
                y * 2
            }
            #[test]
            fn my_test() {
                let x = 1;
                let y = x + 1;
                assert_eq!(y, 2);
            }
        "#;

        let with_tests = write_and_parse_exclude(code, 1, false);
        let without_tests = write_and_parse_exclude(code, 1, true);

        assert!(with_tests.len() > without_tests.len());
        assert!(without_tests.iter().all(|u| u.name != "my_test"));
    }

    #[test]
    fn exclude_tests_skips_cfg_test_modules() {
        let code = r#"
            fn production(x: i32) -> i32 {
                let y = x + 1;
                y * 2
            }

            #[cfg(test)]
            mod tests {
                fn helper(x: i32) -> i32 {
                    let y = x + 1;
                    y * 2
                }
            }
        "#;

        let with_tests = write_and_parse_exclude(code, 1, false);
        let without_tests = write_and_parse_exclude(code, 1, true);

        assert!(with_tests.len() > without_tests.len());
        assert!(without_tests.iter().all(|u| u.name != "helper"));
    }

    #[test]
    fn exclude_tests_false_keeps_all() {
        let code = r#"
            fn production(x: i32) -> i32 {
                let y = x + 1;
                y * 2
            }
            #[test]
            fn my_test() {
                let x = 1;
                let y = x + 1;
                assert_eq!(y, 2);
            }
        "#;

        let all_units = write_and_parse_exclude(code, 1, false);
        assert!(all_units.iter().any(|u| u.name == "my_test"));
    }

    #[test]
    fn exclude_tests_skips_cfg_test_impl_blocks() {
        let code = r#"
            struct Foo;

            impl Foo {
                fn production(&self) -> i32 {
                    let x = 42;
                    x + 1
                }
            }

            #[cfg(test)]
            impl Foo {
                fn test_helper(&self) -> i32 {
                    let x = 42;
                    x + 1
                }
            }
        "#;

        let with_tests = write_and_parse_exclude(code, 1, false);
        let without_tests = write_and_parse_exclude(code, 1, true);

        assert!(with_tests.len() > without_tests.len());
        assert!(
            without_tests
                .iter()
                .all(|u| !u.name.contains("test_helper"))
        );
    }
}
