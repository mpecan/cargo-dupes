# Changelog

## [0.2.0](https://github.com/mpecan/cargo-dupes/compare/cargo-dupes-v0.1.5...cargo-dupes-v0.2.0) (2026-02-17)


### ⚠ BREAKING CHANGES

* add code-dupes multi-language CLI and extract shared CLI module ([#29](https://github.com/mpecan/cargo-dupes/issues/29))
* enable pedantic lints, split large files, add file size CI gate ([#28](https://github.com/mpecan/cargo-dupes/issues/28))
* flatten NormalizedNode into data-driven { kind, children } struct ([#26](https://github.com/mpecan/cargo-dupes/issues/26))

### Features

* add code-dupes multi-language CLI and extract shared CLI module ([#29](https://github.com/mpecan/cargo-dupes/issues/29)) ([949001d](https://github.com/mpecan/cargo-dupes/commit/949001d47d73de0f93bc2682f7217168dd3be806))
* introduce LanguageAnalyzer trait and extract dupes-rust crate ([#27](https://github.com/mpecan/cargo-dupes/issues/27)) ([953d8dd](https://github.com/mpecan/cargo-dupes/commit/953d8ddff7e2e0dd94426d62e68a0429b11066d0))


### Bug Fixes

* **ci:** inline crate versions for release-please compatibility ([#31](https://github.com/mpecan/cargo-dupes/issues/31)) ([e48c5da](https://github.com/mpecan/cargo-dupes/commit/e48c5daa381ec3427fe0ae8fa2cb3de4147ff94d))
* enable pedantic lints, split large files, add file size CI gate ([#28](https://github.com/mpecan/cargo-dupes/issues/28)) ([327fdca](https://github.com/mpecan/cargo-dupes/commit/327fdca544aff77a3d042ebcaf31e982615f354e))


### Code Refactoring

* flatten NormalizedNode into data-driven { kind, children } struct ([#26](https://github.com/mpecan/cargo-dupes/issues/26)) ([e871f36](https://github.com/mpecan/cargo-dupes/commit/e871f36cfd6c9c23c183222035f67208c6eb324e))
* split into workspace with dupes-core and cargo-dupes crates ([#24](https://github.com/mpecan/cargo-dupes/issues/24)) ([475c3f1](https://github.com/mpecan/cargo-dupes/commit/475c3f143b42aa8974f092799000342543fb6329))


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * dupes-core bumped from 0.1.5 to 0.2.0
    * dupes-rust bumped from 0.1.5 to 0.2.0
