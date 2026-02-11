# Changelog
All notable changes to this project will be documented in this file.
See [conventional commits](https://www.conventionalcommits.org/) for commit guidelines.

---
## [0.1.2](https://github.com/mpecan/cargo-dupes/compare/cargo-dupes-v0.1.1...cargo-dupes-v0.1.2) (2026-02-11)


### Bug Fixes

* **ci:** trigger release on GitHub release instead of tag push ([#5](https://github.com/mpecan/cargo-dupes/issues/5)) ([2bc5059](https://github.com/mpecan/cargo-dupes/commit/2bc505956df8ce4b8a78e461aa038e7c1c07fc06))

## [0.1.1](https://github.com/mpecan/cargo-dupes/compare/cargo-dupes-v0.1.0...cargo-dupes-v0.1.1) (2026-02-11)


### Features

* add --min-lines filter and duplicated line statistics ([7de772b](https://github.com/mpecan/cargo-dupes/commit/7de772bda588c92d71341be0b50edfeddd26ccb5))
* add --min-lines filter and duplicated line statistics ([c47dd33](https://github.com/mpecan/cargo-dupes/commit/c47dd3348799a9f735122b29de2af92c49c7b02c))
* initial implementation of cargo-dupes ([5c8b4c1](https://github.com/mpecan/cargo-dupes/commit/5c8b4c1c47eb1a1c89a380e44a7adb53d24cb905))


### Bug Fixes

* **ci:** upgrade cocogitto-action from v3 to v4 ([b5621a6](https://github.com/mpecan/cargo-dupes/commit/b5621a66042da23cc8936d4070b4b15a975c491f))


### Documentation

* update CLAUDE.md for --min-lines and line stats ([faac27b](https://github.com/mpecan/cargo-dupes/commit/faac27b1fb913511d33a3249ff912478799d6c3b))
* update README with --min-lines flag and line stats output ([c21ec7c](https://github.com/mpecan/cargo-dupes/commit/c21ec7c9daeccfef6bc8734f6dc7b06b157ddd68))

## [0.1.0] - 2026-02-10

### Initial Release
- AST-based duplicate and near-duplicate code detection
- Text and JSON output formats
- CLI with report, stats, check, ignore, and ignored subcommands
- Configuration via dupes.toml or Cargo.toml metadata
