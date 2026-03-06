# Changelog

## [0.2.2](https://github.com/mpecan/cargo-dupes/compare/dupes-core-v0.2.1...dupes-core-v0.2.2) (2026-03-06)


### Features

* add dupes-python crate for Python duplicate detection ([#39](https://github.com/mpecan/cargo-dupes/issues/39)) ([056f9b9](https://github.com/mpecan/cargo-dupes/commit/056f9b9137880b64dcbf5ef7d9ae3956495480ff))
* extend Python extraction to cover lambdas and class bodies ([#42](https://github.com/mpecan/cargo-dupes/issues/42)) ([6f5a8c8](https://github.com/mpecan/cargo-dupes/commit/6f5a8c89364c3f70aea0bd2aabbb4c266151cf53))

## [0.2.1](https://github.com/mpecan/cargo-dupes/compare/dupes-core-v0.2.0...dupes-core-v0.2.1) (2026-02-17)


### Bug Fixes

* use is_excluded helper in scan_files instead of inline duplicate ([#34](https://github.com/mpecan/cargo-dupes/issues/34)) ([c812c2d](https://github.com/mpecan/cargo-dupes/commit/c812c2db37dc43abf82f332447ab1fbb3d7de4d0))

## [0.2.0](https://github.com/mpecan/cargo-dupes/compare/dupes-core-v0.1.5...dupes-core-v0.2.0) (2026-02-17)


### ⚠ BREAKING CHANGES

* add code-dupes multi-language CLI and extract shared CLI module ([#29](https://github.com/mpecan/cargo-dupes/issues/29))

### Features

* add code-dupes multi-language CLI and extract shared CLI module ([#29](https://github.com/mpecan/cargo-dupes/issues/29)) ([949001d](https://github.com/mpecan/cargo-dupes/commit/949001d47d73de0f93bc2682f7217168dd3be806))


### Bug Fixes

* **ci:** inline crate versions for release-please compatibility ([#31](https://github.com/mpecan/cargo-dupes/issues/31)) ([e48c5da](https://github.com/mpecan/cargo-dupes/commit/e48c5daa381ec3427fe0ae8fa2cb3de4147ff94d))
