# Change Log

All notable changes to this project will be documented in this file.
This project adheres to [Semantic Versioning](http://semver.org/).

## [Unreleased] - ReleaseDate
### Added

- Methods with closure arguments can now be mocked.  Technically they always
  could be, but until now it wasn't possible to call the closure argument from
  `withf` or `returning`.  No special tricks are required by the user.
  Similarly, methods with bare fn arguments can be mocked, too.
  ([#15](https://github.com/asomers/mockall/pull/15))

### Changed

- The MSRV is now Rust 1.35.0
  ([#15](https://github.com/asomers/mockall/pull/15))

- The `times` method now accepts ranges as arguments.  `types_any` and
  `times_range` are deprecated.
  ([#14](https://github.com/asomers/mockall/pull/14))

### Fixed

- Fixed mocking methods with arguments or return values that use `Self` as an
  associated type of some other trait.
  ([#28](https://github.com/asomers/mockall/pull/28))

- Fixed mocking structs and traits with more than one static method.
  ([#22](https://github.com/asomers/mockall/pull/22))

- Methods of generic structs and traits that reference `Self` (such as
  constructors or comparators) can now be mocked without any hacks.
  ([#21](https://github.com/asomers/mockall/pull/21))
  ([#25](https://github.com/asomers/mockall/pull/25))

- Specializing methods of generic structs (and traits) can now be mocked
  without the hack of duplicating the struct's (or trait's) generic parameters.
  ([#20](https://github.com/asomers/mockall/pull/20))

- Static methods of generic structs (and traits) can now be mocked without the
  hack of duplicating the struct's (or trait's) generic parameters.
  ([#19](https://github.com/asomers/mockall/pull/19))

### Removed

## [0.2.1] - 3 August 2019

### Fixed

- Fixed some hygiene problems introduced in 0.2.0
  ([db25804](https://github.com/asomers/mockall/commit/db25804255861c6a36d1e0d2d0a81926d296c79a))

## [0.2.0] - 2 August 2019
### Added

- Support mocking generic specializing methods.
  ([#13](https://github.com/asomers/mockall/issues/13))

- Correctly handle where clauses on mocked methods.
  ([#13](https://github.com/asomers/mockall/issues/13))

- Allow mocking generic constructor methods
  ([#11](https://github.com/asomers/mockall/issues/11))

- Allow mocking methods that use `super` somewhere in their signatures.
  ([#8](https://github.com/asomers/mockall/issues/8))

### Changed
### Fixed

- Fixed `#[automock]` for `new` methods with arguments.
  ([#3](https://github.com/asomers/mockall/issues/3))

- Fixed the `never` method, which never actually worked.
  ([#10](https://github.com/asomers/mockall/issues/10))

- Fixed mocking generic traits or structs with generic methods
  ([6126359](https://github.com/asomers/mockall/commit/612635978540e06de93a218d937d70016221de9a))

- Fixed an issue with using associated types in generic parameter bounds
  ([116e5a2](https://github.com/asomers/mockall/commit/116e5a293254d6ae0348f73370bbf8623521a4c2))

- Fixed mocking methods with mutable arguments
  ([8058701](https://github.com/asomers/mockall/commit/8058701f7e195636fd1759f10187fa431b7aa61c))

### Removed

- Removed `expectation!`, used to manually mock methods in rare circumstances.
  Now `mock!` and `#[automock]` are the only way to use Mockall.
  ([#12](https://github.com/asomers/mockall/pull/12))

## [0.1.1] - 3 July 2019
### Added
### Changed
### Fixed

- Fixed some issues in the API docs.  No functional change.
  ([ba88fd1](https://github.com/asomers/mockall/commit/ba88fd19ccd1af7f882df43d6fc36019211d2893))

### Removed
