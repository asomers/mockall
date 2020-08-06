# Change Log

All notable changes to this project will be documented in this file.
This project adheres to [Semantic Versioning](http://semver.org/).

## [Unreleased] - ReleaseDate
### Added
### Changed
### Fixed
- Suppressed `incomplete_features` warnings in the latest nightly.
  ([#161](https://github.com/asomers/mockall/pull/161))

### Removed

## [0.7.2] - 28 July 2020
### Added
### Changed
### Fixed

- Fixed handling function attributes.  They already worked on methods, but not
  foreign functions or module functions.
  ([#129](https://github.com/asomers/mockall/pull/129))

- Propagate doc comments for module functions to the generated mock functions.
  ([#132](https://github.com/asomers/mockall/pull/132))

- Fix the formatting of generated doc comments for context methods
  ([#132](https://github.com/asomers/mockall/pull/132))

- Fixed some visibility issues in the generated mocks for modules and extern
  functions.
  ([#133](https://github.com/asomers/mockall/pull/133))

- Fixed mocking methods with complicated types including a `super::` component.
  ([#137](https://github.com/asomers/mockall/pull/137))

- Mocked generic static methods can now use `return_const`.  This capability
  was omitted as an oversight from PR #47.
  ([#141](https://github.com/asomers/mockall/pull/141))

- Suppressed `unused unit` warnings from Clippy in the latest nightly.
  ([#148](https://github.com/asomers/mockall/pull/148))

### Removed

## [0.7.1] - 3 May 2020
### Fixed

- Fixed `unused must_use` warnings in consumers' crates on the latest nightly
  ([#124](https://github.com/asomers/mockall/pull/124))
  ([#125](https://github.com/asomers/mockall/pull/125))

## [0.7.0] - 29 March 2020
### Added

- `mock!` now allows doc comments in any position
  ([#102](https://github.com/asomers/mockall/pull/102))

- Added the ability to match non-`Send` arguments with `withf_st`
  ([#93](https://github.com/asomers/mockall/pull/93))

- Added the ability to mock non-`'static` structs (but not their constructor
  methods)
  ([#114](https://github.com/asomers/mockall/pull/114))

### Changed
### Fixed

- Fixed the docs for `mockall_examples`
  ([#103](https://github.com/asomers/mockall/pull/103))

- Fixed the build with nightly compilers 2020-03-11 and later.  This also
  raises the MSRV to 1.36.0.
  ([#108](https://github.com/asomers/mockall/pull/108))

- The proc macros will now build successfully in crates that set
  `#![deny(missing_docs)]`
  ([#107](https://github.com/asomers/mockall/pull/107))

- Fixed mocking methods with an explicit receiver lifetime like `&'a self`
  ([#112](https://github.com/asomers/mockall/pull/112))

### Removed

## [0.6.0] - 5 December 2019
### Added

- Added the ability to mock generic methods whose return values' lifetimes are
  chosen by the caller.  Especially useful with `std::future::Future`
  ([#86](https://github.com/asomers/mockall/pull/86))

### Changed
### Fixed

- Fixed using `prediate::always` and `prediate::never` with `?Sized` types
  ([#80](https://github.com/asomers/mockall/pull/80))

- Fixed mocking methods when a custom `Result` type is in-scope.
  ([#74](https://github.com/asomers/mockall/pull/74))

### Removed

## [0.5.2] - 2 November 2019
### Added
### Changed

- Mock objects' checkpoint methods will no longer check static expectations.
  This behavior is more useful where static and regular methods are both used
  in the same test.  The old behavior was a relic from release 0.3.0 and
  before, when static methods did not yet have Context objects of their own.
  ([#64](https://github.com/asomers/mockall/pull/64))

### Fixed

- Fixed hygiene violations in some of mockall_derive's warnings.
  ([#63](https://github.com/asomers/mockall/pull/63))

### Removed

## [0.5.1] - 28 September 2019
### Added
### Changed
### Fixed

- Fixed using super:: in the signature of a bare function
  ([#54](https://github.com/asomers/mockall/pull/54))

- Fixed automocking modules that contain use statements importing types that
  are used in function signatures.
  ([#53](https://github.com/asomers/mockall/pull/53))

### Removed

## [0.5.0] - 5 September 2019

### Added

- Many generic methods with lifetime parameters can now be mocked.  The
  condition is that they may not also have generic type parameters, and their
  return values must be `'static`.
  ([#48](https://github.com/asomers/mockall/pull/48))

- Reexport more traits from the predicates crate
  ([09746e9](https://github.com/asomers/mockall/commit/09746e92d4a7a904b1911babbe65cc1043e237d4))

### Changed
### Fixed

- `return_const` now works for static methods with no preceding `with` or
  `withf`.
  ([#47](https://github.com/asomers/mockall/pull/47))

### Removed

## [0.4.0] - 29 August 2019
### Added

- Warnings for misued expectations and context objects
  ([#37](https://github.com/asomers/mockall/pull/37))

- Methods with closure arguments and where clauses can now be mocked.
  ([#35](https://github.com/asomers/mockall/pull/35))

- Mocked static methods and free functions now use a Context object to manage
  their expectations.  The context object will validate call counts when it
  drops, and clean up any leftover expectations.  This makes it practical to
  use mocked free functions from multiple test cases.  The user still will most
  likely need to provide his own synchronization to prevent such test cases
  from running concurrently.
  ([#34](https://github.com/asomers/mockall/pull/34))

### Changed

- Better panic messages when an expectation fails its expected call count.
  ([#33](https://github.com/asomers/mockall/pull/33))

### Fixed

- Fixed mocking methods that return `'static` deref types, like `&'static str`
  ([#39](https://github.com/asomers/mockall/pull/39))

- Methods returning non-`'static` references (mutable or otherwise) will now
  return a default value if no return value is set, if the output type
  implements `Default` and the `nightly` feature is enabled.  This more closely
  matches existing behavior for methods returning non-reference types.
  ([#32](https://github.com/asomers/mockall/pull/32))

### Removed

## [0.3.0] - 10 August 2019
### Added

- Methods with closure arguments can now be mocked.  Technically they always
  could be, but until now it wasn't possible to call the closure argument from
  `withf` or `returning`.  No special tricks are required by the user.
  Similarly, methods with bare fn arguments can be mocked, too.
  ([#15](https://github.com/asomers/mockall/pull/15))

### Changed

- It is no longer necessary for consuming crates to explicitly depend on
  fragile, downcast, or predicates-tree.  Mockall now reexports them.
  ([#29](https://github.com/asomers/mockall/pull/29))

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
