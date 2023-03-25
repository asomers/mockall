# Change Log

All notable changes to this project will be documented in this file.
This project adheres to [Semantic Versioning](http://semver.org/).


## [ Unreleased ] - ReleaseDate

### Added

- Added `#[mockall::concretize]`, which can be used to mock some generic
  methods that have non-`'static` generic parameters.  It works by turning the
  generic arguments into trait objects for the expectation.
  ([#408](https://github.com/asomers/mockall/pull/408))

### Changed

- Raised predicates, which is reexported, to 3.0.0.  This may affect backwards
  compatibility for users who make use of predicates's "color" feature.
  ([#467](https://github.com/asomers/mockall/pull/467))

- Raised MSRV to 1.64.0 because predicates-core and predicates-tree did.
  ([#430](https://github.com/asomers/mockall/pull/430))
  ([#467](https://github.com/asomers/mockall/pull/467))

- Better "No matching expectation found" messages on stable.
  ([#425](https://github.com/asomers/mockall/pull/425))

### Fixed

- Static methods' expectations will now be cleared during a panic.
  ([#443](https://github.com/asomers/mockall/pull/443))

- Methods with unknown size type bounds can now be mocked.
  ([#421](https://github.com/asomers/mockall/pull/421))

### Removed

- Removed syntax deprecated since 0.9.0: using `#[automock]` directly on an
  `extern "C"` block, and using `trait Foo {}` syntax inside of `mock!`.
  ([#476](https://github.com/asomers/mockall/pull/476))

## [ 0.11.3 ] - 2022-10-18

### Fixed

- Methods with a `where Self: ...` clause will now be mocked like concrete
  methods, not generic ones.  Among other effects, this prevents "unused method
  expect" warnings from the latest nightly compiler.
  ([#415](https://github.com/asomers/mockall/pull/415))

## [ 0.11.2 ] - 2022-07-24

### Fixed

- Suppress "dead code" warnings when automocking a struct's private method.  It
  might be used only by other public methods in the same struct.
  ([#397](https://github.com/asomers/mockall/pull/397))

- Fixed using Mockall when a function named `Ok` is in scope.  The `anyhow`
  crate, for example, creates a function by this name.
  ([#389](https://github.com/asomers/mockall/pull/389))

## [ 0.11.1 ] - 2022-05-15

### Fixed

- Fixed mocking methods that use `Self` in their arguments, but not as the
  receiver.  For example, `PartialEq::eq` has a signature like
  `fn eq(&self, other: &Self) -> bool`
  ([#373](https://github.com/asomers/mockall/pull/373))
- Fixed mocking methods that return a reference to a `dyn T` trait object,
  when that trait is not already implemented for `Box<dyn T>`.
  ([#380](https://github.com/asomers/mockall/pull/380))

## [ 0.11.0 ] - 2021-12-11

### Added

- `mock!` and `#[automock]` now support `unsafe` traits.
  ([#313](https://github.com/asomers/mockall/pull/313))

### Changed

- Bump `predicates` to v2.0.1, see all v2 changes in
  [predicates' changelog](https://github.com/assert-rs/predicates-rs/blob/master/CHANGELOG.md).
  ([#325](https://github.com/asomers/mockall/pull/325))

### Fixed

- Fixed nondeterministic code generation in methods with multiple lifetime
  parameters.
  ([#333](https://github.com/asomers/mockall/pull/333))

## [ 0.10.2 ] - 2021-07-12

### Fixed

- Fix mocking specializing methods of non-generic structs, a regression in
  v0.10.0.
  ([#309](https://github.com/asomers/mockall/pull/309))

- Fix mocking generic methods of generic structs returning nonstatic, a
  regression in v0.10.0.
  ([#312](https://github.com/asomers/mockall/pull/312))


## [ 0.10.1 ] - 2021-07-01

### Fixed

- Fix mocking trait methods whose return values have lifetime parameters, a
  regression in v0.10.0.
  ([#304](https://github.com/asomers/mockall/pull/304))

## [ 0.10.0 ] - 2021-06-27

### Added

- `mock!` will now allow both methods and trait impls to be gated with
  `#[cfg()]]` attributes.  The attributes will be forwarded to all generated
  code.  This allows for example only mocking certain traits on certain OSes.
  ([#297](https://github.com/asomers/mockall/pull/297))

- automock will now automatically generate Debug implementations for traits and
  structs.  mock! will to, if you put `#[derive(Debug)]` above the struct's
  name.
  ([#289](https://github.com/asomers/mockall/pull/289))

- Added support for specific impls.  A specific impl is an implementation of a
  trait on a generic struct with specified generic parameters.  For example,
  `impl Foo for Bar<i32>` as opposed to `impl<T> Foo for Bar<T>`.  Mockall does
  not yet support generic methods in such traits.
  ([#274](https://github.com/asomers/mockall/pull/274))

### Changed

- Mockall is pickier now about how you mock a trait on a generic struct.
  Previously you could usually omit the generics.  Now, they're required.
  i.e.,
  ```rust
  mock!{
      MyStruct<T: Bounds> {...}
      impl Foo for MyStruct {...}
  }
  ```
  should now be written as
  ```rust
  mock!{
      MyStruct<T: Bounds> {...}
      impl<T: Bounds> Foo for MyStruct<T> {...}
  }
  ```
  ([#274](https://github.com/asomers/mockall/pull/274))

### Fixed

- Fixed setting simultaneous expectations with different generic types on
  generic methods whose generic parameters appear in neither the arguments nor
  the return type.
  ([#272](https://github.com/asomers/mockall/pull/272))

## [0.9.1] - 2021-02-13
### Added

- When a test fails because of a method sequence violation, the error message
  will now show the method's arguments.  This requires the `nightly` feature,
  and requires that the arguments implement `Debug`.
  ([#247](https://github.com/asomers/mockall/pull/247))

- When a test fails because a mock object receives an unexpected call, the
  error message will now show the method's arguments.  This requires the
  `nightly` feature, and requires that the arguments implement `Debug`.
  ([#246](https://github.com/asomers/mockall/pull/246))

### Changed
### Fixed

- Fixed Clippy warnings in generated code with Rustc 1.52.0.
  ([#255](https://github.com/asomers/mockall/pull/255))

- Fixed using `#[automock]` with `#[tracing::instrument]`.  The mock function
  won't be instrumented, but at least it will compile.
  ([#256](https://github.com/asomers/mockall/pull/256))

### Removed

## [0.9.0] - 2020-12-21
### Added

- Added the ability to mock methods returning `impl Future` or `impl Stream`.
  Unlike other traits, these two aren't very useful in a `Box`.  Instead,
  Mockall will now change the Expectation's return type to `Pin<Box<_>>`.
  ([#229](https://github.com/asomers/mockall/pull/229))

- Added the ability to mock methods returning references to trait objects.
  ([#213](https://github.com/asomers/mockall/pull/213))

- `mock!` supports a new syntax: "impl Trait for".  It has two benefits:
  * It can implement a generic trait for specific generic type(s).
  * It allows mocking a non-local trait.
  The syntax looks like this:
  ```rust
    mock! {
        Bar {}
        impl Foo<i32> for Bar {
            fn foo(&self, x: i32) -> i32;
        }
    }
  ```
  ([#205](https://github.com/asomers/mockall/pull/205))

- `#[automock]` now works on modules even without the `nightly` feature, and no
  longer requires `#[feature(proc_macro_hygiene)]`.
  ([#198](https://github.com/asomers/mockall/pull/198))

### Changed

- `mock!` now requires visibility specifiers for inherent methods.
  ([#207](https://github.com/asomers/mockall/pull/207))

- Changed the syntax for mocking foreign functions.  Instead of using
  `#[automock]` directly on the `extern` block, you must wrap the `extern`
  block in a module, and `#[automock]` that module.  The old method is
  deprecated.
  ([#201](https://github.com/asomers/mockall/pull/201))

### Fixed

- Fixed mocking methods that return `Self` inside of a trait object with
  multiple bounds.  For example: `-> impl Future<Output=Self> + Send`
  ([3400916](https://github.com/asomers/mockall/commit/34009169cf5fe5440a452f1285c20e1fb49c768c))

- Fixed setting multiple expectations on generic methods whose only generic
  type is the return.
  ([#238](https://github.com/asomers/mockall/pull/238))

- Fixed mocking generic structs with generic methods whose only generic types
  are lifetimes.  This is useful for mocking generic structs that implement
  traits like `Future` and `Stream`.
  ([#225](https://github.com/asomers/mockall/pull/225))
  ([#226](https://github.com/asomers/mockall/pull/226))
  ([#228](https://github.com/asomers/mockall/pull/228))

### Removed

- Removed `times_any` and `times_range` methods from Expectations.  They've
  been deprecated since 0.3.0.
  ([#197](https://github.com/asomers/mockall/pull/197))

## [0.8.3] - 21 October 2020
### Added
### Changed
### Fixed

- Suppressed `default_trait_access` pedantic Clippy lint in generated code
  ([#222](https://github.com/asomers/mockall/pull/222))

## [0.8.2] - 12 October 2020
### Added
### Changed
### Fixed

- Fixed Clippy warnings for mocked methods with `Vec` or `String` arguments.
  ([#195](https://github.com/asomers/mockall/pull/195))

## [0.8.1] - 7 September 2020
### Added
### Changed
### Fixed

- Fixed using `<X as Y>::Z` syntax in a where clause or a return type.
  ([#208](https://github.com/asomers/mockall/pull/208))

### Removed

## [0.8.0] - 29 August 2020
### Added
- Added support for mocking structs and traits with associated constants.
  ([#187](https://github.com/asomers/mockall/pull/187))

- Methods returning slices can now be mocked.  Their expectations take `Vec`s.
  ([#185](https://github.com/asomers/mockall/pull/185))

- Compatibility with the `#[async_trait]` macro.
  ([#183](https://github.com/asomers/mockall/pull/183))

- Better support for non-Send types:
  * Added `return_const_st` for returning non-`Send` constants, similar to
    `returning_st`.
  * Added `return_once_st` for static methods.  It was already available for
    non-static methods.
  ([#178](https://github.com/asomers/mockall/pull/178))

- Support mocking methods with arbitrary receivers like `self: Box<Self>`
  ([#176](https://github.com/asomers/mockall/pull/176))

- Support mocking methods with trait object arguments that use implicit
  lifetimes.
  ([#174](https://github.com/asomers/mockall/pull/174))

### Changed
- Raised the minimum supported Rust version (MSRV) to 1.42.0.
  ([#175](https://github.com/asomers/mockall/pull/175))

- Mocked modules now have the same visibility as the original module.
  ([#169](https://github.com/asomers/mockall/pull/169))

### Fixed
- Fixed mocking modules including functions that use "impl Trait" or mutable
  arguments.
  ([#169](https://github.com/asomers/mockall/pull/169))

- Fixed mocking methods whose generic types include `super::`.
  ([#167](https://github.com/asomers/mockall/pull/167))

- Fixed mocking generic methods with where clauses returning references.
  ([#166](https://github.com/asomers/mockall/pull/166))

- Fixed mocking generic methods returning mutable references.
  ([#165](https://github.com/asomers/mockall/pull/165))

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
