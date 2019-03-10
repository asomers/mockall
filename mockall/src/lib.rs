// vim: tw=80
//! A powerful mock object library for Rust.
//!
//! Mockall provides provides tools to create mock versions of almost any trait
//! or struct.  They can be used in unit tests as a stand-in for the real
//! object.
//!
//! # Usage
//!
//! There are three ways to use Mockall.  The easiest is to use
//! [`#[automock]`].  It can mock most
//! traits, or structs that only have a single `impl` block.  For things it
//! can't handle, there is [`mock!`].  Finally, there are rare
//! cases where one may need to manually construct a mock object using
//! [`expectation!`].
//!
//! Whichever method is used, the basic idea is the same.
//! * Create a mock struct.  It's name will be the same as the original, with
//!   "Mock" prepended.
//! * In your test, instantiate the mock struct with its `new` or `default`
//!   method.
//! * Set expectations on the mock struct.  Each expectation can have required
//!   argument matchers, a required call count, and a required position in a
//!   [`Sequence`].  Each expectation must also have a return value.
//! * Supply the mock object to the code that you're testing.  It will return
//!   the preprogrammed return values supplied in the previous step.  Any
//!   accesses contrary to your expectations will cause a panic.
//!
//! # User Guide
//!
//! * [`Getting started`](#getting-started)
//! * [`Return values`](#return-values)
//! * [`Matching arguments`](#matching-arguments)
//! * [`Call counts`](#call-counts)
//! * [`Getting started`](#getting-started)
//! * [`Sequences`](#sequences)
//! * [`Checkpoints`](#checkpoints)
//! * [`Reference arguments`](#reference-arguments)
//! * [`Reference return values`](#reference-return-values)
//! * [`impl Trait`](#impl-trait)
//! * [`Mocking structs`](#mocking-structs)
//! * [`Generic methods`](#generic-methods)
//! * [`Generic traits and structs`](#generic-traits-and-structs)
//! * [`Associated types`](#associated-types-1)
//! * [`Multiple and inherited traits`](#multiple-and-inherited-traits)
//! * [`External traits`](#external-traits)
//! * [`Static methods`](#static-methods)
//! * [`Foreign functions`](#foreign-functions)
//! * [`Modules`](#modules)
//! * [`Crate features`](#crate-features)
//!
//! ## Getting Started
//! ```
//! use mockall::*;
//! use mockall::predicate::*;
//! #[automock]
//! trait MyTrait {
//!     fn foo(&self, x: u32) -> u32;
//! }
//!
//! fn call_with_four(x: &MyTrait) -> u32 {
//!     x.foo(4)
//! }
//!
//! let mut mock = MockMyTrait::new();
//! mock.expect_foo()
//!     .with(predicate::eq(4))
//!     .times(1)
//!     .returning(|x| x + 1);
//! assert_eq!(5, call_with_four(&mock));
//! ```
//!
//! ## Return values
//!
//! Every expectation must have an associated return value (though when the
//! **nightly** feature is enabled expectations will automatically return the
//! default values of their return types, if their return types implement
//! `Default`.).  For `'static` return types there are two ways to set the
//! return value: with a constant or a closure.  A closure will take the
//! method's arguments by value.
//!
//! ```
//! # use mockall::*;
//! #[automock]
//! trait MyTrait {
//!     fn foo(&self) -> u32;
//!     fn bar(&self, x: u32, y: u32) -> u32;
//! }
//!
//! let mut mock = MockMyTrait::new();
//! mock.expect_foo()
//!     .return_const(42u32);
//! mock.expect_bar()
//!     .returning(|x, y| x + y);
//! ```
//!
//! Additionally, constants that aren't `Clone` can be returned with the
//! [`return_once`] method.
//!
//! ```
//! # use mockall::*;
//! struct NonClone();
//! #[automock]
//! trait Foo {
//!     fn foo(&self) -> NonClone;
//! }
//!
//! # fn main() {
//! let mut mock = MockFoo::new();
//! let r = NonClone{};
//! mock.expect_foo()
//!     .return_once(move || r);
//! # }
//! ```
//!
//! [`return_once`] can also be used for computing the return value with an
//! `FnOnce` closure.  This is useful for returning a non-`Clone` value and also
//! triggering side effects at the same time.
//!
//! ```
//! # use mockall::*;
//! fn do_something() {}
//!
//! struct NonClone();
//!
//! #[automock]
//! trait Foo {
//!     fn foo(&self) -> NonClone;
//! }
//!
//! # fn main() {
//! let mut mock = MockFoo::new();
//! let r = NonClone{};
//! mock.expect_foo()
//!     .return_once(move || {
//!         do_something();
//!         r
//!     });
//! # }
//! ```
//!
//! ## Matching arguments
//!
//! Optionally, expectations may have argument matchers set.  A matcher will
//! verify that the expectation was called with the expected arguments, or panic
//! otherwise.  A matcher is anything that implements the [`Predicate`] trait.
//! For example:
//!
//! ```should_panic
//! # use mockall::*;
//! # use mockall::predicate::*;
//! #[automock]
//! trait Foo {
//!     fn foo(&self, x: u32);
//! }
//!
//! let mut mock = MockFoo::new();
//! mock.expect_foo()
//!     .with(eq(42))
//!     .return_const(());
//!
//! mock.foo(0);    // Panics!
//! ```
//!
//! See [`predicates`] for a list of Mockall's builtin predicate functions.
//! For convenience, [`withf`] is a shorthand for setting the commonly used
//! [`function`] predicate.  The arguments to the predicate function are the
//! method's arguments, *by reference*.  For example:
//!
//! ```should_panic
//! # use mockall::*;
//! #[automock]
//! trait Foo {
//!     fn foo(&self, x: u32, y: u32);
//! }
//!
//! # fn main() {
//! let mut mock = MockFoo::new();
//! mock.expect_foo()
//!     .withf(|x: &u32, y: &u32| x == y)
//!     .return_const(());
//!
//! mock.foo(2 + 2, 5);    // Panics!
//! # }
//! ```
//!
//! ### Matching multiple calls
//!
//! Matchers can also be used to discriminate between different invocations of
//! the same function.  Used that way, they can provide different return values
//! for different arguments.  The way this works is that on a method call, all
//! expectations set on a given method are evaluated in FIFO order.  The first
//! matching expectation is used.  Only if none of the expectations match does
//! Mockall panic.  For example:
//!
//! ```
//! # use mockall::*;
//! # use mockall::predicate::*;
//! #[automock]
//! trait Foo {
//!     fn foo(&self, x: u32) -> u32;
//! }
//!
//! # fn main() {
//! let mut mock = MockFoo::new();
//! mock.expect_foo()
//!     .with(eq(5))
//!     .return_const(50u32);
//! mock.expect_foo()
//!     .with(eq(6))
//!     .return_const(60u32);
//! # }
//! ```
//!
//! One common pattern is to use multiple expectations in order of decreasing
//! specificity.  The last expectation can provide a default or fallback value,
//! and earlier ones can be more specific.  For example:
//!
//! ```
//! # use mockall::*;
//! # use mockall::predicate::*;
//! #[automock]
//! trait Foo {
//!     fn open(&self, path: String) -> Option<u32>;
//! }
//!
//! let mut mock = MockFoo::new();
//! mock.expect_open()
//!     .with(eq(String::from("something.txt")))
//!     .returning(|_| Some(5));
//! mock.expect_open()
//!     .return_const(None);
//! ```
//!
//! ## Call counts
//!
//! By default, every expectation is allowed to be called an unlimited number of
//! times.  But Mockall can optionally verify that an expectation was called a
//! fixed number of times, or any number of times within a given range.
//!
//! ```should_panic
//! # use mockall::*;
//! # use mockall::predicate::*;
//! #[automock]
//! trait Foo {
//!     fn foo(&self, x: u32);
//! }
//!
//! let mut mock = MockFoo::new();
//! mock.expect_foo()
//!     .times(1)
//!     .return_const(());
//!
//! mock.foo(0);    // Ok
//! mock.foo(1);    // Panics!
//! ```
//!
//! See also [`never`], [`times`], [`times_any`], and [`times_range`].
//!
//! ## Sequences
//!
//! By default expectations may be matched in any order.  But it's possible to
//! specify the order by using a [`Sequence`].  Any expectations may be added to
//! the same sequence.  They don't even need to come from the same object.
//!
//! ```should_panic(expected = "Method sequence violation")
//! # use mockall::*;
//! #[automock]
//! trait Foo {
//!     fn foo(&self);
//! }
//!
//! # fn main() {
//! let mut seq = Sequence::new();
//!
//! let mut mock1 = MockFoo::new();
//! mock1.expect_foo()
//!     .in_sequence(&mut seq)
//!     .returning(|| ());
//!
//! let mut mock2 = MockFoo::new();
//! mock2.expect_foo()
//!     .in_sequence(&mut seq)
//!     .returning(|| ());
//!
//! mock2.foo();    // Panics!  mock1.foo should've been called first.
//! # }
//! ```
//!
//! ## Checkpoints
//!
//! Sometimes its useful to validate all expectations mid-test, throw them away,
//! and add new ones.  That's what checkpoints are for.  Every mock object has a
//! `checkpoint` method.  When called, it will immediately validate all methods'
//! expectations.  So any expectations that haven't satisfied their call count
//! will panic.  Afterwards, those expectations will be cleared so you can add
//! new expectations and keep testing.
//!
//! ```should_panic
//! # use mockall::*;
//! #[automock]
//! trait Foo {
//!     fn foo(&self);
//! }
//!
//! let mut mock = MockFoo::new();
//! mock.expect_foo()
//!     .times(2)
//!     .returning(|| ());
//!
//! mock.foo();
//! mock.checkpoint();  // Panics!  foo hasn't yet been called twice.
//! ```
//!
//! ```should_panic
//! # use mockall::*;
//! #[automock]
//! trait Foo {
//!     fn foo(&self);
//! }
//!
//! # fn main() {
//! let mut mock = MockFoo::new();
//! mock.expect_foo()
//!     .times(1)
//!     .returning(|| ());
//!
//! mock.foo();
//! mock.checkpoint();
//! mock.foo();         // Panics!  The expectation has been cleared.
//! # }
//! ```
//!
//! ## Reference arguments
//!
//! Mockall can mock methods with reference arguments, too.  There's one catch:
//! the matcher [`Predicate`] will take reference arguments by value, not by
//! reference.
//!
//! ```
//! # use mockall::*;
//! #[automock]
//! trait Foo {
//!     fn foo(&self, x: &u32) -> u32;
//! }
//!
//! let mut mock = MockFoo::new();
//! let e = mock.expect_foo()
//!     // Note that x is a &u32, not a &&u32
//!     .withf(|x: &u32| *x == 5)
//!     .returning(|x: &u32| *x + 1);
//!
//! assert_eq!(6, mock.foo(&5));
//! ```
//!
//! ## Reference return values
//!
//! Mockall can also use reference return values.  There is one restriction: the
//! lifetime of the returned reference must be the same as the lifetime of the
//! mock object.
//!
//! Mockall creates different expectation types for methods that return
//! references.  Their API is the same as the usual [`Expectation`], except for
//! setting return values.
//!
//! Methods that take a `&self` argument use the [`RefExpectation`] class, which
//! gets its return value from the
//! [`return_const`](struct.RefExpectation.html#method.return_const) method.
//!
//! ```
//! # use mockall::*;
//! struct Thing(u32);
//!
//! #[automock]
//! trait Container {
//!     fn get(&self, i: u32) -> &Thing;
//! }
//!
//! # fn main() {
//! let thing = Thing(42);
//! let mut mock = MockContainer::new();
//! mock.expect_get()
//!     .return_const(thing);
//!
//! assert_eq!(42, mock.get(0).0);
//! # }
//! ```
//!
//! Methods that take a `&mut self` argument use the [`RefMutExpectation`]
//! class, regardless of whether the return value is actually mutable.  They can
//! take their return value either from the
//! [`return_var`](struct.RefMutExpectation.html#method.return_var) or
//! [`returning`](struct.RefMutExpectation.html#method.returning) methods.
//!
//! ```
//! # use mockall::*;
//! struct Thing(u32);
//!
//! #[automock]
//! trait Container {
//!     fn get_mut(&mut self, i: u32) -> &mut Thing;
//! }
//!
//! # fn main() {
//! let thing = Thing(42);
//! let mut mock = MockContainer::new();
//! mock.expect_get_mut()
//!     .return_var(thing);
//!
//! mock.get_mut(0).0 = 43;
//! assert_eq!(43, mock.get_mut(0).0);
//! # }
//! ```
//!
//! Unsized types that are common targets for [`Deref`] are special.  Mockall
//! will automatically use the type's owned form for the Expectation.
//! Currently, the [`str`], [`CStr`], [`OsStr`], and [`Path`] types are
//! supported.  Using this feature is automatic:
//!
//! ```
//! # use mockall::*;
//! #[automock]
//! trait Foo {
//!     fn name(&self) -> &str;
//! }
//!
//! let mut mock = MockFoo::new();
//! mock.expect_name().return_const("abcd".to_owned());
//! assert_eq!("abcd", mock.name());
//! ```
//!
//! ## Impl Trait
//!
//! Rust 1.26.0 introduced the `impl Trait` feature.  It allows functions to
//! return concrete but unnamed types (and, less usefully, to take them as
//! arguments).  It's *almost* the same as `Box<dyn Trait>` but without the
//! extra allocation.  Mockall supports deriving mocks for methods that return
//! `impl Trait`, with limitations.  When you derive the mock for such a method,
//! Mockall internally transforms the Expectation's return type to `Box<dyn
//! Trait>`, without changing the mock method's signature.  So you can use it
//! like this:
//!
//! ```
//! # use mockall::*;
//! # use std::fmt::Debug;
//! struct Foo {}
//! #[automock]
//! impl Foo {
//!     fn foo(&self) -> impl Debug {
//!         // ...
//!         # 4
//!     }
//! }
//!
//! # fn main() {
//! let mut mock = MockFoo::new();
//! mock.expect_foo()
//!     .returning(|| Box::new(String::from("Hello, World!")));
//! println!("{:?}", mock.foo());
//! # }
//! ```
//!
//! However, `impl Trait` isn't *exactly* equivalent to `Box<dyn Trait>` but
//! with fewer allocations.  There are some things the former can do but the
//! latter can't.  For one thing, you can't build a trait object out of a
//! `Sized` trait.  So this won't work:
//!
//! ```compile_fail
//! # use mockall::*;
//! struct Foo {}
//! #[automock]
//! impl Foo {
//!     fn foo(&self) -> impl Clone {
//!         // ...
//!         # 4
//!     }
//! }
//! ```
//!
//! Nor can you create a trait object that implements two or more non-auto
//! types.  So this won't work either:
//!
//! ```compile_fail
//! # use mockall::*;
//! struct Foo {}
//! #[automock]
//! impl Foo {
//!     fn foo(&self) -> impl Debug + Display {
//!         // ...
//!         # 4
//!     }
//! }
//! ```
//!
//! For such cases, there is no magic bullet.  The best way to mock methods like
//! those would be to refactor them to return named (but possibly opaque) types
//! instead.
//!
//! See Also [`impl-trait-for-returning-complex-types-with-ease.html`](https://rust-lang-nursery.github.io/edition-guide/rust-2018/trait-system/impl-trait-for-returning-complex-types-with-ease)
//!
//! ## Mocking structs
//!
//! Mockall mock structs as well as traits.  The problem here is a namespace
//! problem: it's hard to supply the mock object to your code under test,
//! because it has a different name.  The solution is to alter import paths
//! during test.  The [`cfg-if`] crate helps.
//!
//! [`#[automock]`] works for structs that have a single `impl` block:
//! ```no_run
//! # use mockall::*;
//! # use cfg_if::cfg_if;
//! mod thing {
//!     # use mockall::automock;
//!     pub struct Thing{}
//!     #[automock]
//!     impl Thing {
//!         pub fn foo(&self) -> u32 {
//!             // ...
//!             # unimplemented!()
//!         }
//!     }
//! }
//!
//! cfg_if! {
//!     if #[cfg(test)] {
//!         use self::thing::MockThing as Thing;
//!     } else {
//!         use self::thing::Thing;
//!     }
//! }
//!
//! fn do_stuff(thing: &Thing) -> u32 {
//!     thing.foo()
//! }
//!
//! #[cfg(test)]
//! mod t {
//!     use super::*;
//!
//!     #[test]
//!     fn test_foo() {
//!         let mut mock = Thing::default();
//!         mock.expect_foo().returning(|| 42);
//!         do_stuff(&mock);
//!     }
//! }
//! # fn main() {}
//! ```
//! For structs with more than one `impl` block, see [`mock!`] instead.
//!
//! ## Generic methods
//!
//! Generic methods can be mocked, too.  Effectively each generic method is an
//! infinite set of regular methods, and each of those works just like any other
//! regular method.  The expect_* method is generic, too, and usually must be
//! called with a turbofish.  The only restrictions on mocking generic methods
//! is that each generic parameter must be `'static`, and generic lifetime
//! parameters are not allowed.
//!
//! ```
//! # use mockall::*;
//! #[automock]
//! trait Foo {
//!     fn foo<T: 'static>(&self, t: T) -> i32;
//! }
//!
//! let mut mock = MockFoo::new();
//! mock.expect_foo::<i16>()
//!     .returning(|t| i32::from(t));
//! mock.expect_foo::<i8>()
//!     .returning(|t| -i32::from(t));
//!
//! assert_eq!(5, mock.foo(5i16));
//! assert_eq!(-5, mock.foo(5i8));
//! ```
//!
//! ## Generic traits and structs
//!
//! Mocking generic structs and generic traits is not a problem.  The mock
//! struct will be generic, too.  The same restrictions apply as for mocking
//! generic methods: each generic parameter must be `'static`, and generic
//! lifetime parameters are not allowed.
//!
//! ```
//! # use mockall::*;
//! #[automock]
//! trait Foo<T: 'static> {
//!     fn foo(&self, t: T) -> i32;
//! }
//!
//! # fn main() {
//! let mut mock = MockFoo::<i16>::new();
//! mock.expect_foo()
//!     .returning(|t| i32::from(t));
//! assert_eq!(5, mock.foo(5i16));
//! # }
//! ```
//!
//! ## Associated types
//!
//! Traits with associated types can be mocked too.  Unlike generic traits, the
//! mock struct will not be generic.  Instead, you must specify the associated
//! types when defining the mock struct.  They're specified as metaitems to the
//! [`#[automock]`] attribute.
//!
//! ```
//! # use mockall::*;
//! #[automock(type Key=u16; type Value=i32;)]
//! pub trait A {
//!     type Key;
//!     type Value;
//!     fn foo(&self, k: Self::Key) -> Self::Value;
//! }
//!
//! let mut mock = MockA::new();
//! mock.expect_foo()
//!     .returning(|x| i32::from(x));
//! assert_eq!(4, mock.foo(4));
//! ```
//!
//! ## Multiple and inherited traits
//!
//! Creating a mock struct that implements multiple traits, whether inherited or
//! not, requires using the [`mock!`] macro.  But once created, using it is just
//! the same as using any other mock object
//!
//! ```
//! # use mockall::*;
//! pub trait A {
//!     fn foo(&self);
//! }
//!
//! pub trait B: A {
//!     fn bar(&self);
//! }
//!
//! mock! {
//!     // Structure to mock
//!     C {}
//!     // First trait to implement on C
//!     trait A {
//!         fn foo(&self);
//!     }
//!     // Second trait to implement on C
//!     trait B: A {
//!         fn bar(&self);
//!     }
//! }
//! # fn main() {
//! let mut mock = MockC::new();
//! mock.expect_foo().returning(|| ());
//! mock.expect_bar().returning(|| ());
//! mock.foo();
//! mock.bar();
//! # }
//! ```
//!
//! ## External traits
//!
//! Mockall can mock traits and structs defined in external crates that are
//! beyond your control, but you must use [`mock!`] instead of [`#[automock]`].
//! Mock an external trait like this:
//!
//! ```
//! # use mockall::*;
//! mock! {
//!     MyStruct {}     // Name of the mock struct, less the "Mock" prefix
//!     trait Clone {   // definition of the trait to mock
//!         fn clone(&self) -> Self;
//!     }
//! }
//!
//! # fn main() {
//! let mut mock1 = MockMyStruct::new();
//! let mock2 = MockMyStruct::new();
//! mock1.expect_clone()
//!     .return_once(move || mock2);
//! let cloned = mock1.clone();
//! # }
//! ```
//!
//! ## Static methods
//!
//! Mockall can also mock static methods.  But be careful!  The expectations are
//! global.  If you want to use a static method in multiple tests, you must
//! provide your own synchronization.
//!
//! ```
//! # use mockall::*;
//! #[automock]
//! pub trait A {
//!     fn foo() -> u32;
//! }
//!
//! MockA::expect_foo().returning(|| 99);
//! assert_eq!(99, MockA::foo());
//! ```
//!
//! A common pattern is mocking a trait with a construtor method.  In this case,
//! you can easily set the mock constructor method to return a mock object.
//!
//! ```
//! # use mockall::*;
//! struct Foo{}
//! #[automock]
//! impl Foo {
//!     fn from_i32(x: i32) -> Self {
//!         // ...
//!         # unimplemented!()
//!     }
//!     fn foo(&self) -> i32 {
//!         // ...
//!         # unimplemented!()
//!     }
//! }
//!
//! # fn main() {
//! MockFoo::expect_from_i32()
//!     .returning(|x| {
//!         let mut mock = MockFoo::default();
//!         mock.expect_foo()
//!             .return_const(x);
//!         mock
//!     });
//! let foo = MockFoo::from_i32(42);
//! assert_eq!(42, foo.foo());
//! # }
//! ```
//!
//! Mocking static methods of generic structs is a little bit tricky.  If the
//! static method uses any generic parameters, then those generic parameters
//! must be duplicated as generic parameters of the static method itself.
//! Here's an example:
//!
//! ```
//! # use mockall::*;
//! // A struct like this:
//! struct Foo<T> {
//!     // ...
//!     # _t0: std::marker::PhantomData<T>
//! }
//! impl<T> Foo<T> {
//!     fn new(t: T) -> Self {
//!         // ...
//!         # unimplemented!()
//!     }
//! }
//!
//! // Can be mocked like this:
//! mock! {
//!     Foo<T: 'static> {
//!         fn new<T2: 'static>(t: T2) -> MockFoo<T2>;
//!     }
//! }
//!
//! // And used like this:
//! # fn main() {
//! MockFoo::<u32>::expect_new::<u32>()
//!     .returning(|_| MockFoo::default());
//! let mock = MockFoo::<u32>::new(42u32);
//! # }
//! ```
//!
//! One more thing: Mockall normally creates a zero-argument `new` method for
//! every mock struct.  But it *won't* do that when mocking a struct that
//! already has a method named `new`.
//!
//! ## Foreign functions
//!
//! Mockall can also mock foreign functions.  Like static methods, the
//! expectations are global.  And like mocking structs, you'll probably have to
//! fiddle with your imports to make the mock function accessible.  Finally,
//! like associated types, you'll need to provide some extra info to
//! [`#[automock]`] to make it work.
//!
//! ```no_run
//! # use mockall::*;
//! # use cfg_if::cfg_if;
//! mod ffi {
//!     # use mockall::automock;
//!     #[automock(mod mock;)]
//!     extern "C" {
//!         pub fn foo(x: u32) -> i64;
//!     }
//! }
//!
//! cfg_if! {
//!     if #[cfg(test)] {
//!         use self::ffi::mock::foo;
//!     } else {
//!         use self::ffi::foo;
//!     }
//! }
//!
//! fn do_stuff() -> i64 {
//!     unsafe{ foo(42) }
//! }
//!
//! #[cfg(test)]
//! mod t {
//!     use super::*;
//!
//!     #[test]
//!     fn test_foo() {
//!         ffi::mock::expect_foo()
//!             .returning(|x| i64::from(x + 1));
//!         do_stuff();
//!     }
//! }
//! # fn main() {}
//! ```
//!
//! ## Modules
//!
//! In addition to mocking foreign functions, Mockall can also derive mocks for
//! entire modules of Rust functions,  This requires the **nightly** feature,
//! and it requires the consuming crate to enable `feature(proc_macro_hygiene)`.
//! Usage is the same as when mocking foreign functions, except that the mock
//! module name is automatically derived.
//!
#![cfg_attr(feature = "nightly", doc = "```")]
#![cfg_attr(not(feature = "nightly"), doc = "```ignore")]
//! #![feature(proc_macro_hygiene)]
//! # use mockall::*;
//! # use cfg_if::cfg_if;
//! mod foo {
//!     # use mockall::automock;
//!     #[automock()]
//!     mod foo {
//!         pub fn bar(x: u32) -> i64 {
//!             // ...
//!             # 4
//!         }
//!     }
//! }
//!
//! cfg_if! {
//!     if #[cfg(test)] {
//!         use mock_foo::*;
//!     } else {
//!         use foo::*;
//!     }
//! }
//!
//! #[cfg(test)]
//! mod t {
//!     use super::*;
//!
//!     #[test]
//!     fn test_foo_bar() {
//!         mock_foo::expect_bar()
//!             .returning(|x| i64::from(x + 1));
//!         assert_eq!(5, foo(4));
//!     }
//! }
//! ```
//!
//! ## Crate features
//!
//! Mockall has a **nightly** feature.  Currently this feature has three
//! effects:
//!
//! * The compiler will produce better error messages.
//!
//! * Mocking modules will be enabled.
//!
//! * Expectations for methods whose return type implements `Default` needn't
//!   have their return values explicitly set.  Instead, they will automatically
//!   return the default value.
//!
//! With **nightly** enabled, you can omit the return value like this:
#![cfg_attr(feature = "nightly", doc = "```")]
#![cfg_attr(not(feature = "nightly"), doc = "```ignore")]
//! # use mockall::*;
//! #[automock]
//! trait Foo {
//!     fn foo(&self) -> Vec<u32>;
//! }
//!
//! let mut mock = MockFoo::new();
//! mock.expect_foo();
//! assert!(mock.foo().is_empty());
//! ```
//!
//! [`#[automock]`]: ../mockall_derive/attr.automock.html
//! [`CStr`]: https://doc.rust-lang.org/stable/std/ffi/struct.CStr.html
//! [`Deref`]: https://doc.rust-lang.org/stable/std/ops/trait.Deref.html
//! [`Expectation`]: struct.Expectation.html
//! [`Expectations`]: struct.Expectations.html
//! [`OsStr`]: https://doc.rust-lang.org/stable/std/ffi/struct.OsStr.html
//! [`Path`]: https://doc.rust-lang.org/stable/std/path/struct.Path.html
//! [`Predicate`]: trait.Predicate.html
//! [`RefExpectation`]: struct.RefExpectation.html
//! [`Sequence`]: struct.Sequence.html
//! [`cfg-if`]: https://crates.io/crates/cfg-if
//! [`expectation!`]: macro.expectation.html
//! [`function`]: predicate/fn.function.html
//! [`mock!`]: ../mockall_derive/macro.mock.html
//! [`never`]: struct.Expectation.html#method.never
//! [`predicates`]: predicate/index.html
//! [`return_once`]: struct.Expectation.html#method.return_once
//! [`str`]: https://doc.rust-lang.org/stable/std/primitive.str.html
//! [`times_any`]: struct.Expectation.html#method.times_any
//! [`times_range`]: struct.Expectation.html#method.times_range
//! [`times`]: struct.Expectation.html#method.times
//! [`withf_unsafe`]: struct.Expectation.html#method.withf_unsafe
//! [`withf`]: struct.Expectation.html#method.withf

#![cfg_attr(feature = "nightly", feature(specialization))]

use cfg_if::cfg_if;
use core::fmt::{self, Display};
use downcast::*;
use fragile::Fragile;
use std::{
    any,
    collections::hash_map::HashMap,
    marker::PhantomData,
    mem,
    ops::{DerefMut, Range},
    sync::{
        Arc,
        Mutex,
        MutexGuard,
        atomic::{AtomicUsize, Ordering}
    },
    thread
};

pub mod omnimock;

pub use mockall_derive::{mock, automock};
pub use predicates::prelude::{Predicate, predicate};

/// For mocking static methods
#[doc(hidden)]
pub use lazy_static::lazy_static;

#[doc(hidden)]
pub trait AnyExpectations : Any + Send + Sync {}
downcast!(AnyExpectations);

#[doc(hidden)]
pub trait ReturnDefault<O> {
    fn return_default() -> O;
}

#[derive(Default)]
#[doc(hidden)]
pub struct DefaultReturner<O: 'static>(PhantomData<O>);

::cfg_if::cfg_if! {
    if #[cfg(feature = "nightly")] {
        impl<O> ReturnDefault<O> for DefaultReturner<O> {
            default fn return_default() -> O {
                panic!("Can only return default values for types that impl std::Default");
            }
        }

        impl<O: Default> ReturnDefault<O> for DefaultReturner<O> {
            fn return_default() -> O {
                O::default()
            }
        }
    } else {
        impl<O> ReturnDefault<O> for DefaultReturner<O> {
            fn return_default() -> O {
                panic!("Returning default values requires the \"nightly\" feature");
            }
        }
    }
}


/// Return functions for expectations
enum Rfunc<I, O> {
    Default,
    // Indicates that a `return_once` expectation has already returned
    Expired,
    Mut(Box<dyn FnMut(I) -> O + Send>),
    // Should be Box<dyn FnOnce> once that feature is stabilized
    // https://github.com/rust-lang/rust/issues/28796
    Once(Box<dyn FnMut(I) -> O + Send>),
}

// TODO: change this to "impl FnMut" once unboxed_closures are stable
// https://github.com/rust-lang/rust/issues/29625
impl<I, O>  Rfunc<I, O> {
    fn call_mut(&mut self, args: I) -> O {
        match self {
            Rfunc::Default => {
                Self::return_default()
            },
            Rfunc::Expired => {
                panic!("Called a method twice that was expected only once")
            },
            Rfunc::Mut(f) => {
                f(args)
            },
            Rfunc::Once(_) => {
                let fo = mem::replace(self, Rfunc::Expired);
                if let Rfunc::Once(mut f) = fo {
                    f(args)
                } else {
                    unreachable!()
                }
            },
        }
    }
}

impl<I, O> Default for Rfunc<I, O> {
    fn default() -> Self {
        Rfunc::Default
    }
}

cfg_if! {
    if #[cfg(feature = "nightly")] {
        impl<I, O> ReturnDefault<O> for Rfunc<I, O> {
            default fn return_default() -> O {
                panic!("Can only return default values for types that impl std::Default");
            }
        }

        impl<I, O: Default> ReturnDefault<O> for Rfunc<I, O> {
            fn return_default() -> O {
                O::default()
            }
        }
    } else {
        impl<I, O> ReturnDefault<O> for Rfunc<I, O> {
            fn return_default() -> O {
                panic!("Returning default values requires the \"nightly\" feature");
            }
        }
    }
}

/// Like predicates::function::FnPredicate, but unsafely implements `Send`.
/// Useful for methods with pointer arguments.  The onus is on the user to
/// ensure that it can be safely sent between threads.
struct UnsafeFnPredicate<F, T>(predicates::function::FnPredicate<F, T>)
    where F: Fn(&T) -> bool, T: ?Sized;

impl<F, T> Display for UnsafeFnPredicate<F, T>
    where F: Fn(&T) -> bool, T: ?Sized
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl<F, T> predicates::reflection::PredicateReflection for UnsafeFnPredicate<F, T>
    where F: Fn(&T) -> bool, T: ?Sized
{}

impl<F, T> predicates::prelude::Predicate<T> for UnsafeFnPredicate<F, T>
    where F: Fn(&T) -> bool, T: ?Sized
{
    fn eval(&self, var: &T) -> bool {
        self.0.eval(var)
    }
}

unsafe impl<F, T> Send for UnsafeFnPredicate<F, T>
    where F: Fn(&T) -> bool, T: ?Sized
{}

struct Matcher<I>(Mutex<Box<dyn Predicate<I> + Send>>);

impl<I> Matcher<I> {
    fn matches(&self, i: &I) -> bool {
        self.0.lock().unwrap().eval(&i)
    }

    fn new<P: Predicate<I> + Send + 'static>(p: P) -> Self {
        Matcher(Mutex::new(Box::new(p)))
    }

    fn verify(&self, i: &I) {
        if let Some(case) = self.0.lock().unwrap().find_case(false, &i) {
            let c = &case as &predicates_tree::CaseTreeExt;
            panic!("Expectation didn't match arguments:\n{}", c.tree());
        }
    }
}

impl<I> Default for Matcher<I> {
    fn default() -> Self {
        Matcher::new(predicates::constant::always())
    }
}

/// Check separate [`Predicate`](trait.Predicate.html)s for each argument of a
/// function.  Used with [`with`](struct.Expectation.html#method.with).
///
/// # Examples
/// ```
/// # use mockall::*;
/// # use mockall::predicate::*;
/// #[automock]
/// trait Foo {
///     fn foo(&self, x: u32, y: u32);
/// }
///
/// let mut mock = MockFoo::new();
/// mock.expect_foo()
///     .with(params!(eq(42), eq(43)))
///     .return_const(());
///
/// mock.foo(42, 43);
/// ```
// TODO: consider rewriting as a proc macro so it can support any number of
// arguments.
// TODO: Instead of using predicate::function, create a custom Predicate
// implementation that will yield better error messages.
#[macro_export]
macro_rules! params{
    ($p0:expr) => {
        ::mockall::predicate::function(move |x0| $p0.eval(x0))
    };
    ($p0:expr, $p1:expr) => {
        ::mockall::predicate::function(move |(x0, x1)|
            $p0.eval(x0) && $p1.eval(x1))
    };
    ($p0:expr, $p1:expr, $p2: expr) => {
        ::mockall::predicate::function(move |(x0, x1, x2)|
            $p0.eval(x0) && $p1.eval(x1) && $p2.eval(x2))
    };
    ($p0:expr, $p1:expr, $p2: expr, $p3: expr) => {
        ::mockall::predicate::function(move |(x0, x1, x2, x3)|
            $p0.eval(x0) && $p1.eval(x1) && $p2.eval(x2) && $p3.eval)
    };
}

#[derive(Debug)]
#[doc(hidden)]
pub struct Times{
    /// How many times has the expectation already been called?
    count: AtomicUsize,
    range: Range<usize>
}

impl Times {
    pub fn call(&self) {
        let count = self.count.fetch_add(1, Ordering::Relaxed) + 1;
        if count >= self.range.end {
            if self.range.end == 0 {
                panic!("Expectation should not have been called");
            } else {
                let lim = self.range.end - 1;
                panic!("Expectation called more than {} times", lim);
            }
        }
    }

    pub fn any(&mut self) {
        self.range = 0..usize::max_value();
    }

    /// Has this expectation already been called the maximum allowed number of
    /// times?
    pub fn is_done(&self) -> bool {
        self.count.load(Ordering::Relaxed) >= self.range.end - 1
    }

    /// Is it required that this expectation be called an exact number of times,
    /// or may it be satisfied by a range of call counts?
    pub fn is_exact(&self) -> bool {
        (self.range.end - self.range.start) == 1
    }

    /// Has this expectation already been called the minimum required number of
    /// times?
    pub fn is_satisfied(&self) -> bool {
        self.count.load(Ordering::Relaxed) >= self.range.start
    }

    // https://github.com/rust-lang/rust-clippy/issues/3307
    #[allow(clippy::range_plus_one)]
    pub fn n(&mut self, n: usize) {
        self.range = n..(n+1);
    }

    pub fn never(&mut self) {
        self.range = 0..0;
    }

    pub fn range(&mut self, range: Range<usize>) {
        self.range = range;
    }
}

impl Default for Times {
    fn default() -> Self {
        // By default, allow any number of calls
        let count = AtomicUsize::default();
        let range = 0..usize::max_value();
        Times{count, range}
    }
}

impl Drop for Times {
    fn drop(&mut self) {
        let count = self.count.load(Ordering::Relaxed);
        if !thread::panicking() && (count < self.range.start) {
            panic!("Expectation called fewer than {} times", self.range.start);
        }
    }
}

impl<I, O> Default for Expectation<I, O> {
    fn default() -> Self {
        Expectation::new()
    }
}

struct ExpectationCommon<I> {
    matcher: Matcher<I>,
    seq_handle: Option<SeqHandle>,
    times: Times
}

impl<I> ExpectationCommon<I> {
    /// Simulating calling the real method for this expectation
    fn call(&self, i: &I) {
        self.matcher.verify(i);
        self.times.call();
        self.verify_sequence();
        if self.times.is_satisfied() {
            self.satisfy_sequence()
        }
    }

    fn in_sequence(&mut self, seq: &mut Sequence) -> &mut Self {
        assert!(self.times.is_exact(),
            "Only Expectations with an exact call count have sequences");
        self.seq_handle = Some(seq.next());
        self
    }

    fn is_done(&self) -> bool {
        self.times.is_done()
    }

    fn matches(&self, i: &I) -> bool {
        self.matcher.matches(i)
    }

    /// Forbid this expectation from ever being called
    fn never(&mut self) {
        self.times.never();
    }

    /// Let the sequence know that this expectation has been satisfied
    fn satisfy_sequence(&self) {
        if let Some(handle) = &self.seq_handle {
            handle.satisfy()
        }
    }

    /// Require this expectation to be called exactly `n` times.
    fn times(&mut self, n: usize) {
        self.times.n(n);
    }

    /// Allow this expectation to be called any number of times
    fn times_any(&mut self) {
        self.times.any();
    }

    /// Allow this expectation to be called any number of times within a given
    /// range
    fn times_range(&mut self, range: Range<usize>) {
        self.times.range(range);
    }

    /// Validate this expectation's sequence constraint
    fn verify_sequence(&self) {
        if let Some(handle) = &self.seq_handle {
            handle.verify()
        }
    }

    fn with<P>(&mut self, p: P)
        where P: Predicate<I> + Send + 'static
    {
        self.matcher = Matcher::new(p);
    }

    fn withf<F>(&mut self, f: F)
        where F: Fn(&I) -> bool + Send + 'static, I: Send + 'static
    {
        let p = predicate::function(f);
        self.matcher = Matcher::new(p);
    }
}

impl<I> Default for ExpectationCommon<I> {
    fn default() -> Self {
        ExpectationCommon {
            times: Times::default(),
            matcher: Matcher::default(),
            seq_handle: None,
        }
    }
}

/// Add common methods to Expectation types.  Must be called from within an
/// impl<I, O> {} block
macro_rules! expectation_common {
    ($common: ident, $klass:ident) => {
        /// Add this expectation to a [`Sequence`](struct.Sequence.html).
        pub fn in_sequence(&mut self, seq: &mut Sequence) -> &mut Self {
            self.$common.in_sequence(seq);
            self
        }

        fn is_done(&self) -> bool {
            self.common.is_done()
        }

        /// Validate this expectation's matcher.
        fn matches(&self, i: &I) -> bool {
            self.common.matches(i)
        }

        /// Forbid this expectation from ever being called
        pub fn never(&mut self) -> &mut Self {
            self.$common.never();
            self
        }

        /// Expect this expectation to be called exactly once.  Shortcut for
        /// [`times(1)`](#method.times).
        pub fn once(&mut self) -> &mut Self {
            self.times(1)
        }

        /// Require this expectation to be called exactly `n` times.
        pub fn times(&mut self, n: usize) -> &mut Self {
            self.$common.times(n);
            self
        }

        /// Allow this expectation to be called any number of times
        ///
        /// This behavior is the default, but the method is provided in case the
        /// default behavior changes.
        pub fn times_any(&mut self) -> &mut Self {
            self.$common.times_any();
            self
        }

        /// Allow this expectation to be called any number of times within a
        /// given range
        pub fn times_range(&mut self, range: Range<usize>) -> &mut Self {
            self.$common.times_range(range);
            self
        }

        /// Set matching crieteria for this Expectation.
        ///
        /// The matching predicate can be anything implemening the
        /// [`Predicate`](#trait.Predicate) trait.  Only one matcher can be set
        /// per `Expectation` at a time.
        ///
        /// # Examples
        /// ```
        /// # use mockall::*;
        /// let e1 = Expectation::<(u32, u32), ()>::new()
        ///     .with(predicate::function(|(x, y)| x == y));
        /// let e2 = Expectation::<u32, ()>::new()
        ///     .with(predicate::eq(5));
        /// ```
        pub fn with<P>(&mut self, p: P) -> &mut Self
            where P: Predicate<I> + Send + 'static
        {
            self.$common.with(p);
            self
        }

        /// Set a matching function for this Expectation.
        ///
        /// This is equivalent to calling [`with`](#method.with) with a function
        /// argument, like `with(predicate::function(f))`.
        ///
        /// # Examples
        /// ```
        /// # use mockall::*;
        /// let e1 = Expectation::<(u32, u32), ()>::new()
        ///     .withf(|(x, y)| x == y);
        /// ```
        pub fn withf<F>(&mut self, f: F) -> &mut Self
            where F: Fn(&I) -> bool + Send + 'static, I: Send + 'static
        {
            self.$common.withf(f);
            self
        }

        /// Like [`withf`](#method.withf), but it unsafely implements `Send`.
        ///
        /// This method is useful when working with methods that take pointer
        /// or reference arguments.
        ///
        /// # Safety
        ///
        /// The only `unsafe` thing this method does is to unsafely implement
        /// `Send` on an object built from the supplied closure.  It's `unsafe`
        /// because raw pointers aren't `Send`.  For the common case of mocking
        /// a method with reference arguments converted into pointers by
        /// Mockall, it should be sufficient that your closure does not copy the
        /// pointers or retain a reference to anything they point to.  And of
        /// course, it's safe to use in single-threaded tests.
        pub unsafe fn withf_unsafe<F>(&mut self, f: F) -> &mut Self
            where F: Fn(&I) -> bool + 'static, I: 'static
        {
            let p = UnsafeFnPredicate(predicate::function(f));
            self.$common.with(p);
            self
        }
    }
}

/// Add common methods to Expectations types.  Must be called from within an
/// impl<I, O> {} block
macro_rules! expectations_common {
    ($klass:ident, $call:ident, $self_ty:ty, $iter:ident, $oty:ty) => {
        /// Simulating calling the real method.  Every current expectation will
        /// be checked in FIFO order and the first one with matching arguments
        /// will be used.
        pub fn $call(self: $self_ty, i: I) -> $oty {
            let n = self.0.len();
            match (self.0).$iter().find(|e| e.matches(&i) &&
                                        (!e.is_done() || n == 1))
            {
                None => panic!("No matching expectation found"),
                Some(e) => e.$call(i)
            }
        }

        /// Verify that all current expectations are satisfied and clear them.
        pub fn checkpoint(&mut self) {
            self.0.drain(..);
        }

        /// Create a new expectation for this method.
        pub fn expect(&mut self) -> &mut $klass<I, O> {
            let e = $klass::new();
            self.0.push(e);
            let l = self.0.len();
            &mut self.0[l - 1]
        }

        pub fn new() -> Self {
            Self::default()
        }
    }
}

/// Expectation type for methods that take return a `'static` type.
pub struct Expectation<I, O> {
    common: ExpectationCommon<I>,
    rfunc: Mutex<Rfunc<I, O>>,
}

impl<I, O> Expectation<I, O> {
    /// Simulating calling the real method for this expectation
    pub fn call(&self, i: I) -> O {
        self.common.call(&i);
        self.rfunc.lock().unwrap()
            .call_mut(i)
    }

    pub fn new() -> Self {
        let common = ExpectationCommon::default();
        let rfunc = Mutex::new(Rfunc::Default);
        Expectation{common, rfunc}
    }

    /// Supply an `FnOnce` closure that will provide the return value for this
    /// Expectation.  This is useful for return types that aren't `Clone`.  It
    /// will be an error to call this Expectation multiple times.
    pub fn return_once<F>(&mut self, f: F) -> &mut Self
        where F: FnOnce(I) -> O + Send + 'static
    {
        let mut fopt = Some(f);
        let fmut = move |i| {
            if let Some(f) = fopt.take() {
                f(i)
            } else {
                panic!("Called a method twice that was expected only once")
            }
        };
        {
            let mut guard = self.rfunc.lock().unwrap();
            mem::replace(guard.deref_mut(), Rfunc::Once(Box::new(fmut)));
        }
        self
    }

    /// Single-threaded version of [`return_once`](#method.return_once).  This
    /// is useful for return types that are neither `Send` nor `Clone`.
    ///
    /// It is a runtime error to call the mock method from a different thread
    /// than the one that originally called this method.  It is also a runtime
    /// error to call the method more than once.
    pub fn return_once_st<F>(&mut self, f: F) -> &mut Self
        where F: FnOnce(I) -> O + 'static
    {
        let mut fragile = Some(Fragile::new(f));
        let fmut = Box::new(move |i: I| {
            match fragile.take() {
                Some(frag) => (frag.into_inner())(i),
                None => panic!(
                    "Called a method twice that was expected only once")
            }
        });
        {
            let mut guard = self.rfunc.lock().unwrap();
            mem::replace(guard.deref_mut(), Rfunc::Once(fmut));
        }
        self
    }

    /// Supply a closure that will provide the return value for this
    /// Expectation.  The method's arguments are passed to the closure by value.
    pub fn returning<F>(&mut self, f: F) -> &mut Self
        where F: FnMut(I) -> O + Send + 'static
    {
        {
            let mut guard = self.rfunc.lock().unwrap();
            mem::replace(guard.deref_mut(), Rfunc::Mut(Box::new(f)));
        }
        self
    }

    /// Single-threaded version of [`returning`](#method.returning).  Can be
    /// used when the argument or return type isn't `Send`.
    ///
    /// It is a runtime error to call the mock method from a different thread
    /// than the one that originally called this method.
    pub fn returning_st<F>(&mut self, f: F) -> &mut Self
        where F: FnMut(I) -> O + 'static
    {
        let mut fragile = Fragile::new(f);
        let fmut = move |i: I| {
            (fragile.get_mut())(i)
        };
        {
            let mut guard = self.rfunc.lock().unwrap();
            mem::replace(guard.deref_mut(), Rfunc::Mut(Box::new(fmut)));
        }
        self
    }

    expectation_common!{common, Expectation}
}

impl<I, O: Clone + Send + 'static> Expectation<I, O> {
    /// Return a constant value from the `Expectation`
    ///
    /// The output type must be `Clone`.
    pub fn return_const(&mut self, c: O) -> &mut Self {
        let f = move |_| c.clone();
        self.returning(f)
    }
}

/// A Collection of [`Expectation`](struct.Expectation.html)s.  Allows you to
/// set more than one expectation on the same method.
pub struct Expectations<I, O>(Vec<Expectation<I, O>>);

impl<I, O> Expectations<I, O> {
    expectations_common!{Expectation, call, &Self, iter, O}
}

impl<I, O> Default for Expectations<I, O> {
    fn default() -> Self {
        Expectations(Vec::new())
    }
}

impl<I: 'static, O: 'static> AnyExpectations for Expectations<I, O> {}

/// Expectation type for methods that take a `&self` argument and return a
/// reference with the same lifetime as `self`.
pub struct RefExpectation<I, O> {
    common: ExpectationCommon<I>,
    result: Option<O>,
}

impl<I, O> RefExpectation<I, O> {
    /// Simulating calling the real method for this expectation
    pub fn call(&self, i: I) -> &O {
        self.common.call(&i);
        &self.result.as_ref()
            .expect("Must set return value with RefExpectation::return_const")
    }

    pub fn new() -> Self {
        let common = ExpectationCommon::default();
        RefExpectation{common, result: None}
    }

    /// Return a reference to a constant value from the `RefExpectation`
    pub fn return_const(&mut self, o: O) -> &mut Self {
        self.result = Some(o);
        self
    }

    expectation_common!{common, RefExpectation}
}

impl<I, O> Default for RefExpectation<I, O> {
    fn default() -> Self {
        RefExpectation::new()
    }
}

/// A Collection of [`RefExpectation`](struct.RefExpectation.html)s.  Allows you
/// to set more than one expectation on the same method.
pub struct RefExpectations<I, O>(Vec<RefExpectation<I, O>>);

impl<I, O> RefExpectations<I, O> {
    expectations_common!{RefExpectation, call, &Self, iter, &O}
}

impl<I, O> Default for RefExpectations<I, O> {
    fn default() -> Self {
        RefExpectations(Vec::new())
    }
}

impl<I, O> AnyExpectations for RefExpectations<I, O>
    where I: 'static, O: Send + Sync + 'static
{}

/// Expectation type for methods that take a `&mut self` argument and return a
/// mutable or immutable reference with the same lifetime as `self`.
pub struct RefMutExpectation<I, O> {
    common: ExpectationCommon<I>,
    result: Option<O>,
    rfunc: Option<Box<dyn FnMut(I) -> O + Send + Sync>>,
}

impl<I, O> RefMutExpectation<I, O> {
    /// Simulating calling the real method for this expectation
    pub fn call_mut(&mut self, i: I) -> &mut O {
        self.common.call(&i);
        if let Some(ref mut f) = self.rfunc {
            self.result = Some(f(i));
        }
        self.result.as_mut().expect("Must first set return function with RefMutExpectation::returning or return_var")
    }

    pub fn new() -> Self {
        let common = ExpectationCommon::default();
        RefMutExpectation{common, result: None, rfunc: None}
    }

    /// Convenience method that can be used to supply a return value for a
    /// `RefMutExpectation`.  The value will be returned by mutable reference.
    pub fn return_var(&mut self, o: O) -> &mut Self
    {
        self.result = Some(o);
        self
    }

    /// Supply a closure that the `RefMutExpectation` will use to create its
    /// return value.  The return value will be returned by mutable reference.
    pub fn returning<F>(&mut self, f: F) -> &mut Self
        where F: FnMut(I) -> O + Send + Sync + 'static
    {
        mem::replace(&mut self.rfunc, Some(Box::new(f)));
        self
    }

    /// Single-threaded version of [`returning`](#method.returning).  Can be
    /// used when the argument or return type isn't `Send`.
    pub fn returning_st<F>(&mut self, f: F) -> &mut Self
        where F: FnMut(I) -> O + 'static
    {
        let mut fragile = Fragile::new(f);
        let fmut = move |i: I| {
            (fragile.get_mut())(i)
        };
        mem::replace(&mut self.rfunc, Some(Box::new(fmut)));
        self
    }

    expectation_common!{common, RefMutExpectation}
}

impl<I, O> Default for RefMutExpectation<I, O> {
    fn default() -> Self {
        RefMutExpectation::new()
    }
}

/// A Collection of [`RefMutExpectation`](struct.RefMutExpectation.html)s.
/// Allows you to set more than one expectation on the same method.
pub struct RefMutExpectations<I, O>(Vec<RefMutExpectation<I, O>>);

impl<I, O> RefMutExpectations<I, O> {
    expectations_common!{RefMutExpectation, call_mut, &mut Self, iter_mut,
        &mut O}
}

impl<I, O> Default for RefMutExpectations<I, O> {
    fn default() -> Self {
        RefMutExpectations(Vec::new())
    }
}

impl<I, O> AnyExpectations for RefMutExpectations<I, O>
    where I: 'static, O: Send + Sync + 'static
{}

/// Non-generic keys to `GenericExpectation` internal storage
#[doc(hidden)]
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct Key(any::TypeId);

impl Key {
    pub fn new<I: 'static, O: 'static>() -> Self {
        Key(any::TypeId::of::<(I, O)>())
    }
}

/// Container for the expectations of generic methods.
///
/// Effectively this contains a separate
/// [`Expectations`](struct.Expectations.html) object for each unique set of
/// generic parameters.
///
/// Requires the methods to have `'static` return values.  Currently requires
/// `'static` arguments as well.
#[derive(Default)]
pub struct GenericExpectations {
    store: HashMap<Key, Box<dyn AnyExpectations>>
}

impl GenericExpectations {
    /// Simulating calling the real method.
    pub fn call<I: 'static, O: 'static>(&self, args: I) -> O {
        let key = Key::new::<I, O>();
        let e: &Expectations<I, O> = self.store.get(&key)
            .expect("No matching expectation found")
            .downcast_ref()
            .unwrap();
        e.call(args)
    }

    /// Verify that all current expectations are satisfied and clear them.  This
    /// applies to all sets of generic parameters!
    pub fn checkpoint(&mut self) {
        self.store.clear();
    }

    /// Create a new [`Expectation`](struct.Expectation.html).
    pub fn expect<'e, I, O>(&'e mut self) -> &'e mut Expectation<I, O>
        where I: 'static, O: 'static
    {
        let key = Key::new::<I, O>();
        let ee: &mut Expectations<I, O> = self.store.entry(key)
            .or_insert_with(|| Box::new(Expectations::<I, O>::new()))
            .downcast_mut()
            .unwrap();
        ee.expect()
    }

    pub fn new() -> Self {
        Self::default()
    }
}

/// Container for the expectations of generic methods that take `&self`
/// arguments and return immutable references with the same lifetime as `&self`.
///
/// Effectively this contains a separate
/// [`RefExpectations`](struct.RefExpectations.html) object for each unique set
/// of generic parameters.
///
/// Currently requires `'static` input types and `Send + Sync` output types, but
/// those restrictions may be reduced in the future.
#[derive(Default)]
pub struct GenericRefExpectations {
    store: HashMap<Key, Box<dyn AnyExpectations>>,
}

impl GenericRefExpectations {
    /// Simulating calling the real method.
    pub fn call<I: 'static, O: 'static>(&self, args: I) -> &O {
        let key = Key::new::<I, O>();
        let e: &RefExpectations<I, O> = self.store.get(&key)
            .expect("No matching expectation found")
            .downcast_ref()
            .unwrap();
        e.call(args)
    }

    /// Verify that all current expectations are satisfied and clear them.  This
    /// affects all sets of generic parameters!
    pub fn checkpoint(&mut self) {
        self.store.clear();
    }

    /// Create a new [`RefExpectation`](struct.RefExpectation.html).
    pub fn expect<'e, I, O>(&'e mut self) -> &'e mut RefExpectation<I, O>
        where I: 'static, O: Send + Sync + 'static
    {
        let key = Key::new::<I, O>();
        let ee: &mut RefExpectations<I, O> = self.store.entry(key)
            .or_insert_with(|| Box::new(RefExpectations::<I, O>::new()))
            .downcast_mut()
            .unwrap();
        ee.expect()
    }
}

/// Container for the expectations of generic methods that take `&mut self`
/// arguments and return references with the same lifetime as `&self`.
///
/// Effectively this contains a separate
/// [`RefMutExpectations`](struct.RefMutExpectations.html) object for each
/// unique set of generic parameters.
///
/// Currently requires `'static` input types and `Send + Sync` output types, but
/// those restrictions may be reduced in the future.
#[derive(Default)]
pub struct GenericRefMutExpectations {
    store: HashMap<Key, Box<dyn AnyExpectations>>,
}

impl GenericRefMutExpectations {
    /// Simulating calling the real method.
    pub fn call_mut<I: 'static, O: 'static>(&mut self, args: I) -> &mut O {
        let key = Key::new::<I, O>();
        let e: &mut RefMutExpectations<I, O> = self.store.get_mut(&key)
            .expect("No matching expectation found")
            .downcast_mut()
            .unwrap();
        e.call_mut(args)
    }

    /// Verify that all current expectations are satisfied and clear them.  This
    /// affects all sets of generic parameters!
    pub fn checkpoint(&mut self) {
        self.store.clear();
    }

    /// Create a new [`RefMutExpectation`](struct.RefMutExpectation.html).
    pub fn expect<'e, I, O>(&'e mut self) -> &'e mut RefMutExpectation<I, O>
        where I: 'static, O: Send + Sync + 'static
    {
        let key = Key::new::<I, O>();
        let ee: &mut RefMutExpectations<I, O> = self.store.entry(key)
            .or_insert_with(|| Box::new(RefMutExpectations::<I, O>::new()))
            .downcast_mut()
            .unwrap();
        ee.expect()
    }
}

/// Like an [`&Expectation`](struct.Expectation.html) but protected by a Mutex
/// guard.  Useful for mocking static methods.  Forwards accesses to an
/// `Expectation<I, O>` object.
// We must return the MutexGuard to the caller so he can configure the
// expectation.  But we can't bundle both the guard and the &Expectation into
// the same structure; the borrow checker won't let us.  Instead we'll record
// the expectation's position within the Expectations vector so we can proxy its
// methods.
pub struct ExpectationGuard<'guard, I, O>{
    guard: MutexGuard<'guard, Expectations<I, O>>,
    i: usize
}

impl<'guard, I, O> ExpectationGuard<'guard, I, O> {
    /// Just like
    /// [`Expectation::in_sequence`](struct.Expectation.html#method.in_sequence)
    pub fn in_sequence(&mut self, seq: &mut Sequence) -> &mut Expectation<I, O>
    {
        self.guard.0[self.i].in_sequence(seq)
    }

    /// Just like
    /// [`Expectation::never`](struct.Expectation.html#method.never)
    pub fn never(&mut self) -> &mut Expectation<I, O> {
        self.guard.0[self.i].never()
    }

    // Should only be called from the mockall_derive generated code
    #[doc(hidden)]
    pub fn new(mut guard: MutexGuard<'guard, Expectations<I, O>>) -> Self {
        guard.expect(); // Drop the &Expectation
        let i = guard.0.len() - 1;
        ExpectationGuard{guard, i}
    }

    /// Just like [`Expectation::once`](struct.Expectation.html#method.once)
    pub fn once(&mut self) -> &mut Expectation<I, O> {
        self.guard.0[self.i].once()
    }

    /// Just like
    /// [`Expectation::returning`](struct.Expectation.html#method.returning)
    pub fn returning<F>(&mut self, f: F) -> &mut Expectation<I, O>
        where F: FnMut(I) -> O + Send + 'static
    {
        self.guard.0[self.i].returning(f)
    }

    /// Just like
    /// [`Expectation::return_once`](struct.Expectation.html#method.return_once)
    pub fn return_once<F>(&mut self, f: F) -> &mut Expectation<I, O>
        where F: FnOnce(I) -> O + Send + 'static
    {
        self.guard.0[self.i].return_once(f)
    }

    /// Just like
    /// [`Expectation::returning_st`](struct.Expectation.html#method.returning_st)
    pub fn returning_st<F>(&mut self, f: F) -> &mut Expectation<I, O>
        where F: FnMut(I) -> O + 'static
    {
        self.guard.0[self.i].returning_st(f)
    }

    /// Just like
    /// [`Expectation::times`](struct.Expectation.html#method.times)
    pub fn times(&mut self, n: usize) -> &mut Expectation<I, O> {
        self.guard.0[self.i].times(n)
    }

    /// Just like
    /// [`Expectation::times_any`](struct.Expectation.html#method.times_any)
    pub fn times_any(&mut self) -> &mut Expectation<I, O> {
        self.guard.0[self.i].times_any()
    }

    /// Just like
    /// [`Expectation::times_range`](struct.Expectation.html#method.times_range)
    pub fn times_range(&mut self, range: Range<usize>) -> &mut Expectation<I, O>
    {
        self.guard.0[self.i].times_range(range)
    }

    /// Just like
    /// [`Expectation::with`](struct.Expectation.html#method.with)
    pub fn with<P>(&mut self, p: P) -> &mut Expectation<I, O>
        where P: Predicate<I> + Send + 'static
    {
        self.guard.0[self.i].with(p)
    }

    /// Just like
    /// [`Expectation::withf`](struct.Expectation.html#method.withf)
    pub fn withf<F>(&mut self, f: F) -> &mut Expectation<I, O>
        where F: Fn(&I) -> bool + Send + 'static, I: Send + 'static
    {
        self.guard.0[self.i].withf(f)
    }

    /// Just like
    /// [`Expectation::withf_unsafe`](struct.Expectation.html#method.withf_unsafe)
    pub unsafe fn withf_unsafe<F>(&mut self, f: F) -> &mut Expectation<I, O>
        where F: Fn(&I) -> bool + 'static, I: 'static
    {
        self.guard.0[self.i].withf_unsafe(f)
    }
}

/// Like an [`&ExpectationGuard`](struct.ExpectationGuard.html) but for generic
/// methods.
pub struct GenericExpectationGuard<'guard, I: 'static, O: 'static> {
    guard: MutexGuard<'guard, GenericExpectations>,
    i: usize,
    _i: PhantomData<I>,
    _o: PhantomData<O>,
}

impl<'guard, I: 'static, O: 'static> GenericExpectationGuard<'guard, I, O> {
    /// Just like
    /// [`Expectation::in_sequence`](struct.Expectation.html#method.in_sequence)
    pub fn in_sequence(&mut self, seq: &mut Sequence) -> &mut Expectation<I, O>
    {
        let key = Key::new::<I, O>();
        let ee: &mut Expectations<I, O> =
            self.guard.store.get_mut(&key).unwrap()
            .downcast_mut().unwrap();
        ee.0[self.i].in_sequence(seq)
    }

    /// Just like
    /// [`Expectation::never`](struct.Expectation.html#method.never)
    pub fn never(&mut self) -> &mut Expectation<I, O> {
        let key = Key::new::<I, O>();
        let ee: &mut Expectations<I, O> =
            self.guard.store.get_mut(&key).unwrap()
            .downcast_mut().unwrap();
        ee.0[self.i].never()
    }

    // Should only be called from the mockall_derive generated code
    #[doc(hidden)]
    pub fn new(mut guard: MutexGuard<'guard, GenericExpectations>)
        -> Self
    {
        let key = Key::new::<I, O>();
        let ee: &mut Expectations<I, O> = guard.store.entry(key)
            .or_insert_with(|| Box::new(Expectations::<I, O>::new()))
            .downcast_mut()
            .unwrap();
        ee.expect();    // Drop the &Expectation
        let i = ee.0.len() - 1;
        GenericExpectationGuard{guard, i, _i: PhantomData, _o: PhantomData}
    }

    /// Just like
    /// [`Expectation::once`](struct.Expectation.html#method.once)
    pub fn once(&mut self) -> &mut Expectation<I, O> {
        let key = Key::new::<I, O>();
        let ee: &mut Expectations<I, O> =
            self.guard.store.get_mut(&key).unwrap()
            .downcast_mut().unwrap();
        ee.0[self.i].once()
    }

    /// Just like
    /// [`Expectation::returning`](struct.Expectation.html#method.returning)
    pub fn returning<F>(&mut self, f: F) -> &mut Expectation<I, O>
        where F: FnMut(I) -> O + Send + 'static
    {
        let key = Key::new::<I, O>();
        let ee: &mut Expectations<I, O> =
            self.guard.store.get_mut(&key).unwrap()
            .downcast_mut().unwrap();
        ee.0[self.i].returning(f)
    }

    /// Just like
    /// [`Expectation::return_once`](struct.Expectation.html#method.return_once)
    pub fn return_once<F>(&mut self, f: F) -> &mut Expectation<I, O>
        where F: FnOnce(I) -> O + Send + 'static
    {
        let key = Key::new::<I, O>();
        let ee: &mut Expectations<I, O> =
            self.guard.store.get_mut(&key).unwrap()
            .downcast_mut().unwrap();
        ee.0[self.i].return_once(f)
    }

    /// Just like
    /// [`Expectation::returning_st`](struct.Expectation.html#method.returning_st)
    pub fn returning_st<F>(&mut self, f: F) -> &mut Expectation<I, O>
        where F: FnMut(I) -> O + 'static
    {
        let key = Key::new::<I, O>();
        let ee: &mut Expectations<I, O> =
            self.guard.store.get_mut(&key).unwrap()
            .downcast_mut().unwrap();
        ee.0[self.i].returning_st(f)
    }

    /// Just like
    /// [`Expectation::times`](struct.Expectation.html#method.times)
    pub fn times(&mut self, n: usize) -> &mut Expectation<I, O> {
        let key = Key::new::<I, O>();
        let ee: &mut Expectations<I, O> =
            self.guard.store.get_mut(&key).unwrap()
            .downcast_mut().unwrap();
        ee.0[self.i].times(n)
    }

    /// Just like
    /// [`Expectation::times_any`](struct.Expectation.html#method.times_any)
    pub fn times_any(&mut self) -> &mut Expectation<I, O> {
        let key = Key::new::<I, O>();
        let ee: &mut Expectations<I, O> =
            self.guard.store.get_mut(&key).unwrap()
            .downcast_mut().unwrap();
        ee.0[self.i].times_any()
    }

    /// Just like
    /// [`Expectation::times_range`](struct.Expectation.html#method.times_range)
    pub fn times_range(&mut self, range: Range<usize>) -> &mut Expectation<I, O>
    {
        let key = Key::new::<I, O>();
        let ee: &mut Expectations<I, O> =
            self.guard.store.get_mut(&key).unwrap()
            .downcast_mut().unwrap();
        ee.0[self.i].times_range(range)
    }

    /// Just like
    /// [`Expectation::with`](struct.Expectation.html#method.with)
    pub fn with<P>(&mut self, p: P) -> &mut Expectation<I, O>
        where P: Predicate<I> + Send + 'static
    {
        let key = Key::new::<I, O>();
        let ee: &mut Expectations<I, O> =
            self.guard.store.get_mut(&key).unwrap()
            .downcast_mut().unwrap();
        ee.0[self.i].with(p)
    }

    /// Just like
    /// [`Expectation::withf`](struct.Expectation.html#method.withf)
    pub fn withf<F>(&mut self, f: F) -> &mut Expectation<I, O>
        where F: Fn(&I) -> bool + Send + 'static, I: Send + 'static
    {
        let key = Key::new::<I, O>();
        let ee: &mut Expectations<I, O> =
            self.guard.store.get_mut(&key).unwrap()
            .downcast_mut().unwrap();
        ee.0[self.i].withf(f)
    }

    /// Just like
    /// [`Expectation::withf_unsafe`](struct.Expectation.html#method.withf_unsafe)
    pub unsafe fn withf_unsafe<F>(&mut self, f: F) -> &mut Expectation<I, O>
        where F: Fn(&I) -> bool + 'static, I: 'static
    {
        let key = Key::new::<I, O>();
        let ee: &mut Expectations<I, O> =
            self.guard.store.get_mut(&key).unwrap()
            .downcast_mut().unwrap();
        ee.0[self.i].withf_unsafe(f)
    }
}


pub struct SeqHandle {
    inner: Arc<SeqInner>,
    seq: usize
}

impl SeqHandle {
    /// Tell the Sequence that this expectation has been fully satisfied
    pub fn satisfy(&self) {
        self.inner.satisfy(self.seq);
    }

    /// Verify that this handle was called in the correct order
    pub fn verify(&self) {
        self.inner.verify(self.seq);
    }
}

#[derive(Default)]
struct SeqInner {
    satisfaction_level: AtomicUsize,
}

impl SeqInner {
    /// Record the call identified by `seq` as fully satisfied.
    fn satisfy(&self, seq: usize) {
        let old_sl = self.satisfaction_level.fetch_add(1, Ordering::Relaxed);
        assert_eq!(old_sl, seq, "Method sequence violation.  Was an already-satisfied method called another time?");
    }

    /// Verify that the call identified by `seq` was called in the correct order
    fn verify(&self, seq: usize) {
        assert_eq!(seq, self.satisfaction_level.load(Ordering::Relaxed),
            "Method sequence violation")
    }
}

/// Used to enforce that mock calls must happen in the sequence specified.
///
/// Each expectation must expect to be called a fixed number of times.  Once
/// satisfied, the next expectation in the sequence will expect to be called.
///
/// # Examples
/// ```
/// # use mockall::*;
/// let mut seq = Sequence::new();
///
/// let mut e1 = Expectation::<u32, ()>::new();
/// e1.times(1);
/// e1.returning(|_| ());
/// e1.in_sequence(&mut seq);
///
/// let mut e2 = Expectation::<(), u32>::new();
/// e2.times(1);
/// e2.returning(|_| 42);
/// e2.in_sequence(&mut seq);
///
/// e1.call(5);
/// e2.call(());
/// ```
///
/// It is an error to add an expectation to a `Sequence` if its call count is
/// unspecified.
/// ```should_panic(expected = "with an exact call count")
/// # use mockall::*;
/// let mut seq = Sequence::new();
///
/// let mut e1 = Expectation::<u32, ()>::new();
/// e1.returning(|_| ());
/// e1.in_sequence(&mut seq);   // panics!
/// ```
#[derive(Default)]
pub struct Sequence {
    inner: Arc<SeqInner>,
    next_seq: usize,
}

impl Sequence {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn next(&mut self) -> SeqHandle {
        let handle = SeqHandle{inner: self.inner.clone(), seq: self.next_seq};
        self.next_seq += 1;
        handle
    }
}
