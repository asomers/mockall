// vim: tw=80
//! A powerful mock object library for Rust.
//!
//! Mockall provides provides tools to create mock versions of almost any trait
//! or struct.  They can be used in unit tests as a stand-in for the real
//! object.
//!
//! # Usage
//!
//! There are two ways to use Mockall.  The easiest is to use
//! [`#[automock]`](https://docs.rs/mockall_derive/latest/mockall_derive/attr.automock.html).
//! It can mock most
//! traits, or structs that only have a single `impl` block.  For things it
//! can't handle, there is
//! [`mock!`](https://docs.rs/mockall_derive/latest/mockall_derive/macro.mock.html).
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
//! * [`Static Return values`](#static-return-values)
//! * [`Matching arguments`](#matching-arguments)
//! * [`Call counts`](#call-counts)
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
//! * [`Examples`](#examples)
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
//! ## Static Return values
//!
//! Every expectation must have an associated return value (though when the
//! **nightly** feature is enabled expectations will automatically return the
//! default values of their return types, if their return types implement
//! `Default`.).  For methods that return a `static` value, the macros will
//! generate an `Expectation` struct like
//! [`this`](https://docs.rs/mockall_examples/latest/mockall_examples/__mock_Foo_Foo/foo/struct.Expectation.html).
//! There are two ways to set such an expectation's return value: with a
//! constant
//! ([`return_const`](https://docs.rs/mockall_examples/latest/mockall_examples/__mock_Foo_Foo/foo/struct.Expectation.html#method.return_const))
//! or a closure
//! ([`returning`](https://docs.rs/mockall_examples/latest/mockall_examples/__mock_Foo_Foo/foo/struct.Expectation.html#method.returning)).
//! A closure will take the method's arguments by value.
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
//! [`return_once`](https://docs.rs/mockall_examples/latest/mockall_examples/__mock_Foo_Foo/foo/struct.Expectation.html#method.return_once)
//! method.
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
//! `return_once` can also be used for computing the return value with an
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
//! Mock objects are always `Send`.  If you need to use a return type that
//! isn't, you can use the
//! [`returning_st`](https://docs.rs/mockall_examples/latest/mockall_examples/__mock_Foo_Foo/foo/struct.Expectation.html#method.returning_st)
//! or
//! [`return_once_st`](https://docs.rs/mockall_examples/latest/mockall_examples/__mock_Foo_Foo/foo/struct.Expectation.html#method.return_once_st)
//! methods.
//! These take a non-`Send` object and add runtime access checks.  The wrapped
//! object will be `Send`, but accessing it from multiple threads will cause a
//! runtime panic.
//!
//! ```
//! # use mockall::*;
//! # use std::rc::Rc;
//! #[automock]
//! trait Foo {
//!     fn foo(&self, x: Rc<u32>) -> Rc<u32>;   // Rc<u32> isn't Send
//! }
//!
//! # fn main() {
//! let mut mock = MockFoo::new();
//! mock.expect_foo()
//!     .withf(|x| **x == 5)
//!     .returning_st(move |_| Rc::new(42u32));
//! assert_eq!(42, *mock.foo(Rc::new(5)));
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
//! For convenience,
//! [`withf`](https://docs.rs/mockall_examples/latest/mockall_examples/__mock_Foo_Foo/foo/struct.Expectation.html#method.withf)
//! is a shorthand for setting the commonly used
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
//! See also
//! [`never`](https://docs.rs/mockall_examples/latest/mockall_examples/__mock_Foo_Foo/foo/struct.Expectation.html#method.never),
//! [`times`](https://docs.rs/mockall_examples/latest/mockall_examples/__mock_Foo_Foo/foo/struct.Expectation.html#method.times),
//! [`times_any`](https://docs.rs/mockall_examples/latest/mockall_examples/__mock_Foo_Foo/foo/struct.Expectation.html#method.times_any),
//! and
//! [`times_range`](https://docs.rs/mockall_examples/latest/mockall_examples/__mock_Foo_Foo/foo/struct.Expectation.html#method.times_range).
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
//! and add new ones.  That's what checkpoints do.  Every mock object has a
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
//! lifetime of the returned reference must be either the same as the lifetime
//! of the mock object, or `'static`.
//!
//! Mockall creates different expectation types for methods that return
//! references.  Their API is the same as the basic `Expectation`, except for
//! setting return values.
//!
//! Methods that return `'static` references work just like methods that return
//! any other `'static` value.
//! ```
//! # use mockall::*;
//! struct Thing(u32);
//!
//! #[automock]
//! trait Container {
//!     fn get(&self, i: u32) -> &'static Thing;
//! }
//!
//! # fn main() {
//! const THING: Thing = Thing(42);
//! let mut mock = MockContainer::new();
//! mock.expect_get()
//!     .return_const(&THING);
//!
//! assert_eq!(42, mock.get(0).0);
//! # }
//! ```
//!
//! Methods that take a `&self` argument use an `Expectation` class like
//! [this](https://docs.rs/mockall_examples/latest/mockall_examples/__mock_Foo_Foo/bar/struct.Expectation.html),
//! which
//! gets its return value from the
//! [`return_const`](https://docs.rs/mockall_examples/latest/mockall_examples/__mock_Foo_Foo/bar/struct.Expectation.html#method.return_const) method.
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
//! Methods that take a `&mut self` argument use an `Expectation` class like
//! [this](https://docs.rs/mockall_examples/latest/mockall_examples/__mock_Foo_Foo/baz/struct.Expectation.html),
//! class, regardless of whether the return value is actually mutable.  They can
//! take their return value either from the
//! [`return_var`](https://docs.rs/mockall_examples/latest/mockall_examples/__mock_Foo_Foo/baz/struct.Expectation.html#method.return_var)
//! or
//! [`returning`](https://docs.rs/mockall_examples/latest/mockall_examples/__mock_Foo_Foo/baz/struct.Expectation.html#method.returning)
//! methods.
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
//! Unsized types that are common targets for
//! [`Deref`](https://doc.rust-lang.org/stable/std/ops/trait.Deref.html)
//! are special.  Mockall
//! will automatically use the type's owned form for the Expectation.
//! Currently, the
//! [`CStr`](https://doc.rust-lang.org/stable/std/ffi/struct.CStr.html),
//! [`OsStr`](https://doc.rust-lang.org/stable/std/ffi/struct.OsStr.html),
//! [`Path`](https://doc.rust-lang.org/stable/std/path/struct.Path.html),
//! and
//! [`str`](https://doc.rust-lang.org/stable/std/primitive.str.html)
//! types are
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
//! [`#[automock]`](https://docs.rs/mockall_derive/latest/mockall_derive/attr.automock.html)
//! works for structs that have a single `impl` block:
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
//! For structs with more than one `impl` block, see
//! [`mock!`](https://docs.rs/mockall_derive/latest/mockall_derive/macro.mock.html)
//! instead.
//!
//! ## Generic methods
//!
//! Generic methods can be mocked, too.  Effectively each generic method is an
//! infinite set of regular methods, and each of those works just like any other
//! regular method.  The expect_* method is generic, too, and usually must be
//! called with a turbofish.  The only restrictions on mocking generic methods
//! are that each generic parameter must be `'static`, and generic lifetime
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
//! [`#[automock]`](https://docs.rs/mockall_derive/latest/mockall_derive/attr.automock.html)
//! attribute.
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
//!     .returning(|x: u16| i32::from(x));
//! assert_eq!(4, mock.foo(4));
//! ```
//!
//! ## Multiple and inherited traits
//!
//! Creating a mock struct that implements multiple traits, whether inherited or
//! not, requires using the
//! [`mock!`](https://docs.rs/mockall_derive/latest/mockall_derive/macro.mock.html)
//! macro.  But once created, using it is just the same as using any other mock
//! object
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
//! beyond your control, but you must use 
//! [`mock!`](https://docs.rs/mockall_derive/latest/mockall_derive/macro.mock.html)
//! instead of [`#[automock]`](https://docs.rs/mockall_derive/latest/mockall_derive/attr.automock.html).
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
//! [`#[automock]`](https://docs.rs/mockall_derive/latest/mockall_derive/attr.automock.html)
//! to make it work.
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
//!         assert_eq!(43, do_stuff());
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
//! mod outer {
//!     # use mockall::automock;
//!     #[automock()]
//!     pub(super) mod inner {
//!         pub fn bar(x: u32) -> i64 {
//!             // ...
//!             # 4
//!         }
//!     }
//! }
//!
//! cfg_if! {
//!     if #[cfg(test)] {
//!         use outer::mock_inner as inner;
//!     } else {
//!         use outer::inner;
//!     }
//! }
//!
//! #[cfg(test)]
//! mod t {
//!     use super::*;
//!
//!     #[test]
//!     fn test_foo_bar() {
//!         inner::expect_bar()
//!             .returning(|x| i64::from(x + 1));
//!         assert_eq!(5, inner::bar(4));
//!     }
//! }
//! # fn main() {}
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
//! ## Examples
//!
//! For additional examples of Mockall in action, including detailed
//! documentation on the autogenerated methods, see
//! [`mockall_examples`](https://docs.rs/mockall_examples/latest/mockall_examples/).
//!
//! [`Predicate`]: trait.Predicate.html
//! [`Sequence`]: Sequence
//! [`cfg-if`]: https://crates.io/crates/cfg-if
//! [`function`]: predicate/fn.function.html
//! [`predicates`]: predicate/index.html

#![cfg_attr(feature = "nightly", feature(specialization))]
#![deny(intra_doc_link_resolution_failure)]

use downcast::*;
use std::{
    any,
    marker::PhantomData,
    ops::Range,
    sync::{
        Arc,
        atomic::{AtomicUsize, Ordering}
    },
    thread
};

pub use mockall_derive::{mock, automock};
pub use predicates::{
    boolean::PredicateBooleanExt,
    prelude::{Predicate, predicate}
};

/// For mocking static methods
#[doc(hidden)]
pub use lazy_static::lazy_static;

#[doc(hidden)]
pub trait AnyExpectations : Any + Send + Sync {}
downcast!(dyn AnyExpectations);

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
            if self.range.end == 1 {
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
        self.range = 0..1;
    }

    pub fn range(&mut self, range: Range<usize>) {
        assert!(range.end > range.start, "Backwards range");
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

/// Non-generic keys to `GenericExpectation` internal storage
#[doc(hidden)]
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct Key(any::TypeId);

impl Key {
    pub fn new<T: 'static>() -> Self {
        Key(any::TypeId::of::<T>())
    }
}

#[doc(hidden)]
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
/// #[automock]
/// trait Foo {
///     fn foo(&self);
///     fn bar(&self) -> u32;
/// }
/// let mut seq = Sequence::new();
///
/// let mut mock0 = MockFoo::new();
/// let mut mock1 = MockFoo::new();
///
/// mock0.expect_foo()
///     .times(1)
///     .returning(|| ())
///     .in_sequence(&mut seq);
///
/// mock1.expect_bar()
///     .times(1)
///     .returning(|| 42)
///     .in_sequence(&mut seq);
///
/// mock0.foo();
/// mock1.bar();
/// ```
///
/// It is an error to add an expectation to a `Sequence` if its call count is
/// unspecified.
/// ```should_panic(expected = "with an exact call count")
/// # use mockall::*;
/// #[automock]
/// trait Foo {
///     fn foo(&self);
/// }
/// let mut seq = Sequence::new();
///
/// let mut mock = MockFoo::new();
/// mock.expect_foo()
///     .returning(|| ())
///     .in_sequence(&mut seq);  // panics!
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

    /// Not for public consumption, but it must be public so the generated code
    /// can call it.
    #[doc(hidden)]
    pub fn next_handle(&mut self) -> SeqHandle {
        let handle = SeqHandle{inner: self.inner.clone(), seq: self.next_seq};
        self.next_seq += 1;
        handle
    }
}
