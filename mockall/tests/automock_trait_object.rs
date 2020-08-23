// vim: tw=80
//! a method that uses unboxed trait object arguments
#![deny(warnings)]

use mockall::*;

#[automock]
trait Foo {
    fn foo(&self, x: &dyn PartialEq<u32>);
}

// It's almost impossible to construct a Predicate object that will work with
// trait objects for two reasons:
// * Mockall requires the predicates to be Debug, and any useful predicate will
//   also be Eq, Ord, or similar, but Rust only allows up to one non-auto trait
//   per trait object.
// * The predicate is requird to meet the HRTB
//   for<'a> Predicate<(dyn Trait + 'a)>
//   That's not impossible, but Mockall doesn't provide any way to construct
//   such a predicate.
// So, use of `with` is pretty much limited to predicates like `always` and
// `function`.
#[test]
fn with() {
    let mut mock = MockFoo::new();
    mock.expect_foo()
        .with(predicate::always())
        .return_const(());
    mock.foo(&42u32)
}

/// trait object arguments can't be matched with `predicate::eq`, because
/// `PartialEq<Self>` cannot be made into a trait object.  But withf still
/// works.
#[test]
fn withf() {
    let mut mock = MockFoo::new();
    mock.expect_foo()
        .withf(|x| *x == 42u32)
        .return_const(());
    mock.foo(&42u32);
}
