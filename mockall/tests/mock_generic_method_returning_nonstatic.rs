// vim: tw=80
//! A generic method whose return value's lifetime is a generic parameter is a
//! special case.  Mockall can only mock such methods if the expectation is
//! 'static.
//! https://github.com/asomers/mockall/issues/76

use mockall::*;

#[derive(Clone)]
struct X<'a>(&'a u32);

mock! {
    Thing {
        fn foo<'a>(&self) -> X<'a>;
        // XXX static methods don't work yet.
        // fn bar<'a>() -> X<'a>;
        // TODO: add test cases for methods returning immutable and mutable
        // references and for generic methods
    }
}

#[test]
fn return_static() {
    const D: u32 = 42;
    let x = X(&D);
    let mut thing = MockThing::new();
    thing.expect_foo()
        .return_const(x);

    assert_eq!(42u32, *thing.foo().0);
}

// It isn't possible to safely set an expectation for a non-'static return value
// (because the mock object doesn't have any lifetime parameters itself), but
// unsafely setting such an expectation is a common use case.
#[test]
fn return_nonstatic() {
    let d = 42u32;
    let x = X(&d);
    let xstatic: X<'static> = unsafe{ std::mem::transmute(x) };
    let mut thing = MockThing::new();
    thing.expect_foo()
        .returning(move || xstatic.clone());

    assert_eq!(42u32, *thing.foo().0);
}
