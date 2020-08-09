// vim: tw=80
//! A generic method whose return value's lifetime is a generic parameter is a
//! special case.  Mockall can only mock such methods if the expectation is
//! 'static.
//! https://github.com/asomers/mockall/issues/76
#![deny(warnings)]

use mockall::*;

#[derive(Clone)]
struct X<'a>(&'a u32);

mock! {
    Thing {
        fn foo<'a>(&self) -> X<'a>;

        // XXX static methods don't work yet.
        // fn bar<'a>() -> X<'a>;

        fn baz<'a>(&self) -> &X<'a>;

        // Methods returning a mutable reference to a non-static value won't
        // work unless 'a is static.  I doubt there are any real-life methods
        // that fit this pattern; open an issue if you find one.
        // fn bang<'a>(&mut self) -> &mut X<'a>;

        // Generic methods can't return non-static values either, because
        // Mockall requires generic methods' generic parameters to implement
        // std::any::Any, which means they must be 'static.
        //fn bean<'a, T: 'static>(&self, t: T) -> X<'a>;
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

#[test]
fn return_static_ref() {
    const D: u32 = 42;
    let x = X(&D);
    let mut thing = MockThing::new();
    thing.expect_baz()
        .return_const(x);

    assert_eq!(42u32, *(*thing.baz()).0);
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
