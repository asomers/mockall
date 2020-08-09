// vim: tw=80
//! A specializing method is a non-generic method of a generic struct that
//! places additional bounds on the struct's generic types via a where
//! clause.
#![deny(warnings)]

use mockall::*;

struct G<T: Copy + Default + 'static>(T);

#[derive(Clone, Copy)]
struct NonDefault(u32);

#[automock]
trait Foo<T> where T: Copy + 'static {
    fn foo(&self, t: T) -> G<T> where T: Default;
}

#[test]
fn returning() {
    let mut mock = MockFoo::<u32>::default();
    mock.expect_foo()
        .returning(G);
    assert_eq!(42u32, mock.foo(42u32).0);

    // It's possible to instantiate a mock object that doesn't satisfy the
    // specializing method's requirements:
    let _mock2 = MockFoo::<NonDefault>::default();
    // But you can't call the specializing method.  This won't work:
    // _mock2.expect_foo()
    //    .returning(|h| G(h));
    // _mock2.foo(NonDefault(42));
}
