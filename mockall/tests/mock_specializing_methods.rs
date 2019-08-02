// vim: tw=80
//! A specializing method is a non-generic method of a generic struct that
//! places additional bounds on the struct's generic types via a where
//! clause.
//!
//! Without trait specialization, it probably won't be possible for Mockall
//! to support specializing methods.  Instead, the method must be made
//! generic, just like static methods of generic structs.

use mockall::*;

struct G<T: Copy + Default + 'static>(T);

// A struct with a definition like this:
// struct Foo<T> where T: Copy + 'static {
//     ...
// }
// impl<T> Foo where T: Copy + 'static {
//     fn foo(&self, t: T) -> G<T> where T: Copy + Default + 'static {
//         unimplemented!()
//     }
// }
// Could be mocked like this:
mock!{
    Foo<T> where T: Copy + 'static {
        fn foo<T2>(&self, t: T2) -> G<T2> where T2: Copy + Default + 'static;
    }
}

#[test]
fn returning() {
    let mut mock = MockFoo::<u32>::default();
    mock.expect_foo::<u32>()
        .returning(|t| G(t));
    assert_eq!(42u32, mock.foo(42u32).0);
}
