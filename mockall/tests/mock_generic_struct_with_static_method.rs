// vim: ts=80

use mockall::*;

// Static methods parameterized on the struct's generic parameter need to be
// turned into generic methods for mocking.  A struct like this:
//
// struct Foo<T> {}
// impl<T> Foo<T> {
//     fn foo(t: T) {...}
// }
//
// Can be mocked like this:
mock! {
    Foo<T: 'static> {
        fn foo<T2: 'static>(t: T2);
    }
}

#[test]
fn returning() {
    MockFoo::<u32>::expect_foo::<u32>()
        .returning(|_| ());
    MockFoo::<u32>::foo(42u32);
}
