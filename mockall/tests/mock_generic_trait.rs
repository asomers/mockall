// vim: tw=80
#![deny(warnings)]

use mockall::*;

trait Foo {
    fn foo(&self);
}

mock! {
    Bar<T: 'static> {}
    impl<T: 'static> Foo for Bar<T> {
        fn foo(&self);
    }
}

#[test]
fn return_const() {
    let mut mock = MockBar::<u32>::new();
    mock.expect_foo().return_const(());
    mock.foo();
}
