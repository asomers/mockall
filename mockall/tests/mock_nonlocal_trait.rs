// vim: tw=80
//! A trait that isn't imported directly into the local namespace
#![deny(warnings)]

use mockall::*;

mod my_module {
    pub trait Foo {
        fn foo(&self) -> i32;
    }
}

mock! {
    Bar {}
    impl my_module::Foo for Bar {
        fn foo(&self) -> i32;
    }
}

#[test]
fn returning() {
    use my_module::Foo;

    let mut mock = MockBar::new();
    mock.expect_foo()
        .returning(|| 42);
    assert_eq!(42, mock.foo());
}
