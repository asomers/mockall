// vim: tw=80
#![deny(warnings)]

use mockall::*;

trait Foo {
    fn foo<T: 'static>(&self, t: T) -> &u32;
}

mock!{
    MyStruct {}
    impl Foo for MyStruct {
        fn foo<T: 'static>(&self, t: T) -> &u32;
    }
}

#[test]
fn returning() {
    let mut mock = MockMyStruct::new();
    mock.expect_foo::<i16>().return_const(5u32);
    assert_eq!(5u32, *mock.foo(99i16));
}
