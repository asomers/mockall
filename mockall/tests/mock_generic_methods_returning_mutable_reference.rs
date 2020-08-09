// vim: tw=80
#![deny(warnings)]

use mockall::*;

mock!{
    MyStruct {
        fn foo<T: 'static>(&mut self, t: T) -> &mut u32;
    }
}

#[test]
fn return_var() {
    let mut mock = MockMyStruct::new();
    mock.expect_foo::<i16>().return_var(5u32);
    *mock.foo(1i16) += 1;
    assert_eq!(6u32, *mock.foo(2i16));
}
