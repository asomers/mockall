// vim: tw=80
#![deny(warnings)]

use mockall::*;

trait MyTrait<T> {}

struct MyStruct<T>(T);
impl<T> MyTrait<T> for MyStruct<T> {}

mock!{
    Foo {
        fn foo<R: 'static>(&self) -> impl MyTrait<R>;
    }
}

#[test]
fn returning() {
    let mut mock = MockFoo::new();
    mock.expect_foo::<u32>().returning(|| Box::new(MyStruct(42u32)));
    let _mto: Box<dyn MyTrait<u32>> = mock.foo();
}
