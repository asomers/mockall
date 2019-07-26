// vim: tw=80
//! mock a generic struct and instantiate it with a parameter type that does not
//! implement Default

use mockall::*;

struct NonDefault();

trait Foo<T: 'static> {
    fn foo(&self) -> T;
}
mock! {
    ExternalStruct<T: 'static> {}
    trait Foo<T: 'static> {
        fn foo(&self) -> T;
    }
}

#[test]
fn returning() {
    let mut mock = MockExternalStruct::<NonDefault>::new();
    mock.expect_foo().returning(|| NonDefault());
    mock.foo();
}
