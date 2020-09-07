// vim: tw=80
#![deny(warnings)]

use mockall::*;

mock! {
    Foo {
        fn bar<T: 'static>(x: T);
    }
}

#[test]
fn returning() {
    let ctx = MockFoo::bar_context();
    ctx.expect::<i16>().returning(|_| ());
    MockFoo::bar(0i16)
}
