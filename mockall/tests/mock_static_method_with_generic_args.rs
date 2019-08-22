// vim: tw=80

use mockall::*;

trait Foo {
    fn bar<T>(x: T);
}

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
