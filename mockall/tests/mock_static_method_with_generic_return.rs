// vim: tw=80
#![deny(warnings)]

use mockall::*;

mock! {
    Foo {
        fn bar<T: 'static>(x: T) -> Vec<T>;
    }
}

#[test]
fn returning() {
    let ctx = MockFoo::bar_context();
    ctx.expect::<i16>().returning(|x| vec![x]);
    let v = MockFoo::bar(42i16);
    assert_eq!(v[0], 42i16);
}
