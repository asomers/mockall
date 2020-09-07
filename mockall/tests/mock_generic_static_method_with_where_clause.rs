// vim: tw=80
#![deny(warnings)]

use mockall::*;

struct G<T> where T: Copy {t: T}

mock! {
    Foo {
        fn make_g<T: 'static>(x: T) -> G<T> where T: Copy;
    }
}

#[test]
fn returning() {
    let ctx = MockFoo::make_g_context();
    ctx.expect::<i16>()
        .returning(|t| G{t});
    let g = MockFoo::make_g(42i16);
    assert_eq!(g.t, 42i16);
}
