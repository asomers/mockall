// vim: tw=80

use mockall::*;

struct G<T> where T: Copy {t: T}

trait Foo {
    fn make_g<T>(x: T) -> G<T> where T: Copy;
}

mock! {
    Foo {
        fn make_g<T: 'static>(x: T) -> G<T> where T: Copy;
    }
}

#[test]
fn returning() {
    MockFoo::expect_make_g::<i16>()
        .returning(|t| G{t});
    let g = MockFoo::make_g(42i16);
    assert_eq!(g.t, 42i16);
}
