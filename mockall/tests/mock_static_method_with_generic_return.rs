// vim: tw=80

use mockall::*;

trait Foo {
    fn bar<T>(x: T) -> Vec<T>;
}

mock! {
    Foo {
        fn bar<T: 'static>(x: T) -> Vec<T>;
    }
}

#[test]
fn returning() {
    MockFoo::expect_bar::<i16>()
        .returning(|x| vec![x]);
    let v = MockFoo::bar(42i16);
    assert_eq!(v[0], 42i16);
}
