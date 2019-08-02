// vim: tw=80
use mockall::*;

struct G<T> where T: Copy {t: T}

mock! {
    Foo {
        fn foo<T>(&self, t: T) -> G<T> where T: Copy + 'static;
    }
}

#[test]
fn t() {
    let mut mock = MockFoo::new();
    mock.expect_foo::<u32>()
        .returning(|t| G{t});
    assert_eq!(42, mock.foo(42u32).t);
}
