// vim: tw=80
use mockall::*;

struct G<T> where T: Copy {t: T}

mock! {
    Foo {
        fn foo<T>(&self, t: T) -> G<T> where T: Copy + 'static;
        fn bar<T>(&self, g: G<T>) -> T where T: Copy + 'static;
    }
}

#[test]
fn returning() {
    let mut mock = MockFoo::new();
    mock.expect_foo::<u32>()
        .returning(|t| G{t});
    assert_eq!(42, mock.foo(42u32).t);

    mock.expect_bar::<u32>()
        .returning(|g| g.t);
    assert_eq!(42u32, mock.bar(G{t: 42}));
}
