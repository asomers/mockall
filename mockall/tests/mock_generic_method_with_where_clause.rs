// vim: tw=80
#![deny(warnings)]
use mockall::*;

struct G<T> where T: Copy {t: T}

mock! {
    Foo {
        fn foo<T>(&self, t: T) -> G<T> where T: Copy + 'static;
        fn bar<T>(&self, g: G<T>) -> T where T: Copy + 'static;
        fn baz<T>(&self) -> &G<T> where T: Copy + 'static;
        fn bean<T>(&mut self) -> &mut G<T> where T: Copy + 'static;
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

#[test]
fn return_const() {
    let mut mock = MockFoo::new();
    mock.expect_baz::<u32>()
        .return_const(G{t: 42});
    assert_eq!(42u32, mock.baz().t);
}

#[test]
fn return_var() {
    let mut mock = MockFoo::new();
    mock.expect_bean::<u32>()
        .return_var(G{t: 42});
    mock.bean::<u32>().t += 1;
    assert_eq!(43u32, mock.bean().t);
}
