// vim: tw=80
//! A generic struct with a where clause on its generic parameters can have a
//! constructor method

use mockall::*;

mock! {
    pub Foo<T> where T: Default + 'static {
        fn build() -> MockFoo<T>;
    }
}

#[test]
fn returning_once() {
    MockFoo::<i16>::expect_build()
        .return_once(MockFoo::<i16>::default);

    let _mock: MockFoo<i16> = MockFoo::<i16>::build();
}
