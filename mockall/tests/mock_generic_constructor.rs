// vim: tw=80
//! A generic struct with bounds on its generic parameters can have a
//! constructor method

use mockall::*;

mock! {
    pub Foo<T: Default +'static> {
        fn build<T2: Default + 'static>() -> MockFoo<T2>;
    }
}

#[test]
fn returning_once() {
    MockFoo::<i16>::expect_build::<i16>()
        .return_once(MockFoo::<i16>::default);

    let _mock: MockFoo<i16> = MockFoo::<i16>::build();
}
