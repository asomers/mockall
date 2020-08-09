// vim: tw=80
//! A generic struct with bounds on its generic parameters can have a
//! constructor method
#![deny(warnings)]

use mockall::*;

mock! {
    pub Foo<T: Default +'static> {
        fn build() -> MockFoo<T>;
    }
}

#[test]
fn returning_once() {
    let ctx = MockFoo::<i16>::build_context();
    ctx.expect()
        .return_once(MockFoo::<i16>::default);

    let _mock: MockFoo<i16> = MockFoo::<i16>::build();
}
