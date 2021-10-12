// vim: tw=80
//! A struct with generic parameters bounded by a where clause
#![deny(warnings)]

use mockall::*;

pub struct GenericStruct<T: 'static> {
    _t: T,
}
#[automock]
impl<T> GenericStruct<T>
    where T: Clone + Default + 'static
{
    #[allow(clippy::redundant_clone)]
    pub fn foo(&self, x: T) -> T {
        x.clone()
    }
}

#[test]
fn returning() {
    let mut mock = MockGenericStruct::<u8>::default();
    mock.expect_foo()
        .returning(|x| x);
    assert_eq!(4, mock.foo(4u8));
}
