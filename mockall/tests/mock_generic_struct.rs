// vim: tw=80
#![deny(warnings)]

use mockall::*;

#[derive(Clone)]
struct NonCopy(u32);

// A struct with a definition like this:
// pub struct ExtGenericStruct<T: Clone> {
//     _x: i16
// }
// impl<T: Clone> ExtGenericStruct<T> {
//     fn foo(&self, _x: T) -> T {
//         42
//     }
// }
// Could be mocked like this:
mock!{
    pub ExtGenericStruct<T: Clone + 'static> {
        fn foo(&self, x: T) -> T;
    }
}

#[test]
#[allow(clippy::redundant_clone)]
fn returning() {
    let mut mock = MockExtGenericStruct::<NonCopy>::new();
    // An explicit .clone() is required so as not to return by move
    mock.expect_foo()
        .returning(|x| x.clone());
    assert_eq!(5, mock.foo(NonCopy(5)).0);
}
