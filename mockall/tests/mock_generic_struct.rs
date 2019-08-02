// vim: tw=80

use mockall::*;

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
fn returning() {
    let mut mock = MockExtGenericStruct::<u32>::new();
    // An explicit .clone() is required so as not to return by move
    mock.expect_foo()
        .returning(|x| x.clone());
    assert_eq!(5, mock.foo(5));
}
