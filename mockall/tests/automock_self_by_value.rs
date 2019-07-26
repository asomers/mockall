// vim: tw=80
//! A method that consumes self

use mockall::*;

#[automock]
trait MethodByValue {
    fn foo(self, _x: u32) -> i64;
}

#[test]
fn returning() {
    let mut mock = MockMethodByValue::new();
    mock.expect_foo()
        .returning(|x| i64::from(x) + 1);
    assert_eq!(5, mock.foo(4));
}
