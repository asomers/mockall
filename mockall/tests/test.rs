// vim: tw=80

use mockall::*;

/// A MockObject with a simple method like:
/// fn foo(&self, x: i32) -> u32
#[test]
fn simple_method() {
    let mut e = Expectations::default();
    e.expect::<i32, u32>(&"foo")
        .returning(|_| 42);
    let r = e.called::<i32, u32>(&"foo", 5);
    assert_eq!(42, r);
}
