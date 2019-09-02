// vim: tw=80

use mockall::*;

#[automock(mod mock_ffi;)]
extern "C" {
    #[allow(unused)]
    fn foo(x: u32) -> i64;
}

#[test]
#[should_panic(expected = "mock_ffi::foo: No matching expectation found")]
fn with_no_matches() {
    let ctx = mock_ffi::foo_context();
    ctx.expect()
        .with(predicate::eq(4))
        .returning(i64::from);
    unsafe{ mock_ffi::foo(5) };
}

#[test]
fn returning() {
    let ctx = mock_ffi::foo_context();
    ctx.expect().returning(i64::from);
    assert_eq!(42, unsafe{mock_ffi::foo(42)});
}
