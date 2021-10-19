// vim: tw=80
//! Mocking modules should not generated "unused_imports" warnings for imports
//! used by the bodies of the mocked functions.
//! https://github.com/asomers/mockall/issues/343
#![deny(warnings)]

use mockall::automock;

#[automock]
pub mod foo {
    use std::convert::TryInto;

    pub fn bar() {
        let x = 42i32;
        let _y: u32 = x.try_into().unwrap_or(0);
    }
}

#[test]
fn return_const() {
    let ctx = mock_foo::bar_context();
    ctx.expect()
        .once()
        .return_const(());
    mock_foo::bar();
}
