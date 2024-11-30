// vim: tw=80
//! Mock a function with a lifetime parameter that isn't single-use, but becomes
//! single-use in the generated code.
#![deny(warnings)]
#![deny(single_use_lifetimes)]

use mockall::*;

#[automock]
pub trait MyTrait {
    #[allow(clippy::needless_lifetimes)]
    fn get<'a>(&'a mut self, data: i32) -> std::io::Result<&'a str>;
}

#[test]
fn returning() {
    let mut mock = MockMyTrait::new();
    mock.expect_get()
        .returning(|_data| Err(std::io::Error::from_raw_os_error(42)));

    mock.get(0x1a7ebabe).unwrap_err();
}
