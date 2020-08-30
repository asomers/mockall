// vim: tw=80
//! An example of unit testing involving mocked FFI functions

#![cfg(unix)]

#[cfg(test)]
use mockall::automock;
use mockall_double::double;

#[cfg_attr(test, automock)]
pub mod mockable_ffi {
    extern "C" {
        pub fn getuid() -> u32;
    }
}

#[double]
use mockable_ffi as ffi;

fn getuid() -> u32 {
    unsafe { ffi::getuid() }
}

fn main() {
    println!("My uid is {}", getuid() );
}


#[test]
fn getuid_test() {
    let ctx = ffi::getuid_context();
    ctx.expect()
        .return_const(42u32);
    getuid();
}
