// vim: tw=80
//! An example of unit testing involving mocked FFI functions

use std::ptr;
#[cfg(test)]
use mockall::automock;
use mockall_double::double;

#[cfg_attr(test, automock)]
pub mod mockable_ffi {
    extern "C" {
        pub fn time(timer: *mut u64) -> u64;
    }
}

#[double]
use mockable_ffi as ffi;

fn time() -> u64 {
    unsafe { ffi::time(ptr::null_mut()) }
}

fn main() {
    println!("Seconds since epoch: {}", time() );
}


#[test]
fn time_test() {
    let ctx = ffi::time_context();
    ctx.expect()
        .return_const(42u64);
    time();
}
