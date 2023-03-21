// vim: tw=80
//! An example of unit testing involving mocked FFI functions

use std::ptr;
#[cfg(test)]
use mockall::automock;
use mockall_double::double;
use std::time::{Duration, SystemTime};

#[cfg_attr(test, automock)]
pub mod mockable_ffi {
    extern "C" {
        pub fn time(timer: *const u64) -> u64;
    }
}

#[double]
use mockable_ffi as ffi;

fn time() -> u64 {
    unsafe { ffi::time(ptr::null()) }
}

fn main() {
    println!("Seconds since epoch: {}", time() );
}


#[test]
fn time_test() {
    let seconds_from_epoch = SystemTime::now()
        .duration_since(SystemTime::UNIX_EPOCH)
        .unwrap_or(Duration::from_secs(0))
        .as_secs();

    let ctx = ffi::time_context();
    ctx.expect()
        .return_const(seconds_from_epoch);
    time();
}
