// vim: tw=80
//! An example of unit testing involving mocked FFI functions

#[cfg(test)]
use mockall::automock;
use mockall_double::double;

#[cfg_attr(test, automock)]
pub mod mockable_ffi {
    extern "C" {
        pub fn abs(i: i32) -> i32;
    }
}

#[double]
use mockable_ffi as ffi;

fn abs(i: i32) -> i32 {
    unsafe { ffi::abs(i) }
}

fn main() {
    let i: i32 = -42;
    println!("abs({}) = {}", i, abs(i) );
}


#[test]
fn time_test() {
    let ctx = ffi::abs_context();
    ctx.expect()
        .return_const(42);
    assert_eq!(42, abs(-42))
}
