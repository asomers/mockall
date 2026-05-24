// vim: tw=80
//! An example of how to use Mockall to create mock functions that can be called
//! from C
use mockall::automock;
#[cfg(test)]
use mockall::predicate::eq;

#[automock]
pub mod ffi {
    extern "C" {
        // This is provided by the OS's C library
        pub fn isupper(c: i32) -> i32;
    }
}

/// But in the test configuration, we override the OS's isupper with a
/// C-compatible wrapper that calls a mock function.
#[cfg(test)]
#[no_mangle]
pub unsafe extern "C-unwind" fn isupper(c: i32) -> i32 {
    mock_ffi::isupper(c)
}

/// This function is written in Rust for our convenience, but it has C linkage
/// and no Rust name mangling.  In this test, it serves as a proxy for a C
/// function.
#[no_mangle]
pub extern "C" fn some_c_func(c: i32) -> i32 {
    unsafe{ffi::isupper(c)}
}

fn main() {
    // In non-test code, we can call the C function, and it will call the OS's
    // isupper function like normal.
    println!("isupper(A) = {}", some_c_func(65));
}

// But in test mode, the OS's isupper symbol is overridden by our own.
#[test]
fn t() {
    let ctx = mock_ffi::isupper_context();
    ctx.expect()
        .with(eq(42))
        .return_const(1);
    assert_eq!(1, some_c_func(42));
}
