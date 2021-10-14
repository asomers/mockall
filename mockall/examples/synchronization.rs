// vim: tw=80
//! Add synchronization to multiple tests that are accessing the same mock
//!
//! When mockall mocks a function or static method, it does so globally. This
//! can cause hard to debug and non-deterministic failures when one test
//! overwrites the mock that another test is depending on. The solution to this
//! is to add some form of synchronization so that tests that depend on a
//! specific mock will not run in parallel. This is easily achieved using a
//! Mutex and the lazy_static crate.
#![deny(warnings)]

use mockall_double::double;

pub mod my_mock {
    #[cfg(test)]
    use mockall::automock;

    pub struct Thing;
    #[cfg_attr(test, automock)]
    impl Thing {
        pub fn one() -> u32 {
            1
        }
    }
}

#[double]
use my_mock::Thing;

fn main() {
    println!("1 == {}", Thing::one());
}

#[cfg(test)]
mod test {
    use crate::my_mock::MockThing;
    use lazy_static::lazy_static;
    use std::sync::{Mutex, MutexGuard};

    lazy_static! {
        static ref MTX: Mutex<()> = Mutex::new(());
    }

    // When a test panics, it will poison the Mutex. Since we don't actually
    // care about the state of the data we ignore that it is poisoned and grab
    // the lock regardless.  If you just do `let _m = &MTX.lock().unwrap()`, one
    // test panicking will cause all other tests that try and acquire a lock on
    // that Mutex to also panic.
    fn get_lock(m: &'static Mutex<()>) -> MutexGuard<'static, ()> {
        match m.lock() {
            Ok(guard) => guard,
            Err(poisoned) => poisoned.into_inner(),
        }
    }

    #[test]
    fn test_1() {
        let _m = get_lock(&MTX);

        let ctx = MockThing::one_context();
        ctx.expect().returning(|| 1);
        let expected = 1;
        assert_eq!(expected, MockThing::one())
    }

    #[test]
    fn test_2() {
        let _m = get_lock(&MTX);

        let ctx = MockThing::one_context();
        ctx.expect().returning(|| 2);
        let expected = 2;
        assert_eq!(expected, MockThing::one())
    }
}
