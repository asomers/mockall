// vim: tw=80
//! Add synchronization to multiple tests that are accessing the same mock
//!
//! When mockall mocks a function or static method, it does so globally. This
//! can cause hard to debug and non-deterministic failures when one test
//! overwrites the mock that another test is depending on. The solution to this
//! is to add some form of synchronization so that tests that depend on a
//! specific mock will not run in parallel. This is easily achieved using a
//! Mutex.
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
    use std::sync::Mutex;

    static MTX: Mutex<()> = Mutex::new(());

    #[test]
    fn test_1() {
        // The mutex might be poisoned if another test fails.  But we don't
        // care, because it doesn't hold any data.  So don't unwrap the Result
        // object; whether it's poisoned or not, we'll still hold the
        // MutexGuard.
        let _m = MTX.lock();

        let ctx = MockThing::one_context();
        ctx.expect().returning(|| 1);
        let expected = 1;
        assert_eq!(expected, MockThing::one())
    }

    #[test]
    fn test_2() {
        let _m = MTX.lock();

        let ctx = MockThing::one_context();
        ctx.expect().returning(|| 2);
        let expected = 2;
        assert_eq!(expected, MockThing::one())
    }
}
