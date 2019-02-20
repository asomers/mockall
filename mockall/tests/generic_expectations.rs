// vim: tw=80

use mockall::*;

mod generic_expectation {
    use super::*;

    #[test]
    fn checkpoint_ok() {
        expectation!{foo<T>, u32, [T], [&t], [t], [p], [T]}
        let mut e = foo::GenericExpectations::default();
        e.expect::<i32>()
            .returning(|_| 42)
            .times_range(1..3);
        e.call::<i32>(0);
        e.checkpoint();
    }

    #[test]
    fn checkpoint_and_expect_again() {
        expectation!{foo<T>, u32, [T], [&t], [t], [p], [T]}
        let mut e = foo::GenericExpectations::default();
        e.expect::<i32>()
            .returning(|_| 42)
            .times_range(1..3);
        e.call::<i32>(0);
        e.checkpoint();

        e.expect::<i32>()
            .returning(|_| 25);
        assert_eq!(25, e.call::<i32>(0));
    }

    #[test]
    #[should_panic(expected = "Expectation called fewer than 1 times")]
    fn checkpoint_not_yet_satisfied() {
        expectation!{foo<T>, u32, [T], [&t], [t], [p], [T]}
        let mut e = foo::GenericExpectations::default();
        e.expect::<i32>()
            .returning(|_| 42)
            .times(1);
        e.checkpoint();
        panic!("Shouldn't get here!");
    }

    #[test]
    #[should_panic(expected = "No matching expectation found")]
    fn checkpoint_removes_old_expectations() {
        expectation!{foo<T>, u32, [T], [&t], [t], [p], [T]}
        let mut e = foo::GenericExpectations::default();
        e.expect::<i32>()
            .returning(|_| 42)
            .times_range(1..3);
        e.call::<i32>(0);
        e.checkpoint();
        e.call::<i32>(0);
        panic!("Shouldn't get here!");
    }

    #[test]
    fn generic_argument() {
        expectation!{foo<T>, u32, [T], [&t], [t], [p], [T]}
        let mut e = foo::GenericExpectations::default();

        e.expect::<i32>()
            .with(predicate::eq(4))
            .returning(|_| 42u32);

        assert_eq!(42, e.call::<i32>(4));
    }

    #[test]
    fn generic_return() {
        expectation!{foo<T>, T, [i32], [&x], [x], [p], [i32]}
        let mut e = foo::GenericExpectations::default();

        e.expect::<u32>()
            .with(predicate::eq(4i32))
            .returning(|_| 42);

        assert_eq!(42, e.call::<u32>(4));
    }

    /// A generic method can have reference arguments, as long as they aren't
    /// generic
    #[test]
    fn reference_arguments() {
        expectation!{foo<T>, u32, [T, &i16], [&t, x], [t, x], [p, q], [T, i16]}
        let mut e = foo::GenericExpectations::default();

        e.expect::<i32>()
            .with(predicate::eq(4), predicate::eq(-4))
            .returning(|_, _| 42u32);

        let x = -4i16;
        assert_eq!(42, e.call::<i32>(4, &x));
    }

    #[test]
    #[should_panic(expected = "No matching expectation found")]
    fn missing_expectation() {
        expectation!{foo<T>, u32, [T], [&t], [t], [p], [T]}
        let e = foo::GenericExpectations::default();
        e.call::<i32>(5);
    }

    /// An expectation is set, but the mock is called with the wrong generic
    /// parameters
    #[test]
    #[should_panic(expected = "No matching expectation found")]
    fn missing_expectation_for_these_parameters() {
        expectation!{foo<T>, u32, [T], [&t], [t], [p], [T]}
        let mut e = foo::GenericExpectations::default();
        e.expect::<u32>()
            .returning(|_| 42);
        e.call::<i32>(5);
    }

    /// Multiple generic methods with the same set of generic parameters are in
    /// scope at the same time.  Their expectations should not get scrambled.
    #[test]
    fn multiple_methods() {
        expectation!{foo<T>, u32, [T], [&t], [t], [p], [T]}
        expectation!{bar<T>, u32, [T], [&t], [t], [p], [T]}
        let mut e0 = foo::GenericExpectations::default();
        let mut e1 = bar::GenericExpectations::default();

        e0.expect::<i32>()
            .with(predicate::eq(4))
            .returning(|_| 42u32);
        e1.expect::<i32>()
            .with(predicate::eq(4))
            .returning(|_| 99u32);

        assert_eq!(42, e0.call(4i32));
        assert_eq!(99, e1.call(4i32));
    }

    /// Unlike Mockers, calls should use the oldest matching expectation, if
    /// multiple expectations match
    #[test]
    fn fifo_order() {
        expectation!{foo<T>, u32, [T], [&t], [t], [p], [T]}
        let mut e = foo::GenericExpectations::default();
        e.expect::<i32>()
            .with(predicate::eq(5))
            .returning(|_| 99);
        e.expect::<i32>()
            .with(predicate::always())
            .returning(|_| 42);

        assert_eq!(99, e.call::<i32>(5));
    }

    /// Two expectations are set.  Mockall should choose the right one
    #[test]
    fn one_match() {
        expectation!{foo<T>, T, [T], [&t], [t], [p], [T]}

        let mut e0 = foo::GenericExpectations::default();
        e0.expect::<i32>()
            .with(predicate::eq(4))
            .returning(|_| 42);
        e0.expect::<i32>()
            .with(predicate::eq(5))
            .returning(|_| 99);
        assert_eq!(42, e0.call::<i32>(4));

        // And in reverse order:
        let mut e1 = foo::GenericExpectations::default();
        e1.expect::<i32>()
            .with(predicate::eq(5))
            .returning(|_| 99);
        e1.expect::<i32>()
            .with(predicate::eq(4))
            .returning(|_| 42);
        assert_eq!(42, e1.call::<i32>(4));
    }
}

mod generic_ref_expectation {
    use super::*;

    #[test]
    fn no_args() {
        let mut e = GenericRefExpectations::default();
        e.expect::<(), u32>()
            .return_const(5u32);
        e.expect::<(), u64>()
            .return_const(6u64);

        assert_eq!(5u32, *e.call::<(), u32>(()));
        assert_eq!(6u64, *e.call::<(), u64>(()));
    }
}

mod generic_ref_mut_expectation {
    use super::*;

    #[test]
    fn no_args() {
        let mut e = GenericRefMutExpectations::default();
        e.expect::<(), u32>()
            .returning(|_| 5u32);
        e.expect::<(), u64>()
            .returning(|_| 6u64);

        assert_eq!(5u32, *e.call_mut::<(), u32>(()));
        assert_eq!(6u64, *e.call_mut::<(), u64>(()));
    }
}

/// Mocking static methods requires using globals
mod static_method {
    use std::sync::Mutex;
    use super::*;

    mockall::lazy_static! {
        static ref GLOBAL_EXPECTATIONS: Mutex<GenericExpectations> = 
            Mutex::new(GenericExpectations::new());
    }

    /// The contorted syntax mimics what mockall_derive does.
    #[test]
    fn t() {
        {
            let mut eguard = {
                let guard = GLOBAL_EXPECTATIONS.lock().unwrap();
                GenericExpectationGuard::new(guard)
            };
            eguard.returning(|_: i16| 42u32);
        }

        assert_eq!(42, GLOBAL_EXPECTATIONS.lock().unwrap().call::<i16, u32>(5));
    }
}
