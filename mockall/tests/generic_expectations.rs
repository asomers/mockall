// vim: tw=80

use mockall::*;

mod generic_expectation {
    use super::*;

    #[test]
    fn checkpoint_ok() {
        let mut e = GenericExpectations::default();
        e.expect::<i32, u32>()
            .returning(|_| 42)
            .times_range(1..3);
        e.call::<i32, u32>(0);
        e.checkpoint();
    }

    #[test]
    fn checkpoint_and_expect_again() {
        let mut e = GenericExpectations::default();
        e.expect::<i32, u32>()
            .returning(|_| 42)
            .times_range(1..3);
        e.call::<i32, u32>(0);
        e.checkpoint();

        e.expect::<i32, u32>()
            .returning(|_| 25);
        assert_eq!(25, e.call::<i32, u32>(0));
    }

    #[test]
    #[should_panic(expected = "Expectation called fewer than 1 times")]
    fn checkpoint_not_yet_satisfied() {
        let mut e = GenericExpectations::default();
        e.expect::<i32, u32>()
            .returning(|_| 42)
            .times(1);
        e.checkpoint();
        panic!("Shouldn't get here!");
    }

    #[test]
    #[should_panic(expected = "No matching expectation found")]
    fn checkpoint_removes_old_expectations() {
        let mut e = GenericExpectations::default();
        e.expect::<i32, u32>()
            .returning(|_| 42)
            .times_range(1..3);
        e.call::<i32, u32>(0);
        e.checkpoint();
        e.call::<i32, u32>(0);
        panic!("Shouldn't get here!");
    }

    #[test]
    #[should_panic(expected = "No matching expectation found")]
    fn missing_expectation() {
        let e = GenericExpectations::default();
        e.call::<i32, u32>(5);
    }

    /// Unlike Mockers, calls should use the oldest matching expectation, if
    /// multiple expectations match
    #[test]
    fn fifo_order() {
        let mut e = GenericExpectations::new();
        e.expect::<i32, i32>()
            .with(predicate::eq(5))
            .returning(|_| 99);
        e.expect::<i32, i32>()
            .with(predicate::always())
            .returning(|_| 42);

        assert_eq!(99, e.call::<i32, i32>(5));
    }

    #[test]
    fn one_match() {
        let mut e = GenericExpectations::new();
        e.expect::<i32, i32>()
            .with(predicate::eq(4))
            .returning(|_| 42);
        e.expect::<i32, i32>()
            .with(predicate::eq(5))
            .returning(|_| 99);

        assert_eq!(42, e.call::<i32, i32>(4));
    }

    #[test]
    fn no_args_or_returns() {
        let mut e = GenericExpectations::default();
        e.expect::<(), ()>()
            .returning(|_| ());
        e.call::<(), ()>(());
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
