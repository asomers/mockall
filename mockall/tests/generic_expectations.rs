// vim: tw=80

use mockall::*;

mod generic_expectation {
    use super::*;

    #[test]
    #[should_panic(expected = "No matching expectation found")]
    fn missing_expectation() {
        let e = GenericExpectation::default();
        e.call::<i32, u32>(5);
    }

    #[test]
    fn no_args_or_returns() {
        let mut e = GenericExpectation::default();
        e.expect::<(), ()>()
            .returning(|_| ());
        e.call::<(), ()>(());
    }
}

mod generic_ref_expectation {
    use super::*;

    #[test]
    fn no_args() {
        let mut e = GenericRefExpectation::default();
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
        let mut e = GenericRefMutExpectation::default();
        e.expect::<(), u32>()
            .returning(|_| 5u32);
        e.expect::<(), u64>()
            .returning(|_| 6u64);

        assert_eq!(5u32, *e.call_mut::<(), u32>(()));
        assert_eq!(6u64, *e.call_mut::<(), u64>(()));
    }
}
