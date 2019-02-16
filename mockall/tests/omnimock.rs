// vim: tw=80

use mockall::predicate::*;


mod reference_argument {
    use super::*;

    mockall::omnimock!{&u32, u32, i, [&i], [i0], [p0], [u32]}

    #[test]
    fn t() {
        let mut e = Expectation::default();
        e.with(eq(42u32))
            .returning(|x| *x);
        let x = 42u32;
        assert_eq!(42u32, e.call(&x));
    }

    #[test]
    fn match_fn() {
        let mut e = Expectation::default();
        e.withf(|x| *x == 42)
            .returning(|x| *x);
        let x = 42u32;
        assert_eq!(42u32, e.call(&x));
    }
}

mod ref_and_nonref_arguments {
    use super::*;

    mockall::omnimock!{(i32, &u16), i32, i,
        [&i.0, i.1],
        [i0, i1],
        [p0, p1],
        [i32, u16]
    }

    #[test]
    fn t() {
        let mut e = Expectation::default();
        e.with(eq(42), eq(1))
            .returning(|(x, y)| x + i32::from(*y));
        let x = 42i32;
        let y = 1u16;
        assert_eq!(43i32, e.call((x, &y)));
    }

    #[test]
    fn match_fn() {
        let mut e = Expectation::default();
        e.withf(|x, y| *x == i32::from(*y))
            .returning(|(x, y)| x + i32::from(*y));
        let x = 42i32;
        let y = 42u16;
        assert_eq!(84i32, e.call((x, &y)));
    }
}

mod two_reference_arguments {
    use super::*;

    mockall::omnimock!{(&u32, &u16), u32, i,
        [i.0, i.1],
        [i0, i1],
        [p0, p1],
        [u32, u16]
    }

    #[test]
    fn t() {
        let mut e = Expectation::default();
        e.with(eq(42), eq(1))
            .returning(|(x, y)| *x + u32::from(*y));
        let x = 42u32;
        let y = 1u16;
        assert_eq!(43u32, e.call((&x, &y)));
    }
}

mod xxx {
}
