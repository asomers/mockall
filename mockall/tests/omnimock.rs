// vim: tw=80

use mockall::*;
use mockall::predicate::*;
use std::ops::DerefMut;


mod reference_argument {
    use super::*;

    mockall::omnimock!{&u32, i, u32, i, u32}

    #[test]
    fn t() {
        let mut e = Expectation::default();
        e.with(eq(42u32))
            .returning(|x| *x);
        let x = 42u32;
        assert_eq!(42u32, e.call(&x));
    }
}

//mod ref_and_nonref_arguments {
    //use super::*;

    //mockall::omnimock!{(&u32, u16), i, (u32, u16), (i.0, &i.1), u32}

    //#[test]
    //fn t() {
        //let mut e = Expectation::default();
        //e.returning(|(x, y)| *x + u32::from(y));
        //let x = 42u32;
        //let y = 1u16;
        //assert_eq!(43u32, e.call((&x, y)));
    //}
//}

//mod two_reference_arguments {
    //use super::*;

    //mockall::omnimock!{(&u32, &u16), i, (u32, u16), i, u32}

    //#[test]
    //fn t() {
        //let mut e = Expectation::default();
        //e.returning(|(x, y)| *x + u32::from(*y));
        //let x = 42u32;
        //let y = 1u16;
        //assert_eq!(43u32, e.call((&x, &y)));
    //}
//}
