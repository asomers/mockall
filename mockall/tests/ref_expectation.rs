// vim: tw=80

use mockall::*;

mod generic {
    use super::*;

    #[test]
    fn generic_argument_and_return() {
        ref_expectation!{foo<I, O>, O, [I], [&x], [x], [p], [I]}
        let mut e = foo::Expectation::<i16, i32>::default();
        e.with(predicate::eq(4))
            .return_const(4);

        assert_eq!(4i32, *e.call(4i16));
    }

    #[test]
    fn generic_return() {
        ref_expectation!{foo<O>, O, [u32], [&x], [x], [p], [u32]}
        let mut e = foo::Expectation::<i32>::default();
        e.with(predicate::eq(4))
            .return_const(-42);

        assert_eq!(-42, *e.call(4));
    }
}

#[test]
fn match_eq_ok() {
    ref_expectation!{
        foo<>, i32,
        [i32], [&i], [i], [p], [i32]
    }
    let mut e = foo::Expectation::default();
    e.return_const(99i32);
    e.with(predicate::eq(5));
    e.call(5);
}

#[test]
#[should_panic]
fn match_eq_fail() {
    ref_expectation!{
        foo<>, i32,
        [i32], [&i], [i], [p], [i32]
    }
    let mut e = foo::Expectation::default();
    e.return_const(99i32);
    e.with(predicate::eq(4));
    e.call(5);
}

#[test]
fn never_ok() {
    ref_expectation!{
        foo<>, (),
        [], [], [], [], []
    }
    let mut e = foo::Expectation::default();
    e.return_const(());
    e.never();
}

#[test]
#[should_panic(expected = "Expectation should not have been called")]
fn never_fail() {
    ref_expectation!{
        foo<>, (),
        [], [], [], [], []
    }
    let mut e = foo::Expectation::default();
    e.return_const(());
    e.never();
    e.call();
}

mod reference_argument {
    use super::*;

    ref_expectation!{foo<>, (),
        [&u32], [&i0], [i0], [p0], [u32]}

    #[test]
    fn t() {
        let mut e = foo::Expectation::default();
        e.with(predicate::eq(42u32))
            .return_const(());
        let x = 42u32;
        e.call(&x);
    }

    #[test]
    fn match_fn() {
        let mut e = foo::Expectation::default();
        e.withf(|x| *x == 42)
            .return_const(());
        let x = 42u32;
        e.call(&x);
    }
}

mod ref_and_nonref_arguments {
    use super::*;

    ref_expectation!{foo<>, (),
        [i32, &u16],
        [&i0, i1],
        [i0, i1],
        [p0, p1],
        [i32, u16]
    }

    #[test]
    fn t() {
        let mut e = foo::Expectation::default();
        e.with(predicate::eq(42), predicate::eq(1))
            .return_const(());
        let x = 42i32;
        let y = 1u16;
        e.call(x, &y);
    }

    #[test]
    fn match_fn() {
        let mut e = foo::Expectation::default();
        e.withf(|x, y| *x == i32::from(*y))
            .return_const(());
        let x = 42i32;
        let y = 42u16;
        e.call(x, &y);
    }
}

mod reference_arguments {
    use super::*;

    ref_expectation!{foo<>, (),
        [&u32, &u16],
        [i0, i1],
        [i0, i1],
        [p0, p1],
        [u32, u16]
    }

    #[test]
    fn t() {
        let mut e = foo::Expectation::default();
        e.with(predicate::eq(42), predicate::eq(1))
            .return_const(());
        let x = 42u32;
        let y = 1u16;
        e.call(&x, &y);
    }
}

#[test]
#[should_panic(expected = "Method sequence violation")]
fn sequence_fail() {
    ref_expectation!{
        foo<>, (),
        [], [], [], [], []
    }
    let mut e0 = foo::Expectation::default();
    let mut seq = Sequence::new();
    e0.return_const(());
    e0.times(1);
    e0.in_sequence(&mut seq);

    let mut e1 = foo::Expectation::default();
    e1.return_const(());
    e1.times(1);
    e1.in_sequence(&mut seq);

    e1.call();
    e0.call();
}

#[test]
fn sequence_ok() {
    ref_expectation!{
        foo<>, (),
        [], [], [], [], []
    }
    let mut e0 = foo::Expectation::default();
    let mut seq = Sequence::new();
    e0.return_const(());
    e0.times(1);
    e0.in_sequence(&mut seq);

    let mut e1 = foo::Expectation::default();
    e1.return_const(());
    e1.times(1);
    e1.in_sequence(&mut seq);

    e0.call();
    e1.call();
}

#[test]
fn return_reference() {
    ref_expectation!{
        foo<>, i32,
        [], [], [], [], []
    }
    let mut e = foo::Expectation::default();
    e.return_const(5i32);
    assert_eq!(5i32, *e.call());
}

#[test]
fn return_str() {
    // This Expectation can be used for a method that returns &str
    ref_expectation!{
        foo<>, String,
        [], [], [], [], []
    }
    let mut e = foo::Expectation::default();
    e.return_const("abcd".to_owned());
    assert_eq!("abcd", e.call());
}

#[test]
fn times_any() {
    ref_expectation!{
        foo<>, (),
        [], [], [], [], []
    }
    let mut e = foo::Expectation::default();
    e.return_const(());
    e.times(1);
    e.times_any();
    e.call();
    e.call();
}

#[test]
fn times_ok() {
    ref_expectation!{
        foo<>, (),
        [], [], [], [], []
    }
    let mut e = foo::Expectation::default();
    e.return_const(());
    e.times(2);
    e.call();
    e.call();
}

#[test]
#[should_panic(expected = "Expectation called fewer than 2 times")]
fn times_too_few() {
    ref_expectation!{
        foo<>, (),
        [], [], [], [], []
    }
    let mut e = foo::Expectation::default();
    e.return_const(());
    e.times(2);
    e.call();
}

#[test]
#[should_panic(expected = "Expectation called more than 2 times")]
fn times_too_many() {
    ref_expectation!{
        foo<>, (),
        [], [], [], [], []
    }
    let mut e = foo::Expectation::default();
    e.return_const(());
    e.times(2);
    e.call();
    e.call();
    e.call();
    // Verify that we panic quickly and don't reach code below this point.
    panic!("Shouldn't get here!");
}

#[test]
fn times_range_ok() {
    ref_expectation!{
        foo<>, (),
        [], [], [], [], []
    }
    let mut e0 = foo::Expectation::default();
    e0.return_const(());
    e0.times_range(2..4);
    e0.call();
    e0.call();

    let mut e1 = foo::Expectation::default();
    e1.return_const(());
    e1.times_range(2..4);
    e1.call();
    e1.call();
    e1.call();
}

#[test]
#[should_panic(expected = "Expectation called fewer than 2 times")]
fn times_range_too_few() {
    ref_expectation!{
        foo<>, (),
        [], [], [], [], []
    }
    let mut e = foo::Expectation::default();
    e.return_const(());
    e.times_range(2..4);
    e.call();
}

#[test]
#[should_panic(expected = "Expectation called more than 3 times")]
fn times_range_too_many() {
    ref_expectation!{
        foo<>, (),
        [], [], [], [], []
    }
    let mut e = foo::Expectation::default();
    e.return_const(());
    e.times_range(2..4);
    e.call();
    e.call();
    e.call();
    e.call();
    // Verify that we panic quickly and don't reach code below this point.
    panic!("Shouldn't get here!");
}
