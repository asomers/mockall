// vim: tw=80

use mockall::*;

mod generic {
    use super::*;

    #[test]
    fn generic_argument_and_return() {
        expectation!{
            pub fn foo<I, O>(&self, i: I) -> &O {
                let (p: &I) = (&i);
            }
        }
        let mut e = foo::Expectation::<i16, i32>::default();
        e.with(predicate::eq(4))
            .return_const(4);

        assert_eq!(4i32, *e.call(4i16));
    }

    #[test]
    fn generic_return() {
        expectation!{
            pub fn foo<O>(&self, x: u32) -> &O {
                let (p: &u32) = (&x);
            }
        }
        let mut e = foo::Expectation::<i32>::default();
        e.with(predicate::eq(4))
            .return_const(-42);

        assert_eq!(-42, *e.call(4));
    }
}

#[test]
fn match_eq_ok() {
    expectation!{ pub fn foo<>(&self, x: i32) -> &() { let (p: &i32) = (&x); } }
    let mut e = foo::Expectation::default();
    e.return_const(());
    e.with(predicate::eq(5));
    e.call(5);
}

#[test]
#[should_panic]
fn match_eq_fail() {
    expectation!{ pub fn foo<>(&self, x: i32) -> &() { let (p: &i32) = (&x); } }
    let mut e = foo::Expectation::default();
    e.return_const(());
    e.with(predicate::eq(4));
    e.call(5);
}

#[test]
fn never_ok() {
    expectation!{ pub fn foo<>(&self, ) -> &() { let () = (); } }
    let mut e = foo::Expectation::default();
    e.return_const(());
    e.never();
}

#[test]
#[should_panic(expected = "Expectation should not have been called")]
fn never_fail() {
    expectation!{ pub fn foo<>(&self, ) -> &() { let () = (); } }
    let mut e = foo::Expectation::default();
    e.return_const(());
    e.never();
    e.call();
}

mod reference_argument {
    use super::*;

    expectation!{ pub fn foo<>(&self, i: &u32) -> &() { let (p: &u32) = (i); } }

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

    expectation!{
        pub fn foo<>(&self, i0: i32, i1: &u16) -> &() {
            let (p0: &i32, p1: &u16) = (&i0, i1);
        }
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

    expectation!{
        pub fn foo<>(&self, i0: &u32, i1: &u16) -> &() {
            let (p0: &u32, p1: &u16) = (i0, i1);
        }
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
    expectation!{pub fn foo<>(&self, ) -> &() { let () = (); }}
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
    expectation!{pub fn foo<>(&self, ) -> &() { let () = (); }}
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
    expectation!{pub fn foo<>(&self, ) -> &i32 { let () = (); }}
    let mut e = foo::Expectation::default();
    e.return_const(5i32);
    assert_eq!(5i32, *e.call());
}

#[test]
fn return_str() {
    // This Expectation can be used for a method that returns &str
    expectation!{pub fn foo<>(&self, ) -> &String { let () = (); }}
    let mut e = foo::Expectation::default();
    e.return_const("abcd".to_owned());
    assert_eq!("abcd", e.call());
}

#[test]
fn times_any() {
    expectation!{pub fn foo<>(&self, ) -> &() { let () = (); }}
    let mut e = foo::Expectation::default();
    e.return_const(());
    e.times(1);
    e.times_any();
    e.call();
    e.call();
}

#[test]
fn times_ok() {
    expectation!{pub fn foo<>(&self, ) -> &() { let () = (); }}
    let mut e = foo::Expectation::default();
    e.return_const(());
    e.times(2);
    e.call();
    e.call();
}

#[test]
#[should_panic(expected = "Expectation called fewer than 2 times")]
fn times_too_few() {
    expectation!{pub fn foo<>(&self, ) -> &() { let () = (); }}
    let mut e = foo::Expectation::default();
    e.return_const(());
    e.times(2);
    e.call();
}

#[test]
#[should_panic(expected = "Expectation called more than 2 times")]
fn times_too_many() {
    expectation!{pub fn foo<>(&self, ) -> &() { let () = (); }}
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
    expectation!{pub fn foo<>(&self, ) -> &() { let () = (); }}
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
    expectation!{pub fn foo<>(&self, ) -> &() { let () = (); }}
    let mut e = foo::Expectation::default();
    e.return_const(());
    e.times_range(2..4);
    e.call();
}

#[test]
#[should_panic(expected = "Expectation called more than 3 times")]
fn times_range_too_many() {
    expectation!{pub fn foo<>(&self, ) -> &() { let () = (); }}
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
