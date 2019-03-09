// vim: tw=80

use mockall::*;

mod generic {
    use super::*;

    #[test]
    fn generic_argument_and_return() {
        expectation!{
            pub fn foo<I, O>(&mut self, x: I) -> &mut O { let (p: &I) = (&x); }
        }
        let mut e = foo::Expectation::<i16, i32>::default();
        e.with(predicate::eq(4))
            .return_var(4);

        assert_eq!(4i32, *e.call_mut(4i16));
    }

    #[test]
    fn generic_return() {
        expectation!{
            pub fn foo<O>(&mut self, x: u32) -> &mut O { let (p: &u32) = (&x); }
        }
        let mut e = foo::Expectation::<i32>::default();
        e.with(predicate::eq(4))
            .return_var(-42);

        assert_eq!(-42, *e.call_mut(4));
    }
}

#[test]
fn match_eq_ok() {
    expectation!{
        pub fn foo<>(&mut self, i: i32) -> &mut i32 { let (p: &i32) = (&i); }
    }
    let mut e = foo::Expectation::default();
    e.return_var(99i32);
    e.with(predicates::ord::eq(5));
    e.call_mut(5);
}

#[test]
#[should_panic]
fn match_eq_fail() {
    expectation!{
        pub fn foo<>(&mut self, i: i32) -> &mut i32 { let (p: &i32) = (&i); }
    }
    let mut e = foo::Expectation::default();
    e.return_var(99i32);
    e.with(predicates::ord::eq(4));
    e.call_mut(5);
}

#[test]
fn never_ok() {
    expectation!{
        pub fn foo<>(&mut self, ) -> &mut () { let () = (); }
    }
    let mut e = foo::Expectation::default();
    e.returning(|| ());
    e.never();
}

#[test]
#[should_panic(expected = "Expectation should not have been called")]
fn never_fail() {
    expectation!{ pub fn foo<>(&mut self, ) -> &mut () { let () = (); } }
    let mut e = foo::Expectation::default();
    e.returning(|| ());
    e.never();
    e.call_mut();
}

mod reference_argument {
    use super::*;

    expectation!{
        pub fn foo<>(&mut self, i: &u32) -> &mut () { let (p: &u32) = (i); }
    }

    #[test]
    fn t() {
        let mut e = foo::Expectation::default();
        e.with(predicate::eq(42u32))
            .return_var(());
        let x = 42u32;
        e.call_mut(&x);
    }

    #[test]
    fn match_fn() {
        let mut e = foo::Expectation::default();
        e.withf(|x| *x == 42)
            .return_var(());
        let x = 42u32;
        e.call_mut(&x);
    }
}

mod ref_and_nonref_arguments {
    use super::*;

    expectation!{
        pub fn foo<>(&mut self, i0: i32, i1: &u16) -> &mut () {
            let (p0: &i32, p1: &u16) = (&i0, i1);
        }
    }

    #[test]
    fn t() {
        let mut e = foo::Expectation::default();
        e.with(predicate::eq(42), predicate::eq(1))
            .return_var(());
        let x = 42i32;
        let y = 1u16;
        e.call_mut(x, &y);
    }

    #[test]
    fn match_fn() {
        let mut e = foo::Expectation::default();
        e.withf(|x, y| *x == i32::from(*y))
            .return_var(());
        let x = 42i32;
        let y = 42u16;
        e.call_mut(x, &y);
    }
}

mod reference_arguments {
    use super::*;

    expectation!{
        pub fn foo<>(&mut self, i0: &u32, i1: &u16) -> &mut () {
            let (p0: &u32, p1: &u16) = (i0, i1);
        }
    }

    #[test]
    fn t() {
        let mut e = foo::Expectation::default();
        e.with(predicate::eq(42), predicate::eq(1))
            .return_var(());
        let x = 42u32;
        let y = 1u16;
        e.call_mut(&x, &y);
    }
}

#[test]
fn return_mutable_reference() {
    expectation!{ pub fn foo<>(&mut self, ) -> &mut i32 { let () = (); } }
    let mut e = foo::Expectation::default();
    e.returning(|| 5i32);
    assert_eq!(5i32, *e.call_mut());
}

#[test]
fn return_mutable_reference_return_var() {
    expectation!{ pub fn foo<>(&mut self, ) -> &mut i32 { let () = (); } }
    let mut e = foo::Expectation::default();
    e.return_var(5i32);
    assert_eq!(5i32, *e.call_mut());
}

#[test]
#[should_panic(expected = "Method sequence violation")]
fn sequence_fail() {
    expectation!{ pub fn foo<>(&mut self, ) -> &mut () { let () = (); } }
    let mut seq = Sequence::new();
    let mut e0 = foo::Expectation::default();
    e0.returning(|| ());
    e0.times(1);
    e0.in_sequence(&mut seq);

    let mut e1 = foo::Expectation::default();
    e1.returning(|| ());
    e1.times(1);
    e1.in_sequence(&mut seq);

    e1.call_mut();
    e0.call_mut();
}

#[test]
fn sequence_ok() {
    expectation!{ pub fn foo<>(&mut self, ) -> &mut () { let () = (); } }
    let mut seq = Sequence::new();
    let mut e0 = foo::Expectation::default();
    e0.returning(|| ());
    e0.times(1);
    e0.in_sequence(&mut seq);

    let mut e1 = foo::Expectation::default();
    e1.returning(|| ());
    e1.times(1);
    e1.in_sequence(&mut seq);

    e0.call_mut();
    e1.call_mut();
}

#[test]
fn times_any() {
    expectation!{ pub fn foo<>(&mut self, ) -> &mut () { let () = (); } }
    let mut e = foo::Expectation::default();
    e.returning(|| ());
    e.times(1);
    e.times_any();
    e.call_mut();
    e.call_mut();
}

#[test]
fn times_ok() {
    expectation!{ pub fn foo<>(&mut self, ) -> &mut () { let () = (); } }
    let mut e = foo::Expectation::default();
    e.returning(|| ());
    e.times(2);
    e.call_mut();
    e.call_mut();
}

#[test]
#[should_panic(expected = "Expectation called fewer than 2 times")]
fn times_too_few() {
    expectation!{ pub fn foo<>(&mut self, ) -> &mut () { let () = (); } }
    let mut e = foo::Expectation::default();
    e.returning(|| ());
    e.times(2);
    e.call_mut();
}

#[test]
#[should_panic(expected = "Expectation called more than 2 times")]
fn times_too_many() {
    expectation!{ pub fn foo<>(&mut self, ) -> &mut () { let () = (); } }
    let mut e = foo::Expectation::default();
    e.returning(|| ());
    e.times(2);
    e.call_mut();
    e.call_mut();
    e.call_mut();
    // Verify that we panic quickly and don't reach code below this point.
    panic!("Shouldn't get here!");
}

#[test]
fn times_range_ok() {
    expectation!{ pub fn foo<>(&mut self, ) -> &mut () { let () = (); } }
    let mut e0 = foo::Expectation::default();
    e0.returning(|| ());
    e0.times_range(2..4);
    e0.call_mut();
    e0.call_mut();

    let mut e1 = foo::Expectation::default();
    e1.returning(|| ());
    e1.times_range(2..4);
    e1.call_mut();
    e1.call_mut();
    e1.call_mut();
}

#[test]
#[should_panic(expected = "Expectation called fewer than 2 times")]
fn times_range_too_few() {
    expectation!{ pub fn foo<>(&mut self, ) -> &mut () { let () = (); } }
    let mut e = foo::Expectation::default();
    e.returning(|| ());
    e.times_range(2..4);
    e.call_mut();
}

#[test]
#[should_panic(expected = "Expectation called more than 3 times")]
fn times_range_too_many() {
    expectation!{ pub fn foo<>(&mut self, ) -> &mut () { let () = (); } }
    let mut e = foo::Expectation::default();
    e.returning(|| ());
    e.times_range(2..4);
    e.call_mut();
    e.call_mut();
    e.call_mut();
    e.call_mut();
    // Verify that we panic quickly and don't reach code below this point.
    panic!("Shouldn't get here!");
}
