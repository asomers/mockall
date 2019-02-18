// vim: tw=80

use mockall::*;

#[test]
fn match_eq_ok() {
    ref_mut_expectation!{
        FooExpectation, __foo_priv, i32,
        [i32], [&i], [i], [p], [i32]
    }
    let mut e = FooExpectation::default();
    e.return_var(99i32);
    e.with(predicates::ord::eq(5));
    e.call_mut(5);
}

#[test]
#[should_panic]
fn match_eq_fail() {
    ref_mut_expectation!{
        FooExpectation, __foo_priv, i32,
        [i32], [&i], [i], [p], [i32]
    }
    let mut e = FooExpectation::default();
    e.return_var(99i32);
    e.with(predicates::ord::eq(4));
    e.call_mut(5);
}

#[test]
fn never_ok() {
    ref_mut_expectation!{
        FooExpectation, __foo_priv, (),
        [], [], [], [], []
    }
    let mut e = FooExpectation::default();
    e.returning(|| ());
    e.never();
}

#[test]
#[should_panic(expected = "Expectation should not have been called")]
fn never_fail() {
    ref_mut_expectation!{
        FooExpectation, __foo_priv, (),
        [], [], [], [], []
    }
    let mut e = FooExpectation::default();
    e.returning(|| ());
    e.never();
    e.call_mut();
}

#[test]
fn return_mutable_reference() {
    ref_mut_expectation!{
        FooExpectation, __foo_priv, i32,
        [], [], [], [], []
    }
    let mut e = FooExpectation::default();
    e.returning(|| 5i32);
    assert_eq!(5i32, *e.call_mut());
}

#[test]
fn return_mutable_reference_return_var() {
    ref_mut_expectation!{
        FooExpectation, __foo_priv, i32,
        [], [], [], [], []
    }
    let mut e = FooExpectation::default();
    e.return_var(5i32);
    assert_eq!(5i32, *e.call_mut());
}

#[test]
#[should_panic(expected = "Method sequence violation")]
fn sequence_fail() {
    ref_mut_expectation!{
        FooExpectation, __foo_priv, (),
        [], [], [], [], []
    }
    let mut seq = Sequence::new();
    let mut e0 = FooExpectation::default();
    e0.returning(|| ());
    e0.times(1);
    e0.in_sequence(&mut seq);

    let mut e1 = FooExpectation::default();
    e1.returning(|| ());
    e1.times(1);
    e1.in_sequence(&mut seq);

    e1.call_mut();
    e0.call_mut();
}

#[test]
fn sequence_ok() {
    ref_mut_expectation!{
        FooExpectation, __foo_priv, (),
        [], [], [], [], []
    }
    let mut seq = Sequence::new();
    let mut e0 = FooExpectation::default();
    e0.returning(|| ());
    e0.times(1);
    e0.in_sequence(&mut seq);

    let mut e1 = FooExpectation::default();
    e1.returning(|| ());
    e1.times(1);
    e1.in_sequence(&mut seq);

    e0.call_mut();
    e1.call_mut();
}

#[test]
fn times_any() {
    ref_mut_expectation!{
        FooExpectation, __foo_priv, (),
        [], [], [], [], []
    }
    let mut e = FooExpectation::default();
    e.returning(|| ());
    e.times(1);
    e.times_any();
    e.call_mut();
    e.call_mut();
}

#[test]
fn times_ok() {
    ref_mut_expectation!{
        FooExpectation, __foo_priv, (),
        [], [], [], [], []
    }
    let mut e = FooExpectation::default();
    e.returning(|| ());
    e.times(2);
    e.call_mut();
    e.call_mut();
}

#[test]
#[should_panic(expected = "Expectation called fewer than 2 times")]
fn times_too_few() {
    ref_mut_expectation!{
        FooExpectation, __foo_priv, (),
        [], [], [], [], []
    }
    let mut e = FooExpectation::default();
    e.returning(|| ());
    e.times(2);
    e.call_mut();
}

#[test]
#[should_panic(expected = "Expectation called more than 2 times")]
fn times_too_many() {
    ref_mut_expectation!{
        FooExpectation, __foo_priv, (),
        [], [], [], [], []
    }
    let mut e = FooExpectation::default();
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
    ref_mut_expectation!{
        FooExpectation, __foo_priv, (),
        [], [], [], [], []
    }
    let mut e0 = FooExpectation::default();
    e0.returning(|| ());
    e0.times_range(2..4);
    e0.call_mut();
    e0.call_mut();

    let mut e1 = FooExpectation::default();
    e1.returning(|| ());
    e1.times_range(2..4);
    e1.call_mut();
    e1.call_mut();
    e1.call_mut();
}

#[test]
#[should_panic(expected = "Expectation called fewer than 2 times")]
fn times_range_too_few() {
    ref_mut_expectation!{
        FooExpectation, __foo_priv, (),
        [], [], [], [], []
    }
    let mut e = FooExpectation::default();
    e.returning(|| ());
    e.times_range(2..4);
    e.call_mut();
}

#[test]
#[should_panic(expected = "Expectation called more than 3 times")]
fn times_range_too_many() {
    ref_mut_expectation!{
        FooExpectation, __foo_priv, (),
        [], [], [], [], []
    }
    let mut e = FooExpectation::default();
    e.returning(|| ());
    e.times_range(2..4);
    e.call_mut();
    e.call_mut();
    e.call_mut();
    e.call_mut();
    // Verify that we panic quickly and don't reach code below this point.
    panic!("Shouldn't get here!");
}
