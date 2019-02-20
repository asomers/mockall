// vim: tw=80

use mockall::*;

#[test]
fn checkpoint_ok() {
    expectation!{foo<>, i32, [i32], [&x], [x], [p], [i32]}
    let mut e = foo::Expectations::new();
    e.expect()
        .returning(|_| 42)
        .times_range(1..3);
    e.call(0);
    e.checkpoint();
}

#[test]
fn checkpoint_and_expect_again() {
    expectation!{foo<>, i32, [i32], [&x], [x], [p], [i32]}
    let mut e = foo::Expectations::new();
    e.expect()
        .returning(|_| 42)
        .times_range(1..3);
    e.call(0);
    e.checkpoint();

    e.expect()
        .returning(|_| 25);
    assert_eq!(25, e.call(0));
}

#[test]
#[should_panic(expected = "Expectation called fewer than 1 times")]
fn checkpoint_not_yet_satisfied() {
    expectation!{foo<>, i32, [i32], [&x], [x], [p], [i32]}
    let mut e = foo::Expectations::new();
    e.expect()
        .returning(|_| 42)
        .times(1);
    e.checkpoint();
    panic!("Shouldn't get here!");
}

#[test]
#[should_panic(expected = "No matching expectation found")]
fn checkpoint_removes_old_expectations() {
    expectation!{foo<>, i32, [i32], [&x], [x], [p], [i32]}
    let mut e = foo::Expectations::new();
    e.expect()
        .returning(|_| 42)
        .times_range(1..3);
    e.call(0);
    e.checkpoint();
    e.call(0);
    panic!("Shouldn't get here!");
}

#[test]
#[should_panic(expected = "No matching expectation found")]
fn no_expectations() {
    expectation!{foo<>, i32, [i32], [&x], [x], [p], [i32]}
    let e = foo::Expectations::new();
    e.call(5);
}


/// Unlike Mockers, calls should use the oldest matching expectation, if
/// multiple expectations match
#[test]
fn fifo_order() {
    expectation!{foo<>, i32, [i32], [&x], [x], [p], [i32]}
    let mut e = foo::Expectations::new();
    e.expect()
        .with(predicate::eq(5))
        .returning(|_| 99);
    e.expect()
        .with(predicate::always())
        .returning(|_| 42);

    assert_eq!(99, e.call(5));
}

#[test]
fn match_reference() {
    expectation!{foo<>, u32, [&i32], [&x], [x], [p], [i32]}
    let mut e = foo::Expectations::new();
    e.expect()
        .with(predicate::eq(5))
        .returning(|_| 99);

    let x = 5i32;
    assert_eq!(99, e.call(&x));
}

#[test]
#[should_panic(expected = "No matching expectation found")]
fn nothing_matches() {
    expectation!{foo<>, i32, [i32], [&x], [x], [p], [i32]}
    let mut e = foo::Expectations::new();
    e.expect()
        .with(predicate::eq(5))
        .returning(|_| 99);

    e.call(6);
}

#[test]
fn one_match() {
    expectation!{foo<>, i32, [i32], [&x], [x], [p], [i32]}
    let mut e = foo::Expectations::new();
    e.expect()
        .with(predicate::eq(4))
        .returning(|_| 42);
    e.expect()
        .with(predicate::eq(5))
        .returning(|_| 99);

    assert_eq!(42, e.call(4));
}

#[test]
fn ref_expectations() {
    ref_expectation!{foo<>, i32, [i32], [&x], [x], [p], [i32]}
    let mut e = foo::Expectations::new();
    e.expect()
        .with(predicate::eq(4))
        .return_const(42);
    e.expect()
        .with(predicate::eq(5))
        .return_const(99);

    assert_eq!(42, *e.call(4));
}

#[test]
fn ref_mut_expectations() {
    ref_mut_expectation!{
        fn foo<>(x: i32) -> &mut i32 {
            let (p: &i32) = (&x);
        }
    }
    let mut e = foo::Expectations::new();
    e.expect()
        .with(predicate::eq(4))
        .return_var(42);
    e.expect()
        .with(predicate::eq(5))
        .return_var(99);

    assert_eq!(42, *e.call_mut(4));
}

#[test]
#[should_panic(expected = "Method sequence violation")]
fn sequence_fail() {
    expectation!{foo<>, i32, [i32], [&x], [x], [p], [i32]}
    let mut e = foo::Expectations::new();
    let mut seq = Sequence::new();
    e.expect()
        .times(1)
        .in_sequence(&mut seq)
        .with(predicate::eq(1))
        .returning(|_| 42);
    e.expect()
        .times(1)
        .in_sequence(&mut seq)
        .with(predicate::eq(3))
        .returning(|_| 42);
    e.expect()
        .times(1)
        .in_sequence(&mut seq)
        .with(predicate::eq(2))
        .returning(|_| 42);
    e.expect()
        .times(1)
        .in_sequence(&mut seq)
        .with(predicate::eq(4))
        .returning(|_| 42);

    e.call(1);
    e.call(2);
    e.call(3);
    e.call(4);
}

#[test]
fn sequence_ok() {
    expectation!{foo<>, i32, [i32], [&x], [x], [p], [i32]}
    let mut e = foo::Expectations::new();
    let mut seq = Sequence::new();
    e.expect()
        .times(1)
        .in_sequence(&mut seq)
        .with(predicate::eq(1))
        .returning(|_| 42);
    e.expect()
        .times(1)
        .in_sequence(&mut seq)
        .with(predicate::eq(2))
        .returning(|_| 42);
    e.expect()
        .times(1)
        .in_sequence(&mut seq)
        .with(predicate::eq(3))
        .returning(|_| 42);
    e.expect()
        .times(1)
        .in_sequence(&mut seq)
        .with(predicate::eq(4))
        .returning(|_| 42);

    e.call(1);
    e.call(2);
    e.call(3);
    e.call(4);
}

/// When adding multiple calls of a single method, with the same arguments, to a
/// sequence, expectations should not be called after they are done if there are
/// more expectations to follow.
#[test]
fn sequence_of_single_method() {
    expectation!{foo<>, i32, [], [], [], [], []}
    let mut e = foo::Expectations::new();
    let mut seq = Sequence::new();
    e.expect()
        .times(1)
        .in_sequence(&mut seq)
        .returning(|| 1);
    e.expect()
        .times(1)
        .in_sequence(&mut seq)
        .returning(|| 2);
    e.expect()
        .times(1)
        .in_sequence(&mut seq)
        .returning(|| 3);

    assert_eq!(1, e.call());
    assert_eq!(2, e.call());
    assert_eq!(3, e.call());
}

#[test]
#[should_panic(expected = "Expectation called more than 2 times")]
fn times_too_many() {
    expectation!{foo<>, (), [], [], [], [], []}
    let mut e = foo::Expectations::new();
    e.expect()
        .times(2)
        .returning(|| ());
    e.call();
    e.call();
    e.call();
    // Verify that we panic quickly and don't reach code below this point.
    panic!("Shouldn't get here!");
}
