// vim: tw=80

use mockall::*;
use mockall::expectation;
use std::rc::Rc;

#[test]
fn match_eq_ok() {
    expectation!{FooExpectation, __foo_priv, (), [i32], [&x], [x], [p], [i32]}
    let mut e = FooExpectation::default();
    e.returning(|_| ());
    e.with(predicate::eq(5));
    e.call(5);
}

#[test]
#[should_panic]
fn match_eq_fail() {
    expectation!{FooExpectation, __foo_priv, (), [i32], [&x], [x], [p], [i32]}
    let mut e = FooExpectation::default();
    e.returning(|_| ());
    e.with(predicate::eq(4));
    e.call(5);
}

#[test]
fn match_fn_ok() {
    expectation!{FooExpectation, __foo_priv, (), [i32], [&x], [x], [p], [i32]}
    let mut e = FooExpectation::default();
    e.returning(|_| ());
    e.withf(|x: &i32| *x == 5);
    e.call(5);
}

#[test]
#[should_panic]
fn match_fn_fail() {
    expectation!{FooExpectation, __foo_priv, (), [i32], [&x], [x], [p], [i32]}
    let mut e = FooExpectation::default();
    e.returning(|_| ());
    e.withf(|x: &i32| *x == 6);
    e.call(5);
}

mod reference_argument {
    use super::*;

    mockall::expectation!{FooExpectation, __foo_priv, u32,
        [&u32], [&i0], [i0], [p0], [u32]}

    #[test]
    fn t() {
        let mut e = FooExpectation::default();
        e.with(predicate::eq(42u32))
            .returning(|x| *x);
        let x = 42u32;
        assert_eq!(42u32, e.call(&x));
    }

    #[test]
    fn match_fn() {
        let mut e = FooExpectation::default();
        e.withf(|x| *x == 42)
            .returning(|x| *x);
        let x = 42u32;
        assert_eq!(42u32, e.call(&x));
    }
}

mod ref_and_nonref_arguments {
    use super::*;

    mockall::expectation!{FooExpectation, __foo_priv, i32,
        [i32, &u16],
        [&i0, i1],
        [i0, i1],
        [p0, p1],
        [i32, u16]
    }

    #[test]
    fn t() {
        let mut e = FooExpectation::default();
        e.with(predicate::eq(42), predicate::eq(1))
            .returning(|x, y| x + i32::from(*y));
        let x = 42i32;
        let y = 1u16;
        assert_eq!(43i32, e.call(x, &y));
    }

    #[test]
    fn match_fn() {
        let mut e = FooExpectation::default();
        e.withf(|x, y| *x == i32::from(*y))
            .returning(|x, y| x + i32::from(*y));
        let x = 42i32;
        let y = 42u16;
        assert_eq!(84i32, e.call(x, &y));
    }
}

mod reference_arguments {
    use super::*;

    mockall::expectation!{FooExpectation, __foo_priv, u32,
        [&u32, &u16],
        [i0, i1],
        [i0, i1],
        [p0, p1],
        [u32, u16]
    }

    #[test]
    fn t() {
        let mut e = FooExpectation::default();
        e.with(predicate::eq(42), predicate::eq(1))
            .returning(|x, y| *x + u32::from(*y));
        let x = 42u32;
        let y = 1u16;
        assert_eq!(43u32, e.call(&x, &y));
    }
}

/// A MockObject with a method that takes &mut self like:
/// fn foo(&mut self, x: i32) -> u32
#[test]
fn mutable_self() {
    expectation!{FooExpectation, __foo_priv, i32, [i32], [&x], [x], [p], [i32]}
    let mut e = FooExpectation::default();
    let mut count = 0;
    e.returning(move |x| {
            count += x;
            count
        });
    assert_eq!(5, e.call(5));
    assert_eq!(10, e.call(5));
}

///// A mock struct has two different methods with the same name, from different
///// traits.  Neither automock nor mock! can handle this; manual mocking is
///// required.
/////
///// The key is changing the ident names used to store the expectations.  Use
///// "traitname_methodname" as the identifier names, and in the expectation
///// methods' names.
//mod name_conflict {
    //trait Foo {
        //fn meth(&self) -> u32;
    //}
    //trait Bar {
        //fn meth(&self) -> u32;
    //}

    //#[derive(Default)]
    //struct MockA {
        //foo_meth: ::mockall::Expectation<(), u32>,
        //bar_meth: ::mockall::Expectation<(), u32>,
    //}
    //impl MockA {
        //fn expect_foo_meth(&mut self) -> &mut ::mockall::Expectation<(), u32>
        //{
            //self.foo_meth = ::mockall::Expectation::new();
            //&mut self.foo_meth
        //}
        //fn expect_bar_meth(&mut self) -> &mut ::mockall::Expectation<(), u32>
        //{
            //self.bar_meth = ::mockall::Expectation::new();
            //&mut self.bar_meth
        //}
    //}
    //impl Foo for MockA {
        //fn meth(&self) -> u32 {
            //self.foo_meth.call(())
        //}
    //}
    //impl Bar for MockA {
        //fn meth(&self) -> u32 {
            //self.bar_meth.call(())
        //}
    //}

    //#[test]
    //fn t() {
        //let mut mock = MockA::default();
        //mock.expect_foo_meth().returning(|_| 5);
        //mock.expect_bar_meth().returning(|_| 6);
        //assert_eq!(5, Foo::meth(&mock));
        //assert_eq!(6, Bar::meth(&mock));
    //}
//}

#[test]
fn never_ok() {
    expectation!{FooExpectation, __foo_priv, (), [], [], [], [], []}
    let mut e = FooExpectation::default();
    e.returning(|| ());
    e.never();
}

#[test]
#[should_panic(expected = "Expectation should not have been called")]
fn never_fail() {
    expectation!{FooExpectation, __foo_priv, (), [], [], [], [], []}
    let mut e = FooExpectation::default();
    e.returning(|| ());
    e.never();
    e.call();
}

/// A MockObject with a method that has no arguments or returns
/// fn foo(&self)
#[test]
fn no_args_or_returns() {
    expectation!{FooExpectation, __foo_priv, (), [], [], [], [], []}
    let mut e = FooExpectation::default();
    e.returning(|| ());
    e.call();
}

#[test]
fn non_send() {
    expectation!{
        FooExpectation, __foo_priv, Rc<u32>,
        [Rc<u32>], [&x], [x], [p], [Rc<u32>]
    }
    let mut e = FooExpectation::default();
    let y = Rc::new(43u32);
    e.returning_st(move |_| y.clone());
    let x = Rc::new(42u32);
    assert_eq!(43, *e.call(x).as_ref());
}

pub struct MyType(Rc<u32>);
#[test]
fn non_send_nor_clone() {
    expectation!{
        FooExpectation, __foo_priv, MyType,
        [MyType], [&x], [x], [p], [MyType]
    }
    let mut e = FooExpectation::default();
    let y = MyType(Rc::new(43u32));
    e.return_once_st(move |_| y);
    let x = MyType(Rc::new(42u32));
    assert_eq!(43, *e.call(x).0.as_ref());
}

#[test]
fn return_const() {
    expectation!{ FooExpectation, __foo_priv, u32, [], [], [], [], [] }
    let mut e = FooExpectation::default();
    e.return_const(42u32);
    assert_eq!(42, e.call());
}

///// Expectations return O::default() unless otherwise specified
//#[test]
//#[cfg_attr(not(feature = "nightly"), ignore)]
//fn return_default() {
    //let e = Expectation::<(), u32>::default();
    //let r = e.call(());
    //assert_eq!(u32::default(), r);
//}

///// Can't return default for types that aren't Default
//#[test]
//#[should_panic]
//fn return_default_panics_for_non_default_types() {
    //struct NonDefault{}
    //let e = Expectation::<(), NonDefault>::default();
    //e.call(());
//}

pub struct NonCopy{}

#[test]
fn return_owned() {
    expectation!{FooExpectation, __foo_priv, NonCopy, [], [], [], [], []}
    let mut e = FooExpectation::default();
    let r = NonCopy{};
    e.return_once(|| r);
    e.call();
}

#[test]
#[should_panic(expected = "expected only once")]
fn return_owned_too_many_times() {
    expectation!{FooExpectation, __foo_priv, NonCopy, [], [], [], [], []}
    let mut e = FooExpectation::default();
    let r = NonCopy{};
    e.return_once(|| r);
    e.call();
    e.call();
}

#[test]
#[should_panic(expected = "exact call count")]
fn sequence_ambiguous() {
    expectation!{FooExpectation, __foo_priv, (), [], [], [], [], []}
    let mut seq = Sequence::new();
    let mut e0 = FooExpectation::default();
    e0.times_range(1..3);
    e0.in_sequence(&mut seq);
    e0.call();
}

#[test]
#[should_panic(expected = "Method sequence violation")]
fn sequence_fail() {
    expectation!{FooExpectation, __foo_priv, (), [], [], [], [], []}
    let mut seq = Sequence::new();
    let mut e0 = FooExpectation::default();
    e0.times(1);
    e0.returning(|| ());
    e0.in_sequence(&mut seq);

    let mut e1 = FooExpectation::default();
    e1.times(1);
    e1.returning(|| ());
    e1.in_sequence(&mut seq);

    e1.call();
    e0.call();
}

#[test]
fn sequence_ok() {
    expectation!{FooExpectation, __foo_priv, (), [], [], [], [], []}
    let mut seq = Sequence::new();
    let mut e0 = FooExpectation::default();
    e0.times(1);
    e0.returning(|| ());
    e0.in_sequence(&mut seq);

    let mut e1 = FooExpectation::default();
    e1.times(1);
    e1.returning(|| ());
    e1.in_sequence(&mut seq);

    e0.call();
    e1.call();
}

/// A MockObject with a simple method like:
/// fn foo(&self, x: i32) -> u32
#[test]
fn simple_method() {
    expectation!{FooExpectation, __foo_priv, u32, [i32], [&x], [x], [p], [i32]}
    let mut e = FooExpectation::default();
    e.returning(|_| 42);
    let r = e.call(5);
    assert_eq!(42, r);
}

/////// Mocking static methods requires using globals
////mod static_method {
    ////use std::sync::Mutex;
    ////use super::*;

    ////mockall::lazy_static! {
        ////static ref GLOBAL_EXPECTATIONS: Mutex<Expectations<(i32), u32>> = 
            ////Mutex::new(Expectations::new());
    ////}

    /////// The contorted syntax mimics what mockall_derive does.
    ////#[test]
    ////fn t() {
        ////{
            ////let mut eguard = {
                ////let guard = GLOBAL_EXPECTATIONS.lock().unwrap();
                ////ExpectationGuard::new(guard)
            ////};
            ////eguard.returning(|_| 42);
        ////}

        ////assert_eq!(42, GLOBAL_EXPECTATIONS.lock().unwrap().call(5));
    ////}
////}

#[test]
fn times_any() {
    expectation!{FooExpectation, __foo_priv, (), [], [], [], [], []}
    let mut e = FooExpectation::default();
    e.returning(|| ());
    e.times(1);
    e.times_any();
    e.call();
    e.call();
}

#[test]
fn times_ok() {
    expectation!{FooExpectation, __foo_priv, (), [], [], [], [], []}
    let mut e = FooExpectation::default();
    e.returning(|| ());
    e.times(2);
    e.call();
    e.call();
}

#[test]
#[should_panic(expected = "Expectation called fewer than 2 times")]
fn times_too_few() {
    expectation!{FooExpectation, __foo_priv, (), [], [], [], [], []}
    let mut e = FooExpectation::default();
    e.returning(|| ());
    e.times(2);
    e.call();
}

#[test]
#[should_panic(expected = "Expectation called more than 2 times")]
fn times_too_many() {
    expectation!{FooExpectation, __foo_priv, (), [], [], [], [], []}
    let mut e = FooExpectation::default();
    e.returning(|| ());
    e.times(2);
    e.call();
    e.call();
    e.call();
    // Verify that we panic quickly and don't reach code below this point.
    panic!("Shouldn't get here!");
}

#[test]
fn times_range_ok() {
    expectation!{FooExpectation, __foo_priv, (), [], [], [], [], []}
    let mut e0 = FooExpectation::default();
    e0.returning(|| ());
    e0.times_range(2..4);
    e0.call();
    e0.call();

    let mut e1 = FooExpectation::default();
    e1.returning(|| ());
    e1.times_range(2..4);
    e1.call();
    e1.call();
    e1.call();
}

#[test]
#[should_panic(expected = "Expectation called fewer than 2 times")]
fn times_range_too_few() {
    expectation!{FooExpectation, __foo_priv, (), [], [], [], [], []}
    let mut e = FooExpectation::default();
    e.returning(|| ());
    e.times_range(2..4);
    e.call();
}

#[test]
#[should_panic(expected = "Expectation called more than 3 times")]
fn times_range_too_many() {
    expectation!{FooExpectation, __foo_priv, (), [], [], [], [], []}
    let mut e = FooExpectation::default();
    e.returning(|| ());
    e.times_range(2..4);
    e.call();
    e.call();
    e.call();
    e.call();
    // Verify that we panic quickly and don't reach code below this point.
    panic!("Shouldn't get here!");
}

mod xxx {
}
