// vim: tw=80
//! Invoking `mock!` and `#[automock]` inside a `macro_rules!` macro with `:ty`
//! arguments.  Regression test for issue #681.
#![deny(warnings)]

use mockall::*;

pub trait SampleTrait: Sync {
    fn get(&self) -> i32;
}

impl SampleTrait for i32 {
    fn get(&self) -> i32 {
        *self
    }
}

macro_rules! make_mock {
    (
        $mock_name:ident {
            $(
                $(#[$attr:meta])*
                pub fn $method:ident $(<$($gen:ident : $bound:path),+>)? (
                    & $(mut $($mut_dummy:lifetime)?)? self $(, $param:ident: $param_ty:ty)*
                ) $(-> $ret:ty)?;
            )*
        }
        $(
            impl $trait_name:ident for $self_ty:ty {
                $(
                    fn $tmethod:ident(&self $(, $tparam:ident: $tparam_ty:ty)*) $(-> $tret:ty)?;
                )*
            }
        )?
    ) => {
        ::mockall::mock! {
            pub $mock_name {
                $(
                    $(#[$attr])*
                    pub fn $method $(<$($gen : $bound),+>)? (
                        & $(mut $($mut_dummy)?)? self $(, $param: $param_ty)*
                    ) $(-> $ret)?;
                )*
            }
            $(
                impl $trait_name for $self_ty {
                    $(
                        fn $tmethod(&self $(, $tparam: $tparam_ty)*) $(-> $tret)?;
                    )*
                }
            )?
        }
    };
}

macro_rules! make_borrowed_mock {
    ($mock_name:ident, $inner_ty:ty) => {
        ::mockall::mock! {
            pub $mock_name {
                pub fn by_ref(&self, item: &$inner_ty) -> i32;
            }
        }
    };
}

macro_rules! make_closure_mock {
    ($mock_name:ident, $fn_ty:ty) => {
        ::mockall::mock! {
            pub $mock_name {
                pub fn call<F: Fn(i32) -> i32 + 'static>(&self, f: $fn_ty) -> i32;
            }
        }
    };
}

macro_rules! make_automock_trait {
    ($trait_name:ident, $arg_ty:ty, $ret_ty:ty) => {
        #[automock]
        pub trait $trait_name {
            fn process(&self, arg: $arg_ty) -> $ret_ty;
        }
    };
}

make_mock! {
    Example {
        pub fn example(&self, number: u32);
        pub fn add(&self, a: i32, b: i32) -> i32;
        pub fn ref_args(&self, x: &u32, s: &str) -> bool;
        pub fn dyn_arg(&self, d: &dyn SampleTrait) -> i32;
        pub fn ret_ref(&self) -> &u32;
        pub fn ret_str(&self) -> &str;
        pub fn ret_slice(&self) -> &[u8];
        pub fn ret_dyn(&self) -> &dyn SampleTrait;
        pub fn ret_static_dyn(&self) -> &'static dyn SampleTrait;
        pub fn ret_mut(&mut self) -> &mut u32;
        pub fn ret_impl(&self) -> impl SampleTrait;
        #[concretize]
        pub fn concretized<P: AsRef<str>>(&self, p: P, slice: &[P]) -> usize;
    }
    impl SampleTrait for Example {
        fn get(&self) -> i32;
    }
}

make_borrowed_mock!(BorrowedDyn, dyn SampleTrait);
make_closure_mock!(ClosureExample, F);
make_automock_trait!(AutoTrait, &str, &str);

#[test]
fn value_and_return() {
    let mut mock = MockExample::new();
    mock.expect_example()
        .with(predicate::eq(42))
        .return_const(());
    mock.expect_add()
        .with(predicate::eq(20), predicate::eq(22))
        .returning(|a, b| a + b);

    mock.example(42);
    assert_eq!(42, mock.add(20, 22));
}

#[test]
fn reference_arguments() {
    let mut mock = MockExample::new();
    mock.expect_ref_args()
        .with(predicate::eq(7), predicate::eq("abc"))
        .return_const(true);

    assert!(mock.ref_args(&7, "abc"));
}

#[test]
fn trait_object_arguments() {
    let mut mock = MockExample::new();
    mock.expect_dyn_arg()
        .withf(|d| d.get() == 11)
        .return_const(11);

    let val1 = 11i32;
    assert_eq!(11, mock.dyn_arg(&val1));
}

#[test]
fn borrowed_macro_ty_trait_object() {
    let mut mock = MockBorrowedDyn::new();
    mock.expect_by_ref()
        .withf(|d| d.get() == 5)
        .return_const(50);

    let v1 = 5i32;
    assert_eq!(50, mock.by_ref(&v1));
}

#[test]
fn reference_and_impl_returns() {
    let mut mock = MockExample::new();
    mock.expect_ret_ref().return_const(99u32);
    mock.expect_ret_str().return_const("hello".to_string());
    mock.expect_ret_slice().return_const(vec![1u8, 2, 3]);
    mock.expect_ret_dyn()
        .return_const(Box::new(77i32) as Box<dyn SampleTrait>);
    mock.expect_ret_static_dyn()
        .return_const(&88i32 as &'static dyn SampleTrait);
    mock.expect_ret_mut().return_var(10u32);
    mock.expect_ret_impl().returning(|| Box::new(55i32));

    assert_eq!(&99, mock.ret_ref());
    assert_eq!("hello", mock.ret_str());
    assert_eq!(&[1u8, 2, 3][..], mock.ret_slice());
    assert_eq!(77, mock.ret_dyn().get());
    assert_eq!(88, mock.ret_static_dyn().get());
    *mock.ret_mut() += 5;
    assert_eq!(&mut 15, mock.ret_mut());
    assert_eq!(55, mock.ret_impl().get());
}

#[test]
fn concretize_closure_and_trait_impl() {
    let mut mock = MockExample::new();
    mock.expect_concretized()
        .returning(|p, slice| p.as_ref().len() + slice.len());
    mock.expect_get().return_const(123);

    let mut cmock = MockClosureExample::new();
    cmock.expect_call().returning(|f| f(21));

    assert_eq!(42, cmock.call(|x| x * 2));
    assert_eq!(5, mock.concretized("abc", &["x", "y"]));
    assert_eq!(123, SampleTrait::get(&mock));
}

#[test]
fn automock_in_macro_rules() {
    let mut mock = MockAutoTrait::new();
    mock.expect_process()
        .with(predicate::eq("in"))
        .return_const("out".to_string());

    assert_eq!("out", mock.process("in"));
}
