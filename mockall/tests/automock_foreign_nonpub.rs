// vim: tw=80
//! "extern" blocks aren't modules, and shouldn't introduce another layer of
//! "super"s.
//!
//! The tests really just check that the visibility of the functions and
//! expectations functions is correct.
#![deny(warnings)]

#[allow(unused)]
mod outer {
    struct SuperT();

    pub mod inner {
        use mockall::automock;

        #[derive(Debug)]
        pub(crate) struct PubCrateT();
        #[derive(Debug)]
        struct PrivT();

        #[automock(mod mock_ffi;)]
        extern "Rust" {
            pub(crate) fn foo(x: PubCrateT) -> PubCrateT;
            fn bar(x: PrivT) -> PrivT;
            pub(in super) fn baz(x: super::SuperT) -> super::SuperT;
            pub(in crate::outer) fn bang(x: crate::outer::SuperT)
                -> crate::outer::SuperT;
        }

        #[test]
        fn test_bar() {
            let ctx = mock_ffi::bar_context();
            ctx.expect().returning(|_| PrivT());
            unsafe{mock_ffi::bar(PrivT())};
        }
    

        #[test]
        fn test_baz() {
            let ctx = mock_ffi::baz_context();
            ctx.expect().returning(|_| super::SuperT());
            unsafe{mock_ffi::baz(super::SuperT())};
        }
    }

    #[test]
    fn test_bang() {
        let ctx = inner::mock_ffi::bang_context();
        ctx.expect().returning(|_| SuperT());
        unsafe{inner::mock_ffi::bang(SuperT())};
    }
}

#[test]
fn foo() {
    let ctx = outer::inner::mock_ffi::foo_context();
    ctx.expect().returning(|_| outer::inner::PubCrateT());
    unsafe{outer::inner::mock_ffi::foo(outer::inner::PubCrateT())};
}
