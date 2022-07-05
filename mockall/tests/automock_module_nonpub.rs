// vim: tw=80
//! bare functions can use non-public types, as long as the object's visibility is compatible.
#![deny(warnings)]
#![allow(dead_code)]

mod outer {
    struct SuperT();

    mod inner {
        use mockall::automock;

        pub(crate) struct PubCrateT();
        struct PrivT();

        #[automock]
        mod m {
            use super::*;

            pub(crate) fn foo(_x: PubCrateT) -> PubCrateT {
                unimplemented!()
            }
            pub(super) fn bar(_x: PrivT) -> PrivT {
                unimplemented!()
            }
            pub(in super::super) fn baz(_x: super::super::SuperT)
                -> super::super::SuperT
            {
                unimplemented!()
            }
            pub(in crate::outer) fn bang(_x: crate::outer::SuperT)
                -> crate::outer::SuperT
            {
                unimplemented!()
            }
        }

        #[test]
        fn returning() {
            let ctx = mock_m::foo_context();
            ctx.expect()
                .returning(|x| x);
            mock_m::foo(PubCrateT());
        }
    }
}
