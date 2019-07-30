// vim: tw=80
//! methods can use non-public types, as long as the object's visibility is
//! compatible.

use mockall::*;

#[allow(unused)]
mod outer {
    struct SuperT();

    mod inner {
        use super::super::mock;

        pub(crate) struct PubCrateT();
        struct PrivT();

        mock! {
            Foo {
                fn foo(&self, x: PubCrateT) -> PubCrateT;
                fn bar(&self, x: PrivT) -> PrivT;
                fn baz(&self, x: super::SuperT) -> super::SuperT;
                fn bang(&self, x: crate::outer::SuperT) -> crate::outer::SuperT;
                fn bean(&self, x: self::PrivT) -> self::PrivT;
            }
        }
    }
}
