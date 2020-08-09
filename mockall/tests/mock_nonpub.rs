// vim: tw=80
//! methods can use non-public types, as long as the object's visibility is
//! compatible.
#![deny(warnings)]

use mockall::*;

#[allow(unused)]
mod outer {
    struct SuperT();
    trait SuperTrait {}

    mod inner {
        use super::super::mock;

        pub(crate) struct PubCrateT();
        struct PrivT();

        mock! {
            Foo {
                fn foo(&self, x: PubCrateT) -> PubCrateT;
                fn bar(&self, x: PrivT) -> PrivT;
                fn baz(&self, x: super::SuperT) -> super::SuperT;
                fn refbaz(&self, x: super::SuperT) -> &super::SuperT;
                fn refmutbaz(&mut self, x: super::SuperT) -> &mut super::SuperT;
                fn staticbaz(x: super::SuperT) -> super::SuperT;
                fn bang(&self, x: crate::outer::SuperT) -> crate::outer::SuperT;
                fn bean(&self, x: self::PrivT) -> self::PrivT;
                fn goo<T: super::SuperTrait + 'static>(t: T);
                fn goo_wc<T>(t: T) where T: super::SuperTrait + 'static;
                fn boob<F: Fn(u32) -> super::SuperT + 'static>(&self, f: F)
                    -> u32;
                fn boobwc<F>(&self, f: F) -> u32
                    where F: Fn(u32) -> super::SuperT + 'static;
            }
        }
    }
}
