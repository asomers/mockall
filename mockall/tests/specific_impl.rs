// vim: ts=80
#![deny(warnings)]

use mockall::*;

trait Bar {
    fn bar(&self);
}
//trait Baz {
    //fn baz<Y: 'static>(&self, y: Y);
//}

mock! {
    pub Foo<T: 'static> {
    }
    impl Bar for Foo<u32> {
        fn bar(&self);
    }
    impl Bar for Foo<i32> {
        fn bar(&self);
    }
    // TODO: support specific impls with generic methods, like this
    //impl Baz for Foo<u32> {
        //fn baz<Y: 'static>(&self, y: Y);
    //}
}

#[test]
fn return_const() {
    let mut mocku = MockFoo::<u32>::new();
    mocku.expect_bar()
        .return_const(());
    let mut mocki = MockFoo::<i32>::new();
    mocki.expect_bar()
        .return_const(());

    mocku.bar();
    mocki.bar();
}

///// Make sure generic methods work with specific impls, too
//#[test]
//fn withf() {
    //let mut mocku = MockFoo::<u32>::new();
    //mocku.expect_baz::<f32>()
        //.withf(|y| y == 3.14159)
        //.return_const(());
    //mocku.baz::<f32>(3.14159);
//}
