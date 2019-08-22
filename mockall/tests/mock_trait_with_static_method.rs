// vim: tw=80

use mockall::*;

trait Bar {
    fn baz(x: u32) -> u64;
}

mock!{
    pub Foo {}
    trait Bar {
        fn baz(x: u32) -> u64;
    }
}

#[test]
fn returning() {
    let ctx = MockFoo::baz_context();
    ctx.expect()
        .returning(|x| u64::from(x + 1));
    assert_eq!(42, MockFoo::baz(41));
}
