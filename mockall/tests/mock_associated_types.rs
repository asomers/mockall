// vim: tw=80

use mockall::*;

mock! {
    MyIter {}
    trait Iterator {
        type Item=u32;

        fn next(&mut self) -> Option<u32>;
    }
}

#[test]
fn returning() {
    let mut mock = MockMyIter::new();
    mock.expect_next()
        .returning(|| Some(5));
    assert_eq!(5, mock.next().unwrap());
}

